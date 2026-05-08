# ============================================================
# REGRESSION LOGISTIQUE PENALISEE : RIDGE, LASSO, ELASTIC NET
# Projet STA201
# ============================================================

# Objectif :
# - comparer une regression logistique ridge, lasso et elastic net ;
# - choisir le modele et le seuil uniquement sur validation ;
# - evaluer une seule fois le modele retenu sur test.

# ============================================================
# 0. PACKAGES ET DONNEES
# ============================================================

required_packages <- c(
  "tidyverse",
  "rsample",
  "caret",
  "pROC",
  "glmnet",
  "broom",
  "countrycode"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

if (!exists("df_engineered")) {
  source("scripts/01-import.R")
  source("scripts/02-data_engenering.R")
}

# ============================================================
# 1. BASE DE MODELISATION
# ============================================================

df_model_penalized <- df_engineered |>
  dplyr::select(
    is_canceled,
    hotel,
    market_segment,
    customer_type,
    lead_time_cat,
    total_nights_cat,
    adr_cat,
    has_children,
    engagement_cat,
    previous_cancellations_cat,
    saison_tourisme,
    identity_location
  ) |>
  tidyr::drop_na() |>
  mutate(
    is_canceled = factor(is_canceled, levels = c("Non", "Oui")),
    across(-is_canceled, as.factor)
  )

# Une matrice de design est necessaire pour glmnet.
# Les variables qualitatives sont transformees en indicatrices.
model_formula <- is_canceled ~ .

# ============================================================
# 2. DECOUPAGE TRAIN / VALIDATION / TEST
# ============================================================

set.seed(123)

split_1 <- rsample::initial_split(
  df_model_penalized,
  prop = 0.50,
  strata = is_canceled
)

train_penalized <- rsample::training(split_1)
temp_penalized <- rsample::testing(split_1)

split_2 <- rsample::initial_split(
  temp_penalized,
  prop = 0.50,
  strata = is_canceled
)

validation_penalized <- rsample::training(split_2)
test_penalized <- rsample::testing(split_2)

make_x <- function(data) {
  model.matrix(model_formula, data = data)[, -1, drop = FALSE]
}

make_y <- function(data) {
  ifelse(data$is_canceled == "Oui", 1, 0)
}

map_dummy_to_variable <- function(dummy_names, original_vars) {
  purrr::map_chr(
    dummy_names,
    \(dummy_name) {
      matched_var <- original_vars[
        purrr::map_lgl(original_vars, \(var) stringr::str_starts(dummy_name, var))
      ]

      if (length(matched_var) == 0) {
        dummy_name
      } else {
        matched_var[which.max(nchar(matched_var))]
      }
    }
  )
}

x_train <- make_x(train_penalized)
x_validation <- make_x(validation_penalized)
x_test <- make_x(test_penalized)

y_train <- make_y(train_penalized)
y_validation <- make_y(validation_penalized)
y_test <- make_y(test_penalized)

# Securite : garder les memes colonnes dans les trois matrices.
x_validation <- x_validation[, colnames(x_train), drop = FALSE]
x_test <- x_test[, colnames(x_train), drop = FALSE]

original_predictors <- setdiff(names(df_model_penalized), "is_canceled")

# ============================================================
# 3. FONCTIONS D'EVALUATION
# ============================================================

evaluate_threshold <- function(threshold, probs, truth_factor) {
  pred <- if_else(probs >= threshold, "Oui", "Non") |>
    factor(levels = c("Non", "Oui"))

  cm <- caret::confusionMatrix(
    pred,
    truth_factor,
    positive = "Oui"
  )

  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  specificity <- as.numeric(cm$byClass["Specificity"])

  f1_score <- if_else(
    is.na(precision + recall) | (precision + recall == 0),
    NA_real_,
    2 * precision * recall / (precision + recall)
  )

  tibble(
    threshold = threshold,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    sensitivity = recall,
    specificity = specificity,
    precision = precision,
    f1_score = f1_score,
    balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE),
    gap_sens_spec = abs(recall - specificity)
  )
}

choose_threshold <- function(probs, truth_factor) {
  threshold_results <- purrr::map_dfr(
    seq(0.10, 0.90, by = 0.01),
    evaluate_threshold,
    probs = probs,
    truth_factor = truth_factor
  )

  list(
    all = threshold_results,
    best_balanced = threshold_results |>
      arrange(desc(balanced_accuracy), gap_sens_spec) |>
      slice(1),
    best_f1 = threshold_results |>
      arrange(desc(f1_score), desc(sensitivity)) |>
      slice(1)
  )
}

evaluate_predictions <- function(probs, truth_factor, threshold, model_name, sample_name) {
  pred <- if_else(probs >= threshold, "Oui", "Non") |>
    factor(levels = c("Non", "Oui"))

  cm <- caret::confusionMatrix(
    pred,
    truth_factor,
    positive = "Oui"
  )

  roc_obj <- pROC::roc(
    response = truth_factor,
    predictor = probs,
    levels = c("Non", "Oui"),
    quiet = TRUE
  )

  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  specificity <- as.numeric(cm$byClass["Specificity"])

  metrics <- tibble(
    model = model_name,
    sample = sample_name,
    threshold = threshold,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    auc = as.numeric(pROC::auc(roc_obj)),
    sensitivity = recall,
    specificity = specificity,
    precision = precision,
    f1_score = if_else(
      is.na(precision + recall) | (precision + recall == 0),
      NA_real_,
      2 * precision * recall / (precision + recall)
    ),
    balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE)
  )

  list(
    metrics = metrics,
    confusion = cm,
    roc = roc_obj,
    predictions = pred,
    probabilities = probs
  )
}

# ============================================================
# 4. ENTRAINEMENT RIDGE / LASSO / ELASTIC NET
# ============================================================

penalized_grid <- tribble(
  ~model, ~alpha,
  "ridge", 0.00,
  "lasso", 1.00,
  "elastic_net_025", 0.25,
  "elastic_net_050", 0.50,
  "elastic_net_075", 0.75
)

fit_penalized_model <- function(model, alpha) {
  set.seed(123)

  cv_fit <- glmnet::cv.glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = alpha,
    type.measure = "auc",
    nfolds = 5,
    standardize = TRUE
  )

  prob_validation <- as.numeric(
    predict(
      cv_fit,
      newx = x_validation,
      s = "lambda.1se",
      type = "response"
    )
  )

  threshold_choice <- choose_threshold(
    probs = prob_validation,
    truth_factor = validation_penalized$is_canceled
  )

  validation_eval <- evaluate_predictions(
    probs = prob_validation,
    truth_factor = validation_penalized$is_canceled,
    threshold = threshold_choice$best_f1$threshold,
    model_name = model,
    sample_name = "validation"
  )

  list(
    model = model,
    alpha = alpha,
    cv_fit = cv_fit,
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se,
    threshold_choice = threshold_choice,
    validation_eval = validation_eval
  )
}

penalized_fits <- purrr::pmap(
  penalized_grid,
  fit_penalized_model
)

names(penalized_fits) <- penalized_grid$model

penalized_validation_metrics <- purrr::map_dfr(
  penalized_fits,
  \(x) x$validation_eval$metrics
) |>
  arrange(desc(auc), desc(f1_score), desc(balanced_accuracy))

penalized_validation_metrics

# ============================================================
# 5. VARIABLES, IMPORTANCE ET AIC DES MODELES
# ============================================================

extract_penalized_coefficients <- function(fit_object) {
  coefficients_table <- coef(
    fit_object$cv_fit,
    s = "lambda.1se"
  ) |>
    as.matrix() |>
    as.data.frame() |>
    tibble::rownames_to_column("dummy_variable")

  names(coefficients_table)[2] <- "coefficient"

  coefficients_table |>
    filter(dummy_variable != "(Intercept)") |>
    mutate(
      model = fit_object$model,
      alpha = fit_object$alpha,
      lambda_1se = fit_object$lambda_1se,
      original_variable = map_dummy_to_variable(dummy_variable, original_predictors),
      selected = coefficient != 0,
      abs_coefficient = abs(coefficient)
    ) |>
    group_by(model) |>
    mutate(
      max_abs_coefficient = max(abs_coefficient, na.rm = TRUE),
      importance = ifelse(
        max_abs_coefficient == 0,
        0,
        abs_coefficient / max_abs_coefficient
      )
    ) |>
    ungroup() |>
    dplyr::select(-max_abs_coefficient) |>
    arrange(model, desc(abs_coefficient))
}

compute_penalized_aic <- function(fit_object) {
  probs <- as.numeric(
    predict(
      fit_object$cv_fit,
      newx = x_train,
      s = "lambda.1se",
      type = "response"
    )
  )

  eps <- 1e-15
  probs <- pmin(pmax(probs, eps), 1 - eps)

  log_likelihood <- sum(
    y_train * log(probs) + (1 - y_train) * log(1 - probs)
  )

  coefficients <- as.numeric(coef(fit_object$cv_fit, s = "lambda.1se"))
  df_model <- sum(coefficients != 0)

  tibble(
    model = fit_object$model,
    alpha = fit_object$alpha,
    lambda_1se = fit_object$lambda_1se,
    log_likelihood_train = log_likelihood,
    df_nonzero_coefficients = df_model,
    aic_approx = -2 * log_likelihood + 2 * df_model
  )
}

# Tableau detaille : chaque ligne correspond a une modalite indicatrice.
# Pour le lasso et l'elastic net, selected = FALSE indique une modalite
# eliminee par penalisation.
penalized_coefficients_all <- purrr::map_dfr(
  penalized_fits,
  extract_penalized_coefficients
)

# Variables/modalites effectivement retenues par chaque modele.
penalized_selected_terms <- penalized_coefficients_all |>
  filter(selected) |>
  dplyr::select(
    model,
    original_variable,
    dummy_variable,
    coefficient,
    abs_coefficient,
    importance
  ) |>
  arrange(model, desc(importance))

# Importance agregee par variable d'origine.
# Utile pour le rapport, car une variable qualitative peut produire
# plusieurs indicatrices.
penalized_variable_importance <- penalized_coefficients_all |>
  filter(selected) |>
  group_by(model, original_variable) |>
  summarise(
    n_selected_modalities = n(),
    importance_sum = sum(abs_coefficient),
    importance_max = max(abs_coefficient),
    .groups = "drop"
  ) |>
  group_by(model) |>
  mutate(
    importance_relative = importance_sum / sum(importance_sum)
  ) |>
  ungroup() |>
  arrange(model, desc(importance_relative))

# AIC approximatif calcule sur train :
# AIC = -2 log-vraisemblance + 2 * nombre de coefficients non nuls.
# Pour glmnet, ce n'est pas l'AIC classique d'un glm non penalise, mais
# un indicateur comparable entre ces specifications penalisees.
penalized_aic_table <- purrr::map_dfr(
  penalized_fits,
  compute_penalized_aic
) |>
  arrange(aic_approx)

penalized_selected_terms
penalized_variable_importance
penalized_aic_table

# ============================================================
# 6. SELECTION DU MEILLEUR MODELE SUR VALIDATION
# ============================================================

best_penalized_model_name <- penalized_validation_metrics |>
  slice(1) |>
  pull(model)

best_penalized_fit <- penalized_fits[[best_penalized_model_name]]

best_penalized_threshold <- penalized_validation_metrics |>
  slice(1) |>
  pull(threshold)

cat("\nMeilleur modele penalise sur validation :", best_penalized_model_name, "\n")
cat("Seuil retenu sur validation :", best_penalized_threshold, "\n")
cat("Lambda 1se :", best_penalized_fit$lambda_1se, "\n")

# ============================================================
# 7. EVALUATION FINALE SUR TEST
# ============================================================

prob_test_best <- as.numeric(
  predict(
    best_penalized_fit$cv_fit,
    newx = x_test,
    s = "lambda.1se",
    type = "response"
  )
)

best_penalized_test <- evaluate_predictions(
  probs = prob_test_best,
  truth_factor = test_penalized$is_canceled,
  threshold = best_penalized_threshold,
  model_name = best_penalized_model_name,
  sample_name = "test"
)

penalized_test_metrics <- best_penalized_test$metrics
penalized_test_confusion <- best_penalized_test$confusion
penalized_test_roc <- best_penalized_test$roc

penalized_test_metrics
penalized_test_confusion
pROC::auc(penalized_test_roc)

# ============================================================
# 8. COEFFICIENTS DU MODELE RETENU
# ============================================================

best_penalized_coefficients <- penalized_selected_terms |>
  filter(model == best_penalized_model_name) |>
  arrange(desc(abs_coefficient))

best_penalized_variable_importance <- penalized_variable_importance |>
  filter(model == best_penalized_model_name) |>
  arrange(desc(importance_relative))

best_penalized_coefficients
best_penalized_variable_importance

# Lecture :
# - lasso peut mettre certains coefficients a zero ;
# - ridge garde toutes les variables mais reduit les coefficients ;
# - elastic net combine les deux comportements.

# ============================================================
# 9. GRAPHIQUES UTILES
# ============================================================

plot_penalized_validation <- penalized_validation_metrics |>
  dplyr::select(model, auc, accuracy, sensitivity, specificity, f1_score) |>
  pivot_longer(
    -model,
    names_to = "metric",
    values_to = "value"
  ) |>
  ggplot(aes(x = reorder(model, value), y = value, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Comparaison des modeles penalises sur validation",
    x = NULL,
    y = "Valeur",
    fill = "Metrique"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

plot_penalized_threshold <- best_penalized_fit$threshold_choice$all |>
  dplyr::select(
    threshold,
    accuracy,
    sensitivity,
    specificity,
    precision,
    f1_score,
    balanced_accuracy
  ) |>
  pivot_longer(
    -threshold,
    names_to = "metric",
    values_to = "value"
  ) |>
  ggplot(aes(x = threshold, y = value, color = metric)) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = best_penalized_threshold,
    linetype = "dashed",
    color = "black"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = paste("Choix du seuil -", best_penalized_model_name),
    x = "Seuil de classification",
    y = "Valeur",
    color = "Metrique"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

plot_penalized_roc_test <- ggplot(
  data.frame(
    specificity = penalized_test_roc$specificities,
    sensitivity = penalized_test_roc$sensitivities
  ),
  aes(x = 1 - specificity, y = sensitivity)
) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  coord_fixed() +
  labs(
    title = paste("Courbe ROC test -", best_penalized_model_name),
    subtitle = paste0("AUC = ", round(as.numeric(pROC::auc(penalized_test_roc)), 3)),
    x = "1 - Specificite",
    y = "Sensibilite"
  ) +
  theme_minimal(base_size = 13)

plot_best_variable_importance <- best_penalized_variable_importance |>
  slice_max(importance_relative, n = 12) |>
  ggplot(
    aes(
      x = reorder(original_variable, importance_relative),
      y = importance_relative
    )
  ) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = paste("Importance des variables -", best_penalized_model_name),
    x = NULL,
    y = "Importance relative"
  ) +
  theme_minimal(base_size = 13)

plot_penalized_validation
plot_penalized_threshold
plot_penalized_roc_test
plot_best_variable_importance

# ============================================================
# 10. SYNTHESE
# ============================================================

cat("\n====================================================\n")
cat("REGRESSION LOGISTIQUE PENALISEE TERMINEE\n")
cat("====================================================\n")
cat("Modele retenu :", best_penalized_model_name, "\n")
cat("AUC test :", round(penalized_test_metrics$auc, 3), "\n")
cat("Accuracy test :", round(penalized_test_metrics$accuracy, 3), "\n")
cat("F1-score test :", round(penalized_test_metrics$f1_score, 3), "\n")
cat("Meilleur AIC approximatif :", penalized_aic_table$model[1], "\n")
cat("====================================================\n")
