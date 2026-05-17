source("scripts/04-data_preparation.R")

# ============================================================
# REGRESSION LOGISTIQUE : SELECTION, PENALISATION ET VALIDATION
# ============================================================

# Methodologie predictive :
# 1. les bases train / validation / test viennent exclusivement de
#    scripts/04-data_preparation.R, comme pour l'analyse discriminante ;
# 2. les modeles candidats sont estimes sur train ;
# 3. le choix du modele et du seuil est fait sur validation ;
# 4. le modele retenu est reestime sur train + validation ;
# 5. l'evaluation finale sur test est reservee a scripts/07- Comparaison.R.

# ============================================================
# 1. BASES ET MATRICES DE DESIGN
# ============================================================

logistic_train <- train
logistic_validation <- validation
logistic_test <- test
logistic_train_validation <- bind_rows(train, validation)

logistic_formula <- is_canceled ~ .
logistic_predictors <- setdiff(names(logistic_train), "is_canceled")

make_logistic_x <- function(data, reference_columns = NULL) {
  x <- model.matrix(logistic_formula, data = data)[, -1, drop = FALSE]

  if (!is.null(reference_columns)) {
    missing_columns <- setdiff(reference_columns, colnames(x))

    if (length(missing_columns) > 0) {
      missing_matrix <- matrix(
        0,
        nrow = nrow(x),
        ncol = length(missing_columns),
        dimnames = list(NULL, missing_columns)
      )

      x <- cbind(x, missing_matrix)
    }

    x <- x[, reference_columns, drop = FALSE]
  }

  x
}

make_logistic_y <- function(data) {
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

x_train <- make_logistic_x(logistic_train)
x_validation <- make_logistic_x(logistic_validation, colnames(x_train))
x_train_validation <- make_logistic_x(logistic_train_validation, colnames(x_train))
x_test <- make_logistic_x(logistic_test, colnames(x_train))

y_train <- make_logistic_y(logistic_train)
y_validation <- make_logistic_y(logistic_validation)
y_train_validation <- make_logistic_y(logistic_train_validation)
y_test <- make_logistic_y(logistic_test)

# ============================================================
# 2. FONCTIONS D'EVALUATION
# ============================================================

safe_f1 <- function(precision, recall) {
  ifelse(
    is.na(precision + recall) | (precision + recall == 0),
    NA_real_,
    2 * precision * recall / (precision + recall)
  )
}

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

  tibble(
    threshold = threshold,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    sensitivity_recall = recall,
    specificity = specificity,
    precision = precision,
    f1_score = safe_f1(precision, recall),
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
      arrange(desc(f1_score), desc(sensitivity_recall)) |>
      slice(1)
  )
}

evaluate_logistic_probabilities <- function(probs, truth_factor, threshold, model_name, model_type, sample_name) {
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
    direction = "<",
    quiet = TRUE
  )

  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  specificity <- as.numeric(cm$byClass["Specificity"])

  list(
    confusion = cm,
    roc = roc_obj,
    probabilities = probs,
    predictions = pred,
    metrics = tibble(
      model = model_name,
      model_type = model_type,
      sample = sample_name,
      threshold = threshold,
      accuracy = as.numeric(cm$overall["Accuracy"]),
      auc = as.numeric(pROC::auc(roc_obj)),
      sensitivity_recall = recall,
      specificity = specificity,
      precision = precision,
      f1_score = safe_f1(precision, recall),
      balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE)
    )
  )
}

evaluate_logit_model <- function(model, new_data, threshold, scenario) {
  probs <- predict(model, newdata = new_data, type = "response")

  evaluate_logistic_probabilities(
    probs = probs,
    truth_factor = new_data$is_canceled,
    threshold = threshold,
    model_name = "Regression logistique",
    model_type = "glm",
    sample_name = scenario
  )
}

predict_logistic_final <- function(model_object, new_data, newx = NULL) {
  if (model_object$type == "glm") {
    return(as.numeric(
      predict(
        model_object$fit,
        newdata = new_data,
        type = "response"
      )
    ))
  }

  if (is.null(newx)) {
    newx <- make_logistic_x(new_data, model_object$x_columns)
  }

  as.numeric(
    predict(
      model_object$fit,
      newx = newx,
      s = "lambda.1se",
      type = "response"
    )
  )
}

# ============================================================
# 3. MODELES GLM PAR METHODES DE SELECTION
# ============================================================

full_model <- glm(
  logistic_formula,
  data = logistic_train,
  family = binomial
)

null_model <- glm(
  is_canceled ~ 1,
  data = logistic_train,
  family = binomial
)

model_backward <- step(
  full_model,
  direction = "backward",
  trace = FALSE
)

model_forward <- step(
  null_model,
  scope = formula(full_model),
  direction = "forward",
  trace = FALSE
)

model_stepwise <- step(
  full_model,
  direction = "both",
  trace = FALSE
)

glm_fits <- list(
  glm_full = full_model,
  glm_backward = model_backward,
  glm_forward = model_forward,
  glm_stepwise = model_stepwise
)

glm_aic_table <- purrr::imap_dfr(
  glm_fits,
  \(fit, model_name) {
    tibble(
      model = model_name,
      model_type = "glm_selection",
      n_coefficients = length(coef(fit)),
      aic = AIC(fit)
    )
  }
) |>
  arrange(aic)

evaluate_glm_candidate <- function(fit, model_name) {
  probs_validation <- as.numeric(
    predict(
      fit,
      newdata = logistic_validation,
      type = "response"
    )
  )

  threshold_choice <- choose_threshold(
    probs = probs_validation,
    truth_factor = logistic_validation$is_canceled
  )

  validation_eval <- evaluate_logistic_probabilities(
    probs = probs_validation,
    truth_factor = logistic_validation$is_canceled,
    threshold = threshold_choice$best_f1$threshold,
    model_name = model_name,
    model_type = "glm_selection",
    sample_name = "validation"
  )

  list(
    model = model_name,
    type = "glm",
    fit = fit,
    formula = formula(fit),
    threshold_choice = threshold_choice,
    validation_eval = validation_eval
  )
}

glm_candidate_objects <- purrr::imap(
  glm_fits,
  evaluate_glm_candidate
)

glm_validation_metrics <- purrr::map_dfr(
  glm_candidate_objects,
  \(x) x$validation_eval$metrics
)

glm_coefficients_all <- purrr::imap_dfr(
  glm_fits,
  \(fit, model_name) {
    broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(model = model_name, model_type = "glm_selection") |>
      relocate(model, model_type)
  }
)

glm_selected_terms <- glm_coefficients_all |>
  filter(term != "(Intercept)") |>
  mutate(
    selected = TRUE,
    original_variable = map_dummy_to_variable(term, logistic_predictors),
    importance = abs(log(estimate))
  ) |>
  arrange(model, desc(importance))

# ============================================================
# 4. MODELES PENALISES : RIDGE, LASSO, ELASTIC NET
# ============================================================

penalized_grid <- tribble(
  ~model, ~alpha,
  "ridge", 0.00,
  "lasso", 1.00,
  "elastic_net_025", 0.25,
  "elastic_net_050", 0.50,
  "elastic_net_075", 0.75
)

fit_penalized_model <- function(model, alpha, x_fit, y_fit, x_valid, valid_data) {
  set.seed(123)

  cv_fit <- glmnet::cv.glmnet(
    x = x_fit,
    y = y_fit,
    family = "binomial",
    alpha = alpha,
    type.measure = "auc",
    nfolds = 5,
    standardize = TRUE
  )

  prob_validation <- as.numeric(
    predict(
      cv_fit,
      newx = x_valid,
      s = "lambda.1se",
      type = "response"
    )
  )

  threshold_choice <- choose_threshold(
    probs = prob_validation,
    truth_factor = valid_data$is_canceled
  )

  validation_eval <- evaluate_logistic_probabilities(
    probs = prob_validation,
    truth_factor = valid_data$is_canceled,
    threshold = threshold_choice$best_f1$threshold,
    model_name = model,
    model_type = "penalized",
    sample_name = "validation"
  )

  list(
    model = model,
    type = "penalized",
    alpha = alpha,
    fit = cv_fit,
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se,
    x_columns = colnames(x_fit),
    threshold_choice = threshold_choice,
    validation_eval = validation_eval
  )
}

penalized_fits <- purrr::pmap(
  list(
    model = penalized_grid$model,
    alpha = penalized_grid$alpha
  ),
  fit_penalized_model,
  x_fit = x_train,
  y_fit = y_train,
  x_valid = x_validation,
  valid_data = logistic_validation
)

names(penalized_fits) <- penalized_grid$model

penalized_validation_metrics <- purrr::map_dfr(
  penalized_fits,
  \(x) x$validation_eval$metrics
)

extract_penalized_coefficients <- function(fit_object) {
  coefficients_table <- coef(
    fit_object$fit,
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
      model_type = "penalized",
      alpha = fit_object$alpha,
      lambda_1se = fit_object$lambda_1se,
      original_variable = map_dummy_to_variable(dummy_variable, logistic_predictors),
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

compute_penalized_aic <- function(fit_object, x_fit, y_fit) {
  probs <- as.numeric(
    predict(
      fit_object$fit,
      newx = x_fit,
      s = "lambda.1se",
      type = "response"
    )
  )

  eps <- 1e-15
  probs <- pmin(pmax(probs, eps), 1 - eps)

  log_likelihood <- sum(
    y_fit * log(probs) + (1 - y_fit) * log(1 - probs)
  )

  coefficients <- as.numeric(coef(fit_object$fit, s = "lambda.1se"))
  df_model <- sum(coefficients != 0)

  tibble(
    model = fit_object$model,
    model_type = "penalized",
    alpha = fit_object$alpha,
    lambda_1se = fit_object$lambda_1se,
    log_likelihood_train = log_likelihood,
    df_nonzero_coefficients = df_model,
    aic_approx = -2 * log_likelihood + 2 * df_model
  )
}

penalized_coefficients_all <- purrr::map_dfr(
  penalized_fits,
  extract_penalized_coefficients
)

penalized_selected_terms <- penalized_coefficients_all |>
  filter(selected) |>
  dplyr::select(
    model,
    model_type,
    original_variable,
    dummy_variable,
    coefficient,
    abs_coefficient,
    importance
  ) |>
  arrange(model, desc(importance))

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

penalized_aic_table <- purrr::map_dfr(
  penalized_fits,
  compute_penalized_aic,
  x_fit = x_train,
  y_fit = y_train
) |>
  arrange(aic_approx)

# ============================================================
# 5. COMPARAISON DES CANDIDATS SUR VALIDATION
# ============================================================

logistic_validation_metrics <- bind_rows(
  glm_validation_metrics,
  penalized_validation_metrics
) |>
  arrange(desc(auc), desc(f1_score), desc(balanced_accuracy))

logistic_aic_table <- bind_rows(
  glm_aic_table |>
    mutate(
      alpha = NA_real_,
      lambda_1se = NA_real_,
      log_likelihood_train = NA_real_,
      df_nonzero_coefficients = n_coefficients,
      aic_approx = aic
    ) |>
    dplyr::select(
      model,
      model_type,
      alpha,
      lambda_1se,
      log_likelihood_train,
      df_nonzero_coefficients,
      aic_approx
    ),
  penalized_aic_table |>
    dplyr::select(
      model,
      model_type,
      alpha,
      lambda_1se,
      log_likelihood_train,
      df_nonzero_coefficients,
      aic_approx
    )
) |>
  arrange(aic_approx)

best_logistic_choice <- logistic_validation_metrics |>
  filter(model=='lasso')########ici slice(1) si on veut garder le meilleur en f1 score

best_logistic_model_name <- best_logistic_choice$model
best_logistic_model_type <- best_logistic_choice$model_type
best_logistic_threshold <- best_logistic_choice$threshold

if (best_logistic_model_type == "glm_selection") {
  best_logistic_train_object <- glm_candidate_objects[[best_logistic_model_name]]
} else {
  best_logistic_train_object <- penalized_fits[[best_logistic_model_name]]
}

threshold <- best_logistic_train_object$threshold_choice
threshold_table <- threshold$all

logistic_models_recap <- tibble(
  selected_model = best_logistic_model_name,
  selected_model_type = best_logistic_model_type,
  selected_threshold = best_logistic_threshold,
  selection_sample = "validation",
  final_training_sample = "train + validation",
  final_test_role = "evaluation finale dans scripts/07- Comparaison.R"
)

logistic_validation_metrics
logistic_aic_table
logistic_models_recap

# ============================================================
# 6. MODELE FINAL REESTIME SUR TRAIN + VALIDATION
# ============================================================

if (best_logistic_model_type == "glm_selection") {
  logit_final_model <- list(
    type = "glm",
    model = best_logistic_model_name,
    fit = glm(
      best_logistic_train_object$formula,
      data = logistic_train_validation,
      family = binomial
    ),
    formula = best_logistic_train_object$formula,
    threshold = best_logistic_threshold
  )
} else {
  selected_alpha <- penalized_grid |>
    filter(model == best_logistic_model_name) |>
    pull(alpha)

  set.seed(123)

  final_penalized_fit <- glmnet::cv.glmnet(
    x = x_train_validation,
    y = y_train_validation,
    family = "binomial",
    alpha = selected_alpha,
    type.measure = "auc",
    nfolds = 5,
    standardize = TRUE
  )

  logit_final_model <- list(
    type = "penalized",
    model = best_logistic_model_name,
    alpha = selected_alpha,
    fit = final_penalized_fit,
    lambda_min = final_penalized_fit$lambda.min,
    lambda_1se = final_penalized_fit$lambda.1se,
    threshold = best_logistic_threshold,
    x_columns = colnames(x_train_validation)
  )
}

logit_selected_model <- best_logistic_train_object

# ============================================================
# 7. INTERPRETATION DES VARIABLES
# ============================================================

if (best_logistic_model_type == "glm_selection") {
  odds_ratios_clean <- broom::tidy(
    best_logistic_train_object$fit,
    exponentiate = TRUE,
    conf.int = TRUE
  ) |>
    dplyr::select(
      term,
      estimate,
      conf.low,
      conf.high,
      p.value
    ) |>
    rename(
      variable = term,
      odds_ratio = estimate,
      ic_low = conf.low,
      ic_high = conf.high,
      p_value = p.value
    ) |>
    arrange(desc(abs(log(odds_ratio))))
} else {
  odds_ratios_clean <- tibble()
}

best_penalized_model_name <- penalized_validation_metrics |>
  arrange(desc(auc), desc(f1_score), desc(balanced_accuracy)) |>
  slice(1) |>
  pull(model)

best_penalized_fit <- penalized_fits[[best_penalized_model_name]]

best_penalized_coefficients <- penalized_selected_terms |>
  filter(model == best_penalized_model_name) |>
  arrange(desc(importance))

best_logistic_coefficients <- if (best_logistic_model_type == "penalized") {
  penalized_selected_terms |>
    filter(model == best_logistic_model_name) |>
    arrange(desc(importance))
} else {
  glm_selected_terms |>
    filter(model == best_logistic_model_name) |>
    arrange(desc(importance))
}

odds_ratios_clean
penalized_selected_terms
penalized_variable_importance
best_logistic_coefficients
