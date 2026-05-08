
df_model <- df_engineered |>
  select(
    is_canceled,
    hotel,
    # market_segment,
    # distribution_channel,
    # deposit_type,
    # customer_type,
    lead_time_cat,
    total_nights_cat,
    adr_cat,
    # has_children,
    # parking_reserved,
    engagement_cat,
    previous_cancellations_cat,
    saison_tourisme,
    # repeated_guest_cat,
    identity_location
  ) |>
  drop_na()
#######segmentation de la table

set.seed(123)

split1 <- initial_split(
  df_model,
  prop = 0.5,
  strata = is_canceled
)

train <- training(split1)
temp <- testing(split1)

split2 <- initial_split(
  temp,
  prop = 0.5,
  strata = is_canceled
)

test <- training(split2)
validation <- testing(split2)

dim(train)
dim(test)
dim(validation)

###construction des modéles logistiques:
full_model <- glm(
  is_canceled ~ .,
  data = train,
  family = binomial
)

null_model <- glm(
  is_canceled ~ 1,
  data = train,
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

AIC(
  full_model,
  model_backward,
  model_forward,
  model_stepwise
)

tibble(
  model = c("full", "backward", "forward", "stepwise"),
  aic = c(
    AIC(full_model),
    AIC(model_backward),
    AIC(model_forward),
    AIC(model_stepwise)
  )
) |>
  arrange(aic)



# ============================================================
# EVALUATION DU MODELE LOGISTIQUE FINAL
# ============================================================

library(tidyverse)
library(caret)
library(pROC)
library(broom)

# ============================================================
# 1. PREDICTIONS SUR TEST
# ============================================================

# Probabilités prédites

prob_TEST <- predict(
  model_forward,
  newdata = test,
  type = "response"
)

# ============================================================
# 2. CLASSIFICATION
# ============================================================

# Seuil classique = 0.5

pred_TEST <- if_else(
  prob_TEST >= 0.4,
  "Oui",
  "Non"
)

pred_TEST <- factor(
  pred_TEST,
  levels = c("Non", "Oui")
)

# ============================================================
# 3. MATRICE DE CONFUSION
# ============================================================

conf_mat <- confusionMatrix(
  pred_TEST,
  test$is_canceled,
  positive = "Oui"
)

conf_mat
# ============================================================
# optimisation
# ============================================================
evaluate_threshold <- function(threshold, probs, truth) {
  pred <- if_else(probs >= threshold, "Oui", "Non") |>
    factor(levels = c("Non", "Oui"))
  
  cm <- confusionMatrix(
    pred,
    truth,
    positive = "Oui"
  )
  
  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  specificity <- as.numeric(cm$byClass["Specificity"])
  f1 <- 2 * precision * recall / (precision + recall)
  
  tibble(
    threshold = threshold,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    sensitivity = recall,
    specificity = specificity,
    precision = precision,
    f1_score = f1,
    balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE),
    gap_sens_spec = abs(recall - specificity)
  )
}


choose_threshold <- function(model, valid_data) {
  probs <- predict(model, newdata = valid_data, type = "response")
  truth <- valid_data$is_canceled
  
  threshold_results <- map_dfr(
    seq(0.10, 0.90, by = 0.01),
    evaluate_threshold,
    probs = probs,
    truth = truth
  )
  
  list(
    all = threshold_results,
    best_balanced = threshold_results |>
      arrange(desc(balanced_accuracy), gap_sens_spec) |>
      slice(1),
    best_f1 = threshold_results |>
      arrange(desc(f1_score), desc(sensitivity)) |>
      slice(1),
    best_recall_precision = threshold_results |>
      filter(precision >= 0.50) |>
      arrange(desc(sensitivity), desc(f1_score)) |>
      slice(1)
  )
}

threshold <- choose_threshold(
  model_stepwise,
  # splits_sans_deposit$validation
  validation
)

threshold_table= as.data.frame(threshold)

evaluate_model <- function(model, test_data, threshold, scenario) {
  probs <- predict(model, newdata = test_data, type = "response")
  truth <- test_data$is_canceled
  
  pred <- if_else(probs >= threshold, "Oui", "Non") |>
    factor(levels = c("Non", "Oui"))
  
  cm <- confusionMatrix(
    pred,
    truth,
    positive = "Oui"
  )
  
  roc_obj <- roc(
    response = truth,
    predictor = probs,
    levels = c("Non", "Oui"),
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
      scenario = scenario,
      threshold = threshold,
      accuracy = as.numeric(cm$overall["Accuracy"]),
      auc = as.numeric(auc(roc_obj)),
      sensitivity_recall = recall,
      specificity = specificity,
      precision = precision,
      f1_score = 2 * precision * recall / (precision + recall),
      balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE)
    )
  )
}

eval_balanced <- evaluate_model(
  model_stepwise,
  # splits_sans_deposit$test,
  test,
  threshold$best_balanced$threshold,
  "type_seuil_equilibre"
)

eval_f1 <- evaluate_model(
  model_stepwise,
  test,
  threshold$best_f1$threshold,
  "type_seuil_f1"
)


comparison_metrics <- bind_rows(
  eval_balanced$metrics,
  eval_f1$metrics,
  
)

plot_threshold_main <- threshold$all |>
  select(
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
    xintercept = threshold$best_balanced$threshold,
    linetype = "dashed",
    color = "black"
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Evolution des metriques selon le seuil",
    subtitle = "Modele principal",
    x = "Seuil de classification",
    y = "Valeur",
    color = "Metrique"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")



plot_threshold_main
# ============================================================
# 4. METRIQUES PRINCIPALES
# ============================================================

metrics_logit <- tibble(
  
  Accuracy = conf_mat$overall["Accuracy"],
  
  Kappa = conf_mat$overall["Kappa"],
  
  Sensitivity = conf_mat$byClass["Sensitivity"],
  
  Specificity = conf_mat$byClass["Specificity"],
  
  Precision = conf_mat$byClass["Pos Pred Value"],
  
  Recall = conf_mat$byClass["Sensitivity"],
  
  F1_score = 2 * (
    (
      conf_mat$byClass["Pos Pred Value"] *
        conf_mat$byClass["Sensitivity"]
    ) /
      (
        conf_mat$byClass["Pos Pred Value"] +
          conf_mat$byClass["Sensitivity"]
      )
  )
)

metrics_logit

# ============================================================
# 5. COURBE ROC
# ============================================================

roc_logit <- roc(
  response = test$is_canceled,
  predictor = prob_TEST,
  levels = c("Non", "Oui")
)

# ============================================================
# 6. AUC
# ============================================================

auc(roc_logit)

# ============================================================
# 7. PLOT ROC PROFESSIONNEL
# ============================================================

plot(
  roc_logit,
  
  col = "#2C7FB8",
  
  lwd = 3,
  
  main = "Courbe ROC - Régression Logistique"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "gray50"
)

# ============================================================
# 8. TABLEAU DES ODDS RATIOS
# ============================================================

odds_ratios <- broom::tidy(
  model_forward,
  exponentiate = TRUE,
  conf.int = TRUE
)

odds_ratios

# ============================================================
# 9. VERSION PROPRE DES ODDS RATIOS
# ============================================================

odds_ratios_clean <- odds_ratios |>
  
  select(
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
  
  arrange(desc(odds_ratio))

odds_ratios_clean

# ============================================================
# 10. INTERPRETATION RAPIDE
# ============================================================

# odds_ratio > 1 :
# augmente le risque d'annulation

# odds_ratio < 1 :
# réduit le risque d'annulation

# ============================================================
# 11. IMPORTANCE DES VARIABLES
# ============================================================

summary(model_forward)

# ============================================================
# 12. TEST GLOBAL DU MODELE
# ============================================================

anova(
  model_forward,
  test = "Chisq"
)

# ============================================================
# 13. PSEUDO R² DE MCFADDEN
# ============================================================

library(pscl)

pR2(model_forward)

# ============================================================
# 14. TABLEAU FINAL DES PERFORMANCES
# ============================================================

results_logit <- tibble(
  
  Model = "Régression logistique",
  
  Accuracy = round(
    conf_mat$overall["Accuracy"],
    3
  ),
  
  AUC = round(
    as.numeric(auc(roc_logit)),
    3
  ),
  
  Sensitivity = round(
    conf_mat$byClass["Sensitivity"],
    3
  ),
  
  Specificity = round(
    conf_mat$byClass["Specificity"],
    3
  ),
  
  F1_score = round(
    as.numeric(metrics_logit$F1_score),
    3
  )
)

results_logit

# ============================================================
# 15. MATRICE DE CONFUSION VISUELLE
# ============================================================

fourfoldplot(
  conf_mat$table,
  
  color = c(
    "#ff2c2c",
    "#008000"
  ),
  
  conf.level = 0,
  
  margin = 1,
  
  main = "Matrice de confusion - Logistique"
)
