source("scripts/04-data preparation.R")

# ============================================================
# REGRESSION LOGISTIQUE : CONSTRUCTION ET VALIDATION
# ============================================================

# Le test final est volontairement exclu de ce script.
# Il est centralise dans 07- Comparaison.R avec les autres methodes.

# ============================================================
# 1. CONSTRUCTION DES MODELES CANDIDATS
# ============================================================

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

logit_aic_table <- tibble(
  model = c("full", "backward", "forward", "stepwise"),
  aic = c(
    AIC(full_model),
    AIC(model_backward),
    AIC(model_forward),
    AIC(model_stepwise)
  )
) |>
  arrange(aic)

logit_aic_table

# Le modele stepwise est conserve pour rester coherent avec les analyses
# de seuil deja realisees dans le projet.
logit_selected_model <- model_stepwise

# ============================================================
# 2. FONCTIONS D'EVALUATION
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

evaluate_logit_model <- function(model, new_data, threshold, scenario) {
  probs <- predict(model, newdata = new_data, type = "response")
  truth <- new_data$is_canceled

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
      model = "Regression logistique",
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

# ============================================================
# 3. CHOIX DU SEUIL SUR VALIDATION
# ============================================================

threshold <- choose_threshold(
  logit_selected_model,
  validation
)

threshold_table <- threshold$all

logit_validation_balanced <- evaluate_logit_model(
  logit_selected_model,
  validation,
  threshold$best_balanced$threshold,
  "validation_seuil_equilibre"
)

logit_validation_f1 <- evaluate_logit_model(
  logit_selected_model,
  validation,
  threshold$best_f1$threshold,
  "validation_seuil_f1"
)

comparison_metrics_validation <- bind_rows(
  logit_validation_balanced$metrics,
  logit_validation_f1$metrics
)

plot_threshold_main <- threshold$all |>
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
    xintercept = threshold$best_balanced$threshold,
    linetype = "dashed",
    color = "black"
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Evolution des metriques selon le seuil",
    subtitle = "Modele logistique principal, validation",
    x = "Seuil de classification",
    y = "Valeur",
    color = "Metrique"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

plot_threshold_main

# ============================================================
# 4. INTERPRETATION DU MODELE
# ============================================================

odds_ratios <- broom::tidy(
  model_forward,
  exponentiate = TRUE,
  conf.int = TRUE
)

odds_ratios_clean <- odds_ratios |>
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
  arrange(desc(odds_ratio))

odds_ratios_clean

logit_anova <- anova(
  model_forward,
  test = "Chisq"
)

logit_pseudo_r2 <- pscl::pR2(model_forward)

summary(model_forward)
logit_anova
logit_pseudo_r2
