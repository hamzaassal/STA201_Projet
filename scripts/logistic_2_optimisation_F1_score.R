# ============================================================
# OPTIMISATION DU SEUIL - REGRESSION LOGISTIQUE
# ============================================================

library(tidyverse)
library(caret)
library(pROC)
library(scales)

# ============================================================
# 1. PROBABILITES PREDITES SUR TEST
# ============================================================

prob_test <- predict(
  model_forward,
  newdata = test,
  type = "response"
)

true_test <- test$is_canceled

# ============================================================
# 2. GRILLE DE SEUILS A TESTER
# ============================================================

threshold_grid <- seq(0.10, 0.90, by = 0.01)

# ============================================================
# 3. FONCTION D'EVALUATION PAR SEUIL
# ============================================================

evaluate_threshold <- function(threshold, probs, truth) {
  
  pred <- if_else(
    probs >= threshold,
    "Oui",
    "Non"
  ) |>
    factor(levels = c("Non", "Oui"))
  
  cm <- confusionMatrix(
    pred,
    truth,
    positive = "Oui"
  )
  
  sensitivity <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  precision <- cm$byClass["Pos Pred Value"]
  
  f1 <- 2 * precision * sensitivity / (precision + sensitivity)
  
  tibble(
    threshold = threshold,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    sensitivity = as.numeric(sensitivity),
    specificity = as.numeric(specificity),
    precision = as.numeric(precision),
    f1_score = as.numeric(f1),
    balanced_accuracy = mean(c(sensitivity, specificity), na.rm = TRUE),
    gap_sens_spec = abs(sensitivity - specificity)
  )
}

# ============================================================
# 4. EVALUATION DE TOUS LES SEUILS
# ============================================================

threshold_results <- map_dfr(
  threshold_grid,
  evaluate_threshold,
  probs = prob_test,
  truth = true_test
)

threshold_results

# ============================================================
# 5. CHOIX DU SEUIL LE PLUS EQUILIBRE
# ============================================================

best_threshold_balanced <- threshold_results |>
  arrange(desc(balanced_accuracy), gap_sens_spec) |>
  slice(1)

best_threshold_balanced

# Variante : seuil qui rapproche au maximum sensibilité et spécificité
best_threshold_equal <- threshold_results |>
  arrange(gap_sens_spec, desc(balanced_accuracy)) |>
  slice(1)

best_threshold_equal

# Variante : seuil qui maximise le F1-score
best_threshold_f1 <- threshold_results |>
  arrange(desc(f1_score)) |>
  slice(1)

best_threshold_f1

# ============================================================
# 6. PREDICTION AVEC LE SEUIL RETENU
# ============================================================

chosen_threshold <- best_threshold_balanced$threshold

pred_test_optimal <- if_else(
  prob_test >= chosen_threshold,
  "Oui",
  "Non"
) |>
  factor(levels = c("Non", "Oui"))

confusion_optimal <- confusionMatrix(
  pred_test_optimal,
  true_test,
  positive = "Oui"
)

confusion_optimal

# ============================================================
# 7. TABLEAU FINAL DES PERFORMANCES
# ============================================================

roc_logit <- roc(
  response = true_test,
  predictor = prob_test,
  levels = c("Non", "Oui"),
  direction = "<"
)

results_logit_optimal <- tibble(
  Model = "Régression logistique - seuil optimisé",
  Threshold = chosen_threshold,
  Accuracy = as.numeric(confusion_optimal$overall["Accuracy"]),
  AUC = as.numeric(auc(roc_logit)),
  Sensitivity = as.numeric(confusion_optimal$byClass["Sensitivity"]),
  Specificity = as.numeric(confusion_optimal$byClass["Specificity"]),
  Precision = as.numeric(confusion_optimal$byClass["Pos Pred Value"]),
  F1_score = 2 * Precision * Sensitivity / (Precision + Sensitivity),
  Balanced_accuracy = mean(c(Sensitivity, Specificity))
)

results_logit_optimal

# ============================================================
# 8. COURBES DES METRIQUES SELON LE SEUIL
# ============================================================

threshold_results_long <- threshold_results |>
  select(
    threshold,
    accuracy,
    sensitivity,
    specificity,
    f1_score,
    balanced_accuracy
  ) |>
  pivot_longer(
    -threshold,
    names_to = "metric",
    values_to = "value"
  )

ggplot(
  threshold_results_long,
  aes(x = threshold, y = value, color = metric)
) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    xintercept = chosen_threshold,
    linetype = "dashed",
    color = "black"
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(
    values = c(
      accuracy = "#2C7FB8",
      sensitivity = "#D95F0E",
      specificity = "#31A354",
      f1_score = "#756BB1",
      balanced_accuracy = "#E6550D"
    )
  ) +
  labs(
    title = "Évolution des métriques selon le seuil de classification",
    subtitle = paste0("Seuil retenu = ", round(chosen_threshold, 2)),
    x = "Seuil de classification",
    y = "Valeur",
    color = "Métrique"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# ============================================================
# 9. COURBE ROC AVEC SEUILS MARQUES
# ============================================================

roc_df <- tibble(
  specificity = roc_logit$specificities,
  sensitivity = roc_logit$sensitivities,
  threshold = roc_logit$thresholds
) |>
  filter(is.finite(threshold))

selected_thresholds <- threshold_results |>
  filter(threshold %in% c(0.3, 0.4, 0.5, chosen_threshold)) |>
  mutate(
    pred = map(
      threshold,
      \(th) if_else(prob_test >= th, "Oui", "Non") |>
        factor(levels = c("Non", "Oui"))
    ),
    cm = map(pred, \(p) confusionMatrix(p, true_test, positive = "Oui")),
    sensitivity = map_dbl(cm, \(x) x$byClass["Sensitivity"]),
    specificity = map_dbl(cm, \(x) x$byClass["Specificity"])
  )

ggplot(
  roc_df,
  aes(x = 1 - specificity, y = sensitivity)
) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_point(
    data = selected_thresholds,
    aes(x = 1 - specificity, y = sensitivity),
    color = "#D95F0E",
    size = 3
  ) +
  geom_text(
    data = selected_thresholds,
    aes(
      x = 1 - specificity,
      y = sensitivity,
      label = paste0("s=", round(threshold, 2))
    ),
    vjust = -0.8,
    size = 4
  ) +
  labs(
    title = "Courbe ROC avec seuils de classification",
    subtitle = paste0("AUC = ", round(as.numeric(auc(roc_logit)), 3)),
    x = "1 - Spécificité",
    y = "Sensibilité"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

