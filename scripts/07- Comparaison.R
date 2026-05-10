# ============================================================
# COMPARAISON FINALE DES METHODES
# ============================================================

# Ce script centralise les tests et evaluations finales.
# Les scripts 05 et 06 construisent les modeles et choisissent les
# options sur validation ; le test final est reserve a ce fichier.

source("scripts/06-logistic.R")
source("scripts/05-Analyse Discriminante.R")

# ============================================================
# 1. CONTROLES POUR L'ANALYSE DISCRIMINANTE
# ============================================================

# Tests indicatifs des hypotheses LDA / QDA sur les axes ACM.
# Ils servent a documenter les limites de l'approche, sans modifier
# la logique de prediction.

set.seed(123)

train_sample <- train_k |>
  dplyr::slice_sample(n = min(5000, nrow(train_k)))

shapiro_results <- purrr::map_dfr(
  colnames(train_sample |> dplyr::select(-is_canceled)),
  \(var_name) {
    test <- shapiro.test(train_sample[[var_name]])

    tibble(
      variable = var_name,
      statistic = unname(test$statistic),
      p_value = test$p.value
    )
  }
)

box_m_test <- biotools::boxM(
  train_k |> dplyr::select(-is_canceled),
  train_k$is_canceled
)

shapiro_results
box_m_test

# ============================================================
# 2. EVALUATION FINALE DE LA REGRESSION LOGISTIQUE SUR TEST
# ============================================================

logit_test_balanced <- evaluate_logit_model(
  logit_selected_model,
  test,
  threshold$best_balanced$threshold,
  "test_seuil_equilibre"
)

logit_test_f1 <- evaluate_logit_model(
  logit_selected_model,
  test,
  threshold$best_f1$threshold,
  "test_seuil_f1"
)

results_logit_test <- bind_rows(
  logit_test_balanced$metrics,
  logit_test_f1$metrics
) |>
  distinct(
    model,
    threshold,
    accuracy,
    auc,
    sensitivity_recall,
    specificity,
    precision,
    f1_score,
    balanced_accuracy,
    .keep_all = TRUE
  )

results_logit_test
logit_test_balanced$confusion
logit_test_f1$confusion

# ============================================================
# 3. EVALUATION FINALE DE L'ANALYSE DISCRIMINANTE SUR TEST
# ============================================================

evaluate_discriminant_model <- function(true, pred, prob_yes, model_name) {
  cm <- caret::confusionMatrix(
    pred,
    true,
    positive = "Oui"
  )

  roc_obj <- pROC::roc(
    response = true,
    predictor = prob_yes,
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
    metrics = tibble(
      model = model_name,
      scenario = "test_final",
      threshold = NA_real_,
      accuracy = as.numeric(cm$overall["Accuracy"]),
      auc = as.numeric(pROC::auc(roc_obj)),
      sensitivity_recall = recall,
      specificity = specificity,
      precision = precision,
      f1_score = 2 * precision * recall / (precision + recall),
      balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE)
    )
  )
}

train_validation_k <- bind_rows(train_k, validation_k)

lda_final <- MASS::lda(
  is_canceled ~ .,
  data = train_validation_k
)

qda_final <- MASS::qda(
  is_canceled ~ .,
  data = train_validation_k
)

lda_test_pred <- predict(lda_final, newdata = test_k)
qda_test_pred <- predict(qda_final, newdata = test_k)

lda_test_eval <- evaluate_discriminant_model(
  true = test_k$is_canceled,
  pred = lda_test_pred$class,
  prob_yes = lda_test_pred$posterior[, "Oui"],
  model_name = "ACM + LDA"
)

qda_test_eval <- evaluate_discriminant_model(
  true = test_k$is_canceled,
  pred = qda_test_pred$class,
  prob_yes = qda_test_pred$posterior[, "Oui"],
  model_name = "ACM + QDA"
)

results_discriminant_test <- bind_rows(
  lda_test_eval$metrics,
  qda_test_eval$metrics
)

results_discriminant_test
lda_test_eval$confusion
qda_test_eval$confusion

# ============================================================
# 4. TABLEAU DE COMPARAISON FINAL
# ============================================================

comparison_test_metrics <- bind_rows(
  results_logit_test,
  results_discriminant_test
) |>
  arrange(desc(f1_score), desc(auc))

comparison_test_metrics

best_model_test <- comparison_test_metrics |>
  slice(1)

best_model_test
