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

train_sample <- train_validation_k_final |>
  dplyr::slice_sample(n = min(5000, nrow(train_validation_k_final)))

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
  train_validation_k_final |> dplyr::select(-is_canceled),
  train_validation_k_final$is_canceled
)

shapiro_results
box_m_test

# ============================================================
# 2. EVALUATION FINALE DE LA REGRESSION LOGISTIQUE SUR TEST
# ============================================================

logit_test_prob <- predict_logistic_final(
  model_object = logit_final_model,
  new_data = logistic_test,
  newx = if (logit_final_model$type == "penalized") {
    make_logistic_x(logistic_test, logit_final_model$x_columns)
  } else {
    NULL
  }
)

logit_test_eval <- evaluate_logistic_probabilities(
  probs = logit_test_prob,
  truth_factor = logistic_test$is_canceled,
  threshold = logit_final_model$threshold,
  model_name = paste0(
    "Regression logistique - ",
    logit_final_model$model
  ),
  model_type = logit_final_model$type,
  sample_name = "test_final"
)

results_logit_test <- logit_test_eval$metrics

results_logit_test
logit_test_eval$confusion
logistic_models_recap

# ============================================================
# 3. EVALUATION FINALE DE L'ANALYSE DISCRIMINANTE SUR TEST
# ============================================================

evaluate_discriminant_model <- function(true, prob_yes, threshold, model_name) {
  pred <- if_else(prob_yes >= threshold, "Oui", "Non")
  pred <- factor(pred, levels = c("Non", "Oui"))

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
      model_type = "discriminant",
      sample = "test_final",
      scenario = "test_final",
      threshold = threshold,
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

if (best_discriminant_model == "ACM + LDA") {
  discriminant_test_pred <- predict(
    lda_final,
    newdata = test_k_final
  )

  discriminant_test_class <- discriminant_test_pred$class
  discriminant_test_prob_yes <- discriminant_test_pred$posterior[, "Oui"]
} else if (best_discriminant_model == "ACM + QDA") {
  discriminant_test_pred <- predict(
    qda_final,
    newdata = test_k_final
  )

  discriminant_test_class <- discriminant_test_pred$class
  discriminant_test_prob_yes <- discriminant_test_pred$posterior[, "Oui"]
} else {
  x_test_knn_final <- test_k_final |>
    dplyr::select(-is_canceled)

  x_test_knn_scaled <- scale(
    x_test_knn_final,
    center = knn_final$center,
    scale = knn_final$scale
  )

  discriminant_test_class <- class::knn(
    train = knn_final$train,
    test = x_test_knn_scaled,
    cl = knn_final$class,
    k = knn_final$k,
    prob = TRUE,
    use.all = FALSE
  )

  discriminant_test_prob_yes <- get_knn_prob_yes(discriminant_test_class)
}

discriminant_model_label <- if (best_discriminant_model == "ACM + KNN") {
  paste0(
    best_discriminant_model,
    " (",
    best_discriminant_n_axes,
    " axes, k=",
    best_discriminant_knn_k,
    ")"
  )
} else {
  paste0(
    best_discriminant_model,
    " (",
    best_discriminant_n_axes,
    " axes)"
  )
}

discriminant_test_eval <- evaluate_discriminant_model(
  true = test_k_final$is_canceled,
  prob_yes = discriminant_test_prob_yes,
  threshold = best_discriminant_threshold,
  model_name = discriminant_model_label
)

results_discriminant_test <- discriminant_test_eval$metrics

results_discriminant_test
discriminant_test_eval$confusion
discriminant_method_recap

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
