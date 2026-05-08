# ============================================================
# TEST DE PLUSIEURS SEUILS - LDA
# ============================================================

lda_prob <- lda_pred$posterior[, "Oui"]

thresholds <- c(0.2, 0.3, 0.35, 0.4, 0.5)

evaluate_threshold <- function(threshold){
  
  pred_class <- ifelse(
    lda_prob >= threshold,
    "Oui",
    "Non"
  )
  
  pred_class <- factor(
    pred_class,
    levels = c("Non", "Oui")
  )
  
  cm <- caret::confusionMatrix(
    pred_class,
    validation_k$is_canceled,
    positive = "Oui"
  )
  
  tibble(
    threshold = threshold,
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    precision = cm$byClass["Pos Pred Value"],
    accuracy = cm$overall["Accuracy"],
    f1 = 2 *
      cm$byClass["Sensitivity"] *
      cm$byClass["Pos Pred Value"] /
      (
        cm$byClass["Sensitivity"] +
          cm$byClass["Pos Pred Value"]
      )
  )
}

results_thresholds <- purrr::map_dfr(
  thresholds,
  evaluate_threshold
)

results_thresholds
# ============================================================
# TEST DE PLUSIEURS SEUILS - QDA
# ============================================================

# Probabilités prédites pour la classe positive
qda_prob <- qda_pred$posterior[, "Oui"]

# Seuils à tester
thresholds <- c(0.2, 0.3, 0.35, 0.4, 0.5)

# Fonction d'évaluation
evaluate_threshold_qda <- function(threshold){
  
  pred_class <- ifelse(
    qda_prob >= threshold,
    "Oui",
    "Non"
  )
  
  pred_class <- factor(
    pred_class,
    levels = c("Non", "Oui")
  )
  
  cm <- caret::confusionMatrix(
    pred_class,
    validation_k$is_canceled,
    positive = "Oui"
  )
  
  tibble(
    threshold = threshold,
    
    sensitivity = as.numeric(cm$byClass["Sensitivity"]),
    
    specificity = as.numeric(cm$byClass["Specificity"]),
    
    precision = as.numeric(cm$byClass["Pos Pred Value"]),
    
    accuracy = as.numeric(cm$overall["Accuracy"]),
    
    f1 = 2 *
      as.numeric(cm$byClass["Sensitivity"]) *
      as.numeric(cm$byClass["Pos Pred Value"]) /
      (
        as.numeric(cm$byClass["Sensitivity"]) +
          as.numeric(cm$byClass["Pos Pred Value"])
      )
  )
}

# Résultats
results_thresholds_qda <- purrr::map_dfr(
  thresholds,
  evaluate_threshold_qda
)

# Affichage
results_thresholds_qda
# ============================================================
# TEST DE SEUILS POUR LE MEILLEUR KNN
# ============================================================

# Fonction robuste pour récupérer la proba de la classe "Oui"
get_knn_prob_oui <- function(knn_pred){
  
  prob_winner <- attr(knn_pred, "prob")
  
  prob_oui <- ifelse(
    knn_pred == "Oui",
    prob_winner,
    1 - prob_winner
  )
  
  return(prob_oui)
}

# Choisir le meilleur KNN selon l'AUC déjà calculée
best_knn_name <- results_validation %>%
  filter(grepl("KNN", model)) %>%
  arrange(desc(auc)) %>%
  slice(1) %>%
  pull(model)

best_knn_pred <- switch(
  best_knn_name,
  "KNN_10" = knn_pred_10,
  "KNN_20" = knn_pred_20,
  "KNN_50" = knn_pred_50
)

best_knn_prob <- get_knn_prob_oui(best_knn_pred)

thresholds <- c(0.2, 0.3, 0.35, 0.4, 0.5)

evaluate_threshold_knn <- function(threshold){
  
  pred_class <- ifelse(
    best_knn_prob >= threshold,
    "Oui",
    "Non"
  )
  
  pred_class <- factor(
    pred_class,
    levels = c("Non", "Oui")
  )
  
  cm <- caret::confusionMatrix(
    pred_class,
    validation_k$is_canceled,
    positive = "Oui"
  )
  
  tibble(
    model = best_knn_name,
    threshold = threshold,
    sensitivity = as.numeric(cm$byClass["Sensitivity"]),
    specificity = as.numeric(cm$byClass["Specificity"]),
    precision = as.numeric(cm$byClass["Pos Pred Value"]),
    accuracy = as.numeric(cm$overall["Accuracy"]),
    f1 = 2 *
      as.numeric(cm$byClass["Sensitivity"]) *
      as.numeric(cm$byClass["Pos Pred Value"]) /
      (
        as.numeric(cm$byClass["Sensitivity"]) +
          as.numeric(cm$byClass["Pos Pred Value"])
      )
  )
}

results_thresholds_knn <- purrr::map_dfr(
  thresholds,
  evaluate_threshold_knn
)

results_thresholds_knn
