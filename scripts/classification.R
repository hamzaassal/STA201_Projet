# ============================================================
# ANALYSE DISQUAL : ACM + LDA + QDA + KNN
# Projet STA201 - Master Actuariat
# ============================================================

# Chargement des packages et données
source("scripts/00-packages.R")
source("scripts/01-import.R")
source("scripts/02-data_engenering.R")
source("scripts/04-data preparation.R")

# ============================================================
# 1. VERIFICATION DES OBJETS EXISTANTS
# ============================================================

if (!exists("df_model")) {
  stop("df_model n'existe pas. Assurez-vous que scripts/04-data preparation.R a été exécuté.")
}

if (!exists("train") | !exists("validation") | !exists("test")) {
  stop("Les bases train, validation ou test n'existent pas.")
}

cat("✓ df_model existe avec", nrow(df_model), "observations\n")
cat("✓ train :", nrow(train), "observations\n")
cat("✓ validation :", nrow(validation), "observations\n")
cat("✓ test :", nrow(test), "observations\n")

# Alias propres pour DISQUAL
train_disqual_source <- train
validation_disqual_source <- validation
test_disqual_source <- test

# ============================================================
# 2. PREPARATION DES DONNEES DISQUAL
# ============================================================

# Identificateur de la variable cible
target_var <- "is_canceled"

# Variables explicatives qualitatives (exclure is_canceled)
quali_vars_disqual <- setdiff(names(df_model), target_var)

cat("\nVariables qualitatives pour l'ACM :", length(quali_vars_disqual), "\n")
cat("Variables :", paste(quali_vars_disqual, collapse = ", "), "\n")

# Vérifier que toutes sont des facteurs
for (var in quali_vars_disqual) {
  if (!is.factor(train_disqual_source[[var]])) {
    warning(paste("Attention :", var, "n'est pas un factor"))
  }
}

# Séparer explicatives et cible
df_disqual_train <- train_disqual_source[, quali_vars_disqual, drop = FALSE]
df_disqual_validation <- validation_disqual_source[, quali_vars_disqual, drop = FALSE]
df_disqual_test <- test_disqual_source[, quali_vars_disqual, drop = FALSE]

# Cibles (en facteur avec modalités "non" et "oui")
y_train <- train_disqual_source[[target_var]]
y_validation <- validation_disqual_source[[target_var]]
y_test <- test_disqual_source[[target_var]]

# Vérifier les niveaux
cat("\nNiveaux de la cible :\n")
cat("Train :", paste(levels(y_train), collapse = ", "), "\n")
cat("Validation :", paste(levels(y_validation), collapse = ", "), "\n")
cat("Test :", paste(levels(y_test), collapse = ", "), "\n")

# ============================================================
# TABLE DE DESCRIPTION DES VARIABLES DISQUAL
# ============================================================

disqual_variables_table <- data.frame(
  variable = quali_vars_disqual,
  n_modalites = sapply(df_disqual_train, nlevels),
  type = sapply(df_disqual_train, class),
  effectif_train = sapply(df_disqual_train, length),
  taux_na = sapply(df_disqual_train, function(x) sum(is.na(x)) / length(x) * 100)
)

disqual_variables_table$taux_na <- round(disqual_variables_table$taux_na, 2)
disqual_variables_table$taux_na <- paste0(disqual_variables_table$taux_na, "%")

cat("\nTable descriptive des variables DISQUAL :\n")
print(disqual_variables_table)

# ============================================================
# 3. ANALYSE EN COMPOSANTES MULTIPLES (ACM)
# ============================================================

cat("\n========== ANALYSE EN COMPOSANTES MULTIPLES ==========\n")

# Fonction pour harmoniser les niveaux des facteurs
harmonize_factors <- function(data, reference_levels) {
  for (var in names(reference_levels)) {
    if (var %in% names(data)) {
      # Convertir en character d'abord pour éviter les problèmes
      data[[var]] <- as.character(data[[var]])
      # Remplacer les valeurs inconnues par NA
      data[[var]][!data[[var]] %in% reference_levels[[var]]] <- NA
      # Convertir en factor avec les niveaux de référence
      data[[var]] <- factor(data[[var]], levels = reference_levels[[var]])
    }
  }
  return(data)
}

# Obtenir les niveaux de référence depuis train
reference_levels <- lapply(df_disqual_train, levels)

# Harmoniser validation et test
df_disqual_validation_clean <- harmonize_factors(df_disqual_validation, reference_levels)
df_disqual_test_clean <- harmonize_factors(df_disqual_test, reference_levels)

# Supprimer les observations avec NA (modalités inconnues)
valid_rows_val <- complete.cases(df_disqual_validation_clean)
valid_rows_test <- complete.cases(df_disqual_test_clean)

df_disqual_validation_clean <- df_disqual_validation_clean[valid_rows_val, , drop = FALSE]
df_disqual_test_clean <- df_disqual_test_clean[valid_rows_test, , drop = FALSE]

y_validation_clean <- y_validation[valid_rows_val]
y_test_clean <- y_test[valid_rows_test]

cat("Observations après nettoyage :\n")
cat("Validation :", nrow(df_disqual_validation_clean), "/", length(valid_rows_val), "\n")
cat("Test :", nrow(df_disqual_test_clean), "/", length(valid_rows_test), "\n")

# Réaliser l'ACM uniquement sur la base train
res_mca <- tryCatch({
  FactoMineR::MCA(
    df_disqual_train,
    graph = FALSE,
    ncp = Inf
  )
}, error = function(e) {
  stop("Erreur lors de l'ACM : ", e$message)
})

# Récupérer le nombre total d'axes disponibles
n_axes_disqual <- res_mca$call$ncp
cat("\nNombre total d'axes ACM :", n_axes_disqual, "\n")

# Valeurs propres et inertie
eigenvalues_mca <- res_mca$eig
n_axes_80_inertia <- sum(cumsum(eigenvalues_mca[, 3]) <= 80)
cat("Axes pour 80% de l'inertie :", n_axes_80_inertia, "\n")

# Créer la table des valeurs propres de manière robuste
acm_eigen_table <- data.frame(
  Axe = paste0("Axe ", 1:nrow(eigenvalues_mca)),
  Valeur_propre = round(eigenvalues_mca[, 1], 4),
  Inertie_pct = round(eigenvalues_mca[, 2], 2),
  Inertie_cumulee_pct = round(eigenvalues_mca[, 3], 2)
)

cat("\nTable des valeurs propres (premiers 10 axes) :\n")
print(head(acm_eigen_table, 10))

# ============================================================
# GRAPHIQUES DE L'ACM
# ============================================================

# Graphique 1 : Inertie expliquée par axe
p_acm_inertia <- acm_eigen_table %>%
  head(min(15, nrow(acm_eigen_table))) %>%
  ggplot(aes(x = reorder(Axe, -Inertie_pct), y = Inertie_pct)) +
  geom_col(fill = "#2C7FB8", alpha = 0.7) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Inertie expliquée par axe - ACM",
    x = "Axe",
    y = "Inertie expliquée (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graphique 2 : Inertie cumulée
p_acm_inertia_cumsum <- acm_eigen_table %>%
  head(min(15, nrow(acm_eigen_table))) %>%
  ggplot(aes(x = reorder(Axe, row_number()), y = Inertie_cumulee_pct)) +
  geom_line(group = 1, color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 3) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Inertie cumulée - ACM",
    x = "Axe",
    y = "Inertie cumulée (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_acm_inertia <- p_acm_inertia / p_acm_inertia_cumsum

# Graphique 3 : Modalités sur les deux premiers axes
p_acm_modalities <- tryCatch({
  plot_obj <- FactoMineR::plot.MCA(
    res_mca,
    choix = "var",
    invisible = c("ind"),
    cex = 0.8
  )
  ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "Voir plot.MCA(res_mca, choix='var')")
}, error = function(e) {
  cat("Attention : Graphique des modalités non disponible\n")
  ggplot() + theme_void()
})

# ============================================================
# 4. EXTRACTION DES COORDONNEES
# ============================================================

cat("\n========== EXTRACTION DES COORDONNEES ACM ==========\n")

# Coordonnées des individus train
coord_train_disqual <- as.data.frame(res_mca$ind$coord)
coord_train_disqual$id_train <- 1:nrow(coord_train_disqual)

# Projection des individus de validation
coord_validation_disqual <- tryCatch({
  pred_val <- predict(res_mca, newdata = df_disqual_validation_clean)
  as.data.frame(pred_val$coord)
}, error = function(e) {
  cat("Erreur projection validation :", e$message, "\n")
  data.frame(matrix(NA, nrow = nrow(df_disqual_validation_clean), ncol = n_axes_disqual))
})

coord_validation_disqual$id_validation <- 1:nrow(coord_validation_disqual)

# Projection des individus de test
coord_test_disqual <- tryCatch({
  pred_test <- predict(res_mca, newdata = df_disqual_test_clean)
  as.data.frame(pred_test$coord)
}, error = function(e) {
  cat("Erreur projection test :", e$message, "\n")
  data.frame(matrix(NA, nrow = nrow(df_disqual_test_clean), ncol = n_axes_disqual))
})

coord_test_disqual$id_test <- 1:nrow(coord_test_disqual)

cat("✓ Coordonnées train :", nrow(coord_train_disqual), "x", ncol(coord_train_disqual), "\n")
cat("✓ Coordonnées validation :", nrow(coord_validation_disqual), "x", ncol(coord_validation_disqual), "\n")
cat("✓ Coordonnées test :", nrow(coord_test_disqual), "x", ncol(coord_test_disqual), "\n")

# ============================================================
# 5. POUVOIR DISCRIMINANT DES AXES
# ============================================================

cat("\n========== POUVOIR DISCRIMINANT DES AXES ==========\n")

# Pour chaque axe, calculer les statistiques de discrimination
axis_discrimination_table <- data.frame()

for (i in 1:n_axes_disqual) {

  coord_name <- paste0("Dim.", i)

  if (coord_name %in% colnames(coord_train_disqual)) {
    # Moyennes par classe
    mean_non <- mean(coord_train_disqual[[coord_name]][y_train == "non"], na.rm = TRUE)
    mean_oui <- mean(coord_train_disqual[[coord_name]][y_train == "oui"], na.rm = TRUE)

    diff_abs <- abs(mean_non - mean_oui)

    # Test t simple
    t_test <- tryCatch({
      t.test(
        coord_train_disqual[[coord_name]][y_train == "non"],
        coord_train_disqual[[coord_name]][y_train == "oui"]
      )
    }, error = function(e) {
      list(p.value = NA)
    })

    p_value <- t_test$p.value

    # AUC univariée
    auc_val <- tryCatch({
      roc_obj <- pROC::roc(
        response = y_train,
        predictor = coord_train_disqual[[coord_name]],
        levels = c("non", "oui"),
        quiet = TRUE
      )
      as.numeric(pROC::auc(roc_obj))
    }, error = function(e) {
      NA_real_
    })

    axis_discrimination_table <- rbind(axis_discrimination_table, data.frame(
      axe = coord_name,
      mean_non = round(mean_non, 4),
      mean_oui = round(mean_oui, 4),
      diff_abs_moyennes = round(diff_abs, 4),
      p_value = round(p_value, 4),
      auc_univariee = round(auc_val, 4)
    ))
  }
}

cat("\nTable du pouvoir discriminant (premiers 10 axes) :\n")
print(head(axis_discrimination_table, 10))

# Graphique du pouvoir discriminant
p_axis_discrimination <- axis_discrimination_table %>%
  head(min(15, nrow(axis_discrimination_table))) %>%
  ggplot(aes(x = reorder(axe, -auc_univariee), y = auc_univariee)) +
  geom_col(fill = "#2C7FB8", alpha = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "AUC univariée de chaque axe ACM",
    x = "Axe",
    y = "AUC univariée"
  ) +
  ylim(0.4, 1) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================
# 6. TESTS D'HYPOTHESES AVANT LDA/QDA
# ============================================================

cat("\n========== TESTS D'HYPOTHESES ==========\n")

# A. TEST DE NORMALITE MULTIVARIEE

cat("\n--- Test de normalité multivariée ---\n")

# Préparation des données pour les tests
coord_train_matrix <- coord_train_disqual %>%
  select(-id_train) %>%
  as.matrix()

normality_tests_table <- data.frame()

# Test de Mardia
mardia_result <- tryCatch({
  test_result <- MVN::mardia(coord_train_matrix)
  data.frame(
    test = "Mardia",
    statistic = NA_character_,
    p_value = test_result$multivariateNormality$p.value[2],
    conclusion = ifelse(test_result$multivariateNormality$p.value[2] > 0.05,
                       "Normal", "Non-normal")
  )
}, error = function(e) {
  data.frame(
    test = "Mardia",
    statistic = NA_character_,
    p_value = NA_real_,
    conclusion = "Erreur calcul"
  )
})

normality_tests_table <- rbind(normality_tests_table, mardia_result)

# Test de Henze-Zirkler
hz_result <- tryCatch({
  test_result <- MVN::hzTest(coord_train_matrix)
  data.frame(
    test = "Henze-Zirkler",
    statistic = NA_character_,
    p_value = test_result$p.value,
    conclusion = ifelse(test_result$p.value > 0.05, "Normal", "Non-normal")
  )
}, error = function(e) {
  data.frame(
    test = "Henze-Zirkler",
    statistic = NA_character_,
    p_value = NA_real_,
    conclusion = "Erreur calcul"
  )
})

normality_tests_table <- rbind(normality_tests_table, hz_result)

cat("\nConclusion sur la normalité multivariée :\n")
print(normality_tests_table)

# B. TEST D'HOMOGENEITE DES MATRICES DE COVARIANCE (TEST DE BOX M)

cat("\n--- Test d'homogénéité des matrices de covariance (Box M) ---\n")

# Créer un data.frame avec les coordonnées et la cible
coord_train_with_target <- coord_train_disqual %>%
  select(-id_train) %>%
  mutate(target = y_train)

boxm_result <- tryCatch({
  test_result <- biotools::boxM(coord_train_with_target[, -ncol(coord_train_with_target)], coord_train_with_target$target)
  data.frame(
    test = "Box M",
    statistic = round(test_result$statistic, 4),
    p_value = round(test_result$p.value, 4),
    conclusion = ifelse(test_result$p.value > 0.05,
                       "Matrices homogènes",
                       "Matrices hétérogènes")
  )
}, error = function(e) {
  data.frame(
    test = "Box M",
    statistic = NA_real_,
    p_value = NA_real_,
    conclusion = "Erreur calcul"
  )
})

boxm_test_table <- boxm_result

cat("✓ Statistique Box M :", boxm_result$statistic, "\n")
cat("✓ p-value :", boxm_result$p_value, "\n")
cat("✓ Conclusion :", boxm_result$conclusion, "\n")

# ============================================================
# 7. MODELES DE CLASSIFICATION SUR VALIDATION
# ============================================================

cat("\n========== MODELES DE CLASSIFICATION ==========\n")

# Créer des data.frames avec les coordonnées et les cibles pour l'entraînement
train_coords_full <- coord_train_disqual %>%
  select(-id_train) %>%
  mutate(target = y_train)

validation_coords_full <- coord_validation_disqual %>%
  select(-id_validation) %>%
  mutate(target = y_validation_clean)

# Préparer les données en matrice pour les modèles
X_train_lda <- train_coords_full %>% select(-target) %>% as.matrix()
X_validation_lda <- validation_coords_full %>% select(-target) %>% as.matrix()
y_train_lda <- train_coords_full$target
y_validation_lda <- validation_coords_full$target

# ============================================================
# A. MODELE LDA
# ============================================================

cat("\n--- LDA (Linear Discriminant Analysis) ---\n")

lda_model <- tryCatch({
  MASS::lda(
    x = X_train_lda,
    grouping = y_train_lda
  )
}, error = function(e) {
  cat("⚠ Erreur LDA :", e$message, "\n")
  NULL
})

lda_success <- !is.null(lda_model)
lda_prob_validation <- rep(NA, length(y_validation_lda))
lda_class_validation <- rep(NA, length(y_validation_lda))

if (lda_success) {
  # Prédictions sur validation
  lda_pred_validation <- tryCatch({
    predict(lda_model, newdata = X_validation_lda)
  }, error = function(e) {
    cat("⚠ Erreur prédiction LDA :", e$message, "\n")
    NULL
  })

  if (!is.null(lda_pred_validation)) {
    lda_class_validation <- lda_pred_validation$class
    lda_posterior_validation <- lda_pred_validation$posterior

    # Extraire les probabilités pour la classe "oui"
    if ("oui" %in% colnames(lda_posterior_validation)) {
      lda_prob_validation <- lda_posterior_validation[, "oui"]
    } else {
      lda_prob_validation <- lda_posterior_validation[, 2]
    }

    cat("✓ LDA entraîné avec succès\n")
  }
}

# ============================================================
# B. MODELE QDA
# ============================================================

cat("\n--- QDA (Quadratic Discriminant Analysis) ---\n")

qda_model <- tryCatch({
  MASS::qda(
    x = X_train_lda,
    grouping = y_train_lda
  )
}, error = function(e) {
  cat("⚠ Erreur QDA :", e$message, "\n")
  NULL
})

qda_success <- !is.null(qda_model)
qda_prob_validation <- rep(NA, length(y_validation_lda))
qda_class_validation <- rep(NA, length(y_validation_lda))

if (qda_success) {
  # Prédictions sur validation
  qda_pred_validation <- tryCatch({
    predict(qda_model, newdata = X_validation_lda)
  }, error = function(e) {
    cat("⚠ Erreur prédiction QDA :", e$message, "\n")
    NULL
  })

  if (!is.null(qda_pred_validation)) {
    qda_class_validation <- qda_pred_validation$class
    qda_posterior_validation <- qda_pred_validation$posterior

    # Extraire les probabilités pour la classe "oui"
    if ("oui" %in% colnames(qda_posterior_validation)) {
      qda_prob_validation <- qda_posterior_validation[, "oui"]
    } else {
      qda_prob_validation <- qda_posterior_validation[, 2]
    }

    cat("✓ QDA entraîné avec succès\n")
  }
}

# ============================================================
# C. MODELES KNN (K = 5, 10, 20, 50)
# ============================================================

cat("\n--- KNN (k-Nearest Neighbors) ---\n")

# Préparer les données pour KNN (utiliser les coordonnées ACM)
X_train_knn <- coord_train_disqual %>% select(-id_train)
X_validation_knn <- coord_validation_disqual %>% select(-id_validation)

knn_models <- list()
knn_results <- list()

for (k_val in c(5, 10, 20, 50)) {

  tryCatch({
    # Prédictions KNN
    knn_class <- class::knn(
      train = X_train_knn,
      test = X_validation_knn,
      cl = y_train,
      k = k_val,
      prob = TRUE
    )

    # Extraire les probabilités
    knn_prob <- attr(knn_class, "prob")
    # Convertir pour avoir la prob de "oui"
    knn_prob_oui <- ifelse(knn_class == "oui", knn_prob, 1 - knn_prob)

    knn_models[[paste0("knn_k", k_val)]] <- list(
      model = "KNN",
      k = k_val,
      class = knn_class,
      prob = knn_prob_oui
    )

    cat("✓ KNN k=", k_val, "entraîné avec succès\n", sep = "")
  }, error = function(e) {
    cat("⚠ Erreur KNN k=", k_val, ":", e$message, "\n", sep = "")
    knn_models[[paste0("knn_k", k_val)]] <- list(
      model = "KNN",
      k = k_val,
      class = rep(NA, nrow(X_validation_knn)),
      prob = rep(NA, nrow(X_validation_knn))
    )
  })
}

# ============================================================
# 8. EVALUATION COMPARATIVE SUR VALIDATION
# ============================================================

cat("\n========== EVALUATION SUR VALIDATION ==========\n")

evaluate_classification <- function(predicted_class, predicted_prob, actual) {

  # Gérer les NA
  if (all(is.na(predicted_prob)) || all(is.na(predicted_class))) {
    return(data.frame(
      accuracy = NA_real_,
      auc = NA_real_,
      tp = NA_real_,
      tn = NA_real_,
      fp = NA_real_,
      fn = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      f1_score = NA_real_
    ))
  }

  # Créer un facteur avec les niveaux corrects
  pred_factor <- factor(predicted_class, levels = c("non", "oui"))

  # Matrice de confusion
  cm <- tryCatch({
    caret::confusionMatrix(
      pred_factor,
      actual,
      positive = "oui"
    )
  }, error = function(e) {
    cat("Erreur matrice de confusion\n")
    return(NULL)
  })

  if (is.null(cm)) {
    return(data.frame(
      accuracy = NA_real_,
      auc = NA_real_,
      tp = NA_real_,
      tn = NA_real_,
      fp = NA_real_,
      fn = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      f1_score = NA_real_
    ))
  }

  # Calculer les métriques
  tn <- cm$table[1, 1]
  fp <- cm$table[1, 2]
  fn <- cm$table[2, 1]
  tp <- cm$table[2, 2]

  accuracy <- as.numeric(cm$overall["Accuracy"])
  sensitivity <- as.numeric(cm$byClass["Sensitivity"])
  specificity <- as.numeric(cm$byClass["Specificity"])
  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- sensitivity

  f1_score <- ifelse(
    (precision + recall) == 0,
    NA_real_,
    2 * precision * recall / (precision + recall)
  )

  # Calculer AUC
  auc_val <- tryCatch({
    roc_obj <- pROC::roc(
      response = actual,
      predictor = predicted_prob,
      levels = c("non", "oui"),
      quiet = TRUE
    )
    as.numeric(pROC::auc(roc_obj))
  }, error = function(e) {
    NA_real_
  })

  return(data.frame(
    accuracy = round(accuracy, 4),
    auc = round(auc_val, 4),
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    sensitivity = round(sensitivity, 4),
    specificity = round(specificity, 4),
    precision = round(precision, 4),
    recall = round(recall, 4),
    f1_score = round(f1_score, 4)
  ))
}

# Évaluation LDA
results_lda <- evaluate_classification(
  lda_class_validation,
  lda_prob_validation,
  y_validation_clean
)
results_lda$model <- "LDA"

# Évaluation QDA
results_qda <- evaluate_classification(
  qda_class_validation,
  qda_prob_validation,
  y_validation_clean
)
results_qda$model <- "QDA"

# Évaluation KNN
results_knn <- data.frame()
for (i in seq_along(knn_models)) {
  k_name <- names(knn_models)[i]
  k_num <- knn_models[[i]]$k

  results_temp <- evaluate_classification(
    knn_models[[i]]$class,
    knn_models[[i]]$prob,
    y_validation_clean
  )
  results_temp$model <- paste0("KNN (k=", k_num, ")")

  results_knn <- rbind(results_knn, results_temp)
}

# Compilation des résultats
disqual_validation_results <- rbind(results_lda, results_qda, results_knn)

cat("\n--- TABLEAU DE COMPARAISON DES MODELES (VALIDATION) ---\n")
print(disqual_validation_results)

# ============================================================
# 9. GRAPHIQUES ROC SUR VALIDATION
# ============================================================

cat("\n--- Graphiques ROC ---\n")

roc_list <- list()

# ROC LDA
if (lda_success && !all(is.na(lda_prob_validation))) {
  roc_list$lda <- tryCatch({
    pROC::roc(
      response = y_validation_clean,
      predictor = lda_prob_validation,
      levels = c("non", "oui"),
      quiet = TRUE
    )
  }, error = function(e) NULL)
}

# ROC QDA
if (qda_success && !all(is.na(qda_prob_validation))) {
  roc_list$qda <- tryCatch({
    pROC::roc(
      response = y_validation_clean,
      predictor = qda_prob_validation,
      levels = c("non", "oui"),
      quiet = TRUE
    )
  }, error = function(e) NULL)
}

# ROC KNN (pour chaque k)
for (i in seq_along(knn_models)) {
  k_name <- names(knn_models)[i]
  if (!all(is.na(knn_models[[i]]$prob))) {
    roc_list[[k_name]] <- tryCatch({
      pROC::roc(
        response = y_validation_clean,
        predictor = knn_models[[i]]$prob,
        levels = c("non", "oui"),
        quiet = TRUE
      )
    }, error = function(e) NULL)
  }
}

# Créer le graphique ROC
p_disqual_roc_validation <- ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               linetype = "dashed", color = "gray50", linewidth = 0.8) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Courbes ROC - Comparaison des modèles (Validation)",
    x = "1 - Spécificité",
    y = "Sensibilité",
    color = "Modèle"
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  coord_fixed()

colors <- c("LDA" = "#1f77b4", "QDA" = "#ff7f0e",
            "KNN (k=5)" = "#2ca02c", "KNN (k=10)" = "#d62728",
            "KNN (k=20)" = "#9467bd", "KNN (k=50)" = "#8c564b")

for (name in names(roc_list)) {
  if (!is.null(roc_list[[name]])) {
    coords <- pROC::coords(roc_list[[name]], "all")
    model_name <- ifelse(grepl("knn_k", name),
                        sub("knn_k", "KNN (k=", name) %>% paste0(")"),
                        name)

    p_disqual_roc_validation <- p_disqual_roc_validation +
      geom_line(
        data = as.data.frame(coords),
        aes(x = 1 - specificity, y = sensitivity, color = model_name),
        linewidth = 1
      )
  }
}

p_disqual_roc_validation <- p_disqual_roc_validation +
  scale_color_manual(values = colors)

# ============================================================
# 10. SELECTION DU MEILLEUR MODELE
# ============================================================

cat("\n========== SELECTION DU MEILLEUR MODELE ==========\n")

# Critère de sélection : AUC max, puis F1-score max, puis FN min
best_model <- disqual_validation_results %>%
  mutate(
    auc = ifelse(is.na(auc), -Inf, auc),
    f1_score = ifelse(is.na(f1_score), -Inf, f1_score),
    fn = ifelse(is.na(fn), Inf, fn)
  ) %>%
  arrange(desc(auc), desc(f1_score), fn) %>%
  slice(1)

best_disqual_model_name <- best_model$model

cat("\n✓ MEILLEUR MODELE : ", best_disqual_model_name, "\n")
cat("  - AUC validation : ", best_model$auc, "\n")
cat("  - Accuracy : ", best_model$accuracy, "\n")
cat("  - F1-score : ", best_model$f1_score, "\n")
cat("  - Faux négatifs : ", best_model$fn, "\n")

best_disqual_summary <- data.frame(
  modele = best_disqual_model_name,
  auc_validation = best_model$auc,
  accuracy_validation = best_model$accuracy,
  f1_score_validation = best_model$f1_score,
  faux_negatifs_validation = best_model$fn,
  justification = paste0(
    "Sélectionné sur la base de l'AUC la plus élevée (", best_model$auc, ") ",
    "et du F1-score (", best_model$f1_score, "). ",
    "Faux négatifs : ", best_model$fn, "."
  )
)

# ============================================================
# 11. REENTRENEMENT ET VALIDATION FINALE SUR TEST
# ============================================================

cat("\n========== VALIDATION FINALE SUR TEST ==========\n")

# Combiner train + validation
train_validation_combined <- rbind(
  df_disqual_train,
  df_disqual_validation_clean
)

y_train_validation_combined <- c(y_train, y_validation_clean)

# Réajuster l'ACM sur train + validation
res_mca_final <- tryCatch({
  FactoMineR::MCA(
    train_validation_combined,
    graph = FALSE,
    ncp = Inf
  )
}, error = function(e) {
  cat("Erreur ACM finale :", e$message, "\n")
  NULL
})

if (!is.null(res_mca_final)) {
  cat("\nACM réajustée sur train + validation\n")
  cat("Nombre d'axes :", res_mca_final$call$ncp, "\n")

  # Projeter test dans le nouvel espace ACM
  # Harmoniser les niveaux pour test
  df_disqual_test_final_harmonized <- df_disqual_test

  for (var in quali_vars_disqual) {
    levels_train_val <- levels(train_validation_combined[[var]])
    df_disqual_test_final_harmonized[[var]] <- factor(
      df_disqual_test_final_harmonized[[var]],
      levels = levels_train_val
    )
  }

  coord_test_final <- tryCatch({
    pred_test_final <- predict(res_mca_final, newdata = df_disqual_test_final_harmonized)
    as.data.frame(pred_test_final$coord)
  }, error = function(e) {
    cat("Erreur projection test finale :", e$message, "\n")
    data.frame(matrix(NA, nrow = nrow(df_disqual_test_final_harmonized), ncol = res_mca_final$call$ncp))
  })

  coord_test_final$id_test <- 1:nrow(coord_test_final)

  # Réentraîner le meilleur modèle sur train + validation
  train_validation_coords_full <- rbind(
    coord_train_disqual %>% select(-id_train) %>% mutate(target = y_train),
    coord_validation_disqual %>% select(-id_validation) %>% mutate(target = y_validation_clean)
  )

  X_train_validation <- train_validation_coords_full %>% select(-target) %>% as.matrix()
  X_test_final <- coord_test_final %>% select(-id_test) %>% as.matrix()
  y_train_validation <- train_validation_coords_full$target

  # Entraîner le meilleur modèle
  if (grepl("LDA", best_disqual_model_name)) {
    best_final_model <- tryCatch({
      MASS::lda(
        x = X_train_validation,
        grouping = y_train_validation
      )
    }, error = function(e) NULL)

    if (!is.null(best_final_model)) {
      best_pred_test <- tryCatch({
        predict(best_final_model, newdata = X_test_final)
      }, error = function(e) NULL)

      if (!is.null(best_pred_test)) {
        best_class_test <- best_pred_test$class
        best_posterior_test <- best_pred_test$posterior

        if ("oui" %in% colnames(best_posterior_test)) {
          best_prob_test <- best_posterior_test$posterior[, "oui"]
        } else {
          best_prob_test <- best_posterior_test$posterior[, 2]
        }
      }
    }

  } else if (grepl("QDA", best_disqual_model_name)) {
    best_final_model <- tryCatch({
      MASS::qda(
        x = X_train_validation,
        grouping = y_train_validation
      )
    }, error = function(e) NULL)

    if (!is.null(best_final_model)) {
      best_pred_test <- tryCatch({
        predict(best_final_model, newdata = X_test_final)
      }, error = function(e) NULL)

      if (!is.null(best_pred_test)) {
        best_class_test <- best_pred_test$class
        best_posterior_test <- best_pred_test$posterior

        if ("oui" %in% colnames(best_posterior_test)) {
          best_prob_test <- best_posterior_test$posterior[, "oui"]
        } else {
          best_prob_test <- best_posterior_test$posterior[, 2]
        }
      }
    }

  } else if (grepl("KNN", best_disqual_model_name)) {
    # Extraire k depuis le nom du modèle
    k_match <- as.numeric(gsub("[^0-9]", "", best_disqual_model_name))

    X_train_validation_knn <- train_validation_coords_full %>% select(-target)

    best_class_test <- tryCatch({
      class::knn(
        train = X_train_validation_knn,
        test = coord_test_final %>% select(-id_test),
        cl = y_train_validation,
        k = k_match,
        prob = TRUE
      )
    }, error = function(e) rep(NA, nrow(coord_test_final)))

    if (!all(is.na(best_class_test))) {
      best_prob_test <- attr(best_class_test, "prob")
      best_prob_test <- ifelse(best_class_test == "oui", best_prob_test, 1 - best_prob_test)
    } else {
      best_prob_test <- rep(NA, length(best_class_test))
    }
  }

  # Évaluation test
  disqual_test_metrics <- evaluate_classification(
    best_class_test,
    best_prob_test,
    y_test_clean
  )
  disqual_test_metrics$model <- best_disqual_model_name

  cat("\n--- METRIQUES FINALES SUR TEST ---\n")
  print(disqual_test_metrics)

  # Matrice de confusion test
  pred_test_factor <- factor(best_class_test, levels = c("non", "oui"))
  confusion_test <- tryCatch({
    caret::confusionMatrix(pred_test_factor, y_test_clean, positive = "oui")
  }, error = function(e) NULL)

  if (!is.null(confusion_test)) {
    disqual_test_confusion <- as.data.frame(confusion_test$table)
    disqual_test_confusion$Prediction <- rownames(disqual_test_confusion)
    disqual_test_confusion$Reference <- colnames(disqual_test_confusion)

    cat("\n--- MATRICE DE CONFUSION TEST ---\n")
    print(disqual_test_confusion)

    # ROC curve test
    roc_test <- tryCatch({
      pROC::roc(
        response = y_test_clean,
        predictor = best_prob_test,
        levels = c("non", "oui"),
        quiet = TRUE
      )
    }, error = function(e) NULL)

    if (!is.null(roc_test)) {
      p_disqual_roc_test <- ggplot(
        data.frame(
          specificity = 1 - pROC::coords(roc_test, "all")$specificity,
          sensitivity = pROC::coords(roc_test, "all")$sensitivity
        ),
        aes(x = specificity, y = sensitivity)
      ) +
        geom_line(color = "#2C7FB8", linewidth = 1) +
        geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                     linetype = "dashed", color = "gray50", linewidth = 0.8) +
        labs(
          title = paste0("Courbe ROC - ", best_disqual_model_name, " (Test)"),
          x = "1 - Spécificité",
          y = "Sensibilité",
          subtitle = paste0("AUC = ", round(as.numeric(pROC::auc(roc_test)), 4))
        ) +
        xlim(0, 1) +
        ylim(0, 1) +
        coord_fixed() +
        theme_minimal(base_size = 12)

      # Graphique de la matrice de confusion
      p_disqual_confusion_test <- disqual_test_confusion %>%
        ggplot(aes(x = Prediction, y = Reference, fill = Freq)) +
        geom_tile(color = "black", linewidth = 1) +
        geom_text(aes(label = Freq), size = 6, fontface = "bold") +
        scale_fill_gradient(low = "white", high = "#2C7FB8") +
        labs(
          title = paste0("Matrice de confusion - ", best_disqual_model_name, " (Test)"),
          x = "Prédiction",
          y = "Observation réelle"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          axis.text = element_text(size = 12),
          panel.grid = element_blank()
        )
    } else {
      p_disqual_roc_test <- ggplot() + theme_void()
      p_disqual_confusion_test <- ggplot() + theme_void()
    }
  } else {
    disqual_test_confusion <- data.frame()
    p_disqual_roc_test <- ggplot() + theme_void()
    p_disqual_confusion_test <- ggplot() + theme_void()
  }
} else {
  # Si l'ACM finale échoue, utiliser les résultats de validation
  disqual_test_metrics <- best_model
  disqual_test_confusion <- data.frame()
  p_disqual_roc_test <- ggplot() + theme_void()
  p_disqual_confusion_test <- ggplot() + theme_void()
}

# ============================================================
# 12. SORTIES SPECIFIQUES LDA (SI APPLICABLE)
# ============================================================

cat("\n========== RESULTATS SPECIFIQUES LDA ==========\n")

if (lda_success && !is.null(lda_model)) {
  # Moyennes de classe
  lda_class_means <- as.data.frame(lda_model$means)
  lda_class_means$classe <- rownames(lda_class_means)
  lda_class_means_table <- lda_class_means

  # Coefficients discriminants
  lda_coefficients <- as.data.frame(lda_model$scaling)
  lda_coefficients$dimension <- rownames(lda_coefficients)
  lda_coefficients_table <- lda_coefficients

  cat("✓ Moyennes de classe LDA disponibles\n")
  cat("✓ Coefficients discriminants LDA disponibles\n")

} else {

  lda_class_means_table <- data.frame(
    note = "LDA n'a pas pu être entraîné"
  )

  lda_coefficients_table <- data.frame(
    note = "LDA n'a pas pu être entraîné"
  )

  cat("⚠ LDA n'a pas pu être entraîné\n")
}

# ============================================================
# 13. SYNTHESE FINALE
# ============================================================

cat("\n" )
cat("========== SYNTHESE FINALE ==========\n")
cat("✓ ACM réalisée avec", n_axes_disqual, "axes\n")
cat("✓ ", nrow(disqual_validation_results), "modèles évalués sur validation\n")
cat("✓ Meilleur modèle : ", best_disqual_model_name, "\n")
cat("✓ AUC test : ", disqual_test_metrics$auc, "\n")
cat("✓ Accuracy test : ", disqual_test_metrics$accuracy, "\n")
cat("✓ F1-score test : ", disqual_test_metrics$f1_score, "\n")

# ============================================================
# 14. BLOC QUARTO COMMENTAIRE
# ============================================================

# Le code suivant montre comment utiliser les objets dans rapport.qmd :
#
# ```{r}
# source("scripts/classification.R")
# ```
#
# ## Section ACM
#
# ```{r}
# #| label: tbl-disqual-variables
# #| tbl-cap: "Description des variables DISQUAL"
#
# knitr::kable(disqual_variables_table, format = "latex", booktabs = TRUE)
# ```
#
# ```{r}
# #| label: fig-acm-inertia
# #| fig-cap: "Inertie expliquée par axe - ACM"
# #| fig-height: 6
# #| fig-width: 10
#
# p_acm_inertia
# ```
#
# ```{r}
# #| label: tbl-acm-eigen
# #| tbl-cap: "Valeurs propres et inertie - ACM"
#
# knitr::kable(acm_eigen_table %>% head(15), format = "latex", booktabs = TRUE)
# ```
#
# ## Section Tests de Normalité
#
# ```{r}
# #| label: tbl-normality
# #| tbl-cap: "Tests de normalité multivariée"
#
# knitr::kable(normality_tests_table, format = "latex", booktabs = TRUE)
# ```
#
# ```{r}
# #| label: tbl-boxm
# #| tbl-cap: "Test d'homogénéité des matrices de covariance"
#
# knitr::kable(boxm_test_table, format = "latex", booktabs = TRUE)
# ```
#
# ## Section Comparaison des Modèles
#
# ```{r}
# #| label: tbl-disqual-validation
# #| tbl-cap: "Comparaison des modèles sur validation"
#
# knitr::kable(disqual_validation_results %>% select(model, auc, accuracy, f1_score, sensitivity, specificity), format = "latex", booktabs = TRUE)
# ```
#
# ```{r}
# #| label: fig-disqual-roc-validation
# #| fig-cap: "Courbes ROC - Comparaison des modèles (Validation)"
# #| fig-height: 6
# #| fig-width: 8
#
# p_disqual_roc_validation
# ```
#
# ## Section Test Final
#
# ```{r}
# #| label: tbl-disqual-test
# #| tbl-cap: "Metrques finales sur test"
#
# knitr::kable(disqual_test_metrics %>% select(model, auc, accuracy, f1_score, sensitivity, specificity), format = "latex", booktabs = TRUE)
# ```
#
# ```{r}
# #| label: fig-disqual-roc-test
# #| fig-cap: "Courbe ROC - Test final"
#
# p_disqual_roc_test
# ```
#
# ```{r}
# #| label: fig-disqual-confusion-test
# #| fig-cap: "Matrice de confusion - Test final"
#
# p_disqual_confusion_test
# ```
#
# ## Section LDA (si applicable)
#
# ```{r}
# #| label: tbl-lda-means
# #| tbl-cap: "Moyennes de classe - LDA"
#
# knitr::kable(lda_class_means_table %>% head(10), format = "latex", booktabs = TRUE)
# ```
#
# ```{r}
# #| label: tbl-lda-coefficients
# #| tbl-cap: "Coefficients discriminants - LDA"
#
# knitr::kable(lda_coefficients_table %>% head(10), format = "latex", booktabs = TRUE)
# ```

cat("\n✓ Script classification.R complété avec succès !\n")

# ============================================================
# 3. ANALYSE EN COMPOSANTES MULTIPLES (ACM)
# ============================================================

cat("\n========== ANALYSE EN COMPOSANTES MULTIPLES ==========\n")

# Réaliser l'ACM uniquement sur la base train
res_mca <- FactoMineR::MCA(
  df_disqual_train,
  graph = FALSE,
  ncp = Inf  # Garder tous les axes
)

# Récupérer le nombre total d'axes disponibles
n_axes_disqual <- res_mca$call$ncp
cat("\nNombre total d'axes ACM :", n_axes_disqual, "\n")

# Valeurs propres et inertie
eigenvalues_mca <- res_mca$eig
n_axes_80_inertia <- sum(cumsum(eigenvalues_mca[, 3]) <= 80)
cat("Axes pour 80% de l'inertie :", n_axes_80_inertia, "\n")

acm_eigen_table <- as.data.frame(eigenvalues_mca) %>%
  rownames_to_column("axis") %>%
  as_tibble() %>%
  rename(
    Valeur_propre = 2,
    Inertie_pct = 3,
    Inertie_cumulee_pct = 4
  ) %>%
  mutate(
    Axe = paste0("Axe ", row_number()),
    Valeur_propre = round(Valeur_propre, 4),
    Inertie_pct = round(Inertie_pct, 2),
    Inertie_cumulee_pct = round(Inertie_cumulee_pct, 2)
  ) %>%
  select(Axe, Valeur_propre, Inertie_pct, Inertie_cumulee_pct)

cat("\nTable des valeurs propres (premiers 10 axes) :\n")
print(head(acm_eigen_table, 10))

# ============================================================
# GRAPHIQUES DE L'ACM
# ============================================================

# Graphique 1 : Inertie expliquée par axe
p_acm_inertia <- acm_eigen_table %>%
  head(min(15, nrow(acm_eigen_table))) %>%
  ggplot(aes(x = reorder(Axe, -Inertie_pct), y = Inertie_pct)) +
  geom_col(fill = "#2C7FB8", alpha = 0.7) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Inertie expliquée par axe - ACM",
    x = "Axe",
    y = "Inertie expliquée (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graphique 2 : Inertie cumulée
p_acm_inertia_cumsum <- acm_eigen_table %>%
  head(min(15, nrow(acm_eigen_table))) %>%
  ggplot(aes(x = reorder(Axe, row_number()), y = Inertie_cumulee_pct)) +
  geom_line(group = 1, color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 3) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Inertie cumulée - ACM",
    x = "Axe",
    y = "Inertie cumulée (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_acm_inertia <- p_acm_inertia / p_acm_inertia_cumsum

# Graphique 3 : Modalités sur les deux premiers axes (si données disponibles)
tryCatch({
  p_acm_modalities <- FactoMineR::plot.MCA(
    res_mca,
    choix = "var",
    invisible = c("ind"),
    cex = 0.8
  )
  p_acm_modalities <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "Voir plot.MCA(res_mca, choix='var')")
}, error = function(e) {
  cat("Attention : Graphique des modalités non disponible\n")
  p_acm_modalities <<- ggplot() + theme_void()
})

# ============================================================
# 4. EXTRACTION DES COORDONNEES
# ============================================================

cat("\n========== EXTRACTION DES COORDONNEES ACM ==========\n")

# Coordonnées des individus train
coord_train_disqual <- res_mca$ind$coord %>%
  as.data.frame() %>%
  as_tibble(rownames = "id_train") %>%
  mutate(id_train = as.numeric(id_train))

# Fonction pour nettoyer les données avant projection
clean_data_for_projection <- function(data, reference_data, quali_vars) {
  # Identifier les observations avec des catégories manquantes dans train
  valid_obs <- rep(TRUE, nrow(data))

  for (var in quali_vars) {
    levels_ref <- levels(reference_data[[var]])
    # Garder seulement les observations dont toutes les valeurs sont dans les niveaux de référence
    valid_obs <- valid_obs & (data[[var]] %in% levels_ref)
  }

  # Retourner les données filtrées
  data[valid_obs, , drop = FALSE]
}

# Nettoyer validation et test pour la projection
df_disqual_validation_clean <- clean_data_for_projection(
  df_disqual_validation,
  df_disqual_train,
  quali_vars_disqual
)

df_disqual_test_clean <- clean_data_for_projection(
  df_disqual_test,
  df_disqual_train,
  quali_vars_disqual
)

cat("Observations validation après nettoyage :", nrow(df_disqual_validation_clean), "/", nrow(df_disqual_validation), "\n")
cat("Observations test après nettoyage :", nrow(df_disqual_test_clean), "/", nrow(df_disqual_test), "\n")

# Projection des individus de validation (en tant qu'individus supplémentaires)
coord_validation_disqual <- predict(
  res_mca,
  newdata = df_disqual_validation_clean
)$coord %>%
  as.data.frame() %>%
  as_tibble(rownames = "id_validation") %>%
  mutate(id_validation = as.numeric(id_validation))

# Projection des individus de test
coord_test_disqual <- predict(
  res_mca,
  newdata = df_disqual_test_clean
)$coord %>%
  as.data.frame() %>%
  as_tibble(rownames = "id_test") %>%
  mutate(id_test = as.numeric(id_test))

cat("✓ Coordonnées train :", nrow(coord_train_disqual), "x", ncol(coord_train_disqual), "\n")
cat("✓ Coordonnées validation :", nrow(coord_validation_disqual), "x", ncol(coord_validation_disqual), "\n")
cat("✓ Coordonnées test :", nrow(coord_test_disqual), "x", ncol(coord_test_disqual), "\n")

# Ajuster les cibles pour correspondre aux données nettoyées
# Identifier les indices des observations valides
valid_indices_validation <- which(apply(df_disqual_validation, 1, function(row) {
  all(sapply(quali_vars_disqual, function(var) row[var] %in% levels(df_disqual_train[[var]])))
}))

valid_indices_test <- which(apply(df_disqual_test, 1, function(row) {
  all(sapply(quali_vars_disqual, function(var) row[var] %in% levels(df_disqual_train[[var]])))
}))

# Filtrer les cibles correspondantes
y_validation_clean <- y_validation[valid_indices_validation]
y_test_clean <- y_test[valid_indices_test]

cat("✓ Cibles validation après nettoyage :", length(y_validation_clean), "\n")
cat("✓ Cibles test après nettoyage :", length(y_test_clean), "\n")

# ============================================================
# 5. POUVOIR DISCRIMINANT DES AXES
# ============================================================

cat("\n========== POUVOIR DISCRIMINANT DES AXES ==========\n")

# Pour chaque axe, calculer les statistiques de discrimination
axis_discrimination_table <- tibble()

for (i in 1:n_axes_disqual) {
  
  coord_name <- paste0("Dim.", i)
  
  # Moyennes par classe
  mean_non <- mean(coord_train_disqual[[coord_name]][y_train == "non"], na.rm = TRUE)
  mean_oui <- mean(coord_train_disqual[[coord_name]][y_train == "oui"], na.rm = TRUE)
  
  diff_abs <- abs(mean_non - mean_oui)
  
  # Test t simple
  t_test <- t.test(
    coord_train_disqual[[coord_name]][y_train == "non"],
    coord_train_disqual[[coord_name]][y_train == "oui"]
  )
  
  p_value <- t_test$p.value
  
  # AUC univariée
  roc_obj <- pROC::roc(
    response = y_train,
    predictor = coord_train_disqual[[coord_name]],
    levels = c("non", "oui"),
    quiet = TRUE
  )
  
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  axis_discrimination_table <- axis_discrimination_table %>%
    bind_rows(tibble(
      axe = coord_name,
      mean_non = round(mean_non, 4),
      mean_oui = round(mean_oui, 4),
      diff_abs_moyennes = round(diff_abs, 4),
      p_value = round(p_value, 4),
      auc_univariee = round(auc_val, 4)
    ))
}

cat("\nTable du pouvoir discriminant (premiers 10 axes) :\n")
print(head(axis_discrimination_table, 10))

# Graphique du pouvoir discriminant
p_axis_discrimination <- axis_discrimination_table %>%
  head(min(15, nrow(axis_discrimination_table))) %>%
  ggplot(aes(x = reorder(axe, -auc_univariee), y = auc_univariee)) +
  geom_col(fill = "#2C7FB8", alpha = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "AUC univariée de chaque axe ACM",
    x = "Axe",
    y = "AUC univariée"
  ) +
  ylim(0.4, 1) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================
# 6. TESTS D'HYPOTHESES AVANT LDA/QDA
# ============================================================

cat("\n========== TESTS D'HYPOTHESES ==========\n")

# A. TEST DE NORMALITE MULTIVARIEE

cat("\n--- Test de normalité multivariée ---\n")

# Préparation des données pour les tests
coord_train_matrix <- coord_train_disqual %>%
  select(-id_train) %>%
  as.matrix()

# Test de Mardia
tryCatch({
  mardia_test <- MVN::mardia(coord_train_matrix)
  mardia_skewness_p <- mardia_test$univariateNormality$p.value[1]
  mardia_kurtosis_p <- mardia_test$univariateNormality$p.value[2]
  mardia_overall <- mardia_test$multivariateNormality$p.value[2]
  
  normality_tests_table <- tibble(
    test = "Mardia",
    statistic = NA_character_,
    p_value = mardia_overall,
    conclusion = if_else(mardia_overall > 0.05, "Normal", "Non-normal")
  )
  
  cat("✓ Test de Mardia : p-value =", round(mardia_overall, 4), "\n")
}, error = function(e) {
  cat("⚠ Test de Mardia non disponible\n")
  normality_tests_table <<- tibble(
    test = "Mardia",
    statistic = NA_character_,
    p_value = NA_real_,
    conclusion = "Non calculé"
  )
})

# Test de Henze-Zirkler
tryCatch({
  hz_test <- MVN::hzTest(coord_train_matrix)
  hz_p <- hz_test$p.value
  
  normality_tests_table <- normality_tests_table %>%
    bind_rows(tibble(
      test = "Henze-Zirkler",
      statistic = NA_character_,
      p_value = hz_p,
      conclusion = if_else(hz_p > 0.05, "Normal", "Non-normal")
    ))
  
  cat("✓ Test de Henze-Zirkler : p-value =", round(hz_p, 4), "\n")
}, error = function(e) {
  cat("⚠ Test de Henze-Zirkler non disponible\n")
  normality_tests_table <<- normality_tests_table %>%
    bind_rows(tibble(
      test = "Henze-Zirkler",
      statistic = NA_character_,
      p_value = NA_real_,
      conclusion = "Non calculé"
    ))
})

cat("\nConclusion sur la normalité multivariée :\n")
print(normality_tests_table)

# B. TEST D'HOMOGENEITE DES MATRICES DE COVARIANCE (TEST DE BOX M)

cat("\n--- Test d'homogénéité des matrices de covariance (Box M) ---\n")

# Créer un data.frame avec les coordonnées et la cible
coord_train_with_target <- coord_train_disqual %>%
  select(-id_train) %>%
  mutate(target = y_train)

tryCatch({
  boxm_result <- biotools::boxM(
    coord_train_with_target %>% select(-target),
    coord_train_with_target$target
  )
  
  boxm_stat <- boxm_result$statistic
  boxm_p <- boxm_result$p.value
  
  boxm_test_table <- tibble(
    test = "Box M",
    statistic = round(boxm_stat, 4),
    p_value = round(boxm_p, 4),
    conclusion = if_else(boxm_p > 0.05, 
                         "Matrices homogènes", 
                         "Matrices hétérogènes")
  )
  
  cat("✓ Statistique Box M :", round(boxm_stat, 4), "\n")
  cat("✓ p-value :", round(boxm_p, 4), "\n")
  cat("✓ Conclusion :", if_else(boxm_p > 0.05, 
                                "Matrices homogènes", 
                                "Matrices hétérogènes"), "\n")
}, error = function(e) {
  cat("⚠ Test Box M non disponible :", e$message, "\n")
  boxm_test_table <<- tibble(
    test = "Box M",
    statistic = NA_real_,
    p_value = NA_real_,
    conclusion = "Non calculé"
  )
})

# ============================================================
# 7. MODELES DE CLASSIFICATION SUR VALIDATION
# ============================================================

cat("\n========== MODELES DE CLASSIFICATION ==========\n")

# Créer des data.frames avec les coordonnées et les cibles pour l'entraînement
train_coords_full <- coord_train_disqual %>%
  select(-id_train) %>%
  mutate(target = y_train)

validation_coords_full <- coord_validation_disqual %>%
  select(-id_validation) %>%
  mutate(target = y_validation_clean)

# Préparer les données en matrice pour les modèles
X_train_lda <- train_coords_full %>% select(-target) %>% as.matrix()
X_validation_lda <- validation_coords_full %>% select(-target) %>% as.matrix()
y_train_lda <- train_coords_full$target
y_validation_lda <- validation_coords_full$target

# ============================================================
# A. MODELE LDA
# ============================================================

cat("\n--- LDA (Linear Discriminant Analysis) ---\n")

lda_model <- tryCatch({
  MASS::lda(
    x = X_train_lda,
    grouping = y_train_lda
  )
}, error = function(e) {
  cat("⚠ Erreur LDA :", e$message, "\n")
  NULL
})

if (!is.null(lda_model)) {
  # Prédictions sur validation
  lda_pred_validation <- predict(lda_model, newdata = X_validation_lda)
  lda_class_validation <- lda_pred_validation$class
  lda_posterior_validation <- lda_pred_validation$posterior
  
  # Extraire les probabilités pour la classe "oui"
  if ("oui" %in% colnames(lda_posterior_validation)) {
    lda_prob_validation <- lda_posterior_validation[, "oui"]
  } else {
    lda_prob_validation <- lda_posterior_validation[, 2]
  }
  
  cat("✓ LDA entraîné avec succès\n")
  lda_success <- TRUE
} else {
  lda_success <- FALSE
  lda_prob_validation <- rep(NA, length(y_validation_lda))
  lda_class_validation <- rep(NA, length(y_validation_lda))
}

# ============================================================
# B. MODELE QDA
# ============================================================

cat("\n--- QDA (Quadratic Discriminant Analysis) ---\n")

qda_model <- tryCatch({
  MASS::qda(
    x = X_train_lda,
    grouping = y_train_lda
  )
}, error = function(e) {
  cat("⚠ Erreur QDA :", e$message, "\n")
  NULL
})

if (!is.null(qda_model)) {
  # Prédictions sur validation
  qda_pred_validation <- predict(qda_model, newdata = X_validation_lda)
  qda_class_validation <- qda_pred_validation$class
  qda_posterior_validation <- qda_pred_validation$posterior
  
  # Extraire les probabilités pour la classe "oui"
  if ("oui" %in% colnames(qda_posterior_validation)) {
    qda_prob_validation <- qda_posterior_validation[, "oui"]
  } else {
    qda_prob_validation <- qda_posterior_validation[, 2]
  }
  
  cat("✓ QDA entraîné avec succès\n")
  qda_success <- TRUE
} else {
  qda_success <- FALSE
  qda_prob_validation <- rep(NA, length(y_validation_lda))
  qda_class_validation <- rep(NA, length(y_validation_lda))
}

# ============================================================
# C. MODELES KNN (K = 5, 10, 20, 50)
# ============================================================

cat("\n--- KNN (k-Nearest Neighbors) ---\n")

# Préparer les données pour KNN (utiliser les coordonnées ACM)
X_train_knn <- coord_train_disqual %>% select(-id_train)
X_validation_knn <- coord_validation_disqual %>% select(-id_validation)

knn_models <- list()
knn_results <- list()

for (k_val in c(5, 10, 20, 50)) {
  
  tryCatch({
    # Prédictions KNN
    knn_class <- class::knn(
      train = X_train_knn,
      test = X_validation_knn,
      cl = y_train,
      k = k_val,
      prob = TRUE
    )
    
    # Extraire les probabilités
    knn_prob <- attr(knn_class, "prob")
    # Convertir pour avoir la prob de "oui"
    knn_prob_oui <- ifelse(knn_class == "oui", knn_prob, 1 - knn_prob)
    
    knn_models[[paste0("knn_k", k_val)]] <- list(
      model = "KNN",
      k = k_val,
      class = knn_class,
      prob = knn_prob_oui
    )
    
    cat("✓ KNN k=", k_val, "entraîné avec succès\n", sep = "")
  }, error = function(e) {
    cat("⚠ Erreur KNN k=", k_val, ":", e$message, "\n", sep = "")
    knn_models[[paste0("knn_k", k_val)]] <<- list(
      model = "KNN",
      k = k_val,
      class = rep(NA, nrow(X_validation_knn)),
      prob = rep(NA, nrow(X_validation_knn))
    )
  })
}

# ============================================================
# 8. EVALUATION COMPARATIVE SUR VALIDATION
# ============================================================

cat("\n========== EVALUATION SUR VALIDATION ==========\n")

evaluate_classification <- function(predicted_class, predicted_prob, actual, model_name) {
  
  # Gérer les NA
  if (all(is.na(predicted_prob))) {
    return(tibble(
      model = model_name,
      accuracy = NA_real_,
      auc = NA_real_,
      tp = NA_real_,
      tn = NA_real_,
      fp = NA_real_,
      fn = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      f1_score = NA_real_
    ))
  }
  
  # Créer un facteur avec les niveaux corrects
  pred_factor <- factor(predicted_class, levels = c("non", "oui"))
  
  # Matrice de confusion
  cm <- caret::confusionMatrix(
    pred_factor,
    actual,
    positive = "oui"
  )
  
  # Calculer les métriques
  tn <- cm$table[1, 1]
  fp <- cm$table[1, 2]
  fn <- cm$table[2, 1]
  tp <- cm$table[2, 2]
  
  accuracy <- as.numeric(cm$overall["Accuracy"])
  sensitivity <- as.numeric(cm$byClass["Sensitivity"])
  specificity <- as.numeric(cm$byClass["Specificity"])
  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- sensitivity
  
  f1_score <- ifelse(
    (precision + recall) == 0,
    NA_real_,
    2 * precision * recall / (precision + recall)
  )
  
  # Calculer AUC
  roc_obj <- pROC::roc(
    response = actual,
    predictor = predicted_prob,
    levels = c("non", "oui"),
    quiet = TRUE
  )
  
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  return(tibble(
    model = model_name,
    accuracy = round(accuracy, 4),
    auc = round(auc_val, 4),
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    sensitivity = round(sensitivity, 4),
    specificity = round(specificity, 4),
    precision = round(precision, 4),
    recall = round(recall, 4),
    f1_score = round(f1_score, 4)
  ))
}

# Évaluation LDA
results_lda <- evaluate_classification(
  lda_class_validation,
  lda_prob_validation,
  y_validation_clean,
  "LDA"
)

# Évaluation QDA
results_qda <- evaluate_classification(
  qda_class_validation,
  qda_prob_validation,
  y_validation_clean,
  "QDA"
)

# Évaluation KNN
results_knn <- tibble()
for (i in seq_along(knn_models)) {
  k_name <- names(knn_models)[i]
  k_num <- knn_models[[i]]$k
  
  results_knn <- results_knn %>%
    bind_rows(evaluate_classification(
      knn_models[[i]]$class,
      knn_models[[i]]$prob,
      y_validation_clean,
      paste0("KNN (k=", k_num, ")")
    ))
}

# Compilation des résultats
disqual_validation_results <- bind_rows(
  results_lda,
  results_qda,
  results_knn
)

cat("\n--- TABLEAU DE COMPARAISON DES MODELES (VALIDATION) ---\n")
print(disqual_validation_results)

# ============================================================
# 9. GRAPHIQUES ROC SUR VALIDATION
# ============================================================

cat("\n--- Graphiques ROC ---\n")

roc_list <- list()

# ROC LDA
if (lda_success) {
  roc_list$lda <- pROC::roc(
    response = y_validation_clean,
    predictor = lda_prob_validation,
    levels = c("non", "oui"),
    quiet = TRUE
  )
}

# ROC QDA
if (qda_success) {
  roc_list$qda <- pROC::roc(
    response = y_validation_clean,
    predictor = qda_prob_validation,
    levels = c("non", "oui"),
    quiet = TRUE
  )
}

# ROC KNN (pour chaque k)
for (i in seq_along(knn_models)) {
  k_name <- names(knn_models)[i]
  if (!all(is.na(knn_models[[i]]$prob))) {
    roc_list[[k_name]] <- pROC::roc(
      response = y_validation_clean,
      predictor = knn_models[[i]]$prob,
      levels = c("non", "oui"),
      quiet = TRUE
    )
  }
}

# Créer le graphique ROC
p_disqual_roc_validation <- ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               linetype = "dashed", color = "gray50", linewidth = 0.8) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Courbes ROC - Comparaison des modèles (Validation)",
    x = "1 - Spécificité",
    y = "Sensibilité",
    color = "Modèle"
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  coord_fixed()

colors <- c("LDA" = "#1f77b4", "QDA" = "#ff7f0e", 
            "KNN (k=5)" = "#2ca02c", "KNN (k=10)" = "#d62728",
            "KNN (k=20)" = "#9467bd", "KNN (k=50)" = "#8c564b")

for (name in names(roc_list)) {
  coords <- pROC::coords(roc_list[[name]], "all")
  model_name <- gsub("_", " ", sub("^knn_", "KNN (k=", name))
  model_name <- sub("$", ")", model_name)
  
  p_disqual_roc_validation <- p_disqual_roc_validation +
    geom_line(
      data = as.data.frame(coords),
      aes(x = 1 - specificity, y = sensitivity, color = model_name),
      linewidth = 1
    )
}

p_disqual_roc_validation <- p_disqual_roc_validation +
  scale_color_manual(values = colors)

# ============================================================
# 10. SELECTION DU MEILLEUR MODELE
# ============================================================

cat("\n========== SELECTION DU MEILLEUR MODELE ==========\n")

# Critère de sélection : AUC max, puis F1-score max, puis FN min
best_model <- disqual_validation_results %>%
  mutate(
    auc = ifelse(is.na(auc), -Inf, auc),
    f1_score = ifelse(is.na(f1_score), -Inf, f1_score),
    fn = ifelse(is.na(fn), Inf, fn)
  ) %>%
  arrange(desc(auc), desc(f1_score), fn) %>%
  slice(1)

best_disqual_model_name <- best_model$model

cat("\n✓ MEILLEUR MODELE : ", best_disqual_model_name, "\n")
cat("  - AUC validation : ", best_model$auc, "\n")
cat("  - Accuracy : ", best_model$accuracy, "\n")
cat("  - F1-score : ", best_model$f1_score, "\n")
cat("  - Faux négatifs : ", best_model$fn, "\n")

best_disqual_summary <- tibble(
  modele = best_disqual_model_name,
  auc_validation = best_model$auc,
  accuracy_validation = best_model$accuracy,
  f1_score_validation = best_model$f1_score,
  faux_negatifs_validation = best_model$fn,
  justification = paste0(
    "Sélectionné sur la base de l'AUC la plus élevée (", best_model$auc, ") ",
    "et du F1-score (", best_model$f1_score, "). ",
    "Faux négatifs : ", best_model$fn, "."
  )
)

# ============================================================
# 11. REENTRENEMENT ET VALIDATION FINALE SUR TEST
# ============================================================

cat("\n========== VALIDATION FINALE SUR TEST ==========\n")

# Combiner train + validation
train_validation_combined <- bind_rows(
  train_disqual_source,
  validation_disqual_source
)

y_train_validation_combined <- c(y_train, y_validation)

# Réajuster l'ACM sur train + validation
res_mca_final <- FactoMineR::MCA(
  train_validation_combined,
  graph = FALSE,
  ncp = Inf
)

cat("\nACM réajustée sur train + validation\n")
cat("Nombre d'axes :", res_mca_final$call$ncp, "\n")

# Projeter test dans le nouvel espace ACM
# Nettoyer test pour la projection
df_disqual_test_final_clean <- clean_data_for_projection(
  df_disqual_test,
  train_validation_combined,
  quali_vars_disqual
)

cat("Observations test final après nettoyage :", nrow(df_disqual_test_final_clean), "/", nrow(df_disqual_test), "\n")

coord_test_final <- predict(
  res_mca_final,
  newdata = df_disqual_test_final_clean
)$coord %>%
  as.data.frame() %>%
  as_tibble(rownames = "id_test") %>%
  mutate(id_test = as.numeric(id_test))

# Réentraîner le meilleur modèle sur train + validation
train_validation_coords_full <- bind_rows(
  coord_train_disqual %>%
    select(-id_train) %>%
    mutate(target = y_train),
  coord_validation_disqual %>%
    select(-id_validation) %>%
    mutate(target = y_validation)
)

X_train_validation <- train_validation_coords_full %>% select(-target) %>% as.matrix()
X_test_final <- coord_test_final %>% select(-id_test) %>% as.matrix()
y_train_validation <- train_validation_coords_full$target

# Entraîner le meilleur modèle
if (grepl("LDA", best_disqual_model_name)) {
  best_final_model <- MASS::lda(
    x = X_train_validation,
    grouping = y_train_validation
  )
  
  best_pred_test <- predict(best_final_model, newdata = X_test_final)
  best_class_test <- best_pred_test$class
  
  if ("oui" %in% colnames(best_pred_test$posterior)) {
    best_prob_test <- best_pred_test$posterior[, "oui"]
  } else {
    best_prob_test <- best_pred_test$posterior[, 2]
  }
  
} else if (grepl("QDA", best_disqual_model_name)) {
  best_final_model <- MASS::qda(
    x = X_train_validation,
    grouping = y_train_validation
  )
  
  best_pred_test <- predict(best_final_model, newdata = X_test_final)
  best_class_test <- best_pred_test$class
  
  if ("oui" %in% colnames(best_pred_test$posterior)) {
    best_prob_test <- best_pred_test$posterior[, "oui"]
  } else {
    best_prob_test <- best_pred_test$posterior[, 2]
  }
  
} else if (grepl("KNN", best_disqual_model_name)) {
  # Extraire k depuis le nom du modèle
  k_match <- as.numeric(gsub("[^0-9]", "", best_disqual_model_name))
  
  X_train_validation_knn <- train_validation_coords_full %>% select(-target)
  
  best_class_test <- class::knn(
    train = X_train_validation_knn,
    test = coord_test_final %>% select(-id_test),
    cl = y_train_validation,
    k = k_match,
    prob = TRUE
  )
  
  best_prob_test <- attr(best_class_test, "prob")
  best_prob_test <- ifelse(best_class_test == "oui", best_prob_test, 1 - best_prob_test)
}

# Évaluation test
disqual_test_metrics <- evaluate_classification(
  best_class_test,
  best_prob_test,
  y_test_clean,
  best_disqual_model_name
)

cat("\n--- METRIQUES FINALES SUR TEST ---\n")
print(disqual_test_metrics)

# Matrice de confusion test
pred_test_factor <- factor(best_class_test, levels = c("non", "oui"))
confusion_test <- caret::confusionMatrix(pred_test_factor, y_test_clean)

disqual_test_confusion <- as.data.frame(confusion_test$table) %>%
  as_tibble() %>%
  rename(
    Prédiction = Prediction,
    Actual = Reference,
    Frequency = Freq
  )

cat("\n--- MATRICE DE CONFUSION TEST ---\n")
print(disqual_test_confusion)

# ROC curve test
roc_test <- pROC::roc(
  response = y_test_clean,
  predictor = best_prob_test,
  levels = c("non", "oui"),
  quiet = TRUE
)

p_disqual_roc_test <- ggplot(
  data.frame(
    specificity = 1 - pROC::coords(roc_test, "all")$specificity,
    sensitivity = pROC::coords(roc_test, "all")$sensitivity
  ),
  aes(x = specificity, y = sensitivity)
) +
  geom_line(color = "#2C7FB8", linewidth = 1) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               linetype = "dashed", color = "gray50", linewidth = 0.8) +
  labs(
    title = paste0("Courbe ROC - ", best_disqual_model_name, " (Test)"),
    x = "1 - Spécificité",
    y = "Sensibilité",
    subtitle = paste0("AUC = ", round(as.numeric(pROC::auc(roc_test)), 4))
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  coord_fixed() +
  theme_minimal(base_size = 12)

# Graphique de la matrice de confusion
p_disqual_confusion_test <- disqual_test_confusion %>%
  ggplot(aes(x = Prédiction, y = Actual, fill = Frequency)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(aes(label = Frequency), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#2C7FB8") +
  labs(
    title = paste0("Matrice de confusion - ", best_disqual_model_name, " (Test)"),
    x = "Prédiction",
    y = "Observation réelle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(size = 12),
    panel.grid = element_blank()
  )

# ============================================================
# 12. SORTIES SPECIFIQUES LDA (SI APPLICABLE)
# ============================================================

cat("\n========== RESULTATS SPECIFIQUES LDA ==========\n")

if (lda_success) {
  # Moyennes de classe
  lda_class_means <- as.data.frame(lda_model$means) %>%
    rownames_to_column("classe") %>%
    as_tibble()
  
  lda_class_means_table <- lda_class_means %>%
    head(min(10, nrow(lda_class_means)))
  
  cat("✓ Moyennes de classe LDA disponibles\n")
  
  # Coefficients discriminants
  lda_coeff <- as.data.frame(lda_model$scaling) %>%
    rownames_to_column("dimension") %>%
    as_tibble() %>%
    head(min(10, nrow(.)))
  
  lda_coefficients_table <- lda_coeff
  
  cat("✓ Coefficients discriminants LDA disponibles\n")
  
} else {
  
  lda_class_means_table <- tibble(
    note = "LDA n'a pas pu être entraîné"
  )
  
  lda_coefficients_table <- tibble(
    note = "LDA n'a pas pu être entraîné"
  )
  
  cat("⚠ LDA n'a pas pu être entraîné\n")
}

# ============================================================
# 13. SYNTHESE FINALE
# ============================================================

cat("\n" )
cat("========== SYNTHESE FINALE ==========\n")
cat("✓ ACM réalisée avec", n_axes_disqual, "axes\n")
cat("✓ ", nrow(disqual_validation_results), "modèles évalués sur validation\n")
cat("✓ Meilleur modèle : ", best_disqual_model_name, "\n")
cat("✓ AUC test : ", disqual_test_metrics$auc, "\n")
cat("✓ Accuracy test : ", disqual_test_metrics$accuracy, "\n")
cat("✓ F1-score test : ", disqual_test_metrics$f1_score, "\n")

# ============================================================
# 14. BLOC QUARTO COMMENTAIRE
# ============================================================

# Le code suivant montre comment utiliser les objets dans rapport.qmd :

# ====== EXEMPLE DE CHUNKS QUARTO ======
# 
# ```{r}
# source("scripts/classification.R")
# ```
# 
# ## Section ACM
# 
# ```{r}
# #| label: tbl-disqual-variables
# #| tbl-cap: "Description des variables DISQUAL"
# 
# disqual_variables_table %>%
#   knitr::kable(format = "latex", booktabs = TRUE)
# ```
# 
# ```{r}
# #| label: fig-acm-inertia
# #| fig-cap: "Inertie expliquée par axe - ACM"
# #| fig-height: 6
# #| fig-width: 10
# 
# p_acm_inertia
# ```
# 
# ```{r}
# #| label: tbl-acm-eigen
# #| tbl-cap: "Valeurs propres et inertie - ACM"
# 
# acm_eigen_table %>%
#   head(15) %>%
#   knitr::kable(format = "latex", booktabs = TRUE)
# ```
# 
# ## Section Tests de Normalité
# 
# ```{r}
# #| label: tbl-normality
# #| tbl-cap: "Tests de normalité multivariée"
# 
# normality_tests_table %>%
#   knitr::kable(format = "latex", booktabs = TRUE)
# ```
# 
# ```{r}
# #| label: tbl-boxm
# #| tbl-cap: "Test d'homogénéité des matrices de covariance"
# 
# boxm_test_table %>%
#   knitr::kable(format = "latex", booktabs = TRUE)
# ```
# 
# ## Section Comparaison des Modèles
# 
# ```{r}
# #| label: tbl-disqual-validation
# #| tbl-cap: "Comparaison des modèles sur validation"
# 
# disqual_validation_results %>%
#   select(model, auc, accuracy, f1_score, sensitivity, specificity) %>%
#   knitr::kable(format = "latex", booktabs = TRUE)
# ```
# 
# ```{r}
# #| label: fig-disqual-roc-validation
# #| fig-cap: "Courbes ROC - Comparaison des modèles (Validation)"
# #| fig-height: 6
# #| fig-width: 8
# 
# p_disqual_roc_validation
# ```
# 
# ## Section Test Final
# 
# ```{r}
# #| label: tbl-disqual-test
# #| tbl-cap: "Metriques finales sur test"
# 
# disqual_test_metrics %>%
#   select(model, auc, accuracy, f1_score, sensitivity, specificity) %>%
#   knitr::kable(format = "latex", booktabs = TRUE)
# ```
# 
# ```{r}
# #| label: fig-disqual-roc-test
# #| fig-cap: "Courbe ROC - Test final"
# 
# p_disqual_roc_test
# ```
# 
# ```{r}
# #| label: fig-disqual-confusion-test
# #| fig-cap: "Matrice de confusion - Test final"
# 
# p_disqual_confusion_test
# ```
# 
# ## Section LDA (si applicable)
# 
# ```{r}
# #| label: tbl-lda-means
# #| tbl-cap: "Moyennes de classe - LDA"
# 
# lda_class_means_table %>%
#   knitr::kable(format = "latex", booktabs = TRUE)
# ```
# 
# ```{r}
# #| label: tbl-lda-coefficients
# #| tbl-cap: "Coefficients discriminants - LDA"
# 
# lda_coefficients_table %>%
#   knitr::kable(format = "latex", booktabs = TRUE)
# ```

cat("\n✓ Script classification.R complété avec succès !\n")
