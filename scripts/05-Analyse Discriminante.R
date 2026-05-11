source("scripts/04-data preparation.R")

# ============================================================
# ANALYSE DISCRIMINANTE SUR AXES D'ACM
# ============================================================

# Methodologie predictive :
# 1. selection : ACM estimee sur train uniquement, validation/test en
#    individus supplementaires ;
# 2. choix du nombre d'axes et du modele LDA/QDA/KNN sur validation
#    avec l'AUC, independante du seuil ;
# 3. optimisation du seuil seulement pour le modele retenu ;
# 4. modele final : ACM reestimee sur train + validation, test projete
#    en individu supplementaire ;
# 5. le test final est evalue dans scripts/07- Comparaison.R.

# ============================================================
# 1. HARMONISATION DES FACTEURS
# ============================================================

active_vars <- names(df_model)[sapply(df_model, is.factor)]
active_vars <- setdiff(active_vars, "is_canceled")

# Les niveaux de reference sont pris sur train pour eviter d'utiliser
# la structure de validation/test dans la preparation du modele.
train_levels <- lapply(
  train |>
    dplyr::select(all_of(c(active_vars, "is_canceled"))),
  levels
)

align_levels <- function(data, levels_ref) {
  data |>
    mutate(
      across(
        all_of(names(levels_ref)),
        \(x) factor(as.character(x), levels = levels_ref[[cur_column()]])
      )
    )
}

train_raw <- align_levels(train, train_levels)
validation_raw <- align_levels(validation, train_levels)
test_raw <- align_levels(test, train_levels)

unseen_modalities_recap <- bind_rows(
  validation_raw |>
    summarise(across(all_of(active_vars), \(x) sum(is.na(x)))) |>
    mutate(echantillon = "validation"),
  test_raw |>
    summarise(across(all_of(active_vars), \(x) sum(is.na(x)))) |>
    mutate(echantillon = "test")
) |>
  relocate(echantillon)

unseen_modalities_recap

# ============================================================
# 2. FONCTIONS OUTILS
# ============================================================

build_mca_projection <- function(active_data, supplementary_data, active_vars, target_var = "is_canceled") {
  active_x <- active_data |>
    dplyr::select(all_of(active_vars))

  supplementary_x <- supplementary_data |>
    dplyr::select(all_of(active_vars))

  mca_data <- bind_rows(active_x, supplementary_x) |>
    as.data.frame()

  quali_sup_idx <- NULL

  if (target_var %in% names(active_data) && target_var %in% names(supplementary_data)) {
    mca_data[[target_var]] <- c(
      as.character(active_data[[target_var]]),
      as.character(supplementary_data[[target_var]])
    ) |>
      factor(levels = levels(active_data[[target_var]]))

    quali_sup_idx <- ncol(mca_data)
  }

  n_active <- nrow(active_x)
  n_supplementary <- nrow(supplementary_x)
  ind_sup_idx <- (n_active + 1):(n_active + n_supplementary)
  ncp_max <- min(
    n_active - 1,
    sum(purrr::map_int(active_x, nlevels) - 1)
  )

  res_mca <- FactoMineR::MCA(
    mca_data,
    ind.sup = ind_sup_idx,
    quali.sup = quali_sup_idx,
    ncp = ncp_max,
    graph = FALSE
  )

  list(
    res_mca = res_mca,
    active_coord = as.data.frame(res_mca$ind$coord),
    supplementary_coord = as.data.frame(res_mca$ind.sup$coord),
    n_active = n_active,
    n_supplementary = n_supplementary,
    ncp_max = ncp_max
  )
}

get_knn_prob_yes <- function(knn_pred) {
  prob_winner <- attr(knn_pred, "prob")

  ifelse(
    knn_pred == "Oui",
    prob_winner,
    1 - prob_winner
  )
}

evaluate_discriminant_validation <- function(true, pred, prob_yes, model_name, n_axes, knn_k = NA_integer_) {
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

  tibble(
    model = model_name,
    n_axes = n_axes,
    knn_k = knn_k,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    auc = as.numeric(pROC::auc(roc_obj)),
    sensitivity_recall = recall,
    specificity = specificity,
    precision = precision,
    f1_score = ifelse(
      is.na(precision + recall) | (precision + recall == 0),
      NA_real_,
      2 * precision * recall / (precision + recall)
    ),
    balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE)
  )
}

evaluate_threshold <- function(threshold, probs, truth) {
  pred <- if_else(probs >= threshold, "Oui", "Non") |>
    factor(levels = c("Non", "Oui"))

  cm <- caret::confusionMatrix(
    pred,
    truth,
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
    f1_score = ifelse(
      is.na(precision + recall) | (precision + recall == 0),
      NA_real_,
      2 * precision * recall / (precision + recall)
    ),
    balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE),
    gap_sens_spec = abs(recall - specificity)
  )
}

choose_threshold <- function(probs, truth) {
  threshold_results <- purrr::map_dfr(
    seq(0.10, 0.90, by = 0.01),
    evaluate_threshold,
    probs = probs,
    truth = truth
  )

  list(
    all = threshold_results,
    best_f1 = threshold_results |>
      arrange(desc(f1_score), desc(sensitivity_recall)) |>
      slice(1),
    best_balanced = threshold_results |>
      arrange(desc(balanced_accuracy), gap_sens_spec) |>
      slice(1)
  )
}

fit_discriminant_for_k <- function(k) {
  selected_axes_k <- available_axes[seq_len(min(k, length(available_axes)))]
  effective_k <- length(selected_axes_k)

  train_k_local <- train_coord |>
    dplyr::select(all_of(selected_axes_k), is_canceled)

  validation_k_local <- validation_coord |>
    dplyr::select(all_of(selected_axes_k), is_canceled)

  lda_model_local <- MASS::lda(
    is_canceled ~ .,
    data = train_k_local
  )

  lda_pred_local <- predict(
    lda_model_local,
    newdata = validation_k_local
  )

  lda_metrics <- evaluate_discriminant_validation(
    true = validation_k_local$is_canceled,
    pred = lda_pred_local$class,
    prob_yes = lda_pred_local$posterior[, "Oui"],
    model_name = "ACM + LDA",
    n_axes = effective_k
  ) |>
    mutate(status = "OK")

  qda_metrics <- tryCatch(
    {
      qda_model_local <- MASS::qda(
        is_canceled ~ .,
        data = train_k_local
      )

      qda_pred_local <- predict(
        qda_model_local,
        newdata = validation_k_local
      )

      evaluate_discriminant_validation(
        true = validation_k_local$is_canceled,
        pred = qda_pred_local$class,
        prob_yes = qda_pred_local$posterior[, "Oui"],
        model_name = "ACM + QDA",
        n_axes = effective_k
      ) |>
        mutate(status = "OK")
    },
    error = function(e) {
      tibble(
        model = "ACM + QDA",
        n_axes = effective_k,
        knn_k = NA_integer_,
        accuracy = NA_real_,
        auc = NA_real_,
        sensitivity_recall = NA_real_,
        specificity = NA_real_,
        precision = NA_real_,
        f1_score = NA_real_,
        balanced_accuracy = NA_real_,
        status = paste("QDA non estimee:", e$message)
      )
    }
  )

  x_train_knn <- train_k_local |>
    dplyr::select(-is_canceled)

  x_validation_knn <- validation_k_local |>
    dplyr::select(-is_canceled)

  x_train_knn_scaled <- scale(x_train_knn)

  x_validation_knn_scaled <- scale(
    x_validation_knn,
    center = attr(x_train_knn_scaled, "scaled:center"),
    scale = attr(x_train_knn_scaled, "scaled:scale")
  )

  knn_metrics <- purrr::map_dfr(
    knn_k_grid,
    \(knn_k_value) {
      knn_pred <- class::knn(
        train = x_train_knn_scaled,
        test = x_validation_knn_scaled,
        cl = train_k_local$is_canceled,
        k = knn_k_value,
        prob = TRUE,
        use.all = FALSE
      )

      evaluate_discriminant_validation(
        true = validation_k_local$is_canceled,
        pred = knn_pred,
        prob_yes = get_knn_prob_yes(knn_pred),
        model_name = "ACM + KNN",
        n_axes = effective_k,
        knn_k = knn_k_value
      ) |>
        mutate(status = "OK")
    }
  )

  bind_rows(lda_metrics, qda_metrics, knn_metrics)
}

# ============================================================
# 3. ACM DE SELECTION : TRAIN ACTIF, VALIDATION/TEST SUPPLEMENTAIRES
# ============================================================

selection_projection <- build_mca_projection(
  active_data = train_raw,
  supplementary_data = bind_rows(validation_raw, test_raw),
  active_vars = active_vars
)

res_mca_train <- selection_projection$res_mca
eig_mca_train <- as.data.frame(res_mca_train$eig)
eig_mca_train

fviz_screeplot(res_mca_train, addlabels = TRUE)

fviz_mca_var(
  res_mca_train,
  repel = TRUE
)

fviz_contrib(
  res_mca_train,
  choice = "var",
  axes = 1,
  top = 15
)

fviz_contrib(
  res_mca_train,
  choice = "var",
  axes = 2,
  top = 15
)

n_validation <- nrow(validation_raw)
n_test <- nrow(test_raw)

train_coord <- selection_projection$active_coord |>
  mutate(is_canceled = train_raw$is_canceled)

supplementary_coord <- selection_projection$supplementary_coord

validation_coord <- supplementary_coord[seq_len(n_validation), , drop = FALSE] |>
  mutate(is_canceled = validation_raw$is_canceled)

test_coord <- supplementary_coord[
  (n_validation + 1):(n_validation + n_test),
  ,
  drop = FALSE
] |>
  mutate(is_canceled = test_raw$is_canceled)

available_axes <- colnames(selection_projection$active_coord)
max_factor_axes <- length(available_axes)

# ============================================================
# 4. INTERPRETATION AUTOMATIQUE DES AXES ACM
# ============================================================

interpret_mca_axes <- function(res_mca, n_axes = 5, top_n = 8) {
  max_axes <- min(
    n_axes,
    ncol(res_mca$var$coord),
    nrow(res_mca$eig)
  )

  axis_names <- colnames(res_mca$var$coord)[seq_len(max_axes)]

  eig_table <- as.data.frame(res_mca$eig) |>
    tibble::rownames_to_column("axis") |>
    dplyr::slice(seq_len(max_axes))

  names(eig_table)[seq_len(min(4, ncol(eig_table)))] <- c(
    "axis",
    "eigenvalue",
    "inertia_percent",
    "cumulative_inertia_percent"
  )[seq_len(min(4, ncol(eig_table)))]

  coord_var <- as.data.frame(res_mca$var$coord) |>
    tibble::rownames_to_column("modality")

  contrib_var <- as.data.frame(res_mca$var$contrib) |>
    tibble::rownames_to_column("modality")

  cos2_var <- as.data.frame(res_mca$var$cos2) |>
    tibble::rownames_to_column("modality")

  target_coord <- NULL

  if (!is.null(res_mca$quali.sup$coord)) {
    target_coord <- as.data.frame(res_mca$quali.sup$coord) |>
      tibble::rownames_to_column("target_modality")
  }

  target_eta2 <- NULL

  if (!is.null(res_mca$quali.sup$eta2)) {
    target_eta2 <- as.data.frame(res_mca$quali.sup$eta2) |>
      tibble::rownames_to_column("target_variable")
  }

  axis_details <- purrr::map(
    seq_along(axis_names),
    \(axis_id) {
      axis_name <- axis_names[axis_id]

      positive_modalities <- coord_var |>
        dplyr::select(modality, coordinate = all_of(axis_name)) |>
        arrange(desc(coordinate)) |>
        slice_head(n = top_n)

      negative_modalities <- coord_var |>
        dplyr::select(modality, coordinate = all_of(axis_name)) |>
        arrange(coordinate) |>
        slice_head(n = top_n)

      top_contributions <- contrib_var |>
        dplyr::select(modality, contribution = all_of(axis_name)) |>
        arrange(desc(contribution)) |>
        slice_head(n = top_n)

      top_cos2 <- cos2_var |>
        dplyr::select(modality, cos2 = all_of(axis_name)) |>
        arrange(desc(cos2)) |>
        slice_head(n = top_n)

      target_projection <- NULL

      if (!is.null(target_coord) && axis_name %in% names(target_coord)) {
        target_projection <- target_coord |>
          dplyr::select(target_modality, coordinate = all_of(axis_name)) |>
          arrange(desc(abs(coordinate)))
      }

      tibble(
        axis = axis_name,
        interpretation = paste0(
          axis_name,
          " oppose surtout les modalites a coordonnees negatives (",
          paste(negative_modalities$modality[seq_len(min(3, nrow(negative_modalities)))], collapse = ", "),
          ") aux modalites a coordonnees positives (",
          paste(positive_modalities$modality[seq_len(min(3, nrow(positive_modalities)))], collapse = ", "),
          "). Les modalites les plus contributives sont : ",
          paste(top_contributions$modality[seq_len(min(5, nrow(top_contributions)))], collapse = ", "),
          "."
        ),
        positive_modalities = list(positive_modalities),
        negative_modalities = list(negative_modalities),
        top_contributions = list(top_contributions),
        top_cos2 = list(top_cos2),
        target_projection = list(target_projection)
      )
    }
  ) |>
    bind_rows()

  list(
    eigenvalues = eig_table,
    axis_details = axis_details,
    target_coordinates = target_coord,
    target_eta2 = target_eta2
  )
}

acm_n_axes_interpretation <- min(5, max_factor_axes)

acm_axis_interpretation <- interpret_mca_axes(
  res_mca = res_mca_train,
  n_axes = acm_n_axes_interpretation,
  top_n = 8
)

acm_axis_interpretation$eigenvalues

acm_axis_interpretation$axis_details |>
  dplyr::select(axis, interpretation) |>
  dplyr::mutate(
    texte = paste0("\n", axis, " :\n", interpretation, "\n")
  ) |>
  dplyr::pull(texte) |>
  cat(sep = "\n")

acm_axis_interpretation$target_coordinates
acm_axis_interpretation$target_eta2

# Grid search allegee :
# - les premiers axes sont testes finement ;
# - quelques valeurs plus grandes permettent de verifier si ajouter
#   davantage d'information factorielle ameliore la prediction.
axis_grid <- unique(
  pmin(
    c(1, 2, 3, 4, 5, 7, 10, 15, 20),
    max_factor_axes
  )
)

# KNN est tres couteux : on garde une grille courte et interpretable.
knn_k_grid <- c(5, 15, 26)
knn_k_grid <- knn_k_grid[knn_k_grid < nrow(train_coord)]

# ============================================================
# 4. TESTS D'HYPOTHESES LDA / QDA SUR LES AXES ACM CANDIDATS
# ============================================================

# Ces tests documentent les hypotheses classiques de LDA/QDA.
# Ils sont places juste apres la construction des axes ACM et avant
# la grid search. A ce stade, le nombre d'axes final n'est pas encore
# choisi ; on teste donc les axes candidats jusqu'au maximum explore
# dans la grille de recherche.

diagnostic_axes <- available_axes[seq_len(max(axis_grid))]

train_axes_diagnostic <- train_coord |>
  dplyr::select(all_of(diagnostic_axes), is_canceled)

normality_sample_size <- min(2000, nrow(train_axes_diagnostic))

set.seed(123)

train_axes_sample <- train_axes_diagnostic |>
  dplyr::slice_sample(n = normality_sample_size)

shapiro_axes_results <- purrr::map_dfr(
  colnames(train_axes_sample |> dplyr::select(-is_canceled)),
  \(axis_name) {
    test <- shapiro.test(train_axes_sample[[axis_name]])

    tibble(
      axis = axis_name,
      statistic = unname(test$statistic),
      p_value = test$p.value,
      conclusion = if_else(
        p_value < 0.05,
        "Normalite rejetee",
        "Normalite non rejetee"
      )
    )
  }
)

box_m_test <- biotools::boxM(
  train_axes_diagnostic |> dplyr::select(-is_canceled),
  train_axes_diagnostic$is_canceled
)

box_m_test_table <- tibble(
  test = "Box M",
  statistic = unname(box_m_test$statistic),
  parameter = unname(box_m_test$parameter),
  p_value = box_m_test$p.value,
  conclusion = if_else(
    p_value < 0.05,
    "Homogeneite des covariances rejetee",
    "Homogeneite des covariances non rejetee"
  )
)

mardia_test_table <- tryCatch(
  {
    MVN::mvn(
      data = train_axes_sample |> dplyr::select(-is_canceled),
      mvn_test = "mardia",
      tidy = TRUE,
      descriptives = FALSE
    )$multivariate_normality |>
      as_tibble() |>
      mutate(conclusion = if_else(
        p.value < 0.05,
        "Normalite multivariee rejetee",
        "Normalite multivariee non rejetee"
      ))
  },
  error = function(e) {
    tibble(
      Test = "Mardia",
      Statistic = NA_real_,
      p.value = NA_real_,
      Method = "non calcule",
      MVN = e$message,
      conclusion = "Test non calcule"
    )
  }
)

henze_zirkler_test_table <- tryCatch(
  {
    MVN::mvn(
      data = train_axes_sample |> dplyr::select(-is_canceled),
      mvn_test = "hz",
      tidy = TRUE,
      descriptives = FALSE
    )$multivariate_normality |>
      as_tibble() |>
      mutate(conclusion = if_else(
        p.value < 0.05,
        "Normalite multivariee rejetee",
        "Normalite multivariee non rejetee"
      ))
  },
  error = function(e) {
    tibble(
      Test = "Henze-Zirkler",
      Statistic = NA_real_,
      p.value = NA_real_,
      Method = "non calcule",
      MVN = e$message,
      conclusion = "Test non calcule"
    )
  }
)

shapiro_axes_results
mardia_test_table
henze_zirkler_test_table
box_m_test_table

# ============================================================
# 5. GRID SEARCH SUR VALIDATION : NOMBRE D'AXES ET LDA/QDA/KNN
# ============================================================

results_validation <- purrr::map_dfr(
  axis_grid,
  fit_discriminant_for_k
) |>
  arrange(desc(auc), desc(f1_score), desc(sensitivity_recall))

results_validation

results_validation_summary <- results_validation |>
  filter(!is.na(auc)) |>
  group_by(model, knn_k) |>
  slice_max(auc, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(auc), desc(f1_score), desc(sensitivity_recall))

results_validation_summary

results_validation_auc_wide <- results_validation |>
  mutate(model_grid = if_else(
    model == "ACM + KNN",
    paste0(model, " k=", knn_k),
    model
  )) |>
  dplyr::select(n_axes, model_grid, auc) |>
  tidyr::pivot_wider(
    names_from = model_grid,
    values_from = auc
  ) |>
  arrange(n_axes)

results_validation_auc_wide

knn_validation_summary <- results_validation |>
  filter(model == "ACM + KNN", !is.na(auc)) |>
  group_by(knn_k) |>
  summarise(
    best_auc = max(auc, na.rm = TRUE),
    best_f1_score = max(f1_score, na.rm = TRUE),
    best_n_axes_by_auc = n_axes[which.max(auc)],
    .groups = "drop"
  ) |>
  arrange(desc(best_auc), desc(best_f1_score))

knn_validation_summary

best_discriminant_choice <- results_validation |>
  filter(!is.na(auc)) |>
  arrange(desc(auc), desc(f1_score), desc(sensitivity_recall)) |>
  slice(1)

best_discriminant_model <- best_discriminant_choice$model
best_discriminant_n_axes <- best_discriminant_choice$n_axes
best_discriminant_knn_k <- best_discriminant_choice$knn_k

best_discriminant_choice

selected_axes <- available_axes[seq_len(best_discriminant_n_axes)]

train_k <- train_coord |>
  dplyr::select(all_of(selected_axes), is_canceled)

validation_k <- validation_coord |>
  dplyr::select(all_of(selected_axes), is_canceled)

test_k <- test_coord |>
  dplyr::select(all_of(selected_axes), is_canceled)

# ============================================================
# 6. OPTIMISATION DU SEUIL DU MODELE RETENU SUR VALIDATION
# ============================================================

if (best_discriminant_model == "ACM + LDA") {
  selected_model_validation <- MASS::lda(
    is_canceled ~ .,
    data = train_k
  )

  selected_validation_pred <- predict(
    selected_model_validation,
    newdata = validation_k
  )

  selected_validation_prob <- selected_validation_pred$posterior[, "Oui"]
} else if (best_discriminant_model == "ACM + QDA") {
  selected_model_validation <- MASS::qda(
    is_canceled ~ .,
    data = train_k
  )

  selected_validation_pred <- predict(
    selected_model_validation,
    newdata = validation_k
  )

  selected_validation_prob <- selected_validation_pred$posterior[, "Oui"]
} else {
  x_train_knn_selected <- train_k |>
    dplyr::select(-is_canceled)

  x_validation_knn_selected <- validation_k |>
    dplyr::select(-is_canceled)

  x_train_knn_selected_scaled <- scale(x_train_knn_selected)

  x_validation_knn_selected_scaled <- scale(
    x_validation_knn_selected,
    center = attr(x_train_knn_selected_scaled, "scaled:center"),
    scale = attr(x_train_knn_selected_scaled, "scaled:scale")
  )

  selected_validation_pred <- class::knn(
    train = x_train_knn_selected_scaled,
    test = x_validation_knn_selected_scaled,
    cl = train_k$is_canceled,
    k = best_discriminant_knn_k,
    prob = TRUE,
    use.all = FALSE
  )

  selected_validation_prob <- get_knn_prob_yes(selected_validation_pred)
}

threshold_discriminant <- choose_threshold(
  probs = selected_validation_prob,
  truth = validation_k$is_canceled
)

threshold_discriminant_table <- threshold_discriminant$all
best_discriminant_threshold <- threshold_discriminant$best_f1$threshold

threshold_discriminant$best_f1
threshold_discriminant$best_balanced

plot_discriminant_grid_auc <- results_validation |>
  filter(!is.na(auc)) |>
  mutate(model_grid = if_else(
    model == "ACM + KNN",
    paste0(model, " k=", knn_k),
    model
  )) |>
  ggplot(aes(x = n_axes, y = auc, color = model_grid)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = axis_grid) +
  labs(
    title = "Choix du nombre d'axes ACM",
    subtitle = "Selection sur validation",
    x = "Nombre d'axes ACM",
    y = "AUC validation",
    color = "Modele"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

plot_discriminant_grid_f1 <- results_validation |>
  filter(!is.na(f1_score)) |>
  mutate(model_grid = if_else(
    model == "ACM + KNN",
    paste0(model, " k=", knn_k),
    model
  )) |>
  ggplot(aes(x = n_axes, y = f1_score, color = model_grid)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = axis_grid) +
  labs(
    title = "Choix du nombre d'axes ACM",
    subtitle = "F1-score sur validation",
    x = "Nombre d'axes ACM",
    y = "F1-score validation",
    color = "Modele"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

plot_discriminant_grid_auc
plot_discriminant_grid_f1

# ============================================================
# 5. ACM FINALE : TRAIN + VALIDATION ACTIFS, TEST SUPPLEMENTAIRE
# ============================================================

train_validation_raw <- bind_rows(train, validation)

final_levels <- lapply(
  train_validation_raw |>
    dplyr::select(all_of(c(active_vars, "is_canceled"))),
  levels
)

train_validation_final_raw <- align_levels(train_validation_raw, final_levels)
test_final_raw <- align_levels(test, final_levels)

final_projection <- build_mca_projection(
  active_data = train_validation_final_raw,
  supplementary_data = test_final_raw,
  active_vars = active_vars
)

res_mca_final <- final_projection$res_mca
eig_mca_final <- as.data.frame(res_mca_final$eig)

available_axes_final <- colnames(final_projection$active_coord)
selected_axes_final <- available_axes_final[
  seq_len(min(best_discriminant_n_axes, length(available_axes_final)))
]

train_validation_k_final <- final_projection$active_coord |>
  dplyr::select(all_of(selected_axes_final)) |>
  mutate(is_canceled = train_validation_final_raw$is_canceled)

test_k_final <- final_projection$supplementary_coord |>
  dplyr::select(all_of(selected_axes_final)) |>
  mutate(is_canceled = test_final_raw$is_canceled)

lda_final <- NULL
qda_final <- NULL
knn_final <- NULL

if (best_discriminant_model == "ACM + LDA") {
  lda_final <- MASS::lda(
    is_canceled ~ .,
    data = train_validation_k_final
  )
} else if (best_discriminant_model == "ACM + QDA") {
  qda_final <- MASS::qda(
    is_canceled ~ .,
    data = train_validation_k_final
  )
} else {
  x_train_validation_knn_final <- train_validation_k_final |>
    dplyr::select(-is_canceled)

  knn_final_scaled_train <- scale(x_train_validation_knn_final)

  knn_final <- list(
    k = best_discriminant_knn_k,
    train = knn_final_scaled_train,
    class = train_validation_k_final$is_canceled,
    center = attr(knn_final_scaled_train, "scaled:center"),
    scale = attr(knn_final_scaled_train, "scaled:scale")
  )
}

discriminant_method_recap <- tibble(
  selected_model = best_discriminant_model,
  selected_n_axes = best_discriminant_n_axes,
  selected_knn_k = best_discriminant_knn_k,
  selected_threshold = best_discriminant_threshold,
  max_factor_axes = max_factor_axes,
  max_axes_tested = max(axis_grid),
  max_knn_k_tested = max(knn_k_grid),
  selection_sample = "validation",
  final_acm_active_sample = "train + validation",
  final_test_role = "individu supplementaire"
)

discriminant_method_recap
