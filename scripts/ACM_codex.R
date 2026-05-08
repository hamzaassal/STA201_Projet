# ============================================================
# DISQUAL CODEX : ACM + LDA / QDA
# Projet STA201
# ============================================================

# Objectifs :
# - realiser une ACM avec is_canceled comme variable illustrative ;
# - representer la cible sur la carte des modalites ;
# - automatiser l'interpretation des N premiers axes ;
# - entrainer LDA et QDA sur les coordonnees factorielles.

# ============================================================
# 0. PACKAGES ET DONNEES
# ============================================================

required_packages <- c(
  "tidyverse",
  "rsample",
  "FactoMineR",
  "factoextra",
  "MASS",
  "caret",
  "pROC",
  "ggrepel",
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

# Nombre d'axes a interpreter dans la partie descriptive.
acm_n_axes_interpretation <- 5

# Grille de recherche pour le nombre d'axes utilises en LDA / QDA.
# Le choix final est fait sur validation, pas sur test.
acm_axes_grid <- 2:26

# ============================================================
# 1. BASE ACM / DISQUAL
# ============================================================

df_disqual <- df_engineered |>
  dplyr::select(
    is_canceled,
    hotel,
    market_segment,
    # distribution_channel,
    # deposit_type,
    customer_type,
    lead_time_cat,
    total_nights_cat,
    adr_cat,
    has_children,
    # parking_reserved,
    engagement_cat,
    previous_cancellations_cat,
    saison_tourisme,
    # repeated_guest_cat,
    identity_location
  ) |>
  tidyr::drop_na() |>
  mutate(
    is_canceled = factor(is_canceled, levels = c("Non", "Oui")),
    across(-is_canceled, as.factor)
  )

active_vars <- setdiff(names(df_disqual), "is_canceled")

# ============================================================
# 2. DECOUPAGE TRAIN / VALIDATION / TEST
# ============================================================

set.seed(123)

split_1 <- rsample::initial_split(
  df_disqual,
  prop = 0.50,
  strata = is_canceled
)

train_disqual <- rsample::training(split_1)
temp_disqual <- rsample::testing(split_1)

split_2 <- rsample::initial_split(
  temp_disqual,
  prop = 0.50,
  strata = is_canceled
)

validation_disqual <- rsample::training(split_2)
test_disqual <- rsample::testing(split_2)

# L'ACM est ajustee sur train ; validation et test sont des individus
# supplementaires. La cible est illustrative : elle ne construit pas les axes.
mca_data <- bind_rows(
  train_disqual,
  validation_disqual,
  test_disqual
) |>
  as.data.frame()

n_train <- nrow(train_disqual)
n_validation <- nrow(validation_disqual)
n_test <- nrow(test_disqual)

ind_sup_idx <- (n_train + 1):(n_train + n_validation + n_test)

# ============================================================
# 3. ACM AVEC CIBLE ILLUSTRATIVE
# ============================================================

res_mca_train <- FactoMineR::MCA(
  mca_data,
  quali.sup = 1,
  ind.sup = ind_sup_idx,
  graph = FALSE
)

eig_mca_train <- as.data.frame(res_mca_train$eig) |>
  tibble::rownames_to_column("axis")

eig_mca_train

# ============================================================
# 4. FONCTION D'INTERPRETATION AUTOMATIQUE DES AXES
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

# ============================================================
# 5. GRAPHIQUE DES MODALITES AVEC CIBLE REPRESENTEE
# ============================================================

# Base demandee :
# fviz_mca_var(res_mca_train, repel = TRUE)
# On ajoute explicitement les modalites de la variable cible illustrative
# sur cette meme carte.

target_plot_data <- as.data.frame(res_mca_train$quali.sup$coord) |>
  tibble::rownames_to_column("modalite")

target_plot_data <- tibble(
  modalite = target_plot_data$modalite,
  Dim1 = target_plot_data[[2]],
  Dim2 = target_plot_data[[3]]
)

p_mca_modalities_target <- fviz_mca_var(
  res_mca_train,
  repel = TRUE,
  ggtheme = theme_minimal()
) +
  geom_point(
    data = target_plot_data,
    aes(x = Dim1, y = Dim2),
    inherit.aes = FALSE,
    color = "#D95F0E",
    size = 4
  ) +
  ggrepel::geom_text_repel(
    data = target_plot_data,
    aes(x = Dim1, y = Dim2, label = paste0("Cible: ", modalite)),
    inherit.aes = FALSE,
    color = "#D95F0E",
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Projection des modalites ACM avec variable cible illustrative",
    subtitle = "Les modalites de is_canceled sont projetees sans contribuer a la construction des axes"
  )

p_mca_modalities_target

p_mca_scree <- fviz_screeplot(
  res_mca_train,
  addlabels = TRUE
)

p_mca_scree

# ============================================================
# 6. COORDONNEES FACTORIELLES POUR DISQUAL
# ============================================================

available_axes <- colnames(res_mca_train$ind$coord)

train_coord_all <- as.data.frame(res_mca_train$ind$coord) |>
  mutate(is_canceled = train_disqual$is_canceled)

sup_coord <- as.data.frame(res_mca_train$ind.sup$coord)

validation_coord_all <- sup_coord[seq_len(n_validation), , drop = FALSE] |>
  mutate(is_canceled = validation_disqual$is_canceled)

test_coord_all <- sup_coord[
  (n_validation + 1):(n_validation + n_test),
  ,
  drop = FALSE
] |>
  mutate(is_canceled = test_disqual$is_canceled)

# ============================================================
# 7. GRID SEARCH LDA / QDA SUR LE NOMBRE D'AXES
# ============================================================

evaluate_disqual_model <- function(true, pred_class, prob_yes, model_name, sample_name, n_axes) {
  pred_class <- factor(pred_class, levels = c("Non", "Oui"))

  cm <- caret::confusionMatrix(
    pred_class,
    true,
    positive = "Oui"
  )

  roc_obj <- pROC::roc(
    response = true,
    predictor = prob_yes,
    levels = c("Non", "Oui"),
    quiet = TRUE
  )

  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])

  tibble(
    model = model_name,
    sample = sample_name,
    n_axes = n_axes,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    auc = as.numeric(pROC::auc(roc_obj)),
    sensitivity = recall,
    specificity = as.numeric(cm$byClass["Specificity"]),
    precision = precision,
    f1_score = ifelse(
      is.na(precision + recall) | (precision + recall == 0),
      NA_real_,
      2 * precision * recall / (precision + recall)
    )
  )
}

fit_evaluate_disqual_k <- function(k) {
  selected_axes_k <- available_axes[seq_len(min(k, length(available_axes)))]
  effective_k <- length(selected_axes_k)

  train_k <- train_coord_all |>
    dplyr::select(all_of(selected_axes_k), is_canceled)

  validation_k <- validation_coord_all |>
    dplyr::select(all_of(selected_axes_k), is_canceled)

  lda_model <- MASS::lda(
    is_canceled ~ .,
    data = train_k
  )

  lda_pred <- predict(
    lda_model,
    newdata = validation_k
  )

  lda_metrics <- evaluate_disqual_model(
    true = validation_k$is_canceled,
    pred_class = lda_pred$class,
    prob_yes = lda_pred$posterior[, "Oui"],
    model_name = "ACM_LDA",
    sample_name = "validation",
    n_axes = effective_k
  )

  qda_metrics <- tryCatch(
    {
      qda_model <- MASS::qda(
        is_canceled ~ .,
        data = train_k
      )

      qda_pred <- predict(
        qda_model,
        newdata = validation_k
      )

      evaluate_disqual_model(
        true = validation_k$is_canceled,
        pred_class = qda_pred$class,
        prob_yes = qda_pred$posterior[, "Oui"],
        model_name = "ACM_QDA",
        sample_name = "validation",
        n_axes = effective_k
      )
    },
    error = function(e) {
      tibble(
        model = "ACM_QDA",
        sample = "validation",
        n_axes = effective_k,
        accuracy = NA_real_,
        auc = NA_real_,
        sensitivity = NA_real_,
        specificity = NA_real_,
        precision = NA_real_,
        f1_score = NA_real_,
        status = paste("QDA non estimee:", e$message)
      )
    }
  )

  bind_rows(
    lda_metrics |> mutate(status = "OK"),
    qda_metrics
  )
}

max_grid_axis <- min(max(acm_axes_grid), length(available_axes))
acm_axes_grid_effective <- acm_axes_grid[acm_axes_grid <= max_grid_axis]

disqual_grid_results <- purrr::map_dfr(
  acm_axes_grid_effective,
  fit_evaluate_disqual_k
) |>
  arrange(desc(auc), desc(f1_score), desc(sensitivity))

disqual_grid_results

disqual_grid_summary <- disqual_grid_results |>
  filter(!is.na(auc)) |>
  group_by(model) |>
  slice_max(auc, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(auc), desc(f1_score), desc(sensitivity))

disqual_grid_summary

disqual_grid_wide_auc <- disqual_grid_results |>
  dplyr::select(n_axes, model, auc) |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = auc
  ) |>
  arrange(n_axes)

disqual_grid_wide_auc

plot_disqual_grid_auc <- disqual_grid_results |>
  filter(!is.na(auc)) |>
  ggplot(aes(x = n_axes, y = auc, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = acm_axes_grid_effective) +
  labs(
    title = "Choix du nombre d'axes ACM par validation",
    subtitle = "Comparaison LDA / QDA selon l'AUC validation",
    x = "Nombre d'axes ACM",
    y = "AUC validation",
    color = "Modele"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

plot_disqual_grid_f1 <- disqual_grid_results |>
  filter(!is.na(f1_score)) |>
  ggplot(aes(x = n_axes, y = f1_score, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = acm_axes_grid_effective) +
  labs(
    title = "Choix du nombre d'axes ACM par validation",
    subtitle = "Comparaison LDA / QDA selon le F1-score validation",
    x = "Nombre d'axes ACM",
    y = "F1-score validation",
    color = "Modele"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

plot_disqual_grid_auc
plot_disqual_grid_f1

# ============================================================
# 8. SELECTION DU MEILLEUR COUPLE MODELE / NOMBRE D'AXES
# ============================================================

best_disqual_choice <- disqual_grid_results |>
  filter(!is.na(auc)) |>
  arrange(desc(auc), desc(f1_score), desc(sensitivity)) |>
  slice(1)

best_disqual_model <- best_disqual_choice$model
best_disqual_n_axes <- best_disqual_choice$n_axes
selected_axes <- available_axes[seq_len(best_disqual_n_axes)]

best_disqual_choice

train_coord <- train_coord_all |>
  dplyr::select(all_of(selected_axes), is_canceled)

validation_coord <- validation_coord_all |>
  dplyr::select(all_of(selected_axes), is_canceled)

test_coord <- test_coord_all |>
  dplyr::select(all_of(selected_axes), is_canceled)

train_validation_coord <- bind_rows(
  train_coord,
  validation_coord
)

lda_model_acm_final <- NULL
qda_model_acm_final <- NULL

if (best_disqual_model == "ACM_LDA") {
  lda_model_acm_final <- MASS::lda(
    is_canceled ~ .,
    data = train_validation_coord
  )

  best_test_pred <- predict(
    lda_model_acm_final,
    newdata = test_coord
  )
} else {
  qda_model_acm_final <- MASS::qda(
    is_canceled ~ .,
    data = train_validation_coord
  )

  best_test_pred <- predict(
    qda_model_acm_final,
    newdata = test_coord
  )
}

disqual_test_results <- evaluate_disqual_model(
  true = test_coord$is_canceled,
  pred_class = best_test_pred$class,
  prob_yes = best_test_pred$posterior[, "Oui"],
  model_name = best_disqual_model,
  sample_name = "test",
  n_axes = best_disqual_n_axes
)

disqual_test_confusion <- caret::confusionMatrix(
  factor(best_test_pred$class, levels = c("Non", "Oui")),
  test_coord$is_canceled,
  positive = "Oui"
)

disqual_test_roc <- pROC::roc(
  response = test_coord$is_canceled,
  predictor = best_test_pred$posterior[, "Oui"],
  levels = c("Non", "Oui"),
  quiet = TRUE
)

disqual_test_results
disqual_test_confusion

p_disqual_roc_test <- ggplot(
  data.frame(
    specificity = disqual_test_roc$specificities,
    sensitivity = disqual_test_roc$sensitivities
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
    title = paste("Courbe ROC test -", best_disqual_model),
    subtitle = paste0("AUC = ", round(as.numeric(pROC::auc(disqual_test_roc)), 3)),
    x = "1 - Specificite",
    y = "Sensibilite"
  ) +
  theme_minimal(base_size = 13)

p_disqual_roc_test

# ============================================================
# 9. OBJETS A UTILISER DANS LE RAPPORT
# ============================================================

acm_codex_results <- list(
  res_mca = res_mca_train,
  eigenvalues = eig_mca_train,
  interpretation = acm_axis_interpretation,
  plot_modalities_target = p_mca_modalities_target,
  plot_scree = p_mca_scree,
  grid_results = disqual_grid_results,
  grid_summary = disqual_grid_summary,
  grid_wide_auc = disqual_grid_wide_auc,
  best_choice = best_disqual_choice,
  plot_grid_auc = plot_disqual_grid_auc,
  plot_grid_f1 = plot_disqual_grid_f1,
  test_results = disqual_test_results,
  test_confusion = disqual_test_confusion,
  test_roc = disqual_test_roc,
  plot_test_roc = p_disqual_roc_test
)

cat("\n====================================================\n")
cat("ACM + LDA/QDA TERMINE\n")
cat("====================================================\n")
cat("Nombre d'axes retenu :", best_disqual_n_axes, "\n")
cat("Meilleur modele validation :", best_disqual_model, "\n")
cat("AUC test :", round(disqual_test_results$auc, 3), "\n")
cat("Accuracy test :", round(disqual_test_results$accuracy, 3), "\n")
cat("F1-score test :", round(disqual_test_results$f1_score, 3), "\n")
cat("====================================================\n")
