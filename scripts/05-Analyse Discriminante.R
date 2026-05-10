source("scripts/04-data preparation.R")



# ============================================================
# 3. HARMONISATION DES NIVEAUX DE FACTEURS
# ============================================================
df_disc=df_model

active_vars <- names(df_model)[sapply(df_model, is.factor)]

active_vars <- setdiff(active_vars, "is_canceled")

all_levels <- lapply(
  df_disc |>
    dplyr::select(all_of(names(df_disc))),
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

train_raw <- align_levels(train, all_levels)
validation_raw <- align_levels(validation, all_levels)
test_raw <- align_levels(test, all_levels)
# ============================================================
# 4. BASE POUR ACM
#    Train = individus actifs
#    Validation + test = individus supplémentaires
# ============================================================
train_x <- train_raw |>
  dplyr::select(all_of(active_vars))

validation_x <- validation_raw |>
  dplyr::select(all_of(active_vars))

test_x <- test_raw |>
  dplyr::select(all_of(active_vars))

mca_data <- bind_rows(
  train_x,
  validation_x,
  test_x
) |>
  as.data.frame()

n_train <- nrow(train_x)
n_validation <- nrow(validation_x)
n_test <- nrow(test_x)

ind_sup_idx <- (n_train + 1):(n_train + n_validation + n_test)
# ============================================================
# 5. ACM
# ============================================================
res_mca_train <- MCA(
  mca_data,
  ind.sup = ind_sup_idx,
  graph = FALSE
)

# Résultats ACM
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
# ============================================================
# 6. COORDONNEES FACTORIELLES
# ============================================================
train_coord <- as.data.frame(res_mca_train$ind$coord) |>
  mutate(is_canceled = train_raw$is_canceled)

sup_coord <- as.data.frame(res_mca_train$ind.sup$coord)

validation_coord <- sup_coord[1:n_validation, , drop = FALSE] |>
  mutate(is_canceled = validation_raw$is_canceled)

test_coord <- sup_coord[
  (n_validation + 1):(n_validation + n_test),
  ,
  drop = FALSE
] |>
  mutate(is_canceled = test_raw$is_canceled)
# ============================================================
# 7. CHOIX DES AXES
# ============================================================
train_all <- train_coord
validation_all <- validation_coord
test_all <- test_coord

k <- 5

train_k <- train_coord |>
  dplyr::select(all_of(colnames(train_coord)[1:k]), is_canceled)

validation_k <- validation_coord |>
  dplyr::select(all_of(colnames(validation_coord)[1:k]), is_canceled)

test_k <- test_coord |>
  dplyr::select(all_of(colnames(test_coord)[1:k]), is_canceled)
# ============================================================
# 8. FONCTION D'EVALUATION
# ============================================================
evaluate_model <- function(true, pred, prob_yes) {
  roc_obj <- pROC::roc(
    response = true,
    predictor = prob_yes,
    levels = c("Non", "Oui"),
    direction = "<"
  )
  
  tibble(
    auc = as.numeric(pROC::auc(roc_obj)),
    accuracy = mean(pred == true)
  )
}

get_knn_prob_yes <- function(knn_pred) {
  p_attr <- attr(knn_pred, "prob")
  
  ifelse(
    knn_pred == "yes",
    p_attr,
    1 - p_attr
  )
}
# ============================================================
# 9. MODELES SUR LES k PREMIERS AXES
# ============================================================
lda_model <- MASS::lda(
  is_canceled ~ .,
  data = train_k
)

qda_model <- MASS::qda(
  is_canceled ~ .,
  data = train_k
)

x_train <- train_k |>
  dplyr::select(-is_canceled)

x_validation <- validation_k |>
  dplyr::select(-is_canceled)

x_train_scaled <- scale(x_train)

x_validation_scaled <- scale(
  x_validation,
  center = attr(x_train_scaled, "scaled:center"),
  scale = attr(x_train_scaled, "scaled:scale")
)

knn_pred_10 <- class::knn(
  train = x_train_scaled,
  test = x_validation_scaled,
  cl = train_k$is_canceled,
  k = 10,
  prob = TRUE,
  use.all = FALSE
)

knn_pred_20 <- class::knn(
  train = x_train_scaled,
  test = x_validation_scaled,
  cl = train_k$is_canceled,
  k = 20,
  prob = TRUE,
  use.all = FALSE
)

knn_pred_50 <- class::knn(
  train = x_train_scaled,
  test = x_validation_scaled,
  cl = train_k$is_canceled,
  k = 50,
  prob = TRUE,
  use.all = FALSE
)
# ============================================================
# 10. PREDICTIONS SUR VALIDATION
# ============================================================
lda_pred <- predict(lda_model, newdata = validation_k)
qda_pred <- predict(qda_model, newdata = validation_k)

eval_lda <- evaluate_model(
  true = validation_k$is_canceled,
  pred = lda_pred$class,
  prob_yes = lda_pred$posterior[, "Oui"]
)

eval_qda <- evaluate_model(
  true = validation_k$is_canceled,
  pred = qda_pred$class,
  prob_yes = qda_pred$posterior[, "Oui"]
)

eval_knn_10 <- evaluate_model(
  true = validation_k$is_canceled,
  pred = knn_pred_10,
  prob_yes = get_knn_prob_yes(knn_pred_10)
)

eval_knn_20 <- evaluate_model(
  true = validation_k$is_canceled,
  pred = knn_pred_20,
  prob_yes = get_knn_prob_yes(knn_pred_20)
)

eval_knn_50 <- evaluate_model(
  true = validation_k$is_canceled,
  pred = knn_pred_50,
  prob_yes = get_knn_prob_yes(knn_pred_50)
)

results_validation <- bind_rows(
  eval_lda |>
    mutate(model = "LDA"),
  eval_qda |>
    mutate(model = "QDA"),
  eval_knn_10 |>
    mutate(model = "KNN_10"),
  eval_knn_20 |>
    mutate(model = "KNN_20"),
  eval_knn_50 |>
    mutate(model = "KNN_50")
) |>
  dplyr::select(model, auc, accuracy) |>
  arrange(desc(auc))

results_validation
# ============================================================
# 11. MATRICES DE CONFUSION SUR VALIDATION
# ============================================================
caret::confusionMatrix(
  lda_pred$class,
  validation_k$is_canceled,
  positive = "Oui"
)

caret::confusionMatrix(
  qda_pred$class,
  validation_k$is_canceled,
  positive = "Oui"
)

caret::confusionMatrix(
  knn_pred_10,
  validation_k$is_canceled,
  positive = "Oui"
)

caret::confusionMatrix(
  knn_pred_20,
  validation_k$is_canceled,
  positive = "Oui"
)

caret::confusionMatrix(
  knn_pred_50,
  validation_k$is_canceled,
  positive = "Oui"
)
# ============================================================
# 12. COMPARAISON OPTIONNELLE : LDA AVEC TOUS LES AXES
# ============================================================
lda_all <- MASS::lda(
  is_canceled ~ .,
  data = train_all
)

lda_all_pred <- predict(lda_all, newdata = validation_all)

eval_lda_all <- evaluate_model(
  true = validation_all$is_canceled,
  pred = lda_all_pred$class,
  prob_yes = lda_all_pred$posterior[, "Oui"]
)

eval_lda_all |>
  mutate(model = "LDA_all_axes")
