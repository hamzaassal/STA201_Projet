library(rsample)

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
    # repeated_guest_cat
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
TEST <- testing(split2)

dim(train)
dim(test)
dim(TEST)

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
  prob_TEST >= 0.5,
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
    "#2C7FB8",
    "#D95F0E"
  ),
  
  conf.level = 0,
  
  margin = 1,
  
  main = "Matrice de confusion - Logistique"
)
