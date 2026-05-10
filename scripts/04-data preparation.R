
# ============================================================
# PREPARATION DES DONNEES DE MODELISATION
# ============================================================

if (!exists("df_engineered")) {
  source("scripts/02-data_engenering.R")
}

# La subdivision train / validation / test est centralisee ici.
# Les scripts de modelisation sourcent ce fichier afin d'utiliser
# exactement les memes observations et d'eviter toute fuite de donnees.

df_model <- df_engineered |>
  dplyr::select (
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
  drop_na()


# ============================================================
# SUBDIVISION STRATIFIEE
# ============================================================

set.seed(123)

# 50 % apprentissage, puis 25 % test et 25 % validation.
# La stratification conserve la proportion Oui / Non de is_canceled
# dans les trois sous-echantillons.
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
validation <- testing(split2)

split_recap <- tibble(
  echantillon = c("train", "validation", "test"),
  n = c(nrow(train), nrow(validation), nrow(test)),
  taux_annulation = c(
    mean(train$is_canceled == "Oui"),
    mean(validation$is_canceled == "Oui"),
    mean(test$is_canceled == "Oui")
  )
)

split_recap
