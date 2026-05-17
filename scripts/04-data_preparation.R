# ============================================================
# PREPARATION DES DONNEES DE MODELISATION
# ============================================================

if (!exists("df_engineered")) {
  source("scripts/02-data_engenering.R")
}

# Les variables retenues sont centralisées ici afin que la base de
# modélisation et le tableau du rapport reposent sur la même définition.
variables_modelisation <- c(
  "is_canceled",
  "hotel",
  "market_segment",
  "customer_type",
  "lead_time_cat",
  "total_nights_cat",
  "adr_cat",
  "has_children",
  "engagement_cat",
  "previous_cancellations_cat",
  "saison_tourisme",
  "identity_location"
  # distribution_channel,
  # deposit_type,
  # parking_reserved,
  # repeated_guest_cat,
)

roles_modelisation_specifiques <- tibble::tribble(
  ~Variable, ~Role_dans_l_analyse,
  "hotel", "Distinguer City Hotel et Resort Hotel",
  "market_segment", "Caractériser le canal commercial de réservation",
  "customer_type", "Distinguer les profils de clients selon le type de réservation"
)

# La subdivision train / validation / test est centralisée ici.
# Les scripts de modélisation sourcent ce fichier afin d'utiliser
# exactement les mêmes observations et d'éviter toute fuite de données.
df_model <- df_engineered |>
  dplyr::select(dplyr::all_of(variables_modelisation)) |>
  tidyr::drop_na()

extraire_modalites <- function(var_name, data = df_model) {
  values <- data[[var_name]]

  modalites <- if (is.factor(values)) {
    levels(values)
  } else {
    sort(unique(as.character(values)))
  }

  paste(modalites, collapse = " ; ")
}

extraire_role <- function(var_name) {
  role_specifique <- roles_modelisation_specifiques$Role_dans_l_analyse[
    match(var_name, roles_modelisation_specifiques$Variable)
  ]

  if (!is.na(role_specifique)) {
    return(role_specifique)
  }

  role_table_creees <- table_variables_creees$Role_dans_l_analyse[
    match(var_name, table_variables_creees$Variable)
  ]

  ifelse(is.na(role_table_creees), "", role_table_creees)
}

table_variables_modelisation <- tibble::tibble(
  Variable = variables_modelisation,
  Modalité = purrr::map_chr(variables_modelisation, extraire_modalites),
  Role_dans_l_analyse = purrr::map_chr(variables_modelisation, extraire_role)
)

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
