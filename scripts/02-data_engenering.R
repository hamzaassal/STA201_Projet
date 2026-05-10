if (!exists("df") || !exists("dico_variables")) {
  source("scripts/00-packages.R")
  source("scripts/01-import.R")
}

#########################################################"
#####################################################
####### construction des variables de l'etude
df_cleaning_flags <- df |>
  mutate(
    children = replace_na(children, 0),
    total_guests = adults + children + babies,
    total_nights = stays_in_week_nights + stays_in_weekend_nights,
    cleaning_reason = case_when(
      total_guests == 0 ~ "Aucun client renseigne",
      total_nights == 0 ~ "Sejour de 0 nuit",
      adr < 0 ~ "ADR negatif",
      adr > 1000 ~ "ADR extreme superieur a 1000",
      TRUE ~ "Conserve"
    )
  )

table_cleaning_recap <- df_cleaning_flags |>
  count(cleaning_reason, name = "Effectif") |>
  mutate(
    Pourcentage = scales::percent(Effectif / sum(Effectif), accuracy = 0.01),
    Decision = if_else(cleaning_reason == "Conserve", "Conserve", "Exclu")
  ) |>
  arrange(desc(Decision), cleaning_reason)

df_cleaned <- df_cleaning_flags |>
  filter(cleaning_reason == "Conserve") |>
  dplyr::select(-cleaning_reason)


df_engineered <- df_cleaned |>
  mutate(
    # 0. Cible
    is_canceled = factor(
      is_canceled,
      levels = c(0, 1),
      labels = c("Non", "Oui")
    ),
    
    # 0. Bloc factor
    market_segment = case_when(
      market_segment == "Direct"         ~ "Direct",
      market_segment == "Online TA"      ~ "Online_TA",
      market_segment == "Offline TA/TO"  ~ "Offline_TA",
      market_segment == "Corporate"      ~ "Corporate",
      TRUE                              ~ "Other"
    ) %>% factor(levels = c("Direct", "Online_TA", "Offline_TA",
                            "Corporate", "Other"))
  ,

    distribution_channel = factor(distribution_channel),
    deposit_type = factor(deposit_type),
    customer_type = factor(customer_type),
    hotel=factor(hotel),
    
    # 1. Lead time
    lead_time_cat = case_when(
      lead_time == 0 ~ "derniere_minute",
      lead_time <= 7 ~ "court",
      lead_time <= 30 ~ "moyen",
      lead_time <= 90 ~ "long",
      TRUE ~ "tres_long"
    ),
    
    # 2. Nombre total de nuits
    total_nights = stays_in_week_nights + stays_in_weekend_nights,
    total_nights_cat = case_when(
      total_nights <= 2 ~ "court_sejour",
      total_nights <= 5 ~ "moyen_sejour",
      TRUE ~ "long_sejour"
    ),
    
    # 3. ADR en classes par quartiles
    adr_cat = ntile(adr, 4),
    
    # 4. Présence d'enfants
    has_children = if_else(children + babies > 0, "Oui", "Non"),
    
    # 5. Parking réservé
    parking_reserved = case_when(
      required_car_parking_spaces >= 1 ~ "Oui",
      required_car_parking_spaces == 0 ~ "Non",
      TRUE ~ NA_character_
    ),
    
    parking_binary = if_else(parking_reserved == "Oui", 1, 0),
    
    # 6. Engagement client enrichi
    
    engagement = total_of_special_requests + booking_changes + 1 * parking_binary,# ici l'dée d'integrer un poids plus important pour le parking
    
    engagement_cat = case_when(
      engagement == 0 ~ "faible",
      engagement <= 2 ~ "moyen",
      engagement >= 3 ~ "fort" ####fait gaffe ici au seuils en fonction du coef que tu mets pour la var parking
    ),
    # 7. Historique d'annulations
    previous_cancellations_cat = case_when(
      previous_cancellations == 0 ~ "Non",
      previous_cancellations >= 1 ~ "Une_ou_plus"
    ),
    # 8. saison touristique     
    saison_tourisme = case_when(
      arrival_date_month %in% c("July", "August") ~ "pick_saison",
      arrival_date_month %in% c("June", "September") ~ "haute_saison",
      arrival_date_month %in% c("April", "May", "October") ~ "moyenne_saison",
      TRUE ~ "basse_saison"
    ),
    # 8. client avant 
    repeated_guest_cat = if_else(
      is_repeated_guest == 1,
      "Oui",
      "Non"
    ),
    # 9. Localisation de l'indentité 
    continent = as.factor(replace_na(
      as.character(countrycode(
        country,
        origin       = "iso3c",
        destination  = "continent",
        custom_match = c(
          "TMP" = "Asia",       # Timor oriental (ancien code)
          "ANT" = "Americas",   # Antilles néerlandaises (ancien code)
          "KSV" = "Europe",     # Kosovo (non reconnu ISO)
          "ZAR" = "Africa",     # Zaïre (ancien code RDC)
          "ROM" = "Europe",     # Roumanie (ancien code)
          "CN" = "Asia",        # Chine (code raccourci observe dans la base)
          "ATF" = "Antarctica", # Terres australes et antarctiques francaises
          "UMI" = "Oceania",    # Iles mineures eloignees des Etats-Unis
          "NULL" = NA_character_
        )
      )),
      "Unknown"
    )),
    
    identity_location = as.factor(case_when(
      country   == "PRT"    ~ "Local_Portugal",
      continent == "Europe" ~ "Europe_hors_PRT",
      # continent == "Unknown"~ "Unknown",
      TRUE                  ~ "Other_International"
    )),
  ) |>
  mutate(
    lead_time_cat = factor(
      lead_time_cat,
      levels = c("derniere_minute", "court", "moyen", "long", "tres_long")
    ),
    
    total_nights_cat = factor(
      total_nights_cat,
      levels = c("court_sejour", "moyen_sejour", "long_sejour")
    ),
    
    adr_cat = factor(
      adr_cat,
      levels = c(1, 2, 3, 4),
      labels = c("q1_low", "q2_midlow", "q3_midhigh", "q4_high")
    ),
    
    has_children = factor(
      has_children,
      levels = c("Non", "Oui")
    ),
    
    parking_reserved = factor(
      parking_reserved,
      levels = c("Non", "Oui")
    ),
    
    engagement_cat = factor(
      engagement_cat,
      levels = c("faible", "moyen", "fort")
    ),
    previous_cancellations_cat = factor(
      previous_cancellations_cat,
      levels = c("Non", "Une_ou_plus")
    ),
    saison_tourisme = factor(
      saison_tourisme,
      levels = c("basse_saison", "moyenne_saison", "haute_saison", "pick_saison")
    ),
    repeated_guest_cat = factor(
      repeated_guest_cat,
      levels = c("Non", "Oui")
    )
  )


# ============================================================
# TABLEAUX RECAPITULATIFS POUR LE RAPPORT
# ============================================================

missing_by_var <- colSums(is.na(df))
missing_by_var <- missing_by_var[missing_by_var > 0]

missing_summary <- if (length(missing_by_var) == 0) {
  "Aucune"
} else {
  paste(
    paste0(names(missing_by_var), " (", missing_by_var, ")"),
    collapse = " ; "
  )
}

table_base_recap <- data.frame(
  Indicateur = c(
    "Unite statistique",
    "Nombre d'observations initiales",
    "Nombre de variables initiales",
    "Nombre d'observations exclues au nettoyage",
    "Nombre d'observations conservees apres nettoyage",
    "Part de la base conservee",
    "Nombre de variables apres engineering",
    "Nombre de reservations City Hotel",
    "Nombre de reservations Resort Hotel",
    "Variable cible",
    "Reservations non annulees - base initiale",
    "Reservations annulees - base initiale",
    "Taux d'annulation - base initiale",
    "Reservations non annulees - base nettoyee",
    "Reservations annulees - base nettoyee",
    "Taux d'annulation - base nettoyee",
    "Valeurs manquantes au sens R",
    "Variables avec valeurs manquantes"
  ),
  Valeur = c(
    "Une reservation hoteliere",
    format(nrow(df), big.mark = " ", scientific = FALSE),
    ncol(df),
    format(nrow(df) - nrow(df_cleaned), big.mark = " ", scientific = FALSE),
    format(nrow(df_cleaned), big.mark = " ", scientific = FALSE),
    scales::percent(nrow(df_cleaned) / nrow(df), accuracy = 0.1),
    ncol(df_engineered),
    format(sum(df$hotel == "City Hotel"), big.mark = " ", scientific = FALSE),
    format(sum(df$hotel == "Resort Hotel"), big.mark = " ", scientific = FALSE),
    "is_canceled",
    format(sum(df$is_canceled == 0), big.mark = " ", scientific = FALSE),
    format(sum(df$is_canceled == 1), big.mark = " ", scientific = FALSE),
    paste0(round(mean(df$is_canceled == 1) * 100, 1), " %"),
    format(sum(df_engineered$is_canceled == "Non"), big.mark = " ", scientific = FALSE),
    format(sum(df_engineered$is_canceled == "Oui"), big.mark = " ", scientific = FALSE),
    scales::percent(mean(df_engineered$is_canceled == "Oui"), accuracy = 0.1),
    format(sum(is.na(df)), big.mark = " ", scientific = FALSE),
    missing_summary
  ),
  check.names = FALSE
)

table_variables_retenues <- dico_variables
names(table_variables_retenues)[names(table_variables_retenues) == "Nom.de.la.variable"] <- "Variable"

table_variables_creees <- data.frame(
  Variable = c(
    "is_canceled",
    "lead_time_cat",
    "total_nights",
    "total_nights_cat",
    "adr_cat",
    "has_children",
    "parking_reserved",
    "engagement",
    "engagement_cat",
    "previous_cancellations_cat",
    "saison_tourisme",
    "repeated_guest_cat",
    "continent",
    "identity_location"
  ),
  Variables_sources = c(
    "is_canceled",
    "lead_time",
    "stays_in_week_nights + stays_in_weekend_nights",
    "total_nights",
    "adr",
    "children + babies",
    "required_car_parking_spaces",
    "total_of_special_requests + booking_changes + 1 x parking_binary",
    "engagement",
    "previous_cancellations",
    "arrival_date_month",
    "is_repeated_guest",
    "country",
    "country + continent"
  ),
  Regle_de_construction = c(
    "Recodage de 0/1 en Non/Oui",
    "0 = derniere_minute ; <= 7 = court ; <= 30 = moyen ; <= 90 = long ; > 90 = tres_long",
    "Somme des nuits en semaine et de week-end",
    "<= 2 = court_sejour ; <= 5 = moyen_sejour ; > 5 = long_sejour",
    "Decoupage de l'ADR en quartiles q1_low, q2_midlow, q3_midhigh, q4_high",
    "Oui si au moins un enfant ou bebe, Non sinon",
    "Oui si au moins une place de parking demandee, Non sinon",
    "Somme des demandes speciales et des changements de reservation et de reservation de place de parking ",
    "0 = faible ; <= 2 = moyen ; >= 3 = fort",
    "0 = Non ; >= 1 = Une_ou_plus",
    "Juillet-aout = pick_saison ; juin-septembre = haute_saison ; avril-mai-octobre = moyenne_saison ; autres mois = basse_saison",
    "1 = Oui ; 0 = Non",
    "Conversion du code pays ISO en continent avec countrycode et regroupement des codes non reconnus",
    "Portugal = Local_Portugal ; Europe hors Portugal = Europe_hors_PRT ; autres origines = Other_International"
  ),
  Role_dans_l_analyse = c(
    "Variable cible a predire",
    "Mesurer l'anticipation de la reservation",
    "Mesurer la duree totale du sejour",
    "Regrouper les durees de sejour",
    "Comparer les niveaux de prix",
    "Identifier les reservations familiales",
    "Mesurer un signe d'engagement du client",
    "Score enrichi d'engagement",
    "Comparer les niveaux d'engagement",
    "Capturer l'historique d'annulation",
    "Tenir compte de la saison touristique",
    "Identifier les clients deja venus",
    "Intermediaire de construction de l'origine geographique",
    "Resumer l'origine geographique du client pour la modelisation"
  ),
  check.names = FALSE
)

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
)

table_variables_modelisation <- table_variables_creees[
  table_variables_creees$Variable %in% variables_modelisation,
  c("Variable", "Role_dans_l_analyse")
]

table_variables_modelisation <- rbind(
  data.frame(
    Variable = c("hotel", "market_segment", "customer_type"),
    Role_dans_l_analyse = c(
      "Distinguer City Hotel et Resort Hotel",
      "Caracteriser le canal commercial de reservation",
      "Distinguer les profils de clients selon le type de reservation"
    ),
    check.names = FALSE
  ),
  table_variables_modelisation
)

table_variables_modelisation <- table_variables_modelisation[
  order(match(table_variables_modelisation$Variable, variables_modelisation)),
]

rownames(table_variables_modelisation) <- NULL

