#########################################################"
#####################################################
####### construction des variables de l'etude



df_engineered <- df |>
  mutate(
    # 0. Cible
    is_canceled = factor(
      is_canceled,
      levels = c(0, 1),
      labels = c("Non", "Oui")
    ),
    
    # 0. Bloc factor
    market_segment = factor(market_segment),
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
    engagement_raw = total_of_special_requests + booking_changes,
    
    engagement = engagement_raw + 1 * parking_binary,# ici l'dée d'integrer un poids plus important pour le parking
    
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


