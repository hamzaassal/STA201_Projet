



df=read.csv("data/hotel_bookings.csv")

# Dictionnaire des variables retenues pour le rapport
dico_variables <- data.frame(
  "Nom de la variable" = c(
    "hotel",
    "is_canceled",
    "lead_time",
    "arrival_date_year",
    "arrival_date_month",
    "stays_in_weekend_nights",
    "stays_in_week_nights",
    "adults",
    "children",
    "babies",
    "meal",
    "country",
    "market_segment",
    "deposit_type",
    "customer_type",
    "adr",
    "total_of_special_requests"
  ),
  "Description" = c(
    "Type d’hôtel : Resort Hotel ou City Hotel",
    "Indicateur d’annulation de la réservation : 1 = annulée, 0 = non annulée",
    "Nombre de jours entre la réservation et la date d’arrivée",
    "Année d’arrivée prévue",
    "Mois d’arrivée prévue",
    "Nombre de nuits de week-end réservées",
    "Nombre de nuits en semaine réservées",
    "Nombre d’adultes",
    "Nombre d’enfants",
    "Nombre de bébés",
    "Type de repas réservé",
    "Pays d’origine du client",
    "Canal ou segment de marché de la réservation",
    "Type de dépôt associé à la réservation",
    "Type de client",
    "Prix moyen journalier de la réservation",
    "Nombre de demandes spéciales formulées par le client"
  ),
  "Type" = c(
    "Qualitative",
    "Qualitative",
    "Quantitative",
    "Quantitative",
    "Qualitative",
    "Quantitative",
    "Quantitative",
    "Quantitative",
    "Quantitative",
    "Quantitative",
    "Qualitative",
    "Qualitative",
    "Qualitative",
    "Qualitative",
    "Qualitative",
    "Quantitative",
    "Quantitative"
  )
)