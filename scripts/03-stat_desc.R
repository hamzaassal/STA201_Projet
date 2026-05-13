
# ============================================================
# THEME PRO
# ============================================================

theme_report <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey35"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "grey20"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(12, 20, 12, 12)
  )

theme_set(theme_report)

# ============================================================
# BASE D'ANALYSE a la fin j'ai ajoute saison_tourisme et is_repeated_guest_cat oubli pas de les ajouter au stats desc
# ============================================================

df_desc <- df_engineered |>
  dplyr::select(
    is_canceled,
    hotel,
    market_segment,
    distribution_channel,
    deposit_type,
    customer_type,
    lead_time_cat,
    total_nights_cat,
    adr_cat,
    has_children,
    parking_reserved,
    engagement_cat,
    previous_cancellations_cat,
    saison_tourisme,
    repeated_guest_cat,
    identity_location
  ) |>
  drop_na()

# ============================================================
# FONCTIONS GRAPHIQUES
# ============================================================

plot_univariate <- function(data, var_name, title_label = NULL) {
  if (is.null(title_label)) {
    title_label <- str_replace_all(var_name, "_", " ")
  }
  
  data |>
    count(variable = .data[[var_name]]) |>
    mutate(
      prop = n / sum(n),
      label = percent(prop, accuracy = 0.1)
    ) |>
    ggplot(aes(x = reorder(as.character(variable), n), y = n)) +
    geom_col(fill = "#1F77B4", width = 0.68, alpha = 0.9) +
    geom_text(
      aes(label = label),
      hjust = -0.12,
      size = 4,
      fontface = "bold",
      color = "grey15"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = title_label,
      subtitle = "Répartition des observations",
      x = NULL,
      y = "Effectif"
    ) +
    theme_report
}

# plot_bivariate <- function(data, var_name, title_label = NULL) {
#   if (is.null(title_label)) {
#     title_label <- str_replace_all(var_name, "_", " ")
#   }
#   
#   data |>
#     count(variable = .data[[var_name]], is_canceled) |>
#     group_by(variable) |>
#     mutate(prop = n / sum(n)) |>
#     ungroup() |>
#     ggplot(
#       aes(
#         x = reorder(as.character(variable), prop),
#         y = prop,
#         fill = is_canceled
#       )
#     ) +
#     geom_col(position = "fill", width = 0.68, alpha = 0.95) +
#     coord_flip() +
#     scale_fill_manual(values = c("Non" = "#1F77B4", "Oui" = "#E76F51")) +
#     scale_y_continuous(labels = percent_format()) +
#     labs(
#       title = title_label,
#       subtitle = "Répartition annulé / non annulé",
#       x = NULL,
#       y = "Proportion",
#       fill = "Annulation"
#     ) +
#     theme_report
# }
plot_bivariate <- function(data, var_name, title_label = NULL) {
  if (is.null(title_label)) {
    title_label <- str_replace_all(var_name, "_", " ")
  }
  
  plot_data <- data |>
    count(variable = .data[[var_name]], is_canceled) |>
    group_by(variable) |>
    mutate(
      prop = n / sum(n),
      label = percent(prop, accuracy = 0.1)
    ) |>
    ungroup()
  
  total_data <- plot_data |>
    group_by(variable) |>
    summarise(total_n = sum(n), .groups = "drop") |>
    mutate(
      total_prop = total_n / sum(total_n),
      total_label = paste0(percent(total_prop, accuracy = 0.1), " du total")
    )
  
  variable_levels <- total_data |>
    arrange(total_prop) |>
    pull(variable) |>
    as.character()
  
  plot_data <- plot_data |>
    mutate(variable_ordered = factor(as.character(variable), levels = variable_levels))
  
  total_data <- total_data |>
    mutate(variable_ordered = factor(as.character(variable), levels = variable_levels))
  
  plot_data |>
    ggplot(
      aes(
        x = variable_ordered,
        y = prop,
        fill = is_canceled
      )
    ) +
    geom_col(position = "fill", width = 0.68, alpha = 0.95) +
    geom_text(
      aes(label = label),
      position = position_fill(vjust = 0.5),
      size = 3.4,
      fontface = "bold",
      color = "white"
    ) +
    geom_text(
      data = total_data,
      aes(
        x = variable_ordered,
        y = 1.03,
        label = total_label
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3.2,
      fontface = "bold",
      color = "grey20"
    ) +
    coord_flip() +
    scale_fill_manual(values = c("Non" = "#1F77B4", "Oui" = "#E76F51")) +
    scale_y_continuous(
      labels = percent_format(),
      limits = c(0, 1.18),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title_label,
      subtitle = "Répartition annulé / non annulé",
      x = NULL,
      y = "Proportion",
      fill = "Annulation"
    ) +
    theme_report
}

plot_cancel_rate <- function(data, var_name, title_label = NULL) {
  if (is.null(title_label)) {
    title_label <- str_replace_all(var_name, "_", " ")
  }
  
  data |>
    group_by(variable = .data[[var_name]]) |>
    summarise(
      n = n(),
      cancel_rate = mean(is_canceled == "Oui"),
      .groups = "drop"
    ) |>
    mutate(label = percent(cancel_rate, accuracy = 0.1)) |>
    ggplot(aes(x = reorder(as.character(variable), cancel_rate), y = cancel_rate)) +
    geom_col(fill = "#E76F51", width = 0.68, alpha = 0.95) +
    geom_text(
      aes(label = label),
      hjust = -0.12,
      size = 4,
      fontface = "bold",
      color = "grey15"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      labels = percent_format(),
      expand = expansion(mult = c(0, 0.18))
    ) +
    labs(
      title = title_label,
      subtitle = "Taux d'annulation par modalité",
      x = NULL,
      y = "Taux d'annulation"
    ) +
    theme_report
}

# ============================================================
# FONCTIONS TABLEAUX
# ============================================================

make_univariate_table <- function(data, var_name) {
  data |>
    count(Modalite = .data[[var_name]]) |>
    mutate(Pourcentage = percent(n / sum(n), accuracy = 0.1)) |>
    rename(Effectif = n) |>
    kable(
      caption = paste("Distribution de", str_replace_all(var_name, "_", " ")),
      align = c("l", "r", "r")
    )
}

make_bivariate_table <- function(data, var_name) {
  data |>
    count(Modalite = .data[[var_name]], Annulation = is_canceled) |>
    group_by(Modalite) |>
    mutate(Pourcentage = percent(n / sum(n), accuracy = 0.1)) |>
    ungroup() |>
    rename(Effectif = n) |>
    pivot_wider(
      names_from = Annulation,
      values_from = c(Effectif, Pourcentage),
      values_fill = list(
        Effectif = 0,
        Pourcentage = "0.0%"
      )
    ) |>
    kable(
      caption = paste(
        "Répartition des annulations selon",
        str_replace_all(var_name, "_", " ")
      ),
      align = "lrrrr"
    )
}

# ============================================================
# GRAPHIQUES UNIVARIES INDIVIDUELS
# ============================================================

p1 <- plot_univariate(df_desc, "hotel", "Type d'hôtel")
p2 <- plot_univariate(df_desc, "market_segment", "Segment de marché")
p3 <- plot_univariate(df_desc, "distribution_channel", "Canal de distribution")
p4 <- plot_univariate(df_desc, "deposit_type", "Type de dépôt")
p5 <- plot_univariate(df_desc, "customer_type", "Type de client")
p6 <- plot_univariate(df_desc, "lead_time_cat", "Délai de réservation")
p7 <- plot_univariate(df_desc, "total_nights_cat", "Durée du séjour")
p8 <- plot_univariate(df_desc, "adr_cat", "Niveau de prix ADR")
p9 <- plot_univariate(df_desc, "has_children", "Présence d'enfants")
p10 <- plot_univariate(df_desc, "parking_reserved", "Parking réservé")
p11 <- plot_univariate(df_desc, "engagement_cat", "Engagement client")
p12 <- plot_univariate(df_desc, "previous_cancellations_cat", "Historique d'annulation")
p13  <- plot_univariate(df_desc,  "identity_location")
                 
final_univariate_plot <-
  (p1 | p2) /
  # (p3 | p4) /
  # (p5 | p6) /
  # (p7 | p8) /
  # (p9 | p10) /
  (p11 | p12) +
  plot_annotation(
    title = "Distribution des variables explicatives",
    theme = theme(
      plot.title = element_text(size = 21, face = "bold", hjust = 0.5)
    )
  )

final_univariate_plot # ici quand on decide exactement des variables scinder en deux blocs et non un seul enorme bloc illisible

# ============================================================
# TABLEAUX UNIVARIES INDIVIDUELS
# ============================================================

table_uni_hotel <- make_univariate_table(df_desc, "hotel")
table_uni_market_segment <- make_univariate_table(df_desc, "market_segment")
table_uni_distribution_channel <- make_univariate_table(df_desc, "distribution_channel")
table_uni_deposit_type <- make_univariate_table(df_desc, "deposit_type")
table_uni_customer_type <- make_univariate_table(df_desc, "customer_type")
table_uni_lead_time <- make_univariate_table(df_desc, "lead_time_cat")
table_uni_total_nights <- make_univariate_table(df_desc, "total_nights_cat")
table_uni_adr <- make_univariate_table(df_desc, "adr_cat")
table_uni_children <- make_univariate_table(df_desc, "has_children")
table_uni_parking <- make_univariate_table(df_desc, "parking_reserved")
table_uni_engagement <- make_univariate_table(df_desc, "engagement_cat")
table_uni_previous <- make_univariate_table(df_desc, "previous_cancellations_cat")

table_uni_hotel
table_uni_market_segment
table_uni_distribution_channel
table_uni_deposit_type
table_uni_customer_type
table_uni_lead_time
table_uni_total_nights
table_uni_adr
table_uni_children
table_uni_parking
table_uni_engagement
table_uni_previous

# ============================================================
# GRAPHIQUES BIVARIES INDIVIDUELS
# ============================================================

(b1 <- plot_bivariate(df_desc, "hotel", "Type d'hôtel"))
(b2 <- plot_bivariate(df_desc, "market_segment", "Segment de marché"))
(b3 <- plot_bivariate(df_desc, "distribution_channel", "Canal de distribution"))
(b4 <- plot_bivariate(df_desc, "deposit_type", "Type de dépôt"))
(b5 <- plot_bivariate(df_desc, "customer_type", "Type de client"))
(b6 <- plot_bivariate(df_desc, "lead_time_cat", "Délai de réservation"))
(b7 <- plot_bivariate(df_desc, "total_nights_cat", "Durée du séjour"))
(b8 <- plot_bivariate(df_desc, "adr_cat", "Niveau de prix ADR"))
(b9 <- plot_bivariate(df_desc, "has_children", "Présence d'enfants"))
(b10 <- plot_bivariate(df_desc, "parking_reserved", "Parking réservé"))
(b11 <- plot_bivariate(df_desc, "engagement_cat", "Engagement client"))
(b12 <- plot_bivariate(df_desc, "previous_cancellations_cat", "Historique d'annulation"))
(b13 =plot_bivariate(df_desc,"identity_location"))

final_bivariate_plot <-
  # (b1 | b2) /
  # (b13 | b4) /
  # (b5 | b6) /
  # (b7 | b8) /
  # (b9 | b10) /
  (b6 | b11) +
  plot_annotation(
    title = "Répartition des annulations selon les variables explicatives",
    theme = theme(
      plot.title = element_text(size = 21, face = "bold", hjust = 0.5)
    )
  )

final_bivariate_plot

final_bivariate_plot_all <-
  # (b1 | b2) /
  # (b13 | b4) /
  # (b5 | b6) /
  
  (b2 | b12)/
   (b13 | b5) /
  (b9 | b8) 
   +
  plot_annotation(
    title = "Répartition des annulations selon les variables explicatives",
    theme = theme(
      plot.title = element_text(size = 21, face = "bold", hjust = 0.5)
    )
  )
final_bivariate_plot_all
# ============================================================
# TABLEAUX BIVARIES INDIVIDUELS
# ============================================================

table_biv_hotel <- make_bivariate_table(df_desc, "hotel")
table_biv_market_segment <- make_bivariate_table(df_desc, "market_segment")
table_biv_distribution_channel <- make_bivariate_table(df_desc, "distribution_channel")
table_biv_deposit_type <- make_bivariate_table(df_desc, "deposit_type")
table_biv_customer_type <- make_bivariate_table(df_desc, "customer_type")
table_biv_lead_time <- make_bivariate_table(df_desc, "lead_time_cat")
table_biv_total_nights <- make_bivariate_table(df_desc, "total_nights_cat")
table_biv_adr <- make_bivariate_table(df_desc, "adr_cat")
table_biv_children <- make_bivariate_table(df_desc, "has_children")
table_biv_parking <- make_bivariate_table(df_desc, "parking_reserved")
table_biv_engagement <- make_bivariate_table(df_desc, "engagement_cat")
table_biv_previous <- make_bivariate_table(df_desc, "previous_cancellations_cat")

table_biv_hotel
table_biv_market_segment
table_biv_distribution_channel
table_biv_deposit_type
table_biv_customer_type
table_biv_lead_time
table_biv_total_nights
table_biv_adr
table_biv_children
table_biv_parking
table_biv_engagement
table_biv_previous

# ============================================================
# GRAPHIQUES TAUX D'ANNULATION INDIVIDUELS
# ============================================================

c1 <- plot_cancel_rate(df_desc, "hotel", "Type d'hôtel")
c2 <- plot_cancel_rate(df_desc, "market_segment", "Segment de marché")
c3 <- plot_cancel_rate(df_desc, "distribution_channel", "Canal de distribution")
c4 <- plot_cancel_rate(df_desc, "deposit_type", "Type de dépôt")
c5 <- plot_cancel_rate(df_desc, "customer_type", "Type de client")
c6 <- plot_cancel_rate(df_desc, "lead_time_cat", "Délai de réservation")
c7 <- plot_cancel_rate(df_desc, "total_nights_cat", "Durée du séjour")
c8 <- plot_cancel_rate(df_desc, "adr_cat", "Niveau de prix ADR")
c9 <- plot_cancel_rate(df_desc, "has_children", "Présence d'enfants")
c10 <- plot_cancel_rate(df_desc, "parking_reserved", "Parking réservé")
c11 <- plot_cancel_rate(df_desc, "engagement_cat", "Engagement client")
c12 <- plot_cancel_rate(df_desc, "previous_cancellations_cat", "Historique d'annulation")

final_cancel_plot <-
  (c1 | c2) /
  (c3 | c4) /
  (c5 | c6) /
  (c7 | c8) /
  (c9 | c10) /
  (c11 | c12) +
  plot_annotation(
    title = "Taux d'annulation selon les variables explicatives",
    theme = theme(
      plot.title = element_text(size = 21, face = "bold", hjust = 0.5)
    )
  )

final_cancel_plot

# ============================================================
# ANALYSE DE CORRELATION ENTRE VARIABLES QUALITATIVES
# ============================================================

library(rstatix)
library(reshape2)
library(corrplot)

# ============================================================
# VARIABLES QUALITATIVES A TESTER
# ============================================================

vars_corr <- df_engineered |>
  dplyr::select(
    hotel,
    market_segment,
    distribution_channel,
    customer_type,
    lead_time_cat,
    total_nights_cat,
    adr_cat,
    has_children,
    repeated_guest_cat,
    engagement_cat,
    previous_cancellations_cat,
    saison_tourisme,
    repeated_guest_cat,
    identity_location
  )

# ============================================================
# FONCTION V DE CRAMER
# ============================================================

cramer_v_matrix <- function(data) {
  
  vars <- names(data)
  
  mat <- matrix(
    NA,
    nrow = length(vars),
    ncol = length(vars)
  )
  
  rownames(mat) <- vars
  colnames(mat) <- vars
  
  for (i in seq_along(vars)) {
    
    for (j in seq_along(vars)) {
      
      if (i == j) {
        
        mat[i, j] <- 1
        
      } else {
        
        tbl <- table(
          data[[vars[i]]],
          data[[vars[j]]]
        )
        
        test <- suppressWarnings(
          chisq.test(tbl)
        )
        
        mat[i, j] <- rstatix::cramer_v(tbl)
      }
    }
  }
  
  return(mat)
}

# ============================================================
# MATRICE DE V DE CRAMER
# ============================================================

cramer_matrix <- cramer_v_matrix(vars_corr)

cramer_matrix

# ============================================================
# HEATMAP 
# ============================================================

HEATMAP_CRAMER=corrplot(
  cramer_matrix,
  
  method = "color",
  
  type = "upper",
  
  addCoef.col = "black",
  
  tl.col = "black",
  
  tl.srt = 45,
  
  col = colorRampPalette(
    c(
      "#F7FBFF",
      "#6BAED6",
      "#08306B"
    )
  )(200),
  
  number.cex = 0.7,
  
  mar = c(0, 0, 2, 0),
  
  title = "Matrice des associations (V de Cramér)"
)

# ============================================================
# VERSION TIDY DU TABLEAU
# ============================================================

cramer_table <- as.data.frame(cramer_matrix) |>
  
  rownames_to_column("var1") |>
  
  pivot_longer(
    -var1,
    names_to = "var2",
    values_to = "cramer_v"
  ) |>
  
  filter(var1 != var2) |>
  
  mutate(
    pair = map2_chr(
      var1,
      var2,
      ~ paste(sort(c(.x, .y)), collapse = " - ")
    )
  ) |>
  
  distinct(pair, .keep_all = TRUE) |>
  
  arrange(desc(cramer_v))

cramer_table

# ============================================================
# ASSOCIATIONS FORTES
# ============================================================

cramer_table |>
  filter(cramer_v >= 0.3) |>
  select(var1, var2, cramer_v) |>
  kable(
    caption = "Associations fortes entre variables qualitatives",
    digits = 3
  )

