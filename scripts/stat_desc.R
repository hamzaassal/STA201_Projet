# ---- 04.1 Cible--- stat univarié variable cible

df_eda <- df |>
  clean_names() |>
  mutate(
    # cible
    is_canceled = factor(is_canceled, levels = c(0, 1), labels = c("no", "yes")),
    
    # dates
    reservation_status_date = as.Date(reservation_status_date),
    
    # facteurs (ajuste si besoin)
    hotel = factor(hotel),
    arrival_date_month = factor(
      arrival_date_month,
      levels = c("January","February","March","April","May","June",
                 "July","August","September","October","November","December")
    ),
    meal = factor(meal),
    country = factor(country),
    market_segment = factor(market_segment),
    distribution_channel = factor(distribution_channel),
    deposit_type = factor(deposit_type),
    customer_type = factor(customer_type),
    reserved_room_type = factor(reserved_room_type),
    assigned_room_type = factor(assigned_room_type),
    reservation_status = factor(reservation_status),
    
    # "NULL" en chaîne -> NA (agent/company)
    agent = na_if(agent, "NULL"),
    company = na_if(company, "NULL")
  )


p_target <- df_eda |>
  ggplot(aes(x = is_canceled, fill = is_canceled)) +
  geom_bar(width = 0.7) +
  geom_text(
    stat = "count",
    aes(label = percent(after_stat(count) / sum(after_stat(count)))),
    vjust = -0.4
  ) +
  scale_fill_manual(values = c("no" = "#2C7FB8", "yes" = "#D95F0E")) +
  labs(title = "Répartition de la cible : is_canceled", x = NULL, y = "Nombre") +
  guides(fill = "none")

p_target

# ---- 05.2 Numérique vs cible : boxplot + densité + stats simples bivarié lead time
plot_num_vs_target <- function(data, var) {
  var <- rlang::ensym(var)
  
  stats <- data |>
    group_by(is_canceled) |>
    summarise(
      n = sum(!is.na(!!var)),
      mean = mean(!!var, na.rm = TRUE),
      sd = sd(!!var, na.rm = TRUE),
      median = median(!!var, na.rm = TRUE),
      p25 = quantile(!!var, 0.25, na.rm = TRUE),
      p75 = quantile(!!var, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  p_box <- data |>
    ggplot(aes(x = is_canceled, y = !!var, fill = is_canceled)) +
    geom_boxplot(outlier.alpha = 0.2) +
    scale_fill_manual(values = c("no" = "#2C7FB8", "yes" = "#D95F0E")) +
    labs(
      title = paste0("Boxplot : ", rlang::as_string(var), " vs is_canceled"),
      x = NULL, y = NULL
    ) +
    guides(fill = "none")
  
  p_den <- data |>
    ggplot(aes(x = !!var, color = is_canceled, fill = is_canceled)) +
    geom_density(alpha = 0.2, na.rm = TRUE) +
    scale_fill_manual(values = c("no" = "#2C7FB8", "yes" = "#D95F0E")) +
    scale_color_manual(values = c("no" = "#2C7FB8", "yes" = "#D95F0E")) +
    labs(
      title = paste0("Densité : ", rlang::as_string(var), " vs is_canceled"),
      x = NULL, y = NULL
    )
  
  list(stats = stats, box = p_box, density = p_den)
}

res_lead <- plot_num_vs_target(df_eda, lead_time)
res_lead$stats
res_lead$box
res_lead$density

