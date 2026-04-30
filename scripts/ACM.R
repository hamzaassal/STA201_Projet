# ============================================================
# ACM FINALE - VERSION PROPRE ET ACADEMIQUE
# ============================================================

# ===============================
# LIBRAIRIES
# ===============================

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(janitor)
library(knitr)
library(scales)
library(patchwork)

# ============================================================
# 1. CONSTRUCTION DE LA BASE ACM
# ============================================================

# -------------------------------
# Variables actives :
# -------------------------------
# hotel
# market_segment
# distribution_channel
# customer_type
# lead_time_cat
# total_nights_cat
# adr_cat
# has_children
# engagement_cat
# previous_cancellations_cat

# -------------------------------
# Variable illustrative :
# -------------------------------
# is_canceled

df_acm <- df_engineered |>
  
  clean_names() |>
  
  dplyr::select(
    
    # variable illustrative
    is_canceled,
    
    # variables actives
    # hotel,
    # market_segment,
    # distribution_channel,
    # deposit_type,
    # customer_type,
    lead_time_cat,
    total_nights_cat,
    # adr_cat,
    has_children,
    # parking_reserved,
    engagement_cat,
    previous_cancellations_cat,
    saison_tourisme,
    # is_repeated_guest_cat
  ) |>
  
  mutate(
    across(everything(), as.factor)
  ) |>
  
  drop_na()

# ============================================================
# 2. ANALYSE DES CORRESPONDANCES MULTIPLES
# ============================================================

# quali.sup :
# 1 = is_canceled

res_acm <- MCA(
  df_acm,
  quali.sup = 1,
  graph = FALSE
)

# ============================================================
# 3. VALEURS PROPRES
# ============================================================

eig_acm <- as.data.frame(res_acm$eig)

eig_acm

# ============================================================
# 4. SCREE PLOT
# ============================================================

fviz_screeplot(
  res_acm,
  addlabels = TRUE,
  ylim = c(0, 20)
)

# ============================================================
# 5. TABLEAU SYNTHETIQUE DES AXES
# ============================================================

n_var_active <- 10

kaiser_threshold <- 1 / n_var_active

summary_axes <- eig_acm |>
  
  rownames_to_column("dimension") |>
  
  mutate(
    keep_kaiser = eigenvalue > kaiser_threshold
  )

summary_axes

# ============================================================
# 6. NOMBRE D'AXES A CONSERVER
# ============================================================

sum(eig_acm$eigenvalue > kaiser_threshold)

# ============================================================
# 7. COORDONNEES DES MODALITES
# ============================================================

coord_var_acm <- as.data.frame(
  res_acm$var$coord
) |>
  
  rownames_to_column("modalite")

coord_var_acm

# ============================================================
# 8. CONTRIBUTIONS DES MODALITES
# ============================================================

contrib_var_acm <- as.data.frame(
  res_acm$var$contrib
) |>
  
  rownames_to_column("modalite")

contrib_var_acm

# ============================================================
# 9. COS² DES MODALITES
# ============================================================

cos2_var_acm <- as.data.frame(
  res_acm$var$cos2
) |>
  
  rownames_to_column("modalite")

cos2_var_acm

# ============================================================
# 10. VARIABLE ILLUSTRATIVE
# ============================================================

# -------------------------------
# is_canceled
# -------------------------------

res_acm$quali.sup$coord

# ============================================================
# 11. VISUALISATION DES INDIVIDUS    Trop long ne le fait tourner qu'en cas de besoin
# ============================================================

# fviz_mca_ind(
#   res_acm,
#   
#   habillage = df_acm$is_canceled,
#   
#   addEllipses = TRUE,
#   
#   palette = c(
#     "#2C7FB8",
#     "#D95F0E"
#   ),
#   
#   alpha.ind = 0.15,
#   
#   repel = TRUE
# )

# ============================================================
# 12. VISUALISATION DES MODALITES
# ============================================================

fviz_mca_var(
  res_acm,
  
  repel = TRUE,
  
  ggtheme = theme_minimal()
)

# ============================================================
# 13. CONTRIBUTIONS AXE 1
# ============================================================

fviz_contrib(
  res_acm,
  
  choice = "var",
  
  axes = 1,
  
  top = 20
)

# ============================================================
# 14. CONTRIBUTIONS AXE 2
# ============================================================

fviz_contrib(
  res_acm,
  
  choice = "var",
  
  axes = 2,
  
  top = 20
)

# ============================================================
# 15. CONTRIBUTIONS AXE 3
# ============================================================

fviz_contrib(
  res_acm,
  
  choice = "var",
  
  axes = 3,
  
  top = 20
)

# ============================================================
# 16. QUALITE DE REPRESENTATION (COS²)
# ============================================================

fviz_cos2(
  res_acm,
  
  choice = "var",
  
  axes = 1,
  
  top = 20
)

fviz_cos2(
  res_acm,
  
  choice = "var",
  
  axes = 2,
  
  top = 20
)

# ============================================================
# 17. CARTE FACTORIELLE FINALE
# ============================================================

fviz_mca_biplot(
  res_acm,
  
  repel = TRUE,
  
  ggtheme = theme_minimal(),
  
  habillage = df_acm$is_canceled,
  
  palette = c(
    "#2C7FB8",
    "#D95F0E"
  )
)

# ============================================================
# 18. TABLEAU RESUME DES AXES
# ============================================================

summary_axes |>
  
  kable(
    caption = "Résumé des dimensions de l'ACM",
    digits = 3
  )

# ============================================================
# 19. EXTRACTION DES COORDONNEES FACTORIELLES
# ============================================================

coord_individus <- as.data.frame(
  res_acm$ind$coord
)

head(coord_individus)

# ============================================================
# 20. SAUVEGARDE DES RESULTATS
# ============================================================

acm_results <- list(
  
  eigenvalues = eig_acm,
  
  coord_var = coord_var_acm,
  
  contrib_var = contrib_var_acm,
  
  cos2_var = cos2_var_acm,
  
  coord_ind = coord_individus,
  
  summary_axes = summary_axes
)

# ============================================================
# 21. MESSAGE FINAL
# ============================================================

cat(
  "\n====================================================",
  "\n ACM TERMINEE AVEC SUCCES",
  "\n====================================================\n"
)