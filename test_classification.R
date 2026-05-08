# Test rapide du script classification.R
source("scripts/00-packages.R")
source("scripts/01-import.R")
source("scripts/02-data_engenering.R")
source("scripts/04-logistic.R")

# Vérifier les objets de base
cat("✓ df_model existe:", exists("df_model"), "\n")
cat("✓ train existe:", exists("train"), "\n")
cat("✓ validation existe:", exists("validation"), "\n")
cat("✓ test existe:", exists("test"), "\n")

# Commencer l'analyse DISQUAL de base
cat("\n=== Test de base DISQUAL ===\n")

# Variables qualitatives
target_var <- "is_canceled"
quali_vars_disqual <- setdiff(names(df_model), target_var)
cat("Variables qualitatives:", length(quali_vars_disqual), "\n")

# Test ACM de base
cat("Test ACM...\n")
res_mca_test <- tryCatch({
  FactoMineR::MCA(df_model[, quali_vars_disqual], graph = FALSE, ncp = 5)
}, error = function(e) {
  cat("Erreur ACM:", e$message, "\n")
  NULL
})

if (!is.null(res_mca_test)) {
  cat("✓ ACM fonctionne avec", res_mca_test$call$ncp, "axes\n")
  cat("✓ Script classification.R prêt pour l'exécution complète\n")
} else {
  cat("✗ Problème avec ACM\n")
}