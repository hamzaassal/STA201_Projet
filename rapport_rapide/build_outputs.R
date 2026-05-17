# ============================================================
# CONSTRUCTION DES OBJETS POUR LE RAPPORT RAPIDE
# ============================================================
#
# A executer une seule fois, ou lorsque les scripts/donnees changent :
# source("rapport_rapide/build_outputs.R")
#
# Ce script ne modifie pas les scripts du projet. Il les execute puis
# sauvegarde l'environnement utile dans rapport_rapide/outputs.

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile),
  error = function(e) normalizePath("rapport_rapide/build_outputs.R")
)

project_dir <- normalizePath(file.path(dirname(script_path), ".."))

if (!dir.exists(file.path(project_dir, "rapport_rapide", "outputs"))) {
  dir.create(file.path(project_dir, "rapport_rapide", "outputs"), recursive = TRUE)
}

setwd(project_dir)

source("scripts/00-packages.R")
source("scripts/01-import.R")
source("scripts/02-data_engenering.R")
source("scripts/03-stat_desc.R")
source("scripts/06-logistic.R")
source("scripts/05-Analyse Discriminante.R")
source("scripts/07- Comparaison.R")

objects_to_save <- setdiff(
  ls(envir = globalenv(), all.names = TRUE),
  c("objects_to_save", "project_dir", "script_path")
)

saved_objects <- mget(objects_to_save, envir = globalenv(), inherits = FALSE)

saveRDS(
  saved_objects,
  file = file.path("rapport_rapide", "outputs", "rapport_final_objects.rds")
)

message(
  "Objets sauvegardes dans : ",
  file.path(project_dir, "rapport_rapide", "outputs", "rapport_final_objects.rds")
)
