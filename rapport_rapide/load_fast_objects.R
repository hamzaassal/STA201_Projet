# ============================================================
# CHARGEMENT DES OBJETS DU RAPPORT RAPIDE
# ============================================================

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile),
  error = function(e) normalizePath("rapport_rapide/load_fast_objects.R")
)

project_dir <- normalizePath(file.path(dirname(script_path), ".."))

cache_file <- file.path(
  project_dir,
  "rapport_rapide",
  "outputs",
  "rapport_final_objects.rds"
)

if (!file.exists(cache_file)) {
  stop(
    "Le cache du rapport rapide n'existe pas encore.\n",
    "Executer d'abord : source('rapport_rapide/build_outputs.R')",
    call. = FALSE
  )
}

cached_objects <- readRDS(cache_file)
list2env(cached_objects, envir = parent.frame())

rm(cached_objects, cache_file, project_dir, script_path)
