# Initialise et met a jour l'environnement reproductible du projet.
# A executer une fois avec :
# source("scripts/setup-renv.R")

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

if (!file.exists("renv.lock")) {
  renv::init(project = ".", bare = TRUE)
}

project_packages <- c(
  "tidyverse",
  "skimr",
  "janitor",
  "scales",
  "forcats",
  "ggplot2",
  "corrplot",
  "kableExtra",
  "rsample",
  "patchwork",
  "FactoMineR",
  "factoextra",
  "knitr",
  "countrycode",
  "broom",
  "caret",
  "pROC",
  "MASS",
  "class",
  "biotools",
  "pscl",
  "dplyr",
  "purrr",
  "tibble",
  "tidyr",
  "stringr",
  "ggrepel",
  "glmnet",
  "MVN",
  "reshape2",
  "rmarkdown",
  "rstatix",
  "yaml"
)

renv::hydrate(packages = unique(project_packages), prompt = FALSE)
renv::snapshot(project = ".", prompt = FALSE)
