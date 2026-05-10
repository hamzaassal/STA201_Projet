# ============================================================
# PACKAGES DU PROJET
# ============================================================

# Les versions des packages sont figees dans renv.lock.
# Si un package manque sur un autre ordinateur, executer :
# install.packages("renv")
# renv::restore()

packages <- c(
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
  "pscl"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Packages manquants : ",
    paste(missing_packages, collapse = ", "),
    "\nExecuter install.packages('renv') puis renv::restore().",
    call. = FALSE
  )
}

invisible(lapply(packages, library, character.only = TRUE))
