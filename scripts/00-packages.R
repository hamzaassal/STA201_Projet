# Liste des packages nécessaires
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
  "pROC"
  
)

# Installation automatique des packages manquants
for(pkg in packages) {
  
  if(!require(pkg, character.only = TRUE)) {
    
    install.packages(pkg)
    
    library(pkg, character.only = TRUE)
  }
}