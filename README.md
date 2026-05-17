# Projet STA201 - Analyse multivariee approfondie

Ce depot contient le travail de projet STA201 portant sur la prediction des annulations de reservations hotelieres. L'analyse mobilise deux approches principales : la regression logistique et l'analyse discriminante sur variables qualitatives, construite autour d'une ACM puis de modeles LDA/QDA.

## Architecture du projet

```text
.
|-- data/
|   |-- hotel_bookings.csv
|   `-- Consignes_STA201_2025_26.pdf
|-- assets/
|   `-- logo-cnam.png
|-- scripts/
|   |-- 00-packages.R
|   |-- 01-import.R
|   |-- 02-data_engenering.R
|   |-- 03-stat_desc.R
|   |-- 04-data_preparation.R
|   |-- 05-Analyse Discriminante.R
|   |-- 06-logistic.R
|   |-- 07- Comparaison.R
|   `-- setup-renv.R
|-- Rapport_Final.qmd
|-- Rapport_Final.pdf
|-- renv.lock
|-- .Rprofile
`-- STA201_Projet.Rproj
```

## Role des principaux fichiers

- `data/` contient la base de donnees et les consignes du projet.
- `assets/` contient les ressources visuelles utilisees dans le rapport, notamment le logo du Cnam.
- `Rapport_Final.qmd` est le rapport Quarto principal.
- `Rapport_Final.pdf` est la version PDF generee du rapport.
- `renv.lock` fige les versions des packages R necessaires au projet.
- `.Rprofile` active automatiquement l'environnement `renv` a l'ouverture du projet.

## Organisation des scripts

Les scripts sont numerotes afin d'indiquer leur ordre logique d'execution.

1. `00-packages.R` charge les packages du projet. Si des packages manquent, il invite a utiliser `renv::restore()`.
2. `01-import.R` importe la base de donnees et le dictionnaire des variables.
3. `02-data_engenering.R` realise le nettoyage, construit les variables d'analyse et prepare les tableaux recapitulatif du rapport.
4. `03-stat_desc.R` produit les analyses descriptives et les graphiques exploratoires.
5. `04-data_preparation.R` construit la base de modelisation et effectue la subdivision stratifiee en apprentissage, validation et test.
6. `05-Analyse Discriminante.R` construit l'approche ACM + LDA/QDA et compare les modeles sur validation.
7. `06-logistic.R` construit les modeles de regression logistique, selectionne les variables et choisit les seuils sur validation.
8. `07- Comparaison.R` centralise les tests, les evaluations finales sur l'echantillon test et la comparaison entre methodes.

## Cloner le projet

Assurez-vous d'avoir [Git](https://git-scm.com/) installe, puis executez :

\`\`\`bash
git clone https://github.com/hamzaassal/STA201_Projet.git
cd STA201_Projet
\`\`\`

Ouvrez ensuite le fichier `STA201_Projet.Rproj` dans RStudio pour charger l'environnement du projet.

## Environnement R reproductible

Le projet utilise `renv` afin de faciliter le partage et la reproduction des resultats.

Sur un nouvel ordinateur, ouvrir le projet puis executer :

\`\`\`r
install.packages("renv")
renv::restore()
\`\`\`

Cette commande restaure les versions de packages enregistrees dans `renv.lock`.

Les outils systeme suivants restent necessaires en dehors de `renv` :

- R ;
- RStudio ou Quarto ;
- MiKTeX ou une distribution LaTeX compatible pour generer le PDF.

## Generation du rapport

Le rapport principal est `Rapport_Final.qmd`. Il peut etre rendu depuis RStudio avec le bouton Render, ou en ligne de commande :

\`\`\`bash
quarto render Rapport_Final.qmd --to pdf
\`\`\`

Le rendu PDF s'appuie sur les scripts du dossier `scripts/`, sur la base `data/hotel_bookings.csv` et sur le logo situe dans `assets/`.

## Logique de modelisation

La base est divisee en trois echantillons de facon stratifiee selon la variable cible :

- `train` : estimation des modeles ;
- `validation` : choix des seuils, des variables et des variantes de modeles ;
- `test` : evaluation finale uniquement.

Cette organisation limite le risque de fuite d'information et permet de comparer les performances finales des methodes sur un echantillon non utilise pendant la construction des modeles.
