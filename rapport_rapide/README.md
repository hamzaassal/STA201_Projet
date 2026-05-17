# Rapport rapide

Ce dossier contient une version rapide du rapport final, sans modification des
scripts originaux du projet.

## 1. Construire le cache une seule fois

Depuis la racine du projet :

```r
source("rapport_rapide/build_outputs.R")
```

Cette commande execute les scripts existants du projet et sauvegarde les objets
utiles dans :

```text
rapport_rapide/outputs/rapport_final_objects.rds
```

## 2. Rendre le rapport rapide

Une fois le cache construit, rendre :

```text
rapport_rapide/Rapport_Final_rapide.qmd
```

Depuis un terminal, la commande Quarto equivalente est :

```bash
quarto render rapport_rapide/Rapport_Final_rapide.qmd --to pdf
```

Ce rapport recharge les objets sauvegardes au lieu de relancer les modeles,
les grid search et les scripts lourds.

## 3. Quand reconstruire le cache ?

Reconstruire le cache uniquement si les donnees, les scripts de modelisation ou
les objets du rapport ont change.
