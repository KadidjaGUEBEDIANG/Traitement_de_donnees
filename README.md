#COMMENT UTILISER LE PACKAGE

## Installation

```r
# Installer le package consoLaitierR depuis GitHub
remotes::install_github("KadidjaGUEBEDIANG/Traitement_de_donnees")
```

##  Exemple d'utilisation

```r
library(consoLaitierR)
library(haven)
library(readxl)
# Étape 1 : Importation des données
data <- Etape1_traitement_initial("C:/Users/LENOVO/Desktop/R_project/Data/Raw/boissons.dta")

# Étape 2 : Analyse des valeurs unitaires et sources
baseVUmenage <- Etape2_analyse_valeurs_unitaires_et_sources(data)

# Étape 3 : Conversion et standardisation
base_semi_apuree <- Etape3_conversion_standardisation(
  data,
  baseVUmenage,
  "C:/Users/LENOVO/Desktop/R_project/conversion.xlsx"
)

# Étape 4 : Nettoyage et validation
Ehcvm_all <- read_dta("C:/Users/LENOVO/Desktop/R_project/Ehcvm_all.dta")

Base_X1_Apuree <- Etape4_nettoyage_validation(
  base_semi_apuree,
  data,
  "C:/Users/LENOVO/Desktop/R_project",
  Ehcvm_all
)

# Étape 5 : Calcul des indicateurs finaux
fin <- Etape5_calcul_indicateurs(Base_X1_Apuree, data)

# Affichage des résultats
View(fin$indice_simpson)
View(fin$part_achat)
View(fin$part_autoconsommation)
View(fin$part_recue)
View(fin$diversite_sources)
```
