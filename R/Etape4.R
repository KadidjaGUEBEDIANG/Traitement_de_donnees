#' Nettoyage, détection d’outliers, validation et enrichissement des données
#'
#' Cette fonction réalise la détection des valeurs aberrantes, la validation par calories,
#' et l’ajout des informations géographiques et démographiques.
#'
#' @param Base_X1_SemiApurée Dataframe semi-apuré avec quantités et valeurs.
#' @param mon_path, Chemin du dossier contenant les fichiers de données annexes.
#' @param data_principal, Chemin du dossier contenant les fichiers de données annexes.
#' @param ehcvm_all, Chemin du dossier contenant les fichiers de données annexes
#' @return Dataframe nettoyée et enrichie prête pour analyse.
#' @import dplyr haven
#' @export


Etape4_nettoyage_validation <- function(
    Base_X1_SemiApurée,  # base issue des étapes précédentes
    data_principal,      # base initiale data_principal, pour les NA et variables source etc.
    mon_path,            # chemin vers dossier contenant fichiers externes
    ehcvm_all            # dataframe ehcvm pour enrichissement géographique
) {
  library(dplyr)
  library(tidyr)
  library(haven)  # pour read_dta

  # 4.1 Analyse valeurs manquantes
  cat("Valeurs manquantes dans Base_X1_SemiApurée :\n")
  print(sapply(Base_X1_SemiApurée[, c("QuantitéConsomméeKG", "source", "ValeurConsommée", "produit", "Unité", "Taille")], function(x) sum(is.na(x))))

  # NA groupes dans data_principal
  cat("Groupes de NA spécifiques dans data_principal :\n")
  cat("Groupe 1 NA = ", sum(is.na(data_principal$qte_dernier_achat) & is.na(data_principal$unite_dernier_achat) & is.na(data_principal$taille_dernier_achat) & is.na(data_principal$valeur_dernier_achat)), "\n")
  cat("Groupe 2 NA = ", sum(is.na(data_principal$qte_autoconsommee) ), "\n")
  cat("Groupe 3 NA = ", sum(is.na(data_principal$quantite_consommee) & is.na(data_principal$unite) & is.na(data_principal$taille) & is.na(data_principal$qte_recue) & is.na(data_principal$date_dernier_achat)), "\n")

  # 4.1 Détection d’outliers sur QuantitéConsomméeKG
  outliers_qte <- Base_X1_SemiApurée %>%
    group_by(produit, Unité, Taille) %>%
    summarise(
      Q1 = quantile(QuantitéConsomméeKG, 0.25, na.rm = TRUE),
      Q3 = quantile(QuantitéConsomméeKG, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      .groups = "drop"
    ) %>%
    mutate(
      borne_inf = Q1 - 1.5 * IQR,
      borne_sup = Q3 + 1.5 * IQR
    ) %>%
    left_join(Base_X1_SemiApurée, by = c("produit", "Unité", "Taille")) %>%
    mutate(
      est_outlier_inf = QuantitéConsomméeKG < borne_inf,
      est_outlier_sup = QuantitéConsomméeKG > borne_sup
    ) %>%
    group_by(produit, Unité, Taille) %>%
    summarise(
      nb_outliers_inf = sum(est_outlier_inf, na.rm = TRUE),
      nb_outliers_sup = sum(est_outlier_sup, na.rm = TRUE),
      total = n(),
      prop_outliers = round((nb_outliers_inf + nb_outliers_sup) / total, 3),
      .groups = "drop"
    ) %>%
    arrange(desc(nb_outliers_inf + nb_outliers_sup))

  # 4.1 Détection d’outliers sur ValeurConsommée
  outliers_val <- Base_X1_SemiApurée %>%
    group_by(produit, Unité, Taille) %>%
    summarise(
      Q1 = quantile(ValeurConsommée, 0.25, na.rm = TRUE),
      Q3 = quantile(ValeurConsommée, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      .groups = "drop"
    ) %>%
    mutate(
      borne_inf = Q1 - 1.5 * IQR,
      borne_sup = Q3 + 1.5 * IQR
    ) %>%
    left_join(Base_X1_SemiApurée, by = c("produit", "Unité", "Taille")) %>%
    mutate(
      est_outlier_inf = ValeurConsommée < borne_inf,
      est_outlier_sup = ValeurConsommée > borne_sup
    ) %>%
    group_by(produit, Unité, Taille) %>%
    summarise(
      nb_outliers_inf = sum(est_outlier_inf, na.rm = TRUE),
      nb_outliers_sup = sum(est_outlier_sup, na.rm = TRUE),
      total = n(),
      prop_outliers = round((nb_outliers_inf + nb_outliers_sup) / total, 3),
      .groups = "drop"
    ) %>%
    arrange(desc(nb_outliers_inf + nb_outliers_sup))

  # 4.2 Validation par les Kilocalories
  calorie_conversion <- read_dta(file.path(mon_path, "calorie_conversion_tgo2021.dta")) %>%
    select(produit = codpr, cal)

  Base_X1_SemiApurée <- Base_X1_SemiApurée %>%
    left_join(calorie_conversion, by = "produit") %>%
    mutate(Kcalories = QuantitéConsomméeKG * cal / 100) %>%
    select(-cal)

  # Taille du ménage
  membres <- read_dta(file.path(mon_path, "S00_S01_membres.dta"))
  names(membres)[names(membres) == "interview__id"] <- "IDs"
  membres <- membres %>%
    group_by(IDs) %>%
    summarise(Taille_Menage = n(), .groups = "drop")

  Base_X1_SemiApurée <- Base_X1_SemiApurée %>%
    left_join(membres %>% select(IDs, Taille_Menage), by = "IDs") %>%
    group_by(menage) %>%
    mutate(Kcal_global_men = sum(Kcalories, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(kilopartête = Kcal_global_men / Taille_Menage)

  # 4.3 Enrichissement géographique avec ehcvm_all
  adresse <- ehcvm_all %>%
    select(
      IDs = interview__id,
      département = s00q02,
      région = s00q01,
      milieu = s00q04
    )

  fusion_base <- Base_X1_SemiApurée %>%
    left_join(adresse, by = "IDs")

  Base_X1_Apurée <- fusion_base %>%
    select(IDs, département, région, milieu, everything())

  # Retourner liste avec résultats (la base finale et les tables outliers pour analyse)
  return(
   Base_X1_Apurée

  )
}
