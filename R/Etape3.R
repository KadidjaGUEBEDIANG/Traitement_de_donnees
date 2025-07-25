

#' Conversion des quantités en unités standard (kg) avec gestion des poids manquants
#'
#' Cette fonction fait la jointure avec la table de conversion, ajuste les poids manquants,
#' et calcule les quantités standardisées en kilogrammes.
#'
#' @param data Dataframe contenant les données laitières.
#' @param baseVUmenage Dataframe avec valeurs unitaires consolidées.
#' @param chemin_conversion Chemin vers le fichier Excel de conversion.
#' @return Dataframe fusionnée avec quantités standardisées.
#' @import dplyr readxl
#' @export




Etape3_conversion_standardisation <- function(
    data,          # dataframe principal avec données data
    baseVUmenage,     # dataframe avec valeurs unitaires par ménage/produit/unité/taille
    chemin_conversion # chemin vers le fichier conversion.xlsx
) {
  library(dplyr)
  library(readxl)

  # Charger la table de conversion
  df_conv <- read_excel(chemin_conversion)

  # Merge avec la table conversion
  df_merge <- data %>%
    left_join(df_conv, by = c("produit", "unite", "taille"))

  # Nettoyage et transformation de la variable poids
  df_merge$poids <- as.numeric(sub(",", ".", df_merge$poids))

  # Correction manuelle des poids manquants spécifiques
  df_merge$poids[df_merge$produit == 174 & df_merge$taille == 2 & df_merge$unite == 142] <- 3400
  df_merge$poids[df_merge$produit == 174 & df_merge$taille == 0 & df_merge$unite == 115] <- 1200

  # Calcul des quantités standardisées (en grammes puis en kilogrammes)
  df_merge <- df_merge %>%
    mutate(
      quantite_standard_g = quantite_consommee * poids,
      qte_dernier_achat_standard = qte_dernier_achat * poids,
      quantite_standard_g = quantite_standard_g / 1000
    )

  # Sélection des colonnes utiles
  df_merge <- df_merge %>%
    select(
      menage,
      produit,
      Unité = unite,
      Taille = taille,
      QuantitéConsomméeKG = quantite_standard_g
    )

  # Jointure avec la base des valeurs unitaires
  Base_X1_SemiApurée <- baseVUmenage %>%
    left_join(df_merge, by = c("menage", "produit", "Unité", "Taille"))

  # Ajout des IDs interview__id
  Base_X1_SemiApurée$IDs <- data$interview__id

  # Réorganisation des colonnes
  Base_X1_SemiApurée <- Base_X1_SemiApurée %>%
    select(IDs, everything())

  # Message sur les valeurs manquantes dans QuantitéConsomméeKG
  cat("Nombre de valeurs manquantes dans QuantitéConsomméeKG : ", sum(is.na(Base_X1_SemiApurée$QuantitéConsomméeKG)), "\n")

  return(Base_X1_SemiApurée)
}

