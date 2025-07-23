#' Calcul des indicateurs de consommation laitière
#'
#' Cette fonction calcule plusieurs indicateurs descriptifs à partir des bases fournies.
#'
#' @param Base_X1_Apurée Dataframe contenant les données semi-apurées.
#' @param data Dataframe source contenant les données brutes.
#' @return Liste de dataframes avec les indicateurs.
#' @import dplyr
#' @export



Etape5_calcul_indicateurs <- function(Base_X1_Apurée, data) {
  library(dplyr)

  # Indice de diversité alimentaire (indice de Simpson)
  indice_simpson <- Base_X1_Apurée %>%
    filter(!is.na(produit)) %>%
    group_by(menage, produit) %>%
    summarise(n_i = n(), .groups = "drop") %>%
    group_by(menage) %>%
    summarise(
      N = sum(n_i),
      D = 1 - sum((n_i / N)^2),
      .groups = "drop"
    )
  data$qte_achat_conso<- data$quantite_consommee-(data$qte_recue+data$qte_autoconsommee)

  # Part de produits consommés achetés
  part_achat <- data %>%
    filter(!is.na(qte_achat_conso), !is.na(quantite_consommee)) %>%
    group_by(menage) %>%
    summarise(
      qte_achat_totale = sum(qte_achat_conso, na.rm = TRUE),
      qte_conso_totale = sum(quantite_consommee, na.rm = TRUE),
      part_achat = round(qte_achat_totale / qte_conso_totale, 3),
      .groups = "drop"
    )

  # Part d'autoconsommation
  part_autoconsommation <- data %>%
    filter(!is.na(qte_autoconsommee), !is.na(quantite_consommee)) %>%
    group_by(menage) %>%
    summarise(
      qte_auto_totale = sum(qte_autoconsommee, na.rm = TRUE),
      qte_conso_totale = sum(quantite_consommee, na.rm = TRUE),
      part_autoconso = round(qte_auto_totale / qte_conso_totale, 3),
      .groups = "drop"
    )

  # Part de produits reçus
  part_recue <- data %>%
    filter(!is.na(qte_recue), !is.na(quantite_consommee)) %>%
    group_by(menage) %>%
    summarise(
      qte_recue_totale = sum(qte_recue, na.rm = TRUE),
      qte_conso_totale = sum(quantite_consommee, na.rm = TRUE),
      part_recue = round(qte_recue_totale / qte_conso_totale, 3),
      .groups = "drop"
    )

  # Indice de diversité des sources d’approvisionnement
  diversite_sources <- data %>%
    filter(!is.na(quantite_consommee)) %>%
    group_by(menage) %>%
    summarise(
      qte_achat = sum(qte_achat_conso, na.rm = TRUE),
      qte_auto  = sum(qte_autoconsommee, na.rm = TRUE),
      qte_recue = sum(qte_recue, na.rm = TRUE),
      qte_totale = sum(quantite_consommee, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      part_achat = qte_achat / qte_totale,
      part_auto  = qte_auto / qte_totale,
      part_recue = qte_recue / qte_totale,
      indice_diversite = round(1 - (part_achat^2 + part_auto^2 + part_recue^2), 3)
    )

  # Retourner les indicateurs dans une liste
  return(list(
    indice_simpson = indice_simpson,
    part_achat = part_achat,
    part_autoconsommation = part_autoconsommation,
    part_recue = part_recue,
    diversite_sources = diversite_sources
  ))
}
