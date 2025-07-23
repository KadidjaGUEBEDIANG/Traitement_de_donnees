#' Analyse des valeurs unitaires et classification par source
#'
#' À partir d'un jeu de données contenant la variable VU (valeur unitaire), calcule les statistiques
#' descriptives par produit/unité/taille, choisit une valeur unitaire représentative (mode ou médiane),
#' calcule la valeur consommée par ménage, et classe la source de consommation.
#'
#' @param data Dataframe contenant au moins : produit, unite_dernier_achat, taille_dernier_achat, VU,
#'             quantite_consommee, qte_autoconsommee, qte_recue
#' @return Un dataframe enrichi avec les colonnes QuantitéConsommée, ValeurConsommée, source
#' @import dplyr gt
#' @export
Etape2_analyse_valeurs_unitaires_et_sources <- function(data) {
  library(dplyr)
  library(gt)

  # Fonction mode simple
  mode_stat <- function(x) {
    ux <- na.omit(unique(x))
    ux[which.max(tabulate(match(x, ux)))]
  }

  # 1. Convertir en numérique pour groupe
  data <- data %>%
    mutate(
      produit = as.numeric(produit),
      unite_dernier_achat = as.numeric(unite_dernier_achat),
      taille_dernier_achat = as.numeric(taille_dernier_achat)
    )
  data$VU<- data$valeur_dernier_achat/data$qte_dernier_achat

  # 2. Statistiques descriptives par produit, unité, taille
  VU_par_format <- data %>%
    filter(!is.na(VU)) %>%
    group_by(produit, unite_dernier_achat, taille_dernier_achat) %>%
    summarise(
      moyenne = mean(VU, na.rm = TRUE),
      mediane = median(VU, na.rm = TRUE),
      min = min(VU, na.rm = TRUE),
      max = max(VU, na.rm = TRUE),
      ecart_type = sd(VU, na.rm = TRUE),
      mode = mode_stat(VU),
      n = n(),
      .groups = "drop"
    )

  # 3. Affichage tableau résumé (facultatif)
  VU_par_format %>%
    arrange(desc(n)) %>%
    gt() %>%
    tab_header(
      title = "Valeurs Unitaires par Format de Produit",
      subtitle = "Statistiques descriptives : moyenne, médiane, mode, etc."
    ) %>%
    fmt_number(columns = where(is.numeric), decimals = 2) %>%
    print()

  # 4. Choix valeur unitaire représentative (mode si fiable sinon médiane)
  baseVU <- VU_par_format %>%
    mutate(
      VU = ifelse(
        !is.na(mode) &
          n >= 10 &
          abs(mode - mediane) / mediane < 0.3 &
          ecart_type / mediane < 0.3,
        mode,
        mediane
      ),
      source_VU = ifelse(
        !is.na(mode) &
          n >= 10 &
          abs(mode - mediane) / mediane < 0.3 &
          ecart_type / mediane < 0.3,
        "mode", "mediane"
      )
    ) %>%
    select(produit, unite = unite_dernier_achat, taille = taille_dernier_achat, VU, source_VU)

  # 5. Joindre valeur unitaire choisie à la base initiale
  baseVU_clean <- baseVU %>% select(-source_VU)

  baseVUmenage <- left_join(data, baseVU_clean, by = c("produit", "unite", "taille"))

  # 6. Calcul valeur consommée par ménage
  baseVUmenage <- baseVUmenage %>%
    rename(VU = VU.y) %>%
    mutate(
      QuantitéConsommée = quantite_consommee,
      ValeurConsommée = QuantitéConsommée * VU
    ) %>%
    select(
      menage,
      produit,
      Unité = unite,
      Taille = taille,
      QuantitéConsommée,
      ValeurConsommée
    )

  # 7. Calcul quantité d'achat consommée
  data <- data %>%
    mutate(qte_achat_conso = quantite_consommee - (qte_recue + qte_autoconsommee))

  # 8. Attribution source consommation
  data <- data %>%
    mutate(
      source = case_when(
        qte_autoconsommee == 0 & qte_recue == 0 & qte_achat_conso != 0 ~ "Achat",
        qte_autoconsommee == 0 & qte_recue != 0 & qte_achat_conso == 0 ~ "Don",
        qte_autoconsommee != 0 & qte_recue == 0 & qte_achat_conso == 0 ~ "Autoconsommation",
        qte_autoconsommee != 0 & qte_recue != 0 & qte_achat_conso == 0 ~ "Auto + Don",
        qte_autoconsommee == 0 & qte_recue != 0 & qte_achat_conso != 0 ~ "Achat + Don",
        qte_autoconsommee != 0 & qte_recue == 0 & qte_achat_conso != 0 ~ "Achat + Auto",
        qte_autoconsommee != 0 & qte_recue != 0 & qte_achat_conso != 0 ~ "Mixte (tous)",
        qte_autoconsommee == 0 & qte_recue == 0 & qte_achat_conso == 0 ~ "Aucune",
        TRUE ~ NA_character_
      )
    )

  # 9. Fusion finale avec source dans la table principale
  baseVUmenage1 <- data %>%
    select(
      menage,
      produit,
      Unité = unite,
      Taille = taille,
      source
    )

  baseVUmenage <- left_join(baseVUmenage, baseVUmenage1,
                           by = c("menage", "produit", "Unité", "Taille")
  )

  return(baseVUmenage)
}
