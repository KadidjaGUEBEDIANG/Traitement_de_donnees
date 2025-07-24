#' Traitement initial de la base laitier
#'
#' Cette fonction lit une base .dta, renomme, labellise, vérifie les identifiants et détecte les incohérences
#'
#' @param path Chemin vers le fichier .dta (base produit laitier)
#' @return Un dataframe nettoyé et enrichi avec les colonnes d'incohérences
#' @export
Etape1_traitement_initial <- function(path) {
  library(dplyr)
  library(haven)
  library(labelled)

  # 1. Chargement
  data_principal <- read_dta(path)
  data<- read_dta(path)

  # 2. Vérification des identifiants
  cat("Nombre d'observations :", nrow(data), "\n")
  cat("Ménages uniques (Interview_key) :", n_distinct(data$!!sym(names(data)[1])), "\n")
  cat("Duplications :", sum(duplicated(data$!!sym(names(data)[1]))), "\n")
  cat("Manquants :", sum(is.na(data$!!sym(names(data)[1]))), "\n")

  # 3. Renommage
  data <- data %>%
    rename(
      menage = !!sym(names(data)[2]),
      produit = !!sym(names(data)[3]),
      quantite_consommee = !!sym(names(data)[4]),
      unite = !!sym(names(data)[5]),
      taille = !!sym(names(data)[6]),
      qte_autoconsommee = !!sym(names(data)[7]),
      qte_recue = !!sym(names(data)[8]),
      date_dernier_achat = !!sym(names(data)[9]),
      qte_dernier_achat = !!sym(names(data)[10]),
      unite_dernier_achat = !!sym(names(data)[11]),
      taille_dernier_achat = !!sym(names(data)[12]),
      valeur_dernier_achat = !!sym(names(data)[13])
    )



  # 5. Initialisation colonnes incohérences
  data$incoherence <- "Non"
  data$incoherence_details <- ""

  # 6. Détection incohérences
  data <- data %>%
    mutate(
      autocons_calc = coalesce(qte_autoconsommee, 0),
      autre_source_calc = coalesce(qte_recue, 0),
      err_r1 = quantite_consommee > 0 & (is.na(unite) | is.na(taille)),
      err_r2 = (autocons_calc + autre_source_calc) > quantite_consommee,
      err_r4 = !is.na(qte_dernier_achat) & qte_dernier_achat > 0 &
        (is.na(unite_dernier_achat) | is.na(taille_dernier_achat) | is.na(valeur_dernier_achat)),
      incoherence = case_when(
        err_r1 | err_r2 | err_r4 ~ "Oui",
        TRUE ~ incoherence
      ),
      incoherence_details = paste0(
        incoherence_details,
        if_else(err_r1, "R1: unité/taille manquante; ", ""),
        if_else(err_r2, "R2: auto+dons > conso; ", ""),
        if_else(err_r4, "R4: détails achat manquants; ", "")
      )
    ) %>%
    select(-autocons_calc, -autre_source_calc, -starts_with("err_r"))

  cat("Nombre total d'incohérences détectées :", sum(data$incoherence == "Oui"), "\n")

  return(data)
}
