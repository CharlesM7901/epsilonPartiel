#' Calcul des tailles d'effet partielles (η², ω², ε²)
#'
#' @description
#' Calcule les trois tailles d'effet partielles à partir des valeurs
#' F, df_effect et df_error issues d'une ANOVA factorielle ou d'effets simples.
#'
#' Formules :
#' η²partiel = (F * df_effect) / (F * df_effect + df_error)
#' ω²partiel = (F * df_effect - df_effect) / (F * df_effect + df_error + 1)
#' ε²partiel = (F * df_effect - df_effect) / (F * df_effect + df_error)
#'
#' @param F_val Valeur F obtenue dans l'output de l'ANOVA (F.ratio)
#' @param df_effect Degrés de liberté de l'effet (df1)
#' @param df_error Degrés de liberté de l'erreur (df2)
#'
#' @return Un data.frame contenant les trois tailles d'effet partielles
#'
#' @examples
#' tailles_effet(F_val = 2.41, df_effect = 3, df_error = 318)
#'
#' @export
tailles_effet <- function(F_val, df_effect, df_error) {
  
  # --- VÉRIFICATIONS ---
  # Vérifie que la valeur F est positive (un F ne peut pas être négatif)
  if (F_val <= 0) stop("F_val doit être positif")
  
  # Vérifie que les degrés de liberté sont positifs
  if (df_effect <= 0 || df_error <= 0) stop("Les degrés de liberté doivent être positifs")
  
  
  # --- CALCULS ---
  
  # Calcul du eta carré partiel
  # Formule : (F * df_effect) / (F * df_effect + df_error)
  eta_sq <- (F_val * df_effect) / (F_val * df_effect + df_error)
  
  # Calcul du omega carré partiel
  # Formule : (F * df_effect - df_effect) / (F * df_effect + df_error + 1)
  omega_sq <- (F_val * df_effect - df_effect) / (F_val * df_effect + df_error + 1)
  
  # Calcul du epsilon carré partiel
  # Formule : (F * df_effect - df_effect) / (F * df_effect + df_error)
  epsilon_sq <- (F_val * df_effect - df_effect) / (F_val * df_effect + df_error)
  
  
  # --- INTERPRÉTATION ---
  
  # Fonction interne qui interprète la taille d'effet selon les seuils de Cohen (1988)
  # < .01 = petit, < .09 = petit, < .25 = moyen, >= .25 = grand
  interpreter <- function(x) {
    if (x < 0.01) "Petit effet"
    else if (x < 0.09) "Petit effet"
    else if (x < 0.25) "Moyen effet"
    else "Grand effet"
  }
  
  
  # --- OUTPUT ---
  
  # Affiche un titre pour l'output
  cat("=== Tailles d'effet partielles ===\n")
  
  # Affiche eta carré partiel arrondi à 4 décimales avec son interprétation
  cat("η²partiel  =", round(eta_sq, 4),     "|", interpreter(eta_sq), "\n")
  
  # Affiche omega carré partiel arrondi à 4 décimales avec son interprétation
  cat("ω²partiel  =", round(omega_sq, 4),   "|", interpreter(omega_sq), "\n")
  
  # Affiche epsilon carré partiel arrondi à 4 décimales avec son interprétation
  cat("ε²partiel  =", round(epsilon_sq, 4), "|", interpreter(epsilon_sq), "\n")
  
  
  # --- RETOUR ---
  
  # Crée un data.frame avec les 3 résultats pour pouvoir les réutiliser si besoin
  resultats <- data.frame(
    eta_sq_partiel     = eta_sq,     # Colonne 1 : eta carré partiel
    omega_sq_partiel   = omega_sq,   # Colonne 2 : omega carré partiel
    epsilon_sq_partiel = epsilon_sq  # Colonne 3 : epsilon carré partiel
  )
  
  # Retourne le data.frame de façon invisible (pas affiché automatiquement, mais accessible si l'utilisateur assigne le résultat à une variable)
  return(invisible(resultats))
}