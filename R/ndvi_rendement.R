#' Estimer le rendement par pixel à partir des métriques phénologiques
#'
#' Applique une équation linéaire (constante + somme de coefficients ×
#' métriques) au raster de métriques phénologiques pour produire un rendement
#' par pixel, sur l'ensemble du territoire (sans masquage). Chaque métrique peut
#' être utilisée brute ou transformée en logarithme, et l'équation peut prédire
#' directement le rendement ou son logarithme.
#'
#' @param phen Un \code{SpatRaster} de métriques, issu de \code{ndvi_phenologie}
#'   (couches nommées Vav, Vmn, Vmx, Rrg, Rsd, Aup, Adn, Dmn, Dmx, Dup, Ddn,
#'   iNDVI, iNDVI_seuil, Duree).
#' @param constante La constante (intercept) de l'équation.
#' @param coefficients Vecteur nommé des coefficients, ex.
#'   \code{c(Vav = 1.25, iNDVI_seuil = 0.83)}. Les métriques non citées ont un
#'   coefficient de 0. Les noms doivent correspondre aux couches de \code{phen}.
#' @param log_vars Noms des métriques à passer au logarithme avant application
#'   du coefficient, ex. \code{c("Vav", "iNDVI_seuil")}. Les pixels dont la
#'   valeur est négative ou nulle deviennent NA (log impossible).
#' @param reponse Échelle de l'équation : \code{"brut"} si elle prédit
#'   directement le rendement (kg/ha), \code{"log"} si elle prédit son
#'   logarithme (le résultat est alors exponentié).
#' @param facteur_duan Facteur de correction du biais de rétro-transformation,
#'   utilisé uniquement si \code{reponse = "log"}. Vaut \code{mean(exp(residus))}
#'   du modèle calé. Sans lui, le rendement est systématiquement sous-estimé.
#' @param min_zero Si TRUE (défaut), les rendements négatifs sont ramenés à 0
#'   (un rendement fourrager ne peut pas être négatif).
#'
#' @return Un \code{SpatRaster} à une couche : le rendement estimé (kg/ha), sur
#'   tous les pixels.
#' @export
#'
#' @examples
#' \dontrun{
#' # Equation lineaire simple
#' rendement <- ndvi_rendement(
#'   phen,
#'   constante    = 150,
#'   coefficients = c(Vav = 1800, Vmx = 600, Aup = 12)
#' )
#'
#' # Equation log-log : log(BT) = a + b*log(Vav) + c*log(iNDVI_seuil)
#' rendement <- ndvi_rendement(
#'   phen,
#'   constante    = 8.42,
#'   coefficients = c(Vav = 1.25, iNDVI_seuil = 0.83),
#'   log_vars     = c("Vav", "iNDVI_seuil"),
#'   reponse      = "log",
#'   facteur_duan = 1.09
#' )
#' terra::plot(rendement)
#' }
ndvi_rendement <- function(phen,
                           constante = 0,
                           coefficients = numeric(0),
                           log_vars = character(0),
                           reponse = c("brut", "log"),
                           facteur_duan = 1,
                           min_zero = TRUE) {

  reponse <- match.arg(reponse)
  metriques_dispo <- names(phen)

  # Verifier que les coefficients fournis correspondent a des couches existantes.
  if (length(coefficients) > 0) {
    inconnus <- setdiff(names(coefficients), metriques_dispo)
    if (length(inconnus) > 0) {
      stop("Coefficient(s) pour une metrique inconnue : ",
           paste(inconnus, collapse = ", "),
           ". Metriques disponibles : ", paste(metriques_dispo, collapse = ", "))
    }
  }

  # Verifier que les variables a logger ont bien un coefficient.
  inutiles <- setdiff(log_vars, names(coefficients))
  if (length(inutiles) > 0) {
    warning("log_vars sans coefficient (ignore) : ",
            paste(inutiles, collapse = ", "))
  }

  # Partie lineaire : constante, puis coef * metrique (brute ou loggee).
  lin <- phen[[1]] * 0 + constante      # raster constant, meme grille
  for (m in names(coefficients)) {
    x <- phen[[m]]
    if (m %in% log_vars) {
      x[x <= 0] <- NA                   # log impossible sur valeurs <= 0
      x <- log(x)
    }
    lin <- lin + coefficients[[m]] * x
  }

  # Retour a l'echelle kg/ha si l'equation predit un logarithme.
  # Le facteur de Duan corrige le biais de retro-transformation.
  rendement <- if (reponse == "log") exp(lin) * facteur_duan else lin

  if (min_zero) {
    rendement[rendement < 0] <- 0
  }

  names(rendement) <- "rendement"
  rendement
}
