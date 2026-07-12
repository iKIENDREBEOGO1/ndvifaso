#' Estimer le rendement par pixel à partir des métriques phénologiques
#'
#' Applique une équation linéaire (constante + somme de coefficients ×
#' métriques) au raster de métriques phénologiques pour produire un rendement
#' par pixel, sur l'ensemble du territoire (sans masquage).
#'
#' @param phen Un \code{SpatRaster} de métriques, issu de \code{ndvi_phenologie}
#'   (couches nommées Vav, Vmn, Vmx, Rrg, Rsd, Aup, Adn, Dmn, Dmx, Dup, Ddn).
#' @param constante La constante (intercept) de l'équation.
#' @param coefficients Vecteur nommé des coefficients, ex.
#'   \code{c(Vav = 1200, Vmx = 800)}. Les métriques non citées ont un
#'   coefficient de 0. Les noms doivent correspondre aux couches de \code{phen}.
#' @param min_zero Si TRUE (défaut), les rendements négatifs sont ramenés à 0
#'   (un rendement fourrager ne peut pas être négatif).
#'
#' @return Un \code{SpatRaster} à une couche : le rendement estimé (même unité
#'   que l'équation, typiquement kg/ha), sur tous les pixels.
#' @export
#'
#' @examples
#' \dontrun{
#' rendement <- ndvi_rendement(
#'   phen,
#'   constante    = 150,
#'   coefficients = c(Vav = 1800, Vmx = 600, Aup = 12)
#' )
#' terra::plot(rendement)
#' }
ndvi_rendement <- function(phen,
                           constante = 0,
                           coefficients = numeric(0),
                           min_zero = TRUE) {

  metriques_dispo <- names(phen)

  # Vérifier que les coefficients fournis correspondent à des couches existantes.
  if (length(coefficients) > 0) {
    inconnus <- setdiff(names(coefficients), metriques_dispo)
    if (length(inconnus) > 0) {
      stop("Coefficient(s) pour une metrique inconnue : ",
           paste(inconnus, collapse = ", "),
           ". Metriques disponibles : ", paste(metriques_dispo, collapse = ", "))
    }
  }

  # Rendement = constante, puis on ajoute coef * metrique pour chaque coefficient.
  rendement <- phen[[1]] * 0 + constante      # raster constant, meme grille
  for (m in names(coefficients)) {
    rendement <- rendement + coefficients[[m]] * phen[[m]]
  }

  if (min_zero) {
    rendement[rendement < 0] <- 0
  }

  names(rendement) <- "rendement"
  rendement
}
