#' Calculer les métriques phénologiques à partir d'un cube NDVI saisonnier
#'
#' À partir d'un empilement de décades NDVI (SpatRaster multi-couches, ordonné
#' chronologiquement), calcule pour chaque pixel les métriques phénologiques :
#' moyennes/extrema, amplitudes, angles de croissance/décroissance, dates
#' relatives (numéro de décade) et indicateurs cumulés de la saison.
#'
#' @param cube Un \code{SpatRaster} dont chaque couche est une décade NDVI réel,
#'   dans l'ordre chronologique.
#' @param dekads Vecteur des numéros de décade (1 à 36) correspondant aux
#'   couches, dans le même ordre. Par défaut 10:29 (avril D1 -> octobre D2).
#' @param echelle_angle Convention pour Aup/Adn : "degres" (angles -90 à +90)
#'   ou "byte" (encodage FEWS NET 0 à 180, soit angle + 90).
#' @param seuil_sol Seuil de NDVI en dessous duquel le couvert est considéré
#'   comme sol nu, utilisé pour \code{iNDVI_seuil}. Par défaut 0.15.
#' @param seuil_vert Seuil de NDVI au-dessus duquel une décade est comptée comme
#'   "verte", utilisé pour \code{Duree}. Par défaut 0.2.
#'
#' @return Un \code{SpatRaster} à 14 couches, une par métrique : Vav, Vmn, Vmx,
#'   Rrg, Rsd, Aup, Adn, Dmn, Dmx, Dup, Ddn, iNDVI, iNDVI_seuil, Duree.
#' @export
#'
#' @examples
#' \dontrun{
#' files <- list.files("data/ndvi_bf", pattern = "\\.tif$", full.names = TRUE)
#' cube  <- terra::rast(files)
#' phen  <- ndvi_phenologie(cube)
#' terra::plot(phen[["Vav"]])
#' terra::plot(phen[["iNDVI_seuil"]])
#' }
ndvi_phenologie <- function(cube,
                            dekads = 10:29,
                            echelle_angle = c("degres", "byte"),
                            seuil_sol = 0.15,
                            seuil_vert = 0.2) {

  echelle_angle <- match.arg(echelle_angle)
  n <- terra::nlyr(cube)
  if (length(dekads) != n) {
    stop("Le nombre de decades (", length(dekads),
         ") ne correspond pas au nombre de couches du cube (", n, ").")
  }

  noms <- c("Vav", "Vmn", "Vmx", "Rrg", "Rsd",
            "Aup", "Adn", "Dmn", "Dmx", "Dup", "Ddn",
            "iNDVI", "iNDVI_seuil", "Duree")
  nm <- length(noms)

  # Fonction appliquee a la serie temporelle (v) de CHAQUE pixel.
  calc_metriques <- function(v) {
    # Pixel entierement vide -> tout NA.
    if (all(is.na(v))) return(rep(NA_real_, nm))

    vav <- mean(v, na.rm = TRUE)
    vmn <- min(v,  na.rm = TRUE)
    vmx <- max(v,  na.rm = TRUE)
    rrg <- vmx - vmn
    rsd <- stats::sd(v, na.rm = TRUE)

    # Pentes entre decades consecutives (Delta temps = 1 decade).
    d <- diff(v)

    # Angles de montee/descente les plus forts, en degres (atan(pente)).
    if (all(is.na(d))) {
      aup <- NA_real_; adn <- NA_real_
      i_up <- NA_integer_; i_dn <- NA_integer_
    } else {
      angles <- atan(d) * 180 / pi
      i_up <- which.max(angles)          # indice de la montee la plus forte
      i_dn <- which.min(angles)          # indice de la descente la plus forte
      aup <- angles[i_up]
      adn <- angles[i_dn]
      if (echelle_angle == "byte") {     # encodage FEWS NET : angle + 90
        aup <- aup + 90
        adn <- adn + 90
      }
    }

    # Dates relatives = numero de decade ou survient l'evenement.
    dmn <- dekads[which.min(v)]                 # decade du minimum
    dmx <- dekads[which.max(v)]                 # decade du maximum
    # Dup/Ddn : la pente i relie les decades i et i+1 ; on prend la decade i.
    dup <- if (is.na(i_up)) NA_real_ else dekads[i_up]
    ddn <- if (is.na(i_dn)) NA_real_ else dekads[i_dn]

    # Indicateurs cumules de la dynamique saisonniere.
    indvi       <- sum(v, na.rm = TRUE)                       # aire sous la courbe
    indvi_seuil <- sum(pmax(v - seuil_sol, 0), na.rm = TRUE)  # aire nette du sol nu
    duree       <- sum(v > seuil_vert, na.rm = TRUE)          # nb de decades vertes

    c(Vav = vav, Vmn = vmn, Vmx = vmx, Rrg = rrg, Rsd = rsd,
      Aup = aup, Adn = adn, Dmn = dmn, Dmx = dmx, Dup = dup, Ddn = ddn,
      iNDVI = indvi, iNDVI_seuil = indvi_seuil, Duree = duree)
  }

  out <- terra::app(cube, fun = calc_metriques)
  names(out) <- noms
  out
}
