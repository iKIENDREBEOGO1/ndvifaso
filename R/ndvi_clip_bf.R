#' Découper un raster NDVI eVIIRS sur le Burkina Faso et convertir en NDVI réel
#'
#' Importe un fichier .tif NDVI eVIIRS (FEWS NET), le découpe sur l'emprise du
#' Burkina Faso (frontière embarquée dans le package), met à NA les valeurs
#' invalides, puis convertit les comptes numériques en NDVI réel dans \[-1, 1\].
#'
#' @param path_tif Chemin vers le fichier .tif NDVI à traiter.
#'
#' @return Un objet \code{SpatRaster} (terra) découpé et converti en NDVI réel.
#' @export
#'
#' @examples
#' \dontrun{
#' dossier <- ndvi_download("2025-06-01", "2025-06-10")
#' tif <- list.files(dossier, pattern = "\\.tif$", full.names = TRUE)[1]
#' r <- ndvi_clip_bf(tif)
#' terra::plot(r)
#' }
ndvi_clip_bf <- function(path_tif) {

  if (!file.exists(path_tif)) {
    stop("Fichier introuvable : ", path_tif)
  }

  # Frontiere du Burkina embarquee dans le package (pas de chemin en dur).
  path_bf <- system.file("extdata", "burkina_faso.gpkg", package = "ndvifaso")
  if (path_bf == "") {
    stop("Frontiere du Burkina introuvable dans le package.")
  }

  r  <- terra::rast(path_tif)
  bf <- terra::vect(path_bf)

  # Aligner le CRS de la frontiere sur celui du raster, au cas ou.
  bf <- terra::project(bf, terra::crs(r))

  # Decoupe : crop (emprise) puis mask (contour exact).
  r_crop <- terra::crop(r, bf)
  r_bf   <- terra::mask(r_crop, bf)

  # Valeurs invalides (201-255) -> NA, puis conversion en NDVI reel.
  # Formule USGS eVIIRS : NDVI = (valeur - 100) / 100
  r_bf[r_bf > 200] <- NA
  r_bf <- (r_bf - 100) / 100

  names(r_bf) <- "ndvi"
  r_bf
}
