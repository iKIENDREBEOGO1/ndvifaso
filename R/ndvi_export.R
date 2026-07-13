#' Exporter les résultats : fichier Excel 4 feuilles et cartes de rendement
#'
#' À partir du résultat de \code{ndvi_agregation} et de la carte de rendement
#' brute, écrit un classeur Excel (National, Régions, Provinces, Communes) et
#' deux cartes PNG (rendement tous pixels, rendement pâturage) avec la légende
#' officielle en 4 classes, contour national, échelle et flèche nord.
#'
#' @param agregation Liste renvoyée par \code{ndvi_agregation}.
#' @param rendement_total Le \code{SpatRaster} de rendement sur tous les pixels
#'   (issu de \code{ndvi_rendement}), pour la carte 1.
#' @param contour Objet \code{sf} du contour national (pour tracer la frontière).
#' @param dossier_sortie Dossier où enregistrer les fichiers.
#' @param titre_tous Titre de la carte "tous pixels".
#' @param titre_pat Titre de la carte "pâturage".
#'
#' @return (Invisible) un vecteur des chemins des fichiers créés.
#' @export
#'
#' @examples
#' \dontrun{
#' ndvi_export(res, rendement, pays, "D:/sorties")
#' }
ndvi_export <- function(agregation, rendement_total, contour,
                        dossier_sortie,
                        titre_tous = "Rendement fourrager - tous pixels (kg/ha)",
                        titre_pat  = "Rendement fourrager des paturages (kg/ha)") {

  dir.create(dossier_sortie, recursive = TRUE, showWarnings = FALSE)

  # ---- 1. Fichier Excel 4 feuilles ----
  wb <- openxlsx::createWorkbook()
  feuilles <- list(
    NATIONAL  = agregation$national,
    REGIONS   = agregation$regions,
    PROVINCES = agregation$provinces,
    COMMUNES  = agregation$communes
  )
  entete <- openxlsx::createStyle(textDecoration = "bold",
                                  fgFill = "#1b5e20", fontColour = "#ffffff")
  for (nom in names(feuilles)) {
    openxlsx::addWorksheet(wb, nom)
    openxlsx::writeData(wb, nom, feuilles[[nom]], headerStyle = entete)
    openxlsx::setColWidths(wb, nom, cols = 1:ncol(feuilles[[nom]]),
                           widths = "auto")
  }
  chemin_xlsx <- file.path(dossier_sortie, "bilan_fourrager.xlsx")
  openxlsx::saveWorkbook(wb, chemin_xlsx, overwrite = TRUE)

  # ---- 2. Cartes : découpage en 4 classes + couleurs officielles ----
  classes  <- c(0, 500, 1000, 2000, Inf)
  etiquettes <- c("0-500", "500-1000", "1000-2000", "+2000")
  couleurs <- c("0-500"     = "#d73027",   # rouge
                "500-1000"  = "#fdae61",   # orange
                "1000-2000" = "#a6d96a",   # vert clair
                "+2000"     = "#1a9850")   # vert foncé

  contour_sf <- sf::st_as_sf(contour)

  # Fonction : trace une carte de rendement classée et l'enregistre en PNG.
  tracer_carte <- function(r, titre, fichier) {
    r_cl <- terra::classify(r, rcl = cbind(classes[-length(classes)],
                                           classes[-1],
                                           seq_along(etiquettes)))
    levels(r_cl) <- data.frame(ID = seq_along(etiquettes), classe = etiquettes)

    g <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = r_cl, ggplot2::aes(fill = classe)) +
      ggplot2::geom_sf(data = contour_sf, fill = NA, color = "black",
                       linewidth = 0.5) +
      ggplot2::scale_fill_manual(values = couleurs, na.value = "transparent",
                                 name = "Rendement\n(kg/ha)", drop = FALSE,
                                 na.translate = FALSE) +
      ggplot2::labs(title = titre) +
      ggspatial::annotation_scale(location = "bl") +
      ggspatial::annotation_north_arrow(location = "tr",
                                        style = ggspatial::north_arrow_orienteering()) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    ggplot2::ggsave(fichier, g, width = 9, height = 7, dpi = 200)
    fichier
  }

  c1 <- file.path(dossier_sortie, "carte_rendement_tous_pixels.png")
  c2 <- file.path(dossier_sortie, "carte_rendement_paturage.png")
  tracer_carte(rendement_total,               titre_tous, c1)
  tracer_carte(agregation$rendement_paturage, titre_pat,  c2)

  message("Fichiers crees dans : ", dossier_sortie)
  invisible(c(chemin_xlsx, c1, c2))
}
