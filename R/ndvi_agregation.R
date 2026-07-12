#' Agréger le rendement fourrager par unité administrative
#'
#' À partir d'un raster de rendement (kg/ha) sur tous les pixels, applique le
#' masque de zones pâturables, calcule la production par pixel (rendement ×
#' surface), puis agrège aux niveaux commune, province, région et national.
#' Le rendement d'une zone = somme des productions / somme des surfaces
#' pâturables (moyenne pondérée par la surface).
#'
#' @param rendement Un \code{SpatRaster} de rendement (kg/ha), issu de
#'   \code{ndvi_rendement} (tous pixels).
#' @param paturage Un objet \code{sf} des zones pâturables (ex. savane+steppe).
#' @param communes,provinces,regions Objets \code{sf} des limites
#'   administratives, avec une colonne \code{Nom} (et \code{Province}/\code{Region}
#'   pour le rattachement, si présentes).
#'
#' @return Une liste de 5 éléments : \code{rendement_paturage} (le SpatRaster
#'   masqué, "carte 2") et quatre data.frames \code{national}, \code{regions},
#'   \code{provinces}, \code{communes} avec production totale (kg), superficie
#'   pâturable (ha) et rendement moyen (kg/ha).
#' @export
#'
#' @examples
#' \dontrun{
#' res <- ndvi_agregation(rendement, paturage, com, prov, reg)
#' head(res$communes)
#' }
ndvi_agregation <- function(rendement, paturage,
                            communes, provinces, regions) {

  # 1. Masque pâturable : ne garder que les pixels en savane/steppe. ("carte 2")
  pat_vect <- terra::vect(paturage)
  pat_vect <- terra::project(pat_vect, terra::crs(rendement))
  rend_pat <- terra::mask(rendement, pat_vect)

  # 2. Surface réelle de chaque pixel (en ha), alignée sur les NA du rendement.
  surf <- terra::cellSize(rend_pat, unit = "ha")
  surf <- terra::mask(surf, rend_pat)

  # 3. Production par pixel (kg) = rendement (kg/ha) × surface (ha).
  prod <- rend_pat * surf

  # Empile production + surface pour une extraction zonale en une passe.
  stk <- c(prod, surf)
  names(stk) <- c("prod", "surf")

  # Fonction : agrège prod et surf sur les polygones d'un niveau donné.
  agreger <- function(sf_niveau, cols_id) {
    v <- terra::vect(sf_niveau)
    v <- terra::project(v, terra::crs(stk))
    som <- exactextractr::exact_extract(
      stk, sf::st_as_sf(sf_niveau),
      fun = "sum", progress = FALSE
    )
    df <- sf::st_drop_geometry(sf_niveau)[, cols_id, drop = FALSE]
    df$Production_kg    <- som$sum.prod
    df$Superficie_ha    <- som$sum.surf
    df$Rendement_kg_ha  <- ifelse(df$Superficie_ha > 0,
                                  df$Production_kg / df$Superficie_ha, NA)
    df
  }

  # 4. Agrégation aux trois niveaux (colonnes de rattachement si présentes).
  col_com  <- intersect(c("Region", "Province", "Nom"), names(communes))
  col_prov <- intersect(c("Region", "Nom"), names(provinces))
  col_reg  <- intersect(c("Nom"), names(regions))

  res_com  <- agreger(communes,  col_com)
  res_prov <- agreger(provinces, col_prov)
  res_reg  <- agreger(regions,   col_reg)

  # 5. National = somme de tout le pâturable.
  prod_nat <- terra::global(prod, "sum", na.rm = TRUE)[1, 1]
  surf_nat <- terra::global(surf, "sum", na.rm = TRUE)[1, 1]
  res_nat  <- data.frame(
    Niveau          = "National",
    Production_kg   = prod_nat,
    Superficie_ha   = surf_nat,
    Rendement_kg_ha = prod_nat / surf_nat
  )

  list(
    rendement_paturage = rend_pat,
    national  = res_nat,
    regions   = res_reg,
    provinces = res_prov,
    communes  = res_com
  )
}
