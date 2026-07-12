#' Télécharger les NDVI eVIIRS (FEWS NET) sur une plage de dates
#'
#' Télécharge toutes les décades (dekads) FEWS NET comprises entre deux dates,
#' pour l'Afrique de l'Ouest, puis dézippe les fichiers .tif.
#'
#' @param date_debut Date de début, au format "AAAA-MM-JJ".
#' @param date_fin Date de fin, au format "AAAA-MM-JJ".
#' @param dossier Dossier de destination. Par défaut, un sous-dossier temporaire.
#' @param pause_min,pause_max Bornes (en secondes) du délai aléatoire entre deux
#'   téléchargements. Un délai est tiré au hasard dans cet intervalle.
#' @param pause_annee Pause (en secondes) appliquée au passage à une nouvelle
#'   année dans la séquence des décades.
#' @param on_progress Fonction optionnelle appelée après chaque décade, avec les
#'   arguments \code{i} (indice courant), \code{total} (nombre de décades) et
#'   \code{fichier} (nom du fichier). Sert à alimenter une barre de progression.
#'
#' @return (De façon invisible) le chemin du dossier contenant les .tif.
#' @export
#'
#' @examples
#' \dontrun{
#' ndvi_download("2025-04-01", "2025-06-30")
#' }
ndvi_download <- function(date_debut,
                          date_fin,
                          dossier = file.path(tempdir(), "ndvi"),
                          pause_min = 2,
                          pause_max = 5,
                          pause_annee = 10,
                          on_progress = NULL) {

  base_url <- paste0(
    "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/",
    "africa/west/dekadal/evmodis/ndvi/temporallysmoothedndvi/downloads/dekadal/"
  )

  # Conversion d'une date "AAAA-MM-JJ" en (annee, decade 1..36).
  date_to_dekad <- function(date) {
    parts <- as.integer(strsplit(as.character(date), "-")[[1]])
    y <- parts[1]; m <- parts[2]; d <- parts[3]
    if (is.na(y) || is.na(m) || is.na(d) || m < 1 || m > 12 || d < 1) {
      stop("Date mal formee (attendu 'AAAA-MM-JJ') : ", date)
    }
    dk <- if (d <= 10) 1L else if (d <= 20) 2L else 3L
    c(year = y, dekad = (m - 1L) * 3L + dk)
  }

  # Liste des decades entre debut et fin (gere la bascule d'annee).
  s <- date_to_dekad(date_debut)
  e <- date_to_dekad(date_fin)
  si <- s["year"] * 36 + (s["dekad"] - 1)
  ei <- e["year"] * 36 + (e["dekad"] - 1)
  if (ei < si) stop("La date de fin est anterieure a la date de debut.")
  idx <- seq(si, ei)
  dekads <- data.frame(year = idx %/% 36, dekad = (idx %% 36) + 1)

  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  total <- nrow(dekads)
  message("Decades a telecharger : ", total)

  annee_precedente <- NA_integer_

  for (i in seq_len(total)) {
    annee_courante <- dekads$year[i]

    # Pause longue au changement d'annee (sauf a la toute premiere decade).
    if (!is.na(annee_precedente) && annee_courante != annee_precedente) {
      message("  ... nouvelle annee (", annee_courante, "), pause de ",
              pause_annee, "s")
      Sys.sleep(pause_annee)
    }

    fichier  <- sprintf("wa%d%02d.zip", annee_courante, dekads$dekad[i])
    url      <- paste0(base_url, fichier)
    path_zip <- file.path(dossier, fichier)

    if (file.exists(path_zip) && file.info(path_zip)$size > 0) {
      message("\u21b7 Deja present : ", fichier)
      utils::unzip(path_zip, exdir = dossier)
    } else {
      message("\u2192 Telechargement : ", fichier)
      resp <- tryCatch(
        httr::GET(url,
                  httr::write_disk(path_zip, overwrite = TRUE),
                  httr::timeout(180)),
        error = function(err) err
      )

      if (inherits(resp, "error")) {
        message("  Erreur reseau : ", conditionMessage(resp))
        if (file.exists(path_zip)) file.remove(path_zip)
      } else if (httr::status_code(resp) == 200) {
        message("  OK")
        utils::unzip(path_zip, exdir = dossier)
      } else {
        message("  HTTP ", httr::status_code(resp), " (decade indisponible ?)")
        if (file.exists(path_zip)) file.remove(path_zip)
      }

      # Delai aleatoire entre deux requetes reseau.
      Sys.sleep(stats::runif(1, pause_min, pause_max))
    }

    # Signale la progression a l'appelant (l'app Shiny).
    if (is.function(on_progress)) {
      on_progress(i = i, total = total, fichier = fichier)
    }

    annee_precedente <- annee_courante
  }

  invisible(dossier)
}
