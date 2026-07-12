#' Télécharger les NDVI eVIIRS (FEWS NET) sur une plage de dates
#'
#' @param date_debut,date_fin Dates "AAAA-MM-JJ".
#' @param dossier Dossier de destination.
#' @param pause_min,pause_max Bornes du délai aléatoire entre requêtes (s).
#' @param pause_annee Pause au changement d'année (s).
#' @param essais Nombre de tentatives par décade en cas d'échec transitoire.
#' @param timeout_s Délai maximal par téléchargement (s).
#' @param on_progress Fonction optionnelle appelée après chaque décade.
#' @return (Invisible) le chemin du dossier.
#' @export
ndvi_download <- function(date_debut, date_fin,
                          dossier = file.path(tempdir(), "ndvi"),
                          pause_min = 2, pause_max = 5, pause_annee = 10,
                          essais = 3, timeout_s = 300,
                          on_progress = NULL) {

  base_url <- paste0(
    "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/",
    "africa/west/dekadal/evmodis/ndvi/temporallysmoothedndvi/downloads/dekadal/"
  )

  date_to_dekad <- function(date) {
    parts <- as.integer(strsplit(as.character(date), "-")[[1]])
    y <- parts[1]; m <- parts[2]; d <- parts[3]
    if (is.na(y) || is.na(m) || is.na(d) || m < 1 || m > 12 || d < 1)
      stop("Date mal formee (attendu 'AAAA-MM-JJ') : ", date)
    dk <- if (d <= 10) 1L else if (d <= 20) 2L else 3L
    c(year = y, dekad = (m - 1L) * 3L + dk)
  }

  # Télécharge un zip avec ré-essais. Retourne "ok", "absent" (404) ou "echec".
  telecharger_zip <- function(url, path_zip) {
    for (tentative in seq_len(essais)) {
      resp <- tryCatch(
        httr::GET(url, httr::write_disk(path_zip, overwrite = TRUE),
                  httr::timeout(timeout_s)),
        error = function(e) e
      )
      # Erreur reseau (timeout, connexion coupee) -> on reessaie
      if (inherits(resp, "error")) {
        if (file.exists(path_zip)) file.remove(path_zip)
        Sys.sleep(2 * tentative); next
      }
      code <- httr::status_code(resp)
      if (code == 200) {
        # Verifie que le zip n'est pas tronque (listable sans erreur)
        valide <- tryCatch({ utils::unzip(path_zip, list = TRUE); TRUE },
                           error = function(e) FALSE, warning = function(w) FALSE)
        if (valide) return("ok")
        if (file.exists(path_zip)) file.remove(path_zip)
        Sys.sleep(2 * tentative); next
      }
      if (code == 404) {                       # genuinement absent
        if (file.exists(path_zip)) file.remove(path_zip)
        return("absent")
      }
      # 429, 5xx, etc. -> transitoire, on reessaie
      if (file.exists(path_zip)) file.remove(path_zip)
      Sys.sleep(2 * tentative)
    }
    "echec"
  }

  s <- date_to_dekad(date_debut); e <- date_to_dekad(date_fin)
  si <- s["year"] * 36 + (s["dekad"] - 1)
  ei <- e["year"] * 36 + (e["dekad"] - 1)
  if (ei < si) stop("La date de fin est anterieure a la date de debut.")
  idx <- seq(si, ei)
  dekads <- data.frame(year = idx %/% 36, dekad = (idx %% 36) + 1)

  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  total <- nrow(dekads)
  echoues <- character(0)
  annee_precedente <- NA_integer_

  for (i in seq_len(total)) {
    an <- dekads$year[i]
    if (!is.na(annee_precedente) && an != annee_precedente) Sys.sleep(pause_annee)

    fichier  <- sprintf("wa%d%02d.zip", an, dekads$dekad[i])
    path_zip <- file.path(dossier, fichier)

    if (file.exists(path_zip) && file.info(path_zip)$size > 0) {
      utils::unzip(path_zip, exdir = dossier)          # deja la -> on garde
    } else {
      res <- telecharger_zip(paste0(base_url, fichier), path_zip)
      if (res == "ok") {
        utils::unzip(path_zip, exdir = dossier)
      } else {
        echoues <- c(echoues, fichier)                 # absent ou echec
      }
      Sys.sleep(stats::runif(1, pause_min, pause_max))
    }

    if (is.function(on_progress)) on_progress(i = i, total = total, fichier = fichier)
    annee_precedente <- an
  }

  if (length(echoues) > 0) {
    message("Decades non recuperees (", length(echoues), ") : ",
            paste(echoues, collapse = ", "),
            "\nRelancez la meme plage pour les completer.")
  }
  invisible(dossier)
}
