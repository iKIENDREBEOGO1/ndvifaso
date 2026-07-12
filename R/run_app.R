#' Lancer l'application Shiny NDVI FASO
#'
#' Interface graphique pour télécharger les NDVI décadaires eVIIRS (FEWS NET)
#' sur une plage de dates, dans un dossier choisi par l'utilisateur, avec suivi
#' de progression détaillé.
#'
#' @return Lance l'application Shiny (ne retourne rien).
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function() {

  shiny::addResourcePath("www", system.file("app/www", package = "ndvifaso"))

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                       href = "www/styles.css"),
      shiny::tags$title("NDVI FASO")
    ),

    shiny::div(class = "app-header",
               shiny::h1("NDVI FASO"),
               shiny::div(class = "subtitle",
                          "Telechargement des NDVI decadaires eVIIRS - Burkina Faso")
    ),

    shiny::fluidRow(
      shiny::column(width = 4,
                    shiny::div(class = "panel-card",
                               shiny::h4("Parametres"),
                               shiny::dateInput("date_debut", "Date de debut :",
                                                value = "2025-05-01", language = "fr"),
                               shiny::dateInput("date_fin", "Date de fin :",
                                                value = "2025-05-31", language = "fr"),
                               shiny::tags$br(),
                               shiny::strong("Dossier de sauvegarde :"),
                               shiny::div(style = "margin-top:8px;",
                                          shinyFiles::shinyDirButton("dossier", "Parcourir...",
                                                                     title = "Choisir un dossier",
                                                                     class = "btn-file")
                               ),
                               shiny::uiOutput("dossier_ui"),
                               shiny::tags$br(),
                               shiny::actionButton("go", "Lancer le telechargement",
                                                   class = "btn-go")
                    )
      ),

      shiny::column(width = 8,
                    shiny::div(class = "panel-card",
                               shiny::h4("Suivi du telechargement"),
                               shiny::uiOutput("statut_ui"),
                               shiny::tags$br(),
                               shiny::h4("Fichiers telecharges"),
                               shiny::tableOutput("fichiers"),
                               shiny::div(class = "data-source",
                                          "Source des donnees : USGS / FEWS NET - eVIIRS NDVI (Afrique de l'Ouest)")
                    )
      )
    ),

    shiny::div(class = "app-footer",
               shiny::div(style = "text-align:right;",
                          shiny::div(class = "auteur-nom", "Propose par KIENDREBEOGO Innocent"),
                          shiny::div(class = "auteur-role",
                                     "Ingenieur Statisticien Economiste | Statistical and Economic Engineer"),
                          shiny::tags$a(href = "https://www.linkedin.com/in/innocent-kiendrebeogo-86a3a918b/",
                                        target = "_blank", "Profil LinkedIn")
               ),
               shiny::tags$img(src = "www/photo.png", alt = "Photo")
    )
  )

  server <- function(input, output, session) {

    racines <- c("Accueil" = fs::path_home(), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "dossier", roots = racines)

    dossier_path <- shiny::reactive({
      shiny::req(input$dossier)
      shinyFiles::parseDirPath(racines, input$dossier)
    })

    output$dossier_ui <- shiny::renderUI({
      if (length(dossier_path()) == 0 || dossier_path() == "") {
        shiny::div(class = "dossier-box",
                   style = "border-left-color:#f57c00; background:#fff3e0;",
                   "Aucun dossier choisi.")
      } else {
        shiny::div(class = "dossier-box", dossier_path())
      }
    })

    resultat <- shiny::eventReactive(input$go, {
      shiny::validate(
        shiny::need(length(dossier_path()) > 0 && dossier_path() != "",
                    "Choisissez d'abord un dossier de sauvegarde.")
      )
      dest <- dossier_path()
      t0 <- Sys.time()

      # Barre de progression NATIVE : se rafraichit pendant la boucle.
      shiny::withProgress(message = "Telechargement en cours", value = 0, {

        maj <- function(i, total, fichier) {
          # Estimation du temps restant a partir du rythme observe.
          ecoule <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
          reste_s <- if (i > 0) (ecoule / i) * (total - i) else NA
          reste_txt <- if (is.na(reste_s) || i == total) "termine" else {
            m <- floor(reste_s / 60); s <- round(reste_s %% 60)
            if (m > 0) sprintf("~ %d min %02d s restantes", m, s)
            else sprintf("~ %d s restantes", s)
          }
          # Met a jour la barre : fraction + detail (fichier + temps restant).
          shiny::setProgress(
            value  = i / total,
            detail = sprintf("%s  (%d/%d)  -  %s", fichier, i, total, reste_txt)
          )
        }

        ndvi_download(
          as.character(input$date_debut),
          as.character(input$date_fin),
          dossier     = dest,
          on_progress = maj
        )
      })

      list(
        dossier = dest,
        tifs    = list.files(dest, pattern = "\\.tif$", full.names = FALSE)
      )
    })

    output$statut_ui <- shiny::renderUI({
      res <- resultat()
      n <- length(res$tifs)
      if (n == 0) {
        shiny::div(class = "statut-vide",
                   "Aucun fichier .tif telecharge pour cette periode.")
      } else {
        shiny::div(class = "statut-ok",
                   shiny::strong("Termine. "),
                   sprintf("%d fichier(s) .tif enregistre(s) dans :", n),
                   shiny::br(),
                   shiny::tags$code(res$dossier))
      }
    })

    output$fichiers <- shiny::renderTable({
      res <- resultat()
      if (length(res$tifs) == 0) data.frame(Fichier = character(0))
      else data.frame(`N°` = seq_along(res$tifs), Fichier = res$tifs,
                      check.names = FALSE)
    })
  }

  shiny::shinyApp(ui, server)
}
