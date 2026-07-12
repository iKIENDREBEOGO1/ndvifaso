#' Lancer l'application Shiny de téléchargement NDVI eVIIRS
#'
#' Interface graphique pour télécharger les NDVI décadaires eVIIRS (FEWS NET)
#' sur une plage de dates, dans un dossier choisi par l'utilisateur.
#'
#' @return Lance l'application Shiny (ne retourne rien).
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function() {

  ui <- shiny::fluidPage(
    shiny::titlePanel("Téléchargement NDVI eVIIRS - Afrique de l'Ouest"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::dateInput("date_debut", "Date de début :",
                         value = "2025-05-01"),
        shiny::dateInput("date_fin", "Date de fin :",
                         value = "2025-05-31"),
        shiny::hr(),
        shiny::strong("Dossier de sauvegarde :"),
        shiny::br(), shiny::br(),
        shinyFiles::shinyDirButton("dossier", "Parcourir...",
                                   title = "Choisir un dossier de destination"),
        shiny::br(), shiny::br(),
        shiny::verbatimTextOutput("dossier_choisi"),
        shiny::hr(),
        shiny::actionButton("go", "Lancer le téléchargement",
                            class = "btn-primary")
      ),
      shiny::mainPanel(
        shiny::h4("Suivi du téléchargement"),
        shiny::verbatimTextOutput("statut"),
        shiny::h4("Fichiers téléchargés"),
        shiny::tableOutput("fichiers")
      )
    )
  )

  server <- function(input, output, session) {

    # Racines accessibles dans la boite de dialogue : disques + dossier perso.
    racines <- c(
      "Accueil"  = fs::path_home(),
      shinyFiles::getVolumes()()
    )
    shinyFiles::shinyDirChoose(input, "dossier", roots = racines)

    # Chemin du dossier choisi (NULL tant que rien n'est selectionne).
    dossier_path <- shiny::reactive({
      shiny::req(input$dossier)
      shinyFiles::parseDirPath(racines, input$dossier)
    })

    output$dossier_choisi <- shiny::renderText({
      if (length(dossier_path()) == 0 || dossier_path() == "") {
        "Aucun dossier choisi."
      } else {
        dossier_path()
      }
    })

    # Telechargement au clic.
    resultat <- shiny::eventReactive(input$go, {
      shiny::validate(
        shiny::need(length(dossier_path()) > 0 && dossier_path() != "",
                    "Choisissez d'abord un dossier de sauvegarde.")
      )

      dest <- dossier_path()

      shiny::withProgress(message = "Téléchargement en cours...", value = 0, {
        ndvi_download(
          as.character(input$date_debut),
          as.character(input$date_fin),
          dossier = dest
        )
      })

      list(
        dossier = dest,
        tifs    = list.files(dest, pattern = "\\.tif$", full.names = FALSE)
      )
    })

    output$statut <- shiny::renderText({
      res <- resultat()
      paste0(
        "Termine.\n",
        length(res$tifs), " fichier(s) .tif enregistre(s) dans :\n",
        res$dossier
      )
    })

    output$fichiers <- shiny::renderTable({
      res <- resultat()
      if (length(res$tifs) == 0) {
        data.frame(Fichier = "Aucun fichier .tif")
      } else {
        data.frame(Fichier = res$tifs)
      }
    })
  }

  shiny::shinyApp(ui, server)
}
