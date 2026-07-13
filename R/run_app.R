#' Lancer l'application Shiny NDVI FASO
#'
#' Application à deux onglets : téléchargement des NDVI eVIIRS et estimation du
#' rendement fourrager (bilan par commune, province, région, national).
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

  metriques <- c("Vav", "Vmn", "Vmx", "Rrg", "Rsd",
                 "Aup", "Adn", "Dmn", "Dmx", "Dup", "Ddn")

  # ============================ UI ============================
  ui <- shiny::navbarPage(
    title = "NDVI FASO",
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                       href = "www/styles.css")
    ),

    # ---------- ONGLET 1 : TÉLÉCHARGEMENT ----------
    shiny::tabPanel("Telechargement",
                    shiny::div(class = "app-header",
                               shiny::h1("Telechargement NDVI eVIIRS"),
                               shiny::div(class = "subtitle", "Afrique de l'Ouest - FEWS NET")
                    ),
                    shiny::fluidRow(
                      shiny::column(4,
                                    shiny::div(class = "panel-card",
                                               shiny::h4("Parametres"),
                                               shiny::dateInput("dl_debut", "Date de debut :",
                                                                value = "2025-05-01", language = "fr"),
                                               shiny::dateInput("dl_fin", "Date de fin :",
                                                                value = "2025-05-31", language = "fr"),
                                               shiny::tags$br(),
                                               shiny::strong("Dossier de sauvegarde :"),
                                               shiny::div(style = "margin-top:8px;",
                                                          shinyFiles::shinyDirButton("dl_dossier", "Parcourir...",
                                                                                     title = "Choisir un dossier",
                                                                                     class = "btn-file")),
                                               shiny::uiOutput("dl_dossier_ui"),
                                               shiny::tags$br(),
                                               shiny::actionButton("dl_go", "Lancer le telechargement",
                                                                   class = "btn-go")
                                    )
                      ),
                      shiny::column(8,
                                    shiny::div(class = "panel-card",
                                               shiny::h4("Suivi du telechargement"),
                                               shiny::div(id = "dl_progress_zone", style = "display:none;",
                                                          shiny::div(class = "progress-wrap",
                                                                     shiny::div(class = "progress-outer",
                                                                                shiny::div(id = "dl_bar", class = "progress-inner",
                                                                                           style = "width:0%;", "0%")),
                                                                     shiny::div(class = "progress-meta",
                                                                                shiny::span(id = "dl_file", class = "fichier", ""),
                                                                                shiny::span(id = "dl_reste", class = "reste", "")))),
                                               shiny::uiOutput("dl_statut_ui"),
                                               shiny::tags$br(),
                                               shiny::h4("Fichiers telecharges"),
                                               shiny::tableOutput("dl_fichiers")
                                    )
                      )
                    )
    ),

    # ---------- ONGLET 2 : ESTIMATION ----------
    shiny::tabPanel("Estimation",
                    shiny::div(class = "app-header",
                               shiny::h1("Estimation du rendement fourrager"),
                               shiny::div(class = "subtitle",
                                          "Bilan par commune, province, region et national")
                    ),

                    # --- Section 1 : les 4 dossiers ---
                    shiny::div(class = "panel-card",
                               shiny::h4("1. Dossiers"),
                               shiny::fluidRow(
                                 shiny::column(3,
                                               shiny::strong("NDVI telecharges :"), shiny::br(),
                                               shinyFiles::shinyDirButton("es_ndvi", "Parcourir...",
                                                                          "Dossier NDVI", class = "btn-file"),
                                               shiny::uiOutput("es_ndvi_ui")),
                                 shiny::column(3,
                                               shiny::strong("Limites admin (BNDT) :"), shiny::br(),
                                               shinyFiles::shinyDirButton("es_admin", "Parcourir...",
                                                                          "Dossier limites", class = "btn-file"),
                                               shiny::uiOutput("es_admin_ui")),
                                 shiny::column(3,
                                               shiny::strong("Occupation du sol (BNDT) :"), shiny::br(),
                                               shinyFiles::shinyDirButton("es_occ", "Parcourir...",
                                                                          "Dossier occupation", class = "btn-file"),
                                               shiny::uiOutput("es_occ_ui")),
                                 shiny::column(3,
                                               shiny::strong("Dossier de sortie :"), shiny::br(),
                                               shinyFiles::shinyDirButton("es_out", "Parcourir...",
                                                                          "Dossier sortie", class = "btn-file"),
                                               shiny::uiOutput("es_out_ui"))
                               ),
                               shiny::tags$br(),
                               shiny::actionButton("es_check", "EXECUTER (verifier les donnees)",
                                                   class = "btn-go"),
                               shiny::uiOutput("es_check_ui")
                    ),

                    # --- Section 2 : équation (11 variables) ---
                    shiny::div(class = "panel-card",
                               shiny::h4("2. Equation de rendement"),
                               shiny::helpText("Cochez les variables de votre equation et saisissez ",
                                               "leurs coefficients. Les variables non cochees ont un ",
                                               "coefficient de 0."),
                               shiny::numericInput("es_constante", "Constante :", value = 0),
                               shiny::tags$hr(),
                               shiny::fluidRow(
                                 lapply(metriques, function(m) {
                                   shiny::column(2,
                                                 shiny::div(style = "text-align:center; margin-bottom:10px;",
                                                            shiny::strong(m), shiny::br(),
                                                            shiny::checkboxInput(paste0("chk_", m), NULL, value = FALSE),
                                                            shiny::conditionalPanel(
                                                              condition = sprintf("input.chk_%s == true", m),
                                                              shiny::numericInput(paste0("coef_", m), NULL, value = 0)
                                                            )
                                                 )
                                   )
                                 })
                               ),
                               shiny::tags$br(),
                               shiny::actionButton("es_run", "LANCER L'ESTIMATION", class = "btn-go")
                    ),

                    # --- Section 3 : cartes ---
                    shiny::div(class = "panel-card",
                               shiny::h4("3. Cartes de rendement"),
                               shiny::uiOutput("es_resultat_ui"),
                               shiny::fluidRow(
                                 shiny::column(6, shiny::imageOutput("es_carte_tous", height = 400)),
                                 shiny::column(6, shiny::imageOutput("es_carte_pat", height = 400))
                               )
                    ),

                    shiny::div(class = "app-footer",
                               shiny::div(style = "text-align:right;",
                                          shiny::div(class = "auteur-nom", "Propose par KIENDREBEOGO Innocent"),
                                          shiny::div(class = "auteur-role",
                                                     "Ingenieur Statisticien Economiste"),
                                          shiny::tags$a(href = "https://www.linkedin.com/in/innocent-kiendrebeogo-86a3a918b/",
                                                        target = "_blank", "Profil LinkedIn")),
                               shiny::tags$img(src = "www/photo.png", alt = "Photo"))
    )
  )

  # ============================ SERVER ============================
  server <- function(input, output, session) {
    shinyjs::useShinyjs(html = TRUE)
    racines <- c("Accueil" = fs::path_home(), shinyFiles::getVolumes()())

    # ---------- Logique onglet TÉLÉCHARGEMENT ----------
    shinyFiles::shinyDirChoose(input, "dl_dossier", roots = racines)
    dl_path <- shiny::reactive({
      shiny::req(input$dl_dossier)
      shinyFiles::parseDirPath(racines, input$dl_dossier)
    })
    output$dl_dossier_ui <- shiny::renderUI({
      p <- dl_path()
      if (length(p) == 0 || p == "")
        shiny::div(class = "dossier-box",
                   style="border-left-color:#f57c00;background:#fff3e0;",
                   "Aucun dossier choisi.")
      else shiny::div(class = "dossier-box", p)
    })

    dl_res <- shiny::eventReactive(input$dl_go, {
      shiny::validate(shiny::need(length(dl_path()) > 0 && dl_path() != "",
                                  "Choisissez un dossier."))
      dest <- dl_path(); t0 <- Sys.time()
      shinyjs::show("dl_progress_zone")
      maj <- function(i, total, fichier) {
        pct <- round(100 * i / total)
        ecoule <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        reste <- if (i > 0 && i < total) {
          rs <- (ecoule / i) * (total - i); m <- floor(rs/60); s <- round(rs%%60)
          if (m > 0) sprintf("~ %d min %02d s", m, s) else sprintf("~ %d s", s)
        } else "termine"
        shinyjs::runjs(sprintf(
          "var b=document.getElementById('dl_bar');b.style.width='%d%%';b.innerText='%d%%';
           document.getElementById('dl_file').innerText='%s';
           document.getElementById('dl_reste').innerText='%s';",
          pct, pct, fichier, reste))
        Sys.sleep(0.05)
      }
      ndvi_download(as.character(input$dl_debut), as.character(input$dl_fin),
                    dossier = dest, on_progress = maj)
      list(dossier = dest,
           tifs = list.files(dest, pattern = "\\.tif$"))
    })
    output$dl_statut_ui <- shiny::renderUI({
      r <- dl_res()
      if (length(r$tifs) == 0)
        shiny::div(class = "statut-vide", "Aucun fichier telecharge.")
      else shiny::div(class = "statut-ok", shiny::strong("Termine. "),
                      sprintf("%d fichier(s) dans : ", length(r$tifs)),
                      shiny::tags$code(r$dossier))
    })
    output$dl_fichiers <- shiny::renderTable({
      r <- dl_res()
      if (length(r$tifs) == 0) data.frame(Fichier = character(0))
      else data.frame(Fichier = r$tifs)
    })

    # ---------- Logique onglet ESTIMATION ----------
    for (id in c("es_ndvi","es_admin","es_occ","es_out")) {
      shinyFiles::shinyDirChoose(input, id, roots = racines)
    }
    es_path <- function(id) {
      p <- shinyFiles::parseDirPath(racines, input[[id]])
      if (length(p) == 0) "" else p
    }
    output$es_ndvi_ui  <- shiny::renderUI(shiny::div(class="dossier-box", es_path("es_ndvi")))
    output$es_admin_ui <- shiny::renderUI(shiny::div(class="dossier-box", es_path("es_admin")))
    output$es_occ_ui   <- shiny::renderUI(shiny::div(class="dossier-box", es_path("es_occ")))
    output$es_out_ui   <- shiny::renderUI(shiny::div(class="dossier-box", es_path("es_out")))

    # Vérification des données (EXECUTER)
    output$es_check_ui <- shiny::renderUI({
      input$es_check
      shiny::isolate({
        if (es_path("es_ndvi") == "") return(NULL)
        files <- list.files(es_path("es_ndvi"),
                            pattern = "wa2025(1[0-9]|2[0-9])\\.tif$")
        present <- as.integer(sub("wa2025(\\d{2})\\.tif","\\1", files))
        manquant <- setdiff(10:29, sort(present))
        if (length(manquant) == 0)
          shiny::div(class="statut-ok",
                     sprintf("20/20 decades presentes. Pret pour l'estimation."))
        else
          shiny::div(class="statut-vide",
                     sprintf("%d/20 decades. Manquantes : %s",
                             length(present), paste(manquant, collapse=", ")))
      })
    })

    # Lancer l'estimation complète
    es_result <- shiny::eventReactive(input$es_run, {
      shiny::validate(
        shiny::need(es_path("es_ndvi") != "", "Choisir le dossier NDVI."),
        shiny::need(es_path("es_admin") != "", "Choisir le dossier limites."),
        shiny::need(es_path("es_occ")  != "", "Choisir le dossier occupation."),
        shiny::need(es_path("es_out")  != "", "Choisir le dossier de sortie.")
      )

      shiny::withProgress(message = "Estimation en cours", value = 0, {

        # 1. Cube NDVI
        shiny::setProgress(0.1, detail = "Chargement des decades")
        files <- sort(list.files(es_path("es_ndvi"),
                                 pattern = "wa2025(1[0-9]|2[0-9])\\.tif$", full.names = TRUE))
        cube <- terra::rast(lapply(files, ndvi_clip_bf))

        # 2. Métriques
        shiny::setProgress(0.3, detail = "Metriques phenologiques")
        phen <- ndvi_phenologie(cube, echelle_angle = "degres")

        # 3. Rendement (équation utilisateur)
        shiny::setProgress(0.5, detail = "Application de l'equation")
        coefs <- numeric(0)
        for (m in metriques) {
          if (isTRUE(input[[paste0("chk_", m)]])) {
            coefs[m] <- input[[paste0("coef_", m)]]
          }
        }
        rendement <- ndvi_rendement(phen, constante = input$es_constante,
                                    coefficients = coefs)

        # 4. Couches admin + occupation
        shiny::setProgress(0.6, detail = "Chargement BNDT")
        d_adm <- es_path("es_admin"); d_occ <- es_path("es_occ")
        com  <- sf::st_read(file.path(d_adm, "ADM_Commune.shp"),  quiet = TRUE)
        prov <- sf::st_read(file.path(d_adm, "ADM_Province.shp"), quiet = TRUE)
        reg  <- sf::st_read(file.path(d_adm, "ADM_Region.shp"),   quiet = TRUE)
        pays <- sf::st_read(file.path(d_adm, "ADM_Pays.shp"),     quiet = TRUE)
        veg  <- sf::st_make_valid(
          sf::st_read(file.path(d_occ, "SOL_Zone_vegetation.shp"),
                      quiet = TRUE))
        paturage <- veg[veg$Nature %in% c("SAVANE", "STEPPE"), ]

        # rattachement hiérarchique
        cc <- sf::st_point_on_surface(com)
        com$Province <- prov$Nom[as.integer(sf::st_within(cc, prov))]
        com$Region   <- reg$Nom[as.integer(sf::st_within(cc, reg))]
        pc <- sf::st_point_on_surface(prov)
        prov$Region  <- reg$Nom[as.integer(sf::st_within(pc, reg))]

        # 5. Agrégation
        shiny::setProgress(0.75, detail = "Agregation zonale")
        agg <- ndvi_agregation(rendement, paturage, com, prov, reg)

        # 6. Export
        shiny::setProgress(0.9, detail = "Ecriture Excel et cartes")
        ndvi_export(agg, rendement, pays, es_path("es_out"))

        list(rendement = rendement, agg = agg, out = es_path("es_out"))
      })
    })

    output$es_resultat_ui <- shiny::renderUI({
      r <- es_result()
      shiny::div(class = "statut-ok",
                 shiny::strong("Estimation terminee. "),
                 "Rendement national moyen : ",
                 shiny::strong(sprintf("%.0f kg/ha", r$agg$national$Rendement_kg_ha)),
                 shiny::br(), "Fichiers enregistres dans : ",
                 shiny::tags$code(r$out))
    })

    # Cartes affichées côte à côte (classées)
    output$es_carte_tous <- shiny::renderImage({
      r <- es_result()
      list(src = file.path(r$out, "carte_rendement_tous_pixels.png"),
           width = "100%", contentType = "image/png")
    }, deleteFile = FALSE)

    output$es_carte_pat <- shiny::renderImage({
      r <- es_result()
      list(src = file.path(r$out, "carte_rendement_paturage.png"),
           width = "100%", contentType = "image/png")
    }, deleteFile = FALSE)

  }

  shiny::shinyApp(ui, server)
}

