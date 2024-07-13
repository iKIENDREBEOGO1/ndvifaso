
# ---------- En-tête NDVI FASO ----------

css_entete <- "
.navbar { background:#fff !important; border:0 !important; border-radius:0 !important;
          margin-bottom:0 !important; min-height:46px; box-shadow:0 1px 0 rgba(0,0,0,.06); }
.navbar .navbar-brand { display:none; }
.navbar-nav > li > a { color:#6b7c72 !important; padding:12px 18px !important;
          border-bottom:3px solid transparent; }
.navbar-nav > li.active > a, .navbar-nav > li > a:hover {
          color:#0f6e43 !important; font-weight:500; background:transparent !important;
          border-bottom:3px solid #0f6e43 !important; }
.bf-tricolore { display:flex; height:4px; } .bf-tricolore > div { flex:1; }
body { background:#f5f7f5; }
"

banniere <- shiny::tags$div(
  style = "background:#0f6e43;display:flex;align-items:center;gap:16px;padding:18px 22px;",
  shiny::tags$div(
    style = "width:52px;height:52px;border-radius:12px;background:rgba(255,255,255,.14);
             display:flex;align-items:center;justify-content:center;flex:none;",
    shiny::icon("seedling", style = "font-size:26px;color:#fff;")
  ),
  shiny::tags$div(
    style = "flex:1;min-width:0;",
    shiny::tags$div("NDVI FASO",
                    style = "font-size:24px;font-weight:600;color:#fff;letter-spacing:1px;line-height:1.1;"),
    shiny::tags$div("Estimation du rendement fourrager \u00b7 Burkina Faso",
                    style = "font-size:13px;color:#d8ecdf;margin-top:3px;")
  ),
  shiny::tags$div(
    style = "display:flex;flex-direction:column;align-items:flex-end;gap:6px;flex:none;",
    shiny::tags$span(
      style = "background:rgba(255,255,255,.16);color:#fff;font-size:12px;
               padding:4px 11px;border-radius:20px;white-space:nowrap;",
      shiny::icon("calendar-days"),
      sprintf(" Campagne %d\u2013%d",
              as.integer(format(Sys.Date(), "%Y")),
              as.integer(format(Sys.Date(), "%Y")) + 1)),
    shiny::tags$span(
      style = "color:#c3e2cd;font-size:11px;white-space:nowrap;",
      shiny::icon("satellite-dish"), " Donn\u00e9es : FEWS NET \u00b7 eVIIRS")
  )
)

tricolore <- shiny::tags$div(class = "bf-tricolore",
                             shiny::tags$div(style = "background:#EF2B2D;"),
                             shiny::tags$div(style = "background:#FCD116;"),
                             shiny::tags$div(style = "background:#009543;")
)




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

  # Choix pour la selection par decade
  annee_max    <- as.integer(format(Sys.Date(), "%Y"))
  annees       <- annee_max:2012
  mois_choices <- stats::setNames(1:12,
                                  c("Janvier","Fevrier","Mars","Avril","Mai","Juin",
                                    "Juillet","Aout","Septembre","Octobre","Novembre","Decembre"))
  dec_choices  <- c("Decade1 \u00b7" = 1, "Decade2 \u00b7" = 2, "Decade3 \u00b7" = 3)



  # ============================ UI ============================
  ui <- shiny::tagList(

    shiny::tags$head(
      shiny::tags$style(shiny::HTML(css_entete)),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
    ),

    banniere,
    tricolore,

    shiny::navbarPage(
      title = "", id = "onglets",

      # ---------- ONGLET 1 : TÉLÉCHARGEMENT ----------
      shiny::tabPanel("Telechargement",
                      shiny::div(class = "app-header",
                                 shiny::h1("Telechargement NDVI eVIIRS"),
                                 shiny::div(class = "subtitle", "Afrique de l'Ouest-FEWS NET")
                      ),
                      shiny::fluidRow(
                        shiny::column(4,
                                      shiny::div(class = "panel-card",
                                                 shiny::h4("Parametres"),
                                                 shiny::strong("Periode de debut :"),
                                                 shiny::fluidRow(
                                                   shiny::column(4, shiny::selectInput("dl_an_debut",  "Annee",  annees,       selected = annee_max)),
                                                   shiny::column(4, shiny::selectInput("dl_mois_debut","Mois",   mois_choices, selected = 5)),
                                                   shiny::column(4, shiny::selectInput("dl_dec_debut", "Decade", dec_choices,  selected = 1))
                                                 ),
                                                 shiny::strong("Periode de fin :"),
                                                 shiny::fluidRow(
                                                   shiny::column(4, shiny::selectInput("dl_an_fin",  "Annee",  annees,       selected = annee_max)),
                                                   shiny::column(4, shiny::selectInput("dl_mois_fin","Mois",   mois_choices, selected = 5)),
                                                   shiny::column(4, shiny::selectInput("dl_dec_fin", "Decade", dec_choices,  selected = 3))
                                                 ),
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
  )

  # ============================ SERVER ============================
  server <- function(input, output, session) {
    shinyjs::useShinyjs(html = TRUE)
    shiny::observeEvent(input$dl_go, {
      message(">>> clic dl_go recu, dossier = '",
              tryCatch(dl_path(), error = function(e) "VIDE"), "'")
    })
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


    # Construit "AAAA-MM-JJ" et un indice comparable a partir de annee/mois/decade
    dek_date <- function(an, mois, dec) {
      jour <- c("01", "11", "21")[as.integer(dec)]
      sprintf("%04d-%02d-%s", as.integer(an), as.integer(mois), jour)
    }
    dek_idx <- function(an, mois, dec)
      as.integer(an) * 36 + (as.integer(mois) - 1) * 3 + as.integer(dec)


    dl_res <- shiny::eventReactive(input$dl_go, {
      shiny::validate(
        shiny::need(length(dl_path()) > 0 && dl_path() != "", "Choisissez un dossier."),
        shiny::need(
          dek_idx(input$dl_an_fin,   input$dl_mois_fin,   input$dl_dec_fin) >=
            dek_idx(input$dl_an_debut, input$dl_mois_debut, input$dl_dec_debut),
          "La periode de fin doit etre posterieure ou egale au debut.")
      )
      d_debut <- dek_date(input$dl_an_debut, input$dl_mois_debut, input$dl_dec_debut)
      d_fin   <- dek_date(input$dl_an_fin,   input$dl_mois_fin,   input$dl_dec_fin)

      dest <- dl_path(); t0 <- Sys.time()
      shiny::withProgress(message = "Telechargement en cours", value = 0, {
        maj <- function(i, total, fichier) {
          reste <- if (i > 0 && i < total) {
            ecoule <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
            rs <- (ecoule / i) * (total - i); m <- floor(rs/60); s <- round(rs%%60)
            if (m > 0) sprintf(" - reste ~%d min %02d s", m, s) else sprintf(" - reste ~%d s", s)
          } else ""
          shiny::setProgress(value = i / total,
                             detail = sprintf("%d/%d - %s%s", i, total, fichier, reste))
        }
        ndvi_download(d_debut, d_fin, dossier = dest, on_progress = maj)
      })

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

