#Resultattjeneste for NorScir

#' Brukergrensesnitt (ui) til nordscir-appen
#'
#' @return Brukergrensesnittet (ui) til nordscir-appen
#' @export
ui_norscir <- function() {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))

  startDato <- as.Date(
    paste0(as.numeric(format(Sys.Date()-400, "%Y")), '-01-01')
  )
  startDato2 <- as.Date(
    paste0(as.numeric(format(Sys.Date()-700, "%Y")), '-01-01')
  )

  context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
  regTitle = "Norsk ryggmargsskaderegister"

  #----Valg

  valgAIS <- 0:5
  names(valgAIS) <- c("Alle", "A", "B", "C", "D", "E")

  valgNivaaUt <- c(99, 0, 1, 2, 3, 9)
  names(valgNivaaUt) <- c("Alle", "Paraplegi", "Tetraplegi", "C1-4", "C5-8",
                          "Ukjent")

  tidsenheter <- rev(
    c("År" = "Aar", "Halvår" = "Halvaar", "Kvartal" = "Kvartal",
      "Måned" = "Mnd")
  )

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::navbarPage(
      id = "hovedark",
      title = shiny::div(
        shiny::a(
          shiny::includeHTML(
            system.file("www/logo.svg", package = "rapbase")
          )
        ),
        regTitle),
      # sett inn tittel også i browser-vindu
      windowTitle = regTitle,
      # velg css (foreløpig den eneste bortsett fra "naken" utgave)
      theme = "rap/bootstrap.css",

      #----startside--------
      shiny::tabPanel(
        "Startside",
        shiny::br(),
        shiny::tags$head(
          shiny::tags$style(
            ".butt{background-color:#6baed6;} .butt{color: white;}"
          )
        ),
        shiny::sidebarPanel(
          width = 3,
          shiny::br(),
          shiny::h3("Månedsrapport"), #),
          shiny::downloadButton(
            outputId = "mndRapp.pdf",
            label = "Last ned MÅNEDSRAPPORT",
            class = "butt"
          ),
          shiny::br(),
          shiny::br(paste("NB: Nedlasting tar litt tid. I mellomtida får man",
                          "ikke sett på andre resultater.")),
          shiny::br(),
          shiny::br(paste("Hvis du ønsker månedsrapporten regelmessig tilsendt",
                          "på e-post, kan du gå til fanen 'Abonnement' og",
                          "bestille dette.")),
          shiny::br(),
          shiny::br(),
          shiny::conditionalPanel(
            condition = "input.startside == 'Status'",
            shiny::h4(
              "Velg tidsperiode for nevrologisk klassifikasjon og liggetider"
            ),
            shiny::dateRangeInput(
              inputId = "datovalgDash", start = startDato, end = Sys.Date(),
              label = "Tidsperiode", separator = "t.o.m.", language = "nb")
          )
        ),
        shiny::mainPanel(
          width = 8,
          if (context %in% c("DEV", "TEST", "QA", "PRODUCTION")) {
            rapbase::navbarWidgetInput("navbar-widget")
          },
          shiny::h2("Velkommen til Rapporteket - Norsk Ryggmargsskaderegister!",
                    align='center'),
          shiny::br(),
          shiny::tabsetPanel(
            id = "startside",
            shiny::tabPanel(
              "Brukerveiledning",
              shiny::htmlOutput("guide", inline = TRUE)
            ),
            shiny::tabPanel(
              "Status",
              shiny::h4("Antall registreringer siste år:"),
              shiny::tableOutput("tabAntOpphShMnd12startside"),
              shiny::h5(paste("(Mer informasjon om registreringsstatus finner",
                              "du under fanen 'Registreringsoversikter')")),
              shiny::br(),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::h3("Nevrologisk klassifikasjon", align = "center"),
                  shiny::h4("alle pasienter", align = "center"),
                  shiny::br(),
                  shiny::tableOutput("tabNevrKlass")),
                shiny::column(
                  width = 6,
                  shiny::h3("Nevrologisk klassifikasjon", align = "center"),
                  shiny::h4("pasienter med liggetid over 28 dager i
                                   ryggmargsskadeavdeling", align = "center"),
                  shiny::tableOutput("tabNevrKlass28")
                )
              ),

              shiny::fluidRow(
                shiny::h3("Liggetider, egen avdeling", align = "left"),
                shiny::tableOutput("tabLiggetider")
              )
            )
          )
        ) #main
      ), #tab

      #--------Fordelinger-----------
      shiny::tabPanel(
        "Fordelinger",
        shiny::sidebarPanel(
          width = 3,
          shiny::dateRangeInput(
            inputId = "datovalg",
            start = startDato,
            end = Sys.Date(),
            label = "Tidsperiode",
            separator="t.o.m.",
            language="nb"
          ),
          shiny::radioButtons(
            inputId = "datoUt",
            label = "Bruk utskrivingsdato til datofiltrering?",
            choiceNames = c("nei", "ja"),
            choiceValues = 0:1,
            selected = 0
          ),
          conditionalPanel(
            condition = "input.fordeling != 'Utskriving/Kontoll'",
          selectInput(
            inputId = "valgtVar",
            label="Velg variabel",
            choices = c(
              "Alder" = "Alder",
              "Ais ved innleggelse" = "AAis" ,
              "Ais ved utskriving" = "FAis",
              "Anbefalt tid til kontroll" = "AnbefTidKtr",
              "Lengde på rehab.opphold" = "DagerRehab",
              "Opphold, totalt antall dager" = "OpphTot",
              "Planlagt utskrevet til" = "PPlaceDis",
              #"Fjern? Permisjon (ant. døgn ute av sykehus) " = "Permisjon",
              "Registreringsforsinkelse" = "RegForsinkelse",
              "Skadeårsak " = "SkadeArsak",
              "Skadeårsak, ikke-traumatisk" = "Ntsci",
              "Tid fra skade til oppstart rehab." = "DagerTilRehab",
              "Tid med rehabilitering" = "DagerRehab",
              "Utskrevet til" = "UtTil",
              #"Fjern? Pustehjelp" = "Pustehjelp[VentAssi]",
              "A&D Funksjon: Mobilitet" = "FunkMob",
              "A&D Funksjon: Påkledning" = "FunkKler",
              "A&D Funksjon: Spising" = "FunkSpis",
              "A&D Funksjon: Toalett" = "FunkDo",
              "A&D Tilfredshet: Mobilitet" = "TilfMob",
              "A&D Tilfredshet: Påkledning" = "TilfKler",
              "A&D Tilfredshet: Spising" = "TilfSpis",
              "A&D Tilfredshet: Toalett" = "TilfDo",
              "Livskval.: Tilfredshet med livet" = "LivsGen",
              "Livskval.: Tilfredshet med fysisk helse" = "LivsFys",
              "Livskval.: Tilfredshet med psykisk helse" = "LivsPsyk",
              "Urin: Ufrivillig urinlekkasje (fra 2019)" = "UrinInkontinens",
              "Urin: Ufrivillig urinlekkasje (t.o.m. 2018)" = "UrinInkontinensTom2018",
              "Urin: Kirurgiske inngrep" = "UrinKirInngr",
              "Urin: Legemiddelbruk (fra 2019)" = "UrinLegemidler",
              #  "Urin: Legemiddelbruk (t.o.m. 2018)" = "UrinLegemidlerTom2018",
              "Urin: Legemiddelbruk, hvilke" = "UrinLegemidlerHvilke",
              "Urin: Blæretømming, hovedmetode" = "UrinTomBlareHoved",
              "Urin: Blæretømming, tilleggsmetode" = "UrinTomBlareTillegg",
              "Tarm: Avføring, hovedmetode" = "TarmAvfHoved",
              "Tarm: Avføring, tilleggsmetode" = "TarmAvfTillegg",
              "Tarm: Avføringsmiddelbruk" = "TarmAvfmiddel",
              "Tarm: Avføringsmidler, hvilke" = "TarmAvfmiddelHvilke",
              "Tarm: Fekal inkontinens (fra 2019)" = "TarmInkontinensFra2019",
              "Tarm: Fekal inkontinens (t.o.m. 2018)" = "TarmInkontinensTom2018",
              "Tarm: Kirurgisk inngrep" = "TarmKirInngrep",
              "Tarm: Kirurgiske inngrep, hvilke" = "TarmKirInngrepHvilke",
              "Tarm: NBD" = "TarmNBD",
              "EQ5D: Mobilitet" = "Eq5dQ1Mobility",
              "EQ5D: Personlig stell" = "Eq5dQ2Selfcare",
              "EQ5D: Daglige gjøremål" = "Eq5dQ3UsualActivities",
              "EQ5D: Smerter, ubehag" = "Eq5dQ4PainDiscomfort",
              "EQ5D: Angst og depresjon" = "Eq5dQ5AnxietyDepression",
              "EQ5D: Generell helsetilstand" = "Eq5dQ6HealthToday"
            ),
            selected = c("Registreringsforsinkelse" = "RegForsinkelse")
          )),

          conditionalPanel(
            condition = "input.fordeling != 'Utskriving/Kontoll'",
          shiny::selectInput(
            inputId = "erMann",
            label = "Kjønn",
            choices = c("Begge" = 2, "Menn" = 1, "Kvinner" = 0)
          ),
          shiny::sliderInput(
            inputId = "alder",
            label = "Alder",
            min = 0,
            max = 110,
            value = c(0, 110)
          ),
          shiny::selectInput(
            inputId = "enhetsUtvalg",
            label = "Egen enhet og/eller landet",
            choices = c("Egen mot resten av landet" = 1, "Hele landet" = 0,
                        "Egen enhet" = 2)
          ),
          shiny::selectInput(
            inputId = "AIS",
            label = "AIS-grad ved utreise",
            multiple = T, #selected=0,
            choices = valgAIS
          ),
          shiny::selectInput(
            inputId = "traume",
            label="Traume",
            choices = c("Alle" = " ", #"ikke"
                        "Traume" = "ja",
                        "Ikke traume" = "nei")
          ),
          shiny::selectInput(
            inputId = "nivaaUt",
            label = "Nivå ved utreise",
            choices = valgNivaaUt
          )
          ),

        conditionalPanel(
          condition = "input.fordeling == 'Utskriving/Kontoll'",
          selectInput(
            inputId = "valgtVarPP",
            label="Velg variabel ",
            choices = c("AIS, utskriving/kontroll"="KontFAis",
                        "Utskrevet til "="KontUtTil")
          ),
          selectInput(
            inputId = "enhetsUtvalgPP",
            label="Egen enhet/hele landet",
            choices = c("Hele landet" = 0,
                        "Egen enhet" = 2)
          )
          # dateRangeInput(
          #   inputId = "datovalgPP",
          #   start = startDato2, end = Sys.Date(),
          #   label = "Tidsperiode", separator="t.o.m.",
          #   language="nb"
          # )
          ),
          shiny::selectInput(
            inputId = "bildeformatFord",
            label = "Velg format for nedlasting av figur",
            choices = c("pdf", "png", "jpg", "bmp", "tif", "svg")
          )
      ), #Sidebar

        shiny::mainPanel(
          width = 6,
          shiny::tabsetPanel(
            id="fordeling",
            shiny::tabPanel(
              "Figur",
              shiny::br(),
              em("(Høyreklikk på figuren for å laste den ned)"),
              shiny::br(),
              shiny::br(),
              shiny::plotOutput("fordelinger", height = "auto"),
              shiny::downloadButton(
                outputId = "lastNed_figFord", label="Last ned figur"
              ),
              hr()
            ),
            shiny::tabPanel(
              "Figur, alle sykehus",
              shiny::plotOutput("fordelingPrSh", height = "auto"),
              shiny::downloadButton(
                outputId = "lastNed_figFordSh", label = "Last ned figur")
            ),
            shiny::tabPanel(
              "Tabell",
              shiny::uiOutput("tittelFord"),
              shiny::br(),
              shiny::tableOutput("fordelingTab"),
              shiny::downloadButton(
                outputId = "lastNed_tabFord", label = "Last ned tabell"
              ),
              shiny::br(),
              shiny::br(),
              shiny::br(),
              shiny::br(),
              shiny::tableOutput("fordelingShTab"),
              shiny::downloadButton(
                outputId = "lastNed_tabFordSh", label="Last ned"
              )
            ),
            tabPanel(
              'Utskriving/Kontoll',
              br(),
              h3("Endring fra utskriving til kontroll"), #),
              br(),
              plotOutput("fordPrePost", height = "auto"),
              downloadButton(
                outputId = "lastNed_figfordPrePost", label="Last ned figur"
              )
            )
          ) #tabset
        ) #mainPanel
      ), #tab Fordelinger


      #------------ Gjennomsnitt ------------
      shiny::tabPanel(
        "Gjennomsnitt per sykehus og over tid",
        shiny::sidebarPanel(
          width = 3,
          shiny::selectInput(
            inputId = "valgtVarGjsn",
            label="Velg variabel",
            choices = c("Alder" = "Alder",
                        "Lengde på rehab.opphold" = "DagerRehab",
                        "Opphold, totalt antall dager" = "OpphTot",
                        "Registreringsforsinkelse" = "RegForsinkelse",
                        "Tid fra skade til oppstart rehab." = "DagerTilRehab",
                        "Livskval.: Tilfredshet med livet" = "LivsGen",
                        "Livskval.: Tilfredshet med fysisk helse" = "LivsFys",
                        "Livskval.: Tilfredshet med psykisk helse" = "LivsPsyk"
            ),
            selected = c("Registreringsforsinkelse" = "RegForsinkelse")
          ),
          shiny::dateRangeInput(
            inputId = "datovalgGjsn",
            start = startDato,
            end = Sys.Date(),
            label = "Tidsperiode", separator="t.o.m.", language="nb"
          ),
          shiny::radioButtons(
            inputId = "datoUtGjsn",
            "Bruk utskrivingsdato til datofiltrering?",
            choiceNames = c("nei", "ja"),
            choiceValues = 0:1,
            selected = 0
          ),
          shiny::selectInput(inputId = "erMannGjsn",
                             label="Kjønn",
                             choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
          ),
          shiny::sliderInput(
            inputId="alderGjsn",
            label = "Alder",
            min = 0,
            max = 110,
            value = c(0, 110)
          ),
          shiny::selectInput(
            inputId = "AISGjsn",
            label="AIS-grad ved utreise",
            multiple = TRUE, #selected=0,
            choices = valgAIS
          ),
          shiny::selectInput(
            inputId = "traumeGjsn",
            label="Traume",
            choices = c("Alle"=" ", #"ikke"
                        "Traume"="ja",
                        "Ikke traume"="nei")
          ),
          shiny::selectInput(
            inputId = "paratetraGjsn",
            label="Nivå ved utreise",
            choices = c("Alle" = 99,
                        "Paraplegi" = 0,
                        "Tetraplegi" = 1,
                        "Ukjent" = 9)
          ),
          shiny::selectInput(
            inputId = "sentralmaal",
            label="Velg gjennomsnitt/median ",
            choices = c("Gjennomsnitt"="gjsn", "Median"="med")
          ),
          shiny::br(),
          shiny::p(
            shiny::em(paste("Følgende utvalg gjelder bare figuren/tabellen som",
                            "viser utvikling over tid"))
          ),
          shiny::selectInput(
            inputId = "enhetsUtvalgGjsn",
            label="Egen enhet og/eller landet",
            choices = c("Egen mot resten av landet"=1,
                        "Hele landet" = 0,
                        "Egen enhet" = 2)
          ),
          shiny::selectInput(
            inputId = "tidsenhetGjsn",
            label = "Velg tidsenhet",
            choices = tidsenheter
          ),
          shiny::selectInput(
            inputId = "bildeformatGjsn",
            label = "Velg format for nedlasting av figur",
            choices = c("pdf", "png", "jpg", "bmp", "tif", "svg")
          )
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Figur",
              shiny::br(),
              shiny::h3(shiny::em("Utvikling over tid")),
              shiny::plotOutput("gjsnTid", height = "auto"),
              shiny::downloadButton(
                outputId = "lastNed_figGjsnTid", label="Last ned figur"
              ),
              shiny::br(),
              shiny::h3(em("Sykehusvise resultater")),
              shiny::plotOutput("gjsnGrVar", height = "auto"),
              shiny::downloadButton(
                outputId = "lastNed_figGjsnGrVar", label = "Last ned figur")
            ),
            shiny::tabPanel(
              "Tabell",
              shiny::uiOutput("tittelGjsnGrVar"),
              shiny::br(),
              shiny::tableOutput("gjsnTidTab"),
              shiny::br(),
              shiny::tableOutput("gjsnGrVarTab"),
              shiny::downloadButton(
                outputId = "lastNed_tabGjsnGrVar", label = "Last ned"
              )
            )
          )
        )
      ), #Gjsn


      #-----Registreringsoversikter------------
      shiny::tabPanel(
        "Registreringsoversikter",
        shiny::sidebarPanel(
          width=3,
          shiny::h3("Utvalg"),
          shiny::conditionalPanel(
            condition = "input.ark == 'Antall personer med ryggmargsskade'",
            shiny::dateInput(
              inputId = "sluttDatoReg",
              label = "Velg sluttdato",
              language="nb",
              value = Sys.Date(),
              max = Sys.Date()
            )
          ),
          shiny::conditionalPanel(
            condition = "input.ark == 'Antall personer med ryggmargsskade'",
            shiny::selectInput(
              inputId = "tidsenhetReg",
              label="Velg tidsenhet",
              choices = rev(c("År"= "Aar", "Måned"="Mnd"))
            )
          ),
          shiny::conditionalPanel(
            condition = "input.ark == 'Antall personer med ryggmargsskade'",
            shiny::selectInput(
              inputId = "traumeReg",
              label="Traume",
              choices = c("Alle"=" ", #"ikke"
                          "Traume"="ja",
                          "Ikke traume"="nei"))
          ),
          shiny::conditionalPanel(
            condition = paste0(
              "input.ark == 'Antall hovedskjema med tilknyttede skjema' | ",
              "input.ark == 'Antall kontrollskjema med tilknyttede skjema' "
            ),
            shiny::dateRangeInput(
              inputId = "datovalgReg",
              start = startDato,
              end = Sys.Date(),
              label = "Tidsperiode",
              separator="t.o.m.",
              language="nb"
            )
          )
        ),

        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "ark",
            shiny::tabPanel(
              "Antall personer med ryggmargsskade",
              shiny::uiOutput("undertittelReg"),
              shiny::p(paste("Velg tidsperiode ved å velge sluttdato/tidsenhet",
                             "i menyen til venstre")),
              shiny::br(),
              shiny::fluidRow(
                shiny::tableOutput("tabAntOpphShMnd12"),
                shiny::downloadButton(
                  outputId = "lastNed_tabAntOpph", label="Last ned"
                )
              )
            ),
            shiny::tabPanel(
              "Antall hovedskjema med tilknyttede skjema",
              shiny::h3("Antall hovedskjema med tilknyttede skjema"),
              shiny::tableOutput("tabAntTilknyttedeHovedSkjema"),
              shiny::downloadButton(
                outputId = "lastNed_tabOppfHovedAnt", label = "Last ned"
              ),
              shiny::br(),
              shiny::h3("Andel (%) hovedskjema med tilknyttede skjema"),
              shiny::tableOutput("tabAndelTilknyttedeHovedSkjema"),
              shiny::downloadButton(
                outputId = "lastNed_tabOppfHovedPst", label="Last ned"
              )
            ),
            shiny::tabPanel(
              "Antall kontrollskjema med tilknyttede skjema",
              shiny::h3("Antall kontrollskjema med tilknyttede skjema"),
              shiny::h5("Datoutvalg er basert på dato for kontroll"),
              shiny::tableOutput("tabAntTilknyttedeKtrSkjema"),
              shiny::downloadButton(
                outputId = "lastNed_tabOppfKtrAnt", label="Last ned"
              ),
              shiny::br(),
              shiny::h3("Andel (%) kontrollskjema med tilknyttede skjema"),
              shiny::tableOutput("tabAndelTilknyttedeKtrSkjema"),
              shiny::downloadButton(
                outputId = "lastNed_tabOppfKtrPst", label="Last ned"
              )
            )
          )
        )
      ), #tab Registreringsoversikter

      #----------------------Registeradministrasjon-----------------------------

      shiny::tabPanel(
        "Registeradministrasjon",
        shiny::h2("Fane som bare er synlig for SC-bruker."),

        shiny::tabsetPanel(
          id = "ark",
          shiny::tabPanel(
            "Samlerapporter",
            shiny::sidebarPanel(
              width=3,
              shiny::h3("Utvalg"),
              shiny::dateRangeInput(
                inputId = "datovalgSamleRapp",
                start = startDato-150,
                end = Sys.Date(),
                label = "Tidsperiode",
                separator="t.o.m.",
                language="nb"
              )
            ),

            shiny::mainPanel(
              shiny::br(),
              shiny::br(),
              shiny::h3("Resultater, hele landet"), #),
              shiny::downloadButton(
                outputId = "samleRappLand.pdf",
                label="Last ned samlerapport, hele landet", class = "butt"
              ),
              shiny::br(),
              shiny::h3("Resultater eget sykehus"), #),
              shiny::downloadButton(
                outputId = "samleRappEgen.pdf",
                label="Last ned egen samlerapport", class = "butt"
              ),
              shiny::br(),
              shiny::br()
            )
          ),
          shiny::tabPanel(
            "Utsendinger",
            title = "Utsending av rapporter",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::autoReportOrgInput("NSuts"),
                rapbase::autoReportInput("NSuts")
              ),
              shiny::mainPanel(
                rapbase::autoReportUI("NSuts")
              )
            )
          ),
          shiny::tabPanel(
            "Eksport, krypterte data",
            shiny::sidebarPanel(
              rapbase::exportUCInput("norscirExport")
            ),
            shiny::mainPanel(
              rapbase::exportGuideUI("norscirExportGuide")
            )
          ) #Eksport-tab
        ) #tabsetPanel
      ), #Registeradm-tab

      #------------------Abonnement------------------------
      shiny::tabPanel(
        shiny::p(
          "Abonnement",
          title="Bestill automatisk utsending av månedsrapport på e-post"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("ns-subscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("ns-subscription")
          )
        )
      )
    ) #navbar
  ) #tagList
}


#' Server-del til norscir-appen
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return Server-delen til norscir-appen
#' @export
server_norscir <- function(input, output, session) {
  #library(nordicscir)
#print(session)
  rapbase::appLogger(
    session = session,
    msg = "Starter norscir-app'en"
  )


  # session persistent objects
  if (rapbase::isRapContext()) {
    reshID <- as.numeric(rapbase::getUserReshId(session))
    rolle <- rapbase::getUserRole(session)
    brukernavn <- rapbase::getUserName(session)
  } else {
    reshID <- 0
    rolle <- 'ukjent'
    brukernavn <- 'ukjent'
  }

  isGetDataOk <- TRUE
  isprocessAllDataOk <- TRUE
  AlleTab <- nordicscir::getRealData(register = 'norscir')
  if (is.null(AlleTab)) {
    warning("Not able to get real data!")
    isGetDataOk <- FALSE
    #AlleTab <- nordicscir::getFakeData() #Har foreløpig bare norske, fiktive data. Men blir de hentet...?
  }
  AlleTab <- nordicscir::processAllData(AlleTab, register = 'norscir')
  if (is.null(AlleTab)) {
    warning("Not able to process data.")
    isprocessAllDataOk <- FALSE
  }

  rapbase::appLogger(
    session = session,
    msg = "Hei, hei. NorScirdata er hentet"
  )

  isDataOk <- all(c(isGetDataOk, isprocessAllDataOk))
  attach(AlleTab)
  enhet <- ifelse(exists('reshID'),
    as.character(HovedSkjema$ShNavn[match(reshID, HovedSkjema$ReshId)]),
  'Uidentifisert enhet')

  rapbase::appLogger(
    session = session,
    msg = paste0("Enhet: ", enhet)
  )

  # observe({
  if (rolle != 'SC') { #
    hideTab(inputId = "hovedark", target = "Registeradministrasjon")
  }
  # })
  #--------------Startside------------------------------
  rapbase::navbarWidgetServer(
    id = "navbar-widget", orgName = enhet, caller = "norscir" #caller = pakkenavn
  )

  output$guide <- shiny::renderText(
    rapbase::renderRmd(
      system.file("brukerveiledning.Rmd", package = "nordicscir"),
      outputType = "html_fragment",
      params = list(isDataOk = isDataOk)
    )
  )
  if (isDataOk) {
    output$tabAntOpphShMnd12startside <-
      shiny::renderTable(
        nordicscir::tabAntOpphShMnd(RegData = HovedSkjema, antMnd = 12),
        rownames = T, digits=0, spacing="xs"
      )
  } else {
    output$tabAntOpphShMnd12startside <- NULL
  }
  observe({
    if (isDataOk) {
      output$tabNevrKlass <- shiny::renderTable(
        nordicscir::lagTabNevrKlass(
          HovedSkjema,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2]
        ),
        rownames = TRUE
      )
      output$tabNevrKlass28 <- shiny::renderTable({
        HovedSkjema28 <- HovedSkjema[which(HovedSkjema$DagerRehab > 28), ]
        nordicscir::lagTabNevrKlass(
          HovedSkjema28,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2]
        )
      },
      rownames = TRUE
      )
      output$tabLiggetider <- shiny::renderTable({
        nordicscir::tabLiggetider(
          RegData = HovedSkjema,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2],
          enhetsUtvalg=2,
          reshID=reshID
        )
      },
      rownames = TRUE,
      digits = 0
      )
    } else {
      output$tabNevrKlass <- NULL
      output$tabNevrKlass28 <- NULL
      output$tabLiggetider <- NULL
    }
  })

  #----------Tabeller, registreringsoversikter ----------------------
  output$undertittelReg <- shiny::renderUI({
    shiny::br()
    t1 <- "Tabellen viser innleggelser "
    t2 <- ", basert på første akutte innleggelse"
    shiny::h4(shiny::HTML(
      switch(
        input$tidsenhetReg,
        Mnd = paste0(t1, "siste 12 måneder før ", input$sluttDatoReg, t2,
                     "<br />"),
        Aar = paste0(t1, "siste 5 år før ", input$sluttDatoReg, t2, "<br />")
      )
    ))
  })
  shiny::observe({
    if (isDataOk) {
      tabAntOpphShMndAar <-
        switch(
          input$tidsenhetReg,
          Mnd = nordicscir::tabAntOpphShMnd(
            RegData = HovedSkjema,
            datoTil = input$sluttDatoReg,
            traume = input$traumeReg,
            antMnd = 12
          ),
          Aar = nordicscir::tabAntOpphSh5Aar(
            RegData = HovedSkjema,
            datoTil = input$sluttDatoReg,
            traume = input$traumeReg
          )
        )

      output$tabAntOpphShMnd12 <- shiny::renderTable(
        tabAntOpphShMndAar, rownames = TRUE, digits = 0, spacing = "xs"
      )
      output$lastNed_tabAntOpph <- shiny::downloadHandler(
        filename = function() {paste0("tabAntOpph.csv")},
        content = function(file, filename) {
          write.csv2(tabAntOpphShMndAar, file, row.names = TRUE, na = "")
        }
      )

      #Antall skjema av alle typer.
      tabTilknHovedSkjema <- nordicscir::tabSkjemaTilknyttet(
        Data = AlleTab,
        moderSkjema = "Hoved",
        datoFra = input$datovalgReg[1],
        datoTil = input$datovalgReg[2]
      )

      #Hovedskjema som har tilknyttede skjema av ulik type
      output$tabAntTilknyttedeHovedSkjema <- shiny::renderTable(
        tabTilknHovedSkjema$Antall,
        rownames = TRUE,
        digits = 0,
        spacing = "xs"
      )

      output$lastNed_tabOppfHovedAnt <- shiny::downloadHandler(
        filename = function() {'tabOppfHovedAnt.csv'},
        content = function(file, filename) {
          write.csv2(
            tabTilknHovedSkjema$Antall, file, row.names = TRUE, na = ""
          )
        }
      )
      #Andel (prosent) av registreringsskjemaene som har oppfølgingsskjema.
      output$tabAndelTilknyttedeHovedSkjema <- shiny::renderTable(
        tabTilknHovedSkjema$Andeler,
        rownames = TRUE,
        digits = 0,
        spacing = "xs"
      )

      output$lastNed_tabOppfHovedPst <- shiny::downloadHandler(
        filename = function() {'tabOppfHovedPst.csv'},
        content = function(file, filename) {
          write.csv2(
            tabTilknHovedSkjema$Andeler, file, row.names = TRUE, na = ""
          )
        }
      )

      #Kontrollskjema som har tilknyttede skjema av ulik type
      tabTilknKtrSkjema <- nordicscir::tabSkjemaTilknyttet(
        Data = AlleTab,
        moderSkjema = "Ktr",
        datoFra = input$datovalgReg[1],
        datoTil = input$datovalgReg[2]
      )

      output$tabAntTilknyttedeKtrSkjema <- shiny::renderTable(
        tabTilknKtrSkjema$Antall, rownames = TRUE, digits = 0, spacing = "xs"
      )

      output$lastNed_tabOppfKtrAnt <- shiny::downloadHandler(
        filename = function() {'tabOppfKtrAnt.csv'},
        content = function(file, filename) {
          write.csv2(tabTilknKtrSkjema$Antall, file, row.names = TRUE, na = "")
        }
      )
      #Andel (prosent) av kontrollskjemaene som har oppfølgingsskjema.
      output$tabAndelTilknyttedeKtrSkjema <- shiny::renderTable(
        tabTilknKtrSkjema$Andeler, rownames = TRUE, digits = 0, spacing = "xs" )

      output$lastNed_tabOppfKtrPst <- shiny::downloadHandler(
        filename = function() {"tabOppfKtrPst.csv"},
        content = function(file, filename) {
          write.csv2(tabTilknKtrSkjema$Andeler, file, row.names = TRUE, na = "")
        }
      )
    } else {
      output$tabAntOpphShMnd12 <- NULL
      output$lastNed_tabAntOpph <- NULL
      output$tabAntTilknyttedeHovedSkjema <- NULL
      output$lastNed_tabOppfHovedAnt <- NULL
      output$tabAndelTilknyttedeHovedSkjema <- NULL
      output$lastNed_tabOppfHovedPst <- NULL
      output$lastNed_tabOppfKtrAnt <- NULL
      output$tabAndelTilknyttedeKtrSkjema <- NULL
      output$lastNed_tabOppfKtrPst <- NULL
    }
  })


  #---------Fordelinger:--fordelingsfigurer og tabeller----------
  shiny::observe({
    if (isDataOk) {
      RegData <- nordicscir::finnRegData(valgtVar = input$valgtVar, Data = AlleTab)
      RegData <- nordicscir::TilLogiskeVar(RegData)

      output$fordelinger <- shiny::renderPlot({
        nordicscir::NSFigAndeler(
          RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
          datoFra = input$datovalg[1], datoTil = input$datovalg[2],
          reshID = reshID,
          AIS = as.numeric(input$AIS), traume = input$traume,
          nivaaUt = as.numeric(input$nivaaUt),
          minald = as.numeric(input$alder[1]),
          maxald = as.numeric(input$alder[2]),
          erMann = as.numeric(input$erMann),
          enhetsUtvalg = as.numeric(input$enhetsUtvalg),
          datoUt = as.numeric(input$datoUt),
          session = session
        )},
        height = 800, width = 800
      )

      output$lastNed_figFord <- shiny::downloadHandler(
        filename = function() {
          paste0("Fordeling_", valgtVar = input$valgtVar, "_", Sys.time(), ".",
                 input$bildeformatFord)
        },
        content = function(file) {
          nordicscir::NSFigAndeler(
            RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
            datoFra = input$datovalg[1], datoTil = input$datovalg[2],
            datoUt = as.numeric(input$datoUt), reshID = reshID,
            AIS = as.numeric(input$AIS), traume = input$traume,
            nivaaUt = as.numeric(input$nivaaUt),
            minald = as.numeric(input$alder[1]),
            maxald = as.numeric(input$alder[2]),
            erMann = as.numeric(input$erMann),
            enhetsUtvalg = as.numeric(input$enhetsUtvalg),
            session = session,
            outfile = file
          )
        }
      )

      UtDataFord <-
        nordicscir::NSFigAndeler(
        RegData = RegData, preprosess = 0, valgtVar = input$valgtVar,
        datoFra = input$datovalg[1], datoTil = input$datovalg[2],
        datoUt = as.numeric(input$datoUt),
        reshID = reshID,
        AIS = as.numeric(input$AIS), traume = input$traume,
        nivaaUt = as.numeric(input$nivaaUt),
        minald = as.numeric(input$alder[1]),
        maxald = as.numeric(input$alder[2]),
        erMann = as.numeric(input$erMann),
        enhetsUtvalg = as.numeric(input$enhetsUtvalg),
        session = session
      )

      output$tittelFord <- shiny::renderUI({
        shiny::tagList(
          shiny::h3(UtDataFord$tittel),
          shiny::h5(shiny::HTML(paste0(UtDataFord$utvalgTxt, "<br />")))
        )
      })

      tabFord <- nordicscir::lagTabavFigAndeler(UtDataFraFig = UtDataFord)

      output$fordelingTab <- function() {
        antKol <- ncol(tabFord)
        kableExtra::kable(
          tabFord, format = "html",
          full_width = FALSE,
          digits = c(0, 1, 0, 1)[1:antKol]
        ) %>%
          kableExtra::add_header_above(
            c(" " = 1, "Egen enhet/gruppe" = 2, "Resten" = 2)[1:(antKol/2 + 1)]
          ) %>%
          kableExtra::column_spec(column = 1, width_min = "7em") %>%
          kableExtra::column_spec(
            column = 2:(ncol(tabFord) + 1), width = "7em"
          ) %>%
          kableExtra::row_spec(0, bold = TRUE)
      }
      output$lastNed_tabFord <- shiny::downloadHandler(
        filename = function() {paste0(input$valgtVar, '_fordeling.csv')},
        content = function(file, filename) {
          write.csv2(tabFord, file, row.names = FALSE, na = "")
        }
      )

      output$fordelingPrSh <- shiny::renderPlot({
        nordicscir::NSFigAndelerSh(
          RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
          datoFra = input$datovalg[1], datoTil = input$datovalg[2],
          datoUt = as.numeric(input$datoUt),
          AIS = as.numeric(input$AIS), traume = input$traume,
          nivaaUt = as.numeric(input$nivaaUt),
          minald = as.numeric(input$alder[1]),
          maxald = as.numeric(input$alder[2]),
          erMann = as.numeric(input$erMann),
          session = session
        )},
        height = 800, width = 800
      )

      output$lastNed_figFordSh <- shiny::downloadHandler(
        filename = function() {
          paste0("FordelingPrSh_", valgtVar = input$valgtVar, "_", Sys.time(),
                 ".", input$bildeformatFord)
        },
        content = function(file) {
          nordicscir::NSFigAndelerSh(
            RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
            datoFra = input$datovalg[1], datoTil = input$datovalg[2],
            datoUt = as.numeric(input$datoUt),
            AIS = as.numeric(input$AIS), traume = input$traume,
            nivaaUt = as.numeric(input$nivaaUt),
            minald = as.numeric(input$alder[1]),
            maxald = as.numeric(input$alder[2]),
            erMann = as.numeric(input$erMann),
            session = session,
            outfile = file
          )
        }
      )

      UtDataFordSh <-
        nordicscir::NSFigAndelerSh(
        RegData = RegData, preprosess = 0, valgtVar = input$valgtVar,
        datoFra = input$datovalg[1], datoTil = input$datovalg[2],
        datoUt = as.numeric(input$datoUt),
        AIS = as.numeric(input$AIS), traume = input$traume,
        nivaaUt = as.numeric(input$nivaaUt),
        minald = as.numeric(input$alder[1]),
        maxald = as.numeric(input$alder[2]),
        erMann = as.numeric(input$erMann),
        session = session
      )

      tabFordSh <- nordicscir::lagTabavFigAndelerSh(UtDataFraFig = UtDataFordSh)

      output$fordelingShTab <- function() {
        antKol <- ncol(tabFordSh)
        kableExtra::kable(
          tabFordSh,
          format = "html",
          full_width = FALSE,
          digits = c(0, 0, 0, 1, 1, 1)[1:antKol]
        ) %>%
          kableExtra::add_header_above(
            header = c(" " = 1, "Antall" = 3, "Andel" = 3)
          ) %>%
          kableExtra::column_spec(column = 1, width_min = "7em") %>%
          kableExtra::column_spec(
            column = 2:(ncol(tabFordSh) + 1), width = "7em"
          ) %>%
          kableExtra::row_spec(0, bold = TRUE)
      }
      output$lastNed_tabFordSh <- shiny::downloadHandler(
        filename = function() {paste0(input$valgtVar, "_fordelingSh.csv")},
        content = function(file, filename) {
          write.csv2(tabFordSh, file, row.names = FALSE, na = "")
        }
      )
    } else {
      output$fordelinger <- NULL
      output$lastNed_figFord <- NULL
      output$tittelFord <- NULL
      output$fordelingTab <- NULL
      output$lastNed_tabFord <- NULL
      output$fordelingPrSh <- NULL
      output$lastNed_figFordSh <- NULL
      output$fordelingShTab <- NULL
      output$lastNed_tabFordSh <- NULL
    }
  }) #observe Fordeling


  #-----------------Sykehusvise gjennomsnitt, figur og tabell-------------------
  observe({
    if (isDataOk) {
      RegData <- nordicscir::finnRegData(valgtVar = input$valgtVarGjsn, Data = AlleTab)
      output$gjsnGrVar <- shiny::renderPlot(
        nordicscir::NSFigGjsnGrVar(RegData = RegData, preprosess = 0,
                       valgtVar = input$valgtVarGjsn,
                       datoFra = input$datovalgGjsn[1],
                       datoTil = input$datovalgGjsn[2],
                       datoUt = as.numeric(input$datoUtGjsn),
                       AIS = as.numeric(input$AISGjsn),
                       traume = input$traumeGjsn,
                       nivaaUt = as.numeric(input$paratetraGjsn),
                       minald = as.numeric(input$alderGjsn[1]),
                       maxald = as.numeric(input$alderGjsn[2]),
                       erMann = as.numeric(input$erMannGjsn),
                       valgtMaal = input$sentralmaal, session=session
        ),
        width = 800, height = 600
      )

      output$lastNed_figGjsnGrVar <- shiny::downloadHandler(
        filename = function() {
          paste0("FigGjsn_", valgtVar = input$valgtVarGjsn, "_", Sys.time(),
                 ".", input$bildeformatGjsn)
        },
        content = function(file) {
          nordicscir::NSFigGjsnGrVar(RegData = RegData, preprosess = 0,
                         valgtVar = input$valgtVarGjsn,
                         datoFra = input$datovalgGjsn[1],
                         datoTil = input$datovalgGjsn[2],
                         datoUt = as.numeric(input$datoUtGjsn),
                         AIS = as.numeric(input$AISGjsn),
                         traume = input$traumeGjsn,
                         nivaaUt = as.numeric(input$paratetraGjsn),
                         minald = as.numeric(input$alderGjsn[1]),
                         maxald = as.numeric(input$alderGjsn[2]),
                         erMann = as.numeric(input$erMannGjsn),
                         valgtMaal = input$sentralmaal, session=session,
                         outfile = file)
        })

      UtDataGjsnGrVar <- nordicscir::NSFigGjsnGrVar(
        RegData = RegData, preprosess = 0,
        valgtVar = input$valgtVarGjsn,
        datoFra = input$datovalgGjsn[1],
        datoTil = input$datovalgGjsn[2],
        datoUt = as.numeric(input$datoUtGjsn),
        AIS = as.numeric(input$AISGjsn),
        traume = input$traumeGjsn,
        nivaaUt = as.numeric(input$paratetraGjsn),
        minald = as.numeric(input$alderGjsn[1]),
        maxald = as.numeric(input$alderGjsn[2]),
        erMann = as.numeric(input$erMannGjsn),
        valgtMaal = input$sentralmaal,
        session = session
      )

      tabGjsnGrVar <- nordicscir::lagTabavFigGjsnGrVar(UtDataFraFig = UtDataGjsnGrVar)

      output$tittelGjsnGrVar <- shiny::renderUI({
        shiny::tagList(
          shiny::h3(UtDataGjsnGrVar$tittel),
          shiny::h5(shiny::HTML(paste0(UtDataGjsnGrVar$utvalgTxt, "<br />")))
        )
      })

      output$gjsnGrVarTab <- function() {
        antKol <- ncol(tabGjsnGrVar)
        kableExtra::kable(tabGjsnGrVar, format = "html", digits = c(0, 1)) %>%
          kableExtra::column_spec(column = 1, width_min = "5em") %>%
          kableExtra::column_spec(column = 2:(antKol + 1), width = "4em") %>%
          kableExtra::row_spec(0, bold = TRUE)
      }
      output$lastNed_tabGjsnGrVar <- shiny::downloadHandler(
        filename = function() {
          paste0(input$valgtVar, "_tabGjsnSh.csv")
        },
        content = function(file, filename) {
          write.csv2(tabGjsnGrVar, file, row.names = TRUE, na = "")
        }
      )

      output$titteltabGjsnGrVar <- shiny::renderUI({
        shiny::tagList(
          shiny::h3(tabGjsnGrVar$tittel),
          shiny::h5(shiny::HTML(paste0(tabGjsnGrVar$utvalgTxt, "<br />")))
        )
      })

      #------gjsnTid
      output$gjsnTid <- shiny::renderPlot(
        nordicscir::NSFigGjsnTid(
          RegData = RegData, reshID = reshID, preprosess = 0,
          valgtVar = input$valgtVarGjsn,
          datoFra = input$datovalgGjsn[1], datoTil = input$datovalgGjsn[2],
          datoUt = as.numeric(input$datoUtGjsn),
          minald = as.numeric(input$alderGjsn[1]),
          maxald = as.numeric(input$alderGjsn[2]),
          erMann = as.numeric(input$erMannGjsn),
          AIS = as.numeric(input$AISGjsn),
          traume = input$traumeGjsn,
          nivaaUt = as.numeric(input$paratetraGjsn),
          valgtMaal = input$sentralmaal,
          enhetsUtvalg = as.numeric(input$enhetsUtvalgGjsn),
          tidsenhet = input$tidsenhetGjsn,
          session = session
        ),
        width = 1000, height = 350
      )

      output$lastNed_figGjsnTid <- shiny::downloadHandler(
        filename = function() {
          paste0("FigGjsnTid_", valgtVar = input$valgtVarGjsn, "_", Sys.time(),
                 ".'", input$bildeformatGjsn)
        },
        content = function(file) {
          nordicscir::NSFigGjsnTid(
            RegData = RegData, reshID = reshID, preprosess = 0,
            valgtVar = input$valgtVarGjsn,
            datoFra = input$datovalgGjsn[1], datoTil = input$datovalgGjsn[2],
            datoUt = as.numeric(input$datoUtGjsn),
            minald = as.numeric(input$alderGjsn[1]),
            maxald = as.numeric(input$alderGjsn[2]),
            erMann = as.numeric(input$erMannGjsn),
            AIS = as.numeric(input$AISGjsn),
            traume = input$traumeGjsn,
            nivaaUt = as.numeric(input$paratetraGjsn),
            valgtMaal = input$sentralmaal,
            enhetsUtvalg = as.numeric(input$enhetsUtvalgGjsn),
            tidsenhet = input$tidsenhetGjsn,
            session = session,
            outfile = file
          )
        }
      )

      UtDataGjsnTid <- nordicscir::NSFigGjsnTid(
        RegData = RegData, reshID = reshID, preprosess = 0,
        valgtVar = input$valgtVarGjsn,
        datoFra = input$datovalgGjsn[1], datoTil = input$datovalgGjsn[2],
        datoUt = as.numeric(input$datoUtGjsn),
        minald = as.numeric(input$alderGjsn[1]),
        maxald = as.numeric(input$alderGjsn[2]),
        erMann = as.numeric(input$erMannGjsn),
        AIS = as.numeric(input$AISGjsn),
        traume = input$traumeGjsn,
        nivaaUt = as.numeric(input$paratetraGjsn),
        valgtMaal = input$sentralmaal,
        enhetsUtvalg = as.numeric(input$enhetsUtvalgGjsn),
        tidsenhet = input$tidsenhetGjsn,
        session = session
      )

      tabGjsnTid <- t(UtDataGjsnTid$AggVerdier)
      grtxt <- UtDataGjsnTid$grtxt
      if ((min(nchar(grtxt)) == 5) && (max(nchar(grtxt)) == 5)) {
        grtxt <- paste(substr(grtxt, 1, 3), substr(grtxt, 4, 5))
      }
      rownames(tabGjsnTid) <- grtxt

      antKol <- ncol(tabGjsnTid)
      navnKol <- colnames(tabGjsnTid)
      if (antKol == 6) {
        colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])
      }

      output$gjsnTidTab <- function() {
        kableExtra::kable(
          tabGjsnTid,
          format = "html",
          full_width = FALSE,
          digits = 1
        ) %>%
          kableExtra::add_header_above(
            c(" " = 1, "Egen enhet/gruppe" = 3, "Resten" = 3)[1:(antKol/3 + 1)]
          ) %>%
          kableExtra::column_spec(column = 1, width_min = "7em") %>%
          kableExtra::column_spec(column = 2:(antKol + 1), width = "7em") %>%
          kableExtra::row_spec(0, bold = TRUE)
      }

      output$lastNed_gjsnTidTab <- shiny::downloadHandler(
        filename = function() {
          paste0(input$valgtVarGjsn, "_tabGjsnTid.csv")
        },
        content = function(file, filename) {
          write.csv2(tabGjsnTid, file, row.names = TRUE, na = "")
        }
      )
    } else {
      output$gjsnGrVar <- NULL
      output$lastNed_figGjsnGrVar <- NULL
      output$tittelGjsnGrVar <- NULL
      output$gjsnGrVarTab <- NULL
      output$lastNed_tabGjsnGrVar <- NULL
      output$titteltabGjsnGrVar <- NULL
      output$gjsnTid <- NULL
      output$lastNed_figGjsnTid <- NULL
      output$gjsnTidTab <- NULL
      output$lastNed_gjsnTidTab <- NULL
    }
  }) #observe gjsn

  #----------Kontrollskjema--------------

  shiny::observe({
    if (isDataOk) {
      RegData <- nordicscir::finnRegData(valgtVar = input$valgtVarPP, Data = AlleTab)
      RegData <- nordicscir::TilLogiskeVar(RegData)

      output$fordPrePost <- shiny::renderPlot({
        nordicscir::NSFigPrePost(
          RegData = RegData, valgtVar = input$valgtVarPP, preprosess = 0,
          datoFra = input$datovalg[1], datoTil = input$datovalg[2],
          reshID = reshID,
          # AIS = as.numeric(input$AIS), traume = input$traume,
          # nivaaUt = as.numeric(input$nivaaUt),
          # minald = as.numeric(input$alder[1]),
          # maxald = as.numeric(input$alder[2]),
          # erMann = as.numeric(input$erMann),
          enhetsUtvalg = as.numeric(input$enhetsUtvalgPP),
          datoUt = as.numeric(input$datoUt),
          session = session
        )},
        height = 800, width = 800
      )

      output$lastNed_figfordPrePost <- shiny::downloadHandler(
        filename = function() {
          paste0("FigPreKtr_", input$valgtVarPP, "_", Sys.time(), #input$valgtVarKtr
                 ".'", input$bildeformatFord)
        },
        content = function(file) {
          nordicscir::NSFigGjsnTid(
            RegData = RegData, reshID = reshID, preprosess = 0,
            valgtVar = input$valgtVarPP,
            datoFra = input$datovalg[1], datoTil = input$datovalg[2],
            datoUt = as.numeric(input$datoUt),
            enhetsUtvalg = as.numeric(input$enhetsUtvalgPP),
            session = session,
            outfile = file
          )
        }
      )

    } else {
      output$fordPrePost <- NULL
      output$lastNed_figfordPrePost <- NULL}
    })

  #-------Samlerapporter--------------------
  if (isDataOk) {
    contentFile <- function(file, srcFil, tmpFile, reshID = 0,
                            datoFra = startDato, datoTil = Sys.Date()) {
      src <- normalizePath(system.file(srcFil, package="nordicscir"))
      # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, tmpFile, overwrite = TRUE)
      knitr::knit2pdf(tmpFile)
      file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile) - 3), "pdf"), file)
    }

    output$mndRapp.pdf <- shiny::downloadHandler(
      filename = function() { paste0("MndRapp", Sys.time(), ".pdf")},
      content = function(file) {
        contentFile(
          file,
          srcFil = "NSmndRapp.Rnw",
          tmpFile = "tmpNSmndRapp.Rnw",
          reshID = reshID
        )
      }
    )
    output$samleRappLand.pdf <- shiny::downloadHandler(
      filename = function() {"NorScirSamleRapportLand.pdf"},
      content = function(file) {
        contentFile(
          file,
          srcFil = "NSsamleRappLand.Rnw",
          tmpFile = "tmpNSsamleRappLand.Rnw",
          reshID = reshID,
          datoFra = as.Date(input$datovalgSamleRapp[1]),
          datoTil = as.Date(input$datovalgSamleRapp[2])
        )
      }
    )
    output$samleRappEgen.pdf <- shiny::downloadHandler(
      filename = function() {"NorScirSamleRapportEgen.pdf"},
      content = function(file) {
        contentFile(
          file,
          srcFil = "NSsamleRapp.Rnw",
          tmpFile = "tmpNSsamleRapp.Rnw",
          reshID = reshID,
          datoFra = as.Date(input$datovalgSamleRapp[1]),
          datoTil = as.Date(input$datovalgSamleRapp[2])
        )
      }
    )
  } else {
    output$mndRapp.pdf <- NULL
    output$samleRappLand.pdf <- NULL
    output$samleRappEgen.pdf <- NULL
  }

  #------------------ Abonnement -----------------------------------------------
  subReports <- list(
    `Månedsrapport` = list(
      synopsis = "Rapporteket-NorSCIR: månedsrapport, abonnement",
      fun = "abonnement",
      paramNames = c("rnwFil", "brukernavn", "reshID", "register"),
      paramValues = c("NSmndRapp.Rnw", brukernavn, reshID, 'norscir')
    )
  )

  rapbase::autoReportServer(
    id = "ns-subscription",
    registryName = "norscir", #Character string with the registry name key.
    #Must correspond to the registry R package name.
    #Når norscir benyttes som registryName, kommer bestilte utsendinger opp i den norske appen. Men fungerer utsendinga...? N E I !!
    type = "subscription",
    paramNames = paramNames,
    paramValues = paramValues,
    reports = subReports
  )


  #---Utsendinger---------------
  if (isDataOk) {
    sykehusNavn <- sort(
      unique(as.character(HovedSkjema$ShNavn)),
      index.return = TRUE
    )
    orgs <- c(0, unique(HovedSkjema$ReshId)[sykehusNavn$ix])
    names(orgs) <- c("Alle", sykehusNavn$x)
    orgs <- as.list(orgs)
  } else {
    orgs <- list(`Alle` = 0)
  }


  ## liste med metadata for rapport
  disReports <- list(
    MndRapp = list(
      synopsis = "Rapporteket-NorSCIR: Månedsrapport",
      fun = "abonnement",
      paramNames = c('rnwFil', "reshID", "register"),
      paramValues = c('NSmndRapp.Rnw', 0, 'norscir')
    ),
    SamleRapp = list(
      synopsis = "Rapporteket-NorSCIR: Rapport, div. resultater",
      fun = "abonnement",
      paramNames = c("rnwFil", "reshID", "register"),
      paramValues = c("NSsamleRapp.Rnw", 0, 'norscir')
    )
  )

  org <- rapbase::autoReportOrgServer("NSuts", orgs)

  # oppdatere reaktive parametre, for å få inn valgte verdier
  paramNames <- shiny::reactive("reshID")
  paramValues <- shiny::reactive(org$value())

  rapbase::autoReportServer(
    id = "NSuts", registryName = "norscir", type = "dispatchment",
    org = org$value, paramNames = paramNames, paramValues = paramValues,
    reports = disReports, orgs = orgs, eligible = (rolle == "SC")
  )



  #----------- Eksport ----------------
  ## brukerkontroller
  rapbase::exportUCServer("norscirExport",
                          registryName = 'norscir', #i dbConfig
                          repoName = 'nordicscir') #pakke, for tilhørighet på github
  ## veileding
  rapbase::exportGuideServer("norscirExportGuide",
                             registryName = 'norscir')
}
# Run the application
#shiny::shinyApp(ui = ui_norscir, server = server_norscir)
