
dashboardPage(
  dashboardHeader(title="SBIM"),
  dashboardSidebar(
    # fileInput("courant", "Rdata du mois d'analyse", multiple = FALSE), 
    # fileInput("tarifsant", "Rdata des valorisations aux tarifs precedents", multiple = FALSE), 
    # fileInput("consol", "Rdata année précédente", multiple = FALSE), 
    # fileInput("nonconsol", "Rdata du mois d'analyse année précédente", multiple = FALSE), 
    selectInput("anno", "Selection de l'année", choices = list("Année 2015" = 2015, "Année 2016" = 2016, "Année 2017" = 2017, "Année 2018" = 2018, "Année 2019" = 2019, "Année 2020" = 2020, "Année 2021" = 2021, "Année 2022" = 2022, "Année 2020" = 2023, "Année 2021" = 2024, "Année 2022" = 2025), selected = 2019),
    selectInput("mese", "Selection du mois de remontée", choices = list("M1" = 1, "M2" = 2, "M3" = 3, "M4" = 4, "M5" = 5, "M6" = 6, "M7" = 7, "M8" = 8, "M9" = 9, "M10" = 10, "M11" = 11, "M12" = 12), selected = 1),
    actionButton("build", icon("refresh"), label = "Construction des données"),
    actionButton("load", icon("refresh"), label = "Données préconstruites"),
    actionButton("scores", icon("refresh"), label = "Scores de comorbidité (long)"),
    selectizeInput("uma", "Services", multiple = TRUE, choices = NULL, selected = NULL),
    textInput("ghmfilt1", "GHM à inclure [regex]"),
    textInput("ghmfilt2", "GHM à exclure [regex]"),
    actionButton("calcul", icon("refresh"), label = "Sélection services-GHM"),
    sidebarMenu(
      id="menuchoice",
      menuItem("Activité", tabName="activ", icon = NULL),
      menuItem("Valorisation", tabName="valoglob"),
      menuItem("Detail Valorisation", tabName="anavalo"),
      menuItem("Analyse du PMCT", tabName="pmct"),
      menuItem("Flux de patients", tabName="parcours")
    ) ),
  dashboardBody(
    fluidRow(uiOutput("urlimports"),
             tabItems(
               tabItem(tabName="activ", h2("Activité"),
                       textOutput("listuma"),
                       shinydashboard::box(DTOutput(outputId="activ1"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Descriptif des séjours", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="activ2"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Durée des séjours", status = "primary"),
                       fluidRow(
                         shinydashboard::box(DTOutput(outputId="activ3"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Type d'hospitalisation selon la durée", status = "primary"),
                         shinydashboard::box(plotlyOutput(outputId="activ4"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Type d'hospitalisation selon la durée", status = "primary") ),
                       fluidRow(
                         shinydashboard::box(DTOutput(outputId="activ5"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Type d'hospitalisation selon durée et casemix", status = "primary"),
                         shinydashboard::box(plotlyOutput(outputId="activ6"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Type d'hospitalisation selon durée et casemix", status = "primary") ),
                       fluidRow(
                         shinydashboard::box(plotlyOutput(outputId="activ7"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Nombre de séjours selon le type de GHM", status = "primary"),
                         shinydashboard::box(plotlyOutput(outputId="activ8"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Nombre de séjours selon la sévérité des GHM", status = "primary"),
                         shinydashboard::box(plotlyOutput(outputId="activ9"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Evolution de la comorbidité des séjours", status = "primary"),
                         shinydashboard::box(plotlyOutput(outputId="activ10"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Nombre de séjours selon score de comorbidité", status = "primary"),
                         shinydashboard::box(DTOutput(outputId="activ11"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Détail des GHM", status = "primary") )
               ),
               tabItem(tabName="valoglob", h2("Valorisation globale des rss"),  
                       # shinydashboard::box(plotlyOutput(outputId="valo0"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Répartition des recettes de l'année en cours réelles et théoriques (selon proportion de sévérité par racines de l'année précédente)", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="valo0"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Valorisation globale annuelle (rss des sejours passés par l'unité)", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="valo1"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Valorisation globale mensuelle (rss des sejours passés par l'unité)", status = "primary"),
                       shinydashboard::box(plotOutput(outputId="valo2"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Distribution de la valorisation globale", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="valo3"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Valorisation globale selon la durée", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="valo4"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Valorisation globale selon le type au cours du temps", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="valo5"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Valorisation globale selon la sévérité au cours du temps", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="valo6"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Valorisation et durée des sejours longs dans le temps", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="valo7"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Evolution mensuelle du nombre de suppléments SI/REA/SRC", status = "primary")
               ),
               tabItem(tabName="pmct", h2("Analyse du PMCT et de ses variations"), 
                       shinydashboard::box(plotlyOutput(outputId="pmct1"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Evoution mensuelle du PMCT", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="pmct2"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "PMCT selon tarifs et consolidation", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="pmct3"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "PMCT selon le type du casemix", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="pmct4"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "PMCT selon la sévérité", status = "primary"),
                       shinydashboard::box(plotOutput(outputId="pmct5"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Distribution de la valorisation ventilée sur les rum", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="pmct6"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Valorisation ventilée selon la durée de passage dans l'unité", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="pmct7"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Proportion des sejours mono et multi - rums", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="pmct8"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "PMCT des monorum utilisé pour la clef de répartition", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="pmct9"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "PMCT des monorum des autres unités utilisés pour la clef de répartition", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="pmct10"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Evolution du coefficient de repartition selon l'année", status = "primary"),
                       shinydashboard::box(plotOutput(outputId="pmct11"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Evolution du coefficient de repartition selon l'année", status = "primary")
               ),
               tabItem(tabName="parcours", h2("Analyse des flux de patients"),
                       fluidRow(
                         shinydashboard::box(sunburstOutput(outputId="parcours1"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Parcours patients année en cours", status = "primary"),
                         shinydashboard::box(DTOutput(outputId="parcours2"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Activités les plus representées", status = "primary")),
                       fluidRow(
                         shinydashboard::box(sunburstOutput(outputId="parcours3"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Parcours patients année précédente au mois en cours", status = "primary"),
                         shinydashboard::box(DTOutput(outputId="parcours4"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Activités les plus representées", status = "primary")),
                       fluidRow(
                         shinydashboard::box(sunburstOutput(outputId="parcours5"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Parcours patients année précédente à M12", status = "primary"),
                         shinydashboard::box(DTOutput(outputId="parcours6"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Activités les plus representées", status = "primary")),
                       fluidRow(
                         shinydashboard::box(forceNetworkOutput(outputId="parcours7"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Flux de séjours de l'année en cours"),
                         shinydashboard::box(forceNetworkOutput(outputId="parcours8"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Flux de séjours de l'année précédente au mois en cours"),
                         # shinydashboard::box(sankeyNetworkOutput(outputId="parcours9"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Flux de séjours"),
                         # shinydashboard::box(sankeyNetworkOutput(outputId="parcours10"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Flux de séjours"),
                         shinydashboard::box(plotlyOutput(outputId="parcours11"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Journées d'hospitalisation selon le type d'autorisation des services", status = "primary"))
               ),
               tabItem(tabName="anavalo", h2("Analyse des variations de valorisation des rums selon le casemix"),
                       shinydashboard::box(DTOutput(outputId="anavalo01"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Valorisation des rums", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="anavalo02"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Valorisation des rums - détail", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="anavalo03"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Différentiels de valorisation sur 1 an", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo0"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Différentiels de valorisation sur 1 an, par CMD", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo1"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Valorisation selon le type du casemix", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo2"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Valorisation selon la sévérité", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo3"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Valorisation selon les CMD", status = "primary"),
                       # shinydashboard::box(plotlyOutput(outputId="anavalo4"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Valorisation selon le domaine d'activité", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo5"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Activité et valorisation selon le type du casemix", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo6"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "Activité et valorisation selon la sévérité", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo7"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Activité et valorisation selon le domaine d'activité", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo8"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed =TRUE, title = "Activité et valorisation selon les CMD", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo9"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "ACP pour la valorisation des rum: CP1 vs CP2", status = "primary"),
                       shinydashboard::box(plotlyOutput(outputId="anavalo10"), width = 6, solidHeader = TRUE, collapsible = TRUE, collapsed =FALSE, title = "ACP pour la valorisation des rum: CP3 vs CP4", status = "primary")
               )
             )
    )
  )
)
