###########################################
#                DEBUT UI                 #
###########################################

# Créer une liste de packages
packages <- c(
  "shiny", "shinydashboard", "flexdashboard", "DT", "DBI", "writexl", 
  "shinyWidgets", "shinyalert", "readxl", "shinyjs", "stringr", 
  "dbplyr", "htmltools", "shinyscreenshot", 
  "lubridate", "magrittr", "dplyr", "shinyBS", "shinycssloaders", 
  "rsconnect", "readr", "ggplot2", "reshape2", "Boruta", "tidyr", "fastDummies",
  "plotly",
  "visdat",
  "e1071",
  "lattice",
  "tidyverse",
  "Boruta",
  "clusterSim",
  "rsconnect",
  "caret",
  "grid",
  "shinyWidgets",
  "shinycssloaders",
  "factoextra",
  "corrplot",
  "FactoMineR"
)

library(corrplot)
# Fonction pour installer et charger les packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  # Chargement des bibliothèques
  library(package, character.only = TRUE)
}

# Appliquer la fonction à chaque package
lapply(packages, install_and_load)


# Options for Spinner
options(spinner.color="#03787c", spinner.color.background="#ffffff", spinner.size=2)

ui <- dashboardPage(skin="green",
  dashboardHeader(title="PROJET STA 101"),
  dashboardSidebar (title = "Menu",
    sidebarMenu(
        menuItem("Gérer les données", tabName = "dash", icon = icon("fas fa-chart-bar")
      )
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-green .main-header .logo {
          background-color: #073d60;
          color: #fff;
          border-bottom: 0 solid transparent;
        }
        .skin-blue .main-header .logo {
          background-color: #f4b943;
        }
        .skin-green .main-header .navbar {
          background-color: #03787c;
        }
        .skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper {
          background-color: #073d60;
        }
        .skin-green .sidebar-menu>li.active>a, .skin-green .sidebar-menu>li:hover>a {
          color: #fff;
          background: #03787c;
          border-left-color: #00a65a;
        }
        div#output_signataire {
          color: red;
          text-align: center;
        }'
    ))
    ),
      
      #Display datasets
      tabItem(tabName = "dash",
        fluidRow(
          tabsetPanel(
            tabPanel(
              "Chargement des fichiers",
              fluidRow(
                box(width = 12,
                  wellPanel(
                    fluidRow(
                      
                      ###################
                      # Load data start #
                      ###################
                      
                      box(width = 3, column(12, 
                        # Bouton de recherche du fichier à charger
                        fileInput(inputId = "file1", label = "Veuillez charger le jeu de données",
                          multiple = TRUE,
                          accept = c("text/csv",".xlsx",".xls",".tsv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv",
                                     '.xlsx'),
                          placeholder = "Sélectionner un fichier")
                        )
                      ),
                      
                      #################
                      # Load data end #
                      #################
                      
                      
                      ##################
                      # ANALYSE RAPIDE # 
                      ##################
                      box(width = 3,
                          # Affichage du graphique de la distribution des classes
                          plotOutput("barplot_target", height = "250px"),
                          
                          # Affichage du tableau de fréquence des classes
                          tableOutput("table_target"),
                          
                          # Affichage de la proportion des classes
                          verbatimTextOutput("proportion_target"),
                          collapsible = T
                      ),
                      box(width = 3,
                      # Bloc pour afficher la structure des données
                       box(title = "Structure des données ",
                           status = "primary", solidHeader = TRUE,
                           width = 12,
                           verbatimTextOutput("str_data")
                       ),
                      collapsible = T
                      ),
                      box(width = 3,       
                        # Bloc pour afficher le résumé des données
                       box(title = "Résumé des données",
                           status = "primary", solidHeader = TRUE,
                           width = 12,
                           verbatimTextOutput("summary_data")
                       ),
                       collapsible = T
                      ),
                      ##################
                      # ANALYSE RAPIDE # 
                      ##################
                    ),
                    #Display datasets
                    tabItem(tabName = "stock",
                      fluidRow(
                        tabsetPanel(
                          tabPanel(
                            "Analyse et prétraitement des données",
                            tabsetPanel(
                              tabPanel(
                                "Aperçu du jeu de données HR Analytics",
                                fluidRow(
                                  box(width = 12,
                                    hr(),
                                    br(),
                                    dataTableOutput("table_datahr") %>% withSpinner(color = "#0dc5c1"),title="Apercu du jeu de données",
                                    footer="Données sur les candidats",
                                    collapsible = T)
                                )
                              ),
                              tabPanel(
                                "Analyse exploratoire des données",
                                box(width = 12,
                                
                                    box(width = 12,
                                      wellPanel(
                                        h5("Cette section sera consacrée analyse exploratoire de données, à gauche vous avez la section pour l'analyse univariée, et à droite pour l'analyse bivariée.")
                                    )
                                  ),
                                  tabsetPanel(
                                    tabPanel("Analyse Univariée",
                                     fluidRow(box(width = 12, title = "Variables Quantitatives",
                                      h4("Statistiques des Variables Quantitatives"),
                                       wellPanel("",
                                         verbatimTextOutput("summary_quant") %>% withSpinner(type = 3, color = "purple")     
                                       )
                                      )
                                     ),
                                     
                                     fluidRow(
                                      column(12,
                                        column(6, 
                                          box(
                                            width=12,
                                              plotOutput("hist_city_dev")  %>% withSpinner(type = 4, color = "blue"),
                                              footer = HTML(" En analysant cet histogramme, on observe :<br>
                                                <b>Asymétrie à droite:</b> La distribution est fortement penchée vers la droite. 
                                                Cela signifie qu' un grand nombre de villes ont un indice de développement élevé, proche de 0,9, 
                                                tandis que peu de villes ont un indice faible.<br>
                                                <b>Mode autour de 0,9:</b> Le pic de la distribution se situe autour de 0,9, 
                                                indiquant que c'est la valeur de l'indice de développement la plus fréquente donc, la majorité des candidats sont issus d'une ville developpée.<br>
                                                <b>Quelques valeurs aberrantes à gauche:</b> On observe quelques villes avec un indice de développement très faible, autour de 0,5. 
                                                Ces villes pourraient être considérées comme des outliers.
                                              "), collapsible = T
                                            )
                                          ),
                                          column(6,
                                          box(
                                            width=12,
                                            plotOutput("box_city_dev") %>% withSpinner(type = 7, color = "pink"),
                                            footer = HTML("
                                              <b>Observations clés:</b><br>
                                              <b>Asymétrie à droite:</b> on observe une Asymétrie positive, c'est-à-dire que la distribution est plus étirée vers les valeurs élevées (indice de développement élevé).<br>
                                              <b>Médiane élevée:</b> La médiane est située vers la partie supérieure de la boîte, 
                                              ce qui suggère que la plupart des villes ont un indice de développement élevé.<br>
                                              <b>Peu d'outliers:</b> Il y'a peu d'outliers, c'est-à-dire de valeurs très éloignées des autres comme on peut le voir aussi sur l'histogramme tout à gauche puis, sur la boîte à moustaches tout à gauche également.<br>
                                              <b>Homogénéité relative:</b> La longueur de la boîte indique une certaine homogénéité dans les valeurs de l'indice de développement pour la majorité des villes.<br>
                                              <b>Peu de disparités:</b> Les valeurs de l'indice de développement sont relativement concentrées autour de la médiane, 
                                              ce qui suggère une faible dispersion des valeurs.<br>
                                              <b>Quelques villes moins développées:</b> La présence de la moustache inférieure indique qu'il existe quelques villes 
                                              avec un indice de développement plus faible, mais elles sont en minorité.
                                            "), collapsible = T
                                          )
                                        )
                                      ),
                                      column(12,
                                        column(6, 
                                          box(
                                            width=12,
                                              plotOutput("hist_age")  %>% withSpinner(type = 4, color = "blue"),
                                              footer = HTML("
                                                <b>L'histogramme nous donne une vue d'ensemble de la distribution des âges dans l'échantillon. On peut observer ceci :</b><br>
                                                <b>Mode autour de 30 ans:</b> La fréquence est maximale pour les individus âgés d'environ 30 ans. 
                                                Cela suggère que ce groupe d'âge est le plus représenté dans l'échantillon.<br>
                                                <b>Distribution asymétrique à droite:</b> La distribution est plus étirée vers les âges plus élevés. 
                                                Cela signifie qu'il y a moins d'individus âgés que d'individus jeunes.<br>
                                                <b>Répartition assez large:</b> Les âges sont répartis sur une large plage, allant d'environ 20 à 80 ans.<br>
                                                <b>Groupe jeune:</b> L'échantillon est principalement composé d'individus jeunes et d'âge moyen.<br>
                                                <b>Vieillissement de la population:</b> La diminution progressive de la fréquence avec l'âge suggère un vieillissement de la population.
                                              "), collapsible = T
                                            )
                                          ),
                                          column(6,
                                          box(
                                            width=12,
                                            plotOutput("box_age") %>% withSpinner(type = 7, color = "pink"),
                                            footer = HTML("
                                              <b>L'analyse de la boîte à moustaches nous permet de dire :</b><br>
                                              <b>Médiane autour de 37 ans :</b> La ligne centrale de la boîte, représentant la médiane, est située autour de 37 ans. 
                                              Cela signifie que la moitié des individus ont moins de 37 ans et l'autre moitié en a plus.<br>
                                              <b>Intervalle interquartile relativement étroit :</b> La boîte est assez courte, 
                                              ce qui indique que la moitié centrale des données (entre Q1 et Q3) est concentrée sur une plage d'âge relativement restreinte.<br>
                                              <b>Asymétrie légère à droite :</b> La moustache supérieure est légèrement plus longue que la moustache inférieure, 
                                              suggérant une légère asymétrie positive. Cela signifie qu'il y a quelques individus plus âgés que la moyenne, mais moins d'individus beaucoup plus jeunes.<br>
                                              <b>Un outlier :</b> Le point isolé à droite de la moustache supérieure représente un individu dont l'âge est considéré comme une valeur aberrante (outlier), étant nettement supérieur à la majorité des autres individus.<br>
                                              <b>Résumé :</b> L'âge moyen de la population étudiée semble être autour de 37 ans.<br>
                                              <b>Homogénéité :</b> La majorité des individus sont concentrés autour de l'âge moyen, avec une dispersion relativement faible.<br>
                                              <b>Quelques individus plus âgés :</b> Il y a quelques personnes plus âgées, mais elles représentent une minorité.
                                            "), collapsible = T
                                          )
                                        ),
                                        column(12,
                                          p("En combinant l information des deux graphiques, on peut affirmer que :
                                            La majorité des individus ont entre 30 et 50 ans.
                                            Il y a une dispersion assez importante des âges, avec une queue de distribution à droite.
                                            La présence d'un outlier suggère qu'il pourrait y avoir des sous-groupes d'âges différents dans l'échantillon."
                                          )
                                        )
                                      ),
                                      column(12,
                                        column(6, 
                                          box(
                                            width=12,
                                              plotOutput("hist_hours_per_week")  %>% withSpinner(type = 4, color = "blue"),
                                              footer = 
                                              HTML("
                                                <b>L'histogramme nous donne une vision d'ensemble de la distribution du nombre d'heures travaillées par semaine dans l'échantillon. On peut observer que :</b><br>
                                                <b>Distribution asymétrique à droite :</b> La majorité des individus travaillent moins d'heures, 
                                                et la queue de distribution s'étend vers les nombres d'heures plus élevés. Cela signifie qu'il y a plus de personnes 
                                                travaillant un nombre d'heures faible ou moyen dans l'échantillon.<br>
                                                <b>Pic de la distribution autour de 40 heures :</b> Le nombre d'heures le plus fréquent dans l'échantillon est d'environ 40 heures.<br>
                                                <b>Étendue des heures travaillées :</b> Les nombres d'heures travaillées se situent entre environ 20 et 100 heures, 
                                                ce qui donne une idée de la diversité des horaires dans l'échantillon.<br>
                                                <b>Résumé :</b> La distribution est asymétrique à droite, ce qui indique une plus grande proportion d'individus travaillant à temps plein ou à temps partiel, 
                                                et moins d'individus effectuant de nombreuses heures supplémentaires. Cela reflète une diversité dans les contrats de travail et les modes de vie des individus de l'échantillon.
                                              "), collapsible = T
                                            )
                                          ),
                                          column(6,
                                          box(
                                            width=12,
                                            plotOutput("box_hours_per_week") %>% withSpinner(type = 7, color = "pink"),
                                            footer = HTML("
                                              <b>Le boxplot nous fournit des informations plus précises sur les quantiles de la distribution, notamment :</b><br>
                                              <b>Médiane autour de 40 heures :</b> La ligne centrale de la boîte, représentant la médiane, est située autour de 40 heures. 
                                              Cela signifie que la moitié des individus travaillent moins de 40 heures par semaine et l'autre moitié en travaille plus.<br>
                                              <b>Intervalle interquartile relativement étroit :</b> La boîte est assez courte, 
                                              ce qui indique que la moitié centrale des données (entre Q1 et Q3) est concentrée sur une plage horaire relativement restreinte.<br>
                                              <b>Asymétrie légère à droite :</b> La moustache supérieure est légèrement plus longue que la moustache inférieure, 
                                              suggérant une légère asymétrie positive. Cela signifie qu'il y a quelques individus travaillant plus d'heures que la moyenne, 
                                              mais moins d'individus travaillant beaucoup moins d'heures.<br>
                                              <b>Outliers :</b> Les points isolés à gauche et à droite de la boîte représentent des individus 
                                              dont le nombre d'heures travaillées est considéré comme une valeur aberrante (outlier), étant nettement inférieur ou supérieur à la majorité des autres individus.
                                            "), collapsible = T
                                          )
                                        ),
                                        column(12,
                                          p("En combinant l information des deux graphiques, on peut affirmer que :
                                            La majorité des individus travaillent entre 30 et 50 heures par semaine.
                                            Il y a une dispersion assez importante du nombre d heures travaillées, avec une queue de distribution à droite.
                                            La présence de plusieurs outliers suggère qu il pourrait y avoir des sous-groupes d individus travaillant des nombres d heures très différents."
                                          )
                                        )
                                        ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("hist_experience") %>% withSpinner(type = 5, color = "green"),
                                         footer = HTML("
                                            La distribution des années d'expérience semble être bimodale, c'est-à-dire qu'il y a deux pics principaux.<br>
                                            - <b>Premier pic</b> : Se situe autour de 4 à 6 ans d'expérience, avec des fréquences allant jusqu'à environ 20 individus. Cela pourrait indiquer un groupe de candidats relativement nouveaux (recrutement important) dans leur domaine, potentiellement des jeunes professionnels ou des personnes ayant récemment changé de carrière.<br>
                                            - <b>Second pic</b> : Très marqué à 20 ans d'expérience, avec une fréquence d'environ 35 individus. Ce pic pourrait être dû à une limite supérieure arbitraire dans la collecte des données ou à un effet de plafond où les individus ayant plus de 20 ans d'expérience sont tous regroupés dans cette catégorie. Cela pourrait représenter des candidats très expérimentés ou des seniors dans leur domaine.<br>
                                            - <b>Entre les deux pics</b> : La fréquence diminue puis augmente légèrement autour de 10 à 12 ans d'expérience avant de diminuer à nouveau. Cela suggère une distribution plus dispersée des candidats ayant une expérience intermédiaire. La distribution est assez dispersée entre 6 et 19 ans d'expérience, avec des fréquences généralement plus faibles. Cela pourrait indiquer une variété de parcours professionnels et de niveaux d'expérience parmi les candidats.
                                          "), collapsible = T
                                        )
                                      ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("hist_training_hours") %>% withSpinner(type = 6, color = "red"),
                                         footer = HTML(" Lorsqu'on analyse cet histogramme, on observe les faits suivants : <br>
                                            <b>Distribution :</b><br>
                                            - <b>Forme de la distribution</b> : La distribution est asymétrique à droite (ou positivement asymétrique), ce qui signifie que la majorité des employés ont suivi un nombre relativement faible d'heures de formation, tandis que quelques employés ont suivi un nombre beaucoup plus élevé d'heures de formation.<br>
                                            - <b>Pic de la distribution</b> : Le pic de la distribution se situe autour de 20 à 40 heures de formation, avec une fréquence maximale d'environ 50 employés.<br>
                                            - <b>Queue de la distribution</b> : La queue de la distribution s'étend vers les nombres d'heures plus élevés, avec des fréquences décroissantes. Cela indique que moins d'employés ont suivi un nombre élevé d'heures de formation.<br>
                                            <br>
                                            <b>Observations :</b><br>
                                            - <b>Concentration des faibles heures de formation</b> : Il y a une concentration notable d'employés ayant suivi entre 0 et 60 heures de formation. Cela pourrait indiquer que la plupart des employés ont suivi une formation de base ou minimale.<br>
                                            - <b>Fréquence décroissante</b> : À mesure que le nombre d'heures de formation augmente, la fréquence des employés diminue progressivement. Cela montre que moins d'employés ont suivi des formations plus longues.<br>
                                            - <b>Heures de formation élevées</b> : Un petit nombre d'employés ont suivi plus de 200 heures de formation, ce qui pourrait représenter des formations spécialisées ou avancées.<br>
                                            <br>
                                            <b>Interprétation :</b><br>
                                            - <b>Formation de base majoritaire</b> : La majorité des employés ont suivi une formation de base, ce qui pourrait être suffisant pour leurs rôles actuels ou indiquer un besoin de formation continue pour certains employés.<br>
                                            - <b>Formations avancées minoritaires</b> : Les formations plus longues et avancées sont suivies par une minorité d'employés, ce qui pourrait indiquer des besoins spécifiques ou des opportunités de développement professionnel pour certains rôles.<br>
                                            - <b>Diversité des besoins de formation</b> : La distribution asymétrique suggère une diversité des besoins de formation parmi les employés, avec une majorité ayant des besoins de formation modérés et une minorité ayant des besoins de formation plus importants.
                                          "), collapsible = T
                                       )
                                      ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("box_training_hours") ,
                                         footer = HTML("On observe que : <br>
                                        - <b>Médiane</b> : La ligne noire verticale à l'intérieur de la boîte représente la médiane des heures de formation, qui se situe autour de 50 heures. Cela signifie que 50% des employés ont suivi 50 heures de formation ou moins, et les 50% restants ont suivi plus de 50 heures de formation.<br>
                                        - <b>Quartiles</b> : La boîte elle-même représente l'intervalle interquartile (IQR), c'est-à-dire la plage entre le premier quartile (25e percentile) et le troisième quartile (75e percentile). Ici, l'IQR va d'environ 25 à 75 heures. Cela indique que la moitié centrale des données se situe dans cette plage.<br>
                                        - <b>Moustaches</b> : Les lignes horizontales (moustaches) s'étendent des quartiles aux valeurs minimales et maximales, à l'exclusion des valeurs aberrantes. Elles montrent la dispersion des données. Dans ce cas, les moustaches s'étendent de 0 à environ 175 heures.<br>
                                        - <b>Valeurs aberrantes</b> : Les points individuels en dehors des moustaches représentent les valeurs aberrantes. Ici, il y a plusieurs valeurs aberrantes au-delà de 150 heures, allant jusqu'à environ 300 heures.<br>
                                        <br>
                                        <b>Observations :</b><br>
                                        - <b>Distribution centrale</b> : La majorité des employés ont suivi entre 25 et 75 heures de formation, ce qui correspond à l'IQR. Cela montre que la plupart des employés ont une formation modérée.<br>
                                        - <b>Symétrie</b> : Le boxplot semble relativement symétrique autour de la médiane, indiquant une distribution assez équilibrée des heures de formation. Cela suggère qu'il n'y a pas de biais significatif vers des formations très courtes ou très longues.<br>
                                        - <b>Étendue</b> : Les moustaches s'étendent de 0 à environ 150 heures, montrant que l'échantillon couvre une large gamme d'heures de formation. Cela indique une diversité dans les niveaux de formation parmi les employés.<br>
                                        - <b>Valeurs aberrantes</b> : La présence de valeurs aberrantes au-delà de 150 heures indique que certains employés ont suivi des formations beaucoup plus longues que la majorité. Cela pourrait représenter des formations spécialisées ou avancées.<br>
                                        <br>
                                        <b>Interprétation :</b><br>
                                        - <b>Formation moyenne</b> : La médiane à environ 50 heures suggère que la moitié des employés ont suivi 50 heures de formation ou moins, et l'autre moitié a suivi plus de 50 heures de formation. Cela indique une formation moyenne équilibrée parmi les employés.<br>
                                        - <b>Variabilité</b> : L'IQR de 25 à 75 heures indique que la majorité des employés ont une formation dans cette plage, avec une variabilité modérée. Cela montre que, bien que les niveaux de formation varient, ils restent dans une plage raisonnable.<br>
                                        - <b>Formations spécialisées</b> : Les valeurs aberrantes au-delà de 150 heures suggèrent que certains employés ont suivi des formations beaucoup plus longues et potentiellement spécialisées. Cela pourrait indiquer des besoins spécifiques ou des opportunités de développement professionnel pour certains rôles.

                                        "), collapsible = T
                                       )
                                      ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("box_experience") %>% withSpinner(type = 8, color = "#333333"),
                                         footer = HTML("
                                          <b>Nous faisons les observations suivantes :</b><br>
                                          - <b>Distribution centrale</b> : La majorité des individus ont entre 5 et 15 ans d'expérience. Cela montre que la plupart des candidats ont une expérience modérée.<br>
                                          - <b>Symétrie</b> : Le boxplot semble relativement symétrique autour de la médiane, indiquant une distribution assez équilibrée des années d'expérience. Cela suggère qu'il n'y a pas de biais significatif vers des expériences très faibles ou très élevées.<br>
                                          - <b>Étendue</b> : Les moustaches s'étendent de 0 à environ 20 ans, montrant que l'échantillon couvre une large gamme d'expériences professionnelles. Cela indique une diversité dans les niveaux d'expérience parmi les candidats.<br>
                                          <br>
                                          <b>Interprétation :</b><br>
                                          - <b>Expérience moyenne</b> : La médiane à environ 8 ans suggère que près de la moitié des individus ont 8 ans d'expérience ou moins, et l'autre moitié a plus de 8 ans d'expérience. Cela indique une expérience moyenne équilibrée parmi les candidats.<br>
                                          - <b>Variabilité</b> : L'IQR de 5 à 15 ans indique que la majorité des individus ont une expérience dans cette plage, avec une variabilité modérée. Cela montre que, bien que les niveaux d'expérience varient, ils restent dans une plage raisonnable.<br>
                                          - <b>Absence de valeurs aberrantes</b> : L'absence de points en dehors des moustaches suggère que les données sont relativement homogènes, sans expériences extrêmement faibles ou élevées par rapport au reste de l'échantillon. Cela renforce l'idée d'une distribution équilibrée des années d'expérience.
                                          "), collapsible = T
                                       )
                                      )
                                     ),
                                     
                                     br(),
                                     fluidRow(box(width = 12, title = "Variables Qualitatives",
                                       column(12, 
                                          h4("Statistiques des Variables Qualitatives"),
                                          wellPanel("Statistiques des Variables Qualitatives",
                                          verbatimTextOutput("summary_qual") %>% withSpinner(type = 4, color = "grey")
                                        )         
                                       )
                                      )
                                     ),
                                     fluidRow(
                                       box(width=12,plotOutput("dist2",height = 300),
                                           footer="Certaines colonnes, comme gender, major_discipline et company_size, présentent un nombre significatif de valeurs manquantes. Cela signifie qu\'une grande partie des lignes de données ne contient pas d\'information pour ces colonnes spécifiques.
  Colonnes avec peu de valeurs manquantes: À l'inverse, d'autres colonnes comme experience, enrolled_university ont très peu de valeurs manquantes. Ces colonnes sont donc plus complètes et pourraient être plus fiables pour l'analyse.",collapsible = T)
                                     ),
                                     fluidRow(
                                       column(6,
                                        box(width=12,plotOutput("bar_workclass",height = 300),
                                           footer="La majorité des candidats travaillent dans le secteur privé, avec une fréquence nettement plus élevée que les autres secteurs. Quelques candidats sont des travailleurs indépendants incorporés. On observe aussi qu'il y a quelques candidats dont le secteur d'emploi n'est pas spécifié (valeurs manquantes, ?) nous allons nous en occuper dans le prétraitement des données.<br>
                                            Le secteur privé domine largement, indiquant que la plupart des candidats travaillent dans des entreprises privées. Les autres secteurs (travailleurs indépendants, gouvernements d'État, fédéral et local) sont nettement moins représentés.",collapsible = T)
                                       ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("bar_gender") ,
                                         footer = "La majorité des individus de l'échantillon sont de genre masculin(plus de 70% des candidats de notre dataset.), ce qui montre un déséquilibre entre les genres. Cela pourrait indiquer que les métiers de la data science sont encore dominés par les hommes.
  Ce déséquilibre pourrait influencer les analyses si le genre est un facteur significatif dans les décisions de changement d'emploi ou d'autres comportements. ", collapsible = T
                                       )
                                      ),
                                       column(6,
                                          box(
                                            width=12,
                                            plotOutput("bar_university") ,
                                            footer = "La plupart des candidats ne sont pas inscrits dans une université ('no_enrollment'), tandis qu'une minorité suit des cours à temps plein ou à temps partiel.
  Cela pourrait indiquer que la majorité des candidats sont déjà sur le marché du travail plutôt qu'en formation. Il serait intéressant de voir si l’inscription (ou non) est liée à leur désir de changement de poste.", collapsible = T
                                              )
                                        ),
                                       column(6,
                                          box(
                                            width=12,
                                            plotOutput("bar_education") ,
                                            footer = "Le plus grand groupe de candidats a un diplôme de niveau 'Graduate', suivi des titulaires d\'un 'Master', avec très peu de Primary School.
  La répartition montre que les postes en data science semblent majoritairement accessibles avec un diplôme de niveau licence ou master, tandis que Primary School reste rare, ce qui pourrait être pertinent pour des postes plus spécialisés ou en recherche.", collapsible = T
                                          )
                                        ),
                                       column(6,
                                        box(
                                          width=12,
                                          plotOutput("bar_major") ,
                                          footer = "La majorité des candidats viennent des domaines STEM (sciences, technologie, ingénierie et mathématiques), ce qui est attendu dans le domaine de la data science.
  Les autres disciplines, comme les arts, le commerce et les humanités, sont peu représentées. Cela peut limiter la diversité des perspectives dans le domaine, même si les profils non-STEM apportent souvent des compétences complémentaires utiles dans certains aspects de l'analyse de données.", collapsible = T
                                        )
                                      )
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot4",height = 500),
                                           footer="Environ 59% (soit 119 candidats) des candidats ne sont pas actuellement inscrits a l'universite. on peut supposer que la plupart des candidats ont deja obtenu leur diplome universitaire. Les 40 % restants sont partages par des cours 
                      a temps plein et des cours a temps partiel. Enfin, on note aussi la présence des valeurs manquantes mais, nous alons les gérer dans le prétraitement, peut-être en remplçant par la valeur la plus fréquente, ou en supprimant les lignes ou les colonnes les contenant en fonction de leur impact.",
                                           collapsible = T)
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot5",height = 500),
                                           footer="Le graphe nous indique que plus de 60% (123 candidats) des candidats ont un diplome de premier cycle (Graduate),
                  les candidats ayant un master represente 20%. On note donc, une dominance du niveau Graduate: La majorité des individus dans l'échantillon possèdent un diplôme de baccalauréat.
Faible représentation des diplômés de master: Le nombre d'individus ayant un diplôme de master est nettement inférieur à celui des diplômés de baccalauréat. Et les valeurs manquantes que nous allons gérer dans le prétraitement.",collapsible = T)
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot6",height = 500),
                                           footer="les etudes lies au domaine d'etudes STEM ( sciences, technologie, ingenierie et mathematiques) est le domaine le plus populaire chez les candidats masculins et feminins. On peut donc dire que les data scientists suivent beaucoup de formations dans ce domaine, il ouvre les portes aux emplois.  ",collapsible = T)
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=6,plotOutput("plot17",height = 500) %>% withSpinner(type = 1, color = "yellow"),
                                          footer=
                                          HTML(
                                            "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                              <p>Ce graphe montre la répartition des candidats en fonction de la taille de l'entreprise dans laquelle ils travaillent. On constate  : </p><br>
                                              <b>Dominance des petites et moyennes entreprises :</b> La majorité des entreprises de l'échantillon sont de petite ou moyenne taille (moins de 1000 employés).<br>
                                              <b>Quelques grandes entreprises :</b> Il existe un nombre non négligeable de grandes entreprises (10 000 employés et plus).<br>
                                              <b>Hétérogénéité de la distribution :</b> La distribution des tailles d'entreprises est assez hétérogène, avec des pics pour certaines tailles et des creux pour d'autres.<br><br>
                                              <b>Conclusions préliminaires :</b><br>
                                              <ul>
                                                <li><b>Structure entrepreneuriale diversifiée :</b> L'échantillon étudié présente une structure entrepreneuriale variée, allant des très petites entreprises aux grandes entreprises.</li>
                                                <li><b>Importance des PME :</b> Les petites et moyennes entreprises sont les plus représentées dans l'échantillon.</li>
                                                <li><b>Valeurs manquantes :</b> On constate qu'elles occupent la grande majorité et donc, dans le prétraitement nous n'opterons pas pour la suppression des lignes, sinon on perd un quantité trop importante de données nous allons remplacer ces valeurs manquantes pas la valeur la plus fréquente. A noter qu'au niveau du prétraitement, on aura la possibilité de supprimer, ou de remplacer ces valeurs.</li>
                                                <li><b>NA :</b> On note la présence des valeurs manquantes, nous allons nous en occuper dans le prétraitement.</li>
                                              </ul>
                                            </div>"
                                          ),collapsible = T
                                        ),
                                       box(width=6,plotOutput("plot18",height = 500),
                                           footer=HTML(
                                            "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                              <b>Observations clés :</b><br>
                                              <ul>
                                                <li><b>Dominance des entreprises privées :</b> Les entreprises privées (Pvt Ltd et Ltd) représentent la majorité de l'échantillon.</li>
                                                <li><b>Faible représentation des autres types :</b> Les autres types d'entreprises (Early Stage Startup, Funded Startup, NGO, Other, Public Sector) sont moins nombreux.</li>
                                                <li><b>Hétérogénéité de la distribution :</b> La distribution des types d'entreprises est très hétérogène, avec une forte dominance de certaines catégories.</li>
                                              </ul><br>
                                              <b>Conclusions préliminaires :</b><br>
                                              <ul>
                                                <li><b>Échantillon majoritairement privé :</b> L'échantillon est principalement composé d'entreprises privées, de différentes tailles et maturités.</li>
                                                <li><b>Peu de structures publiques ou à but non lucratif :</b> Les organisations publiques et les ONG sont sous-représentées dans l'échantillon.</li>
                                                <li><b>Valeurs manquantes :</b> On constate qu'elles sont aussi en grande quantité, dans le prétraitement on aura la possibilité de supprimer, ou de remplacer ces valeurs.</li>
                                              </ul>
                                            </div>"
                                          ),collapsible = T
                                        ),
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot19",height = 500),
                                           footer="Beaucoup de candidats ont deja plus de 20 ans d'experience. Cependant, un bon nombre de candidats  ont une experience qui varie entre 2 a 7 ans.",
                                           
                                           collapsible = T)
                                     ),
                                     fluidRow(
                                       box(width=12,plotOutput("plot20",height = 500),
                                           footer="La grande majorité des candidats ont un an d'ecart entre leur emploi precedent et leur emploi actuel, ça pourrait aussi expliquer la facilité à trouver l'emploi.ensuite, on note aussi que plusieurs candidats ont une difference de plus de
                                                      4 ans entre leur emploi precedent et leur emploi actuel."
                                           ,collapsible = T)
                                     ),
                                     fluidRow(
                                       box(width=12,plotOutput("plot21",height = 500),
                                           footer="La plupart des candidats sont issus d'une ville qui a un indice de developpement aux alentours de 0,9."
                                           ,collapsible = T)
                                     ),
                                     fluidRow(
                                       box(width=12,plotOutput("plot22",height = 500),
                                          footer = HTML(
                                            "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                              <b>Observations clés :</b><br>
                                              <ul>
                                                <li><b>Pic de densité pour les deux groupes autour de 0-50 heures :</b> La majorité des candidats, qu'ils aient réussi ou non, semblent avoir suivi entre 0 et 50 heures de formation.</li>
                                                <li><b>Queue de distribution plus longue pour les candidats ayant réussi :</b> La courbe de densité des candidats ayant réussi s'étend plus loin vers la droite, indiquant qu'un nombre non négligeable d'entre eux ont suivi plus de 100 heures de formation.</li>
                                                <li><b>Superposition des courbes :</b> Les deux courbes se superposent en grande partie, suggérant qu'il n'y a pas de différence très marquée en termes de nombre d'heures de formation entre les deux groupes.</li>
                                              </ul><br>
                                              <b>Conclusions préliminaires :</b><br>
                                              <ul>
                                                <li><b>Importance des premières heures de formation :</b> Les premières heures de formation semblent cruciales, car la majorité des candidats, quel que soit leur résultat final, se concentrent sur cette tranche.</li>
                                                <li><b>Pas de corrélation directe entre le nombre d'heures et la réussite :</b> Bien qu'il y ait quelques candidats ayant réussi avec un nombre élevé d'heures de formation, la réussite ne semble pas être directement proportionnelle au nombre d'heures. D'autres facteurs pourraient donc jouer un rôle.</li>
                                              </ul>
                                            </div>"
                                          )
                                          ,
                                          collapsible = T
                                        )
                                     )
                                    ),
                                    
                                    
                                    tabPanel("Analyse Bivariée",
                                     fluidRow(box(width = 12,box(width=12,
                                      h3("Corrélations entre Variables Quantitatives"),
                                       column(12, verbatimTextOutput("cor_matrix") %>% withSpinner(type = 4, color = "#FF5733"),
                                        HTML(
                                          "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                            <b>Analyse des Corrélations :</b><br>
                                            <ul>
                                              <li><b>city_development_index (Indice de développement de la ville) :</b>
                                                <ul>
                                                  <li><b>Faible corrélation avec experience</b> (<i>r = 0.21</i>) : Les candidats des villes à indice élevé pourraient avoir légèrement plus d'expérience.</li>
                                                  <li><b>Très faible corrélation avec les autres variables</b> (<i>r ≈ 0.03 − 0.06</i>) : Cet indice n'est pas un facteur déterminant pour ces variables.</li>
                                                </ul>
                                              </li>
                                              <li><b>experience (Expérience totale) :</b>
                                                <ul>
                                                  <li><b>Très faible corrélation avec training_hours</b> (<i>r = 0.02</i>) et <b>age</b> (<i>r = 0.02</i>) : Cela suggère que l'expérience professionnelle n'est pas directement liée au nombre d'heures de formation ou à l'âge des candidats.</li>
                                                  <li><b>Légère corrélation négative avec hours_per_week</b> (<i>r = −0.05</i>) : Cela indique que les candidats avec plus d'expérience travaillent légèrement moins d'heures, bien que l'effet soit faible.</li>
                                                </ul>
                                              </li>
                                              <li><b>training_hours (Heures de formation) :</b>
                                                <ul>
                                                  <li><b>Faible corrélation avec hours_per_week</b> (<i>r = 0.08</i>) : Cela suggère que les candidats ayant plus d'heures de formation travaillent aussi légèrement plus d'heures par semaine.</li>
                                                  <li><b>Corrélation modérée avec age</b> (<i>r = 0.13</i>) : Cela reflète que les candidats plus âgés ont tendance à suivre plus de formations.</li>
                                                </ul>
                                              </li>
                                              <li><b>hours_per_week (Heures travaillées par semaine) :</b>
                                                <ul>
                                                  <li><b>Corrélation modérée avec age</b> (<i>r = 0.15</i>) : Les candidats plus âgés ont tendance à travailler légèrement plus d'heures par semaine.</li>
                                                </ul>
                                              </li>
                                              <li><b>age (Âge du candidat) :</b>
                                                <ul>
                                                  <li><b>Corrélation faible ou très faible avec toutes les autres variables</b> (<i>r < 0.16</i>) : Cela montre que l'âge n'est pas fortement associé aux autres variables quantitatives dans cet ensemble.</li>
                                                </ul>
                                              </li>
                                            </ul><br>
                                            <b>Conclusion Générale :</b><br>
                                            <ul>
                                              <li>Les corrélations entre les variables quantitatives sont globalement faibles à modérées. Cela indique qu'il n'y a pas de relations fortes ou linéaires entre ces variables dans notre échantillon.</li>
                                            </ul>
                                          </div>"
                                        )
                                       ),collapsible = T),
                                       column(12, plotOutput("pairs_plot") %>% withSpinner(type = 3, color = "#3CB371")),
                                       box(width=12,
                                       column(12,
                                          HTML(
                                            "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                            <b>Analyse Visuelle de la matrice de dispersion :</b><br>
                                            <ul>
                                              <li><b>city_development_index :</b>
                                                <ul>
                                                  <li>Aucune relation claire ou tendance visible avec les autres variables.</li>
                                                  <li>La dispersion des points est relativement homogène, ce qui confirme les faibles corrélations avec d'autres variables comme experience, training_hours, ou hours_per_week.</li>
                                                </ul>
                                              </li>
                                              <li><b>experience :</b>
                                                <ul>
                                                  <li>Pas de tendance apparente avec training_hours, ce qui reflète une absence de relation linéaire notable.</li>
                                                  <li>Avec age, les points montrent une tendance légèrement croissante, ce qui est <b>logique car plus une personne est âgée, plus elle a de chances d'avoir accumulé de l'expérience</b>.</li>
                                                </ul>
                                              </li>
                                              <li><b>training_hours :</b>
                                                <ul>
                                                  <li>Aucune tendance forte avec les autres variables, y compris hours_per_week. Les points sont dispersés.</li>
                                                  <li>Avec age, une faible augmentation des heures de formation est visible pour certains groupes d'âge, mais les écarts restent faibles.</li>
                                                </ul>
                                              </li>
                                              <li><b>hours_per_week :</b>
                                                <ul>
                                                  <li>Légère augmentation visible en fonction de age : les personnes plus âgées semblent travailler un peu plus d'heures par semaine en moyenne.</li>
                                                </ul>
                                              </li>
                                              <li><b>age :</b>
                                                <ul>
                                                  <li>Les relations sont faibles avec les autres variables, bien que les tendances logiques (comme l'accumulation d'expérience avec l'âge) soient faiblement visibles.</li>
                                                </ul>
                                              </li>
                                            </ul><br>
                                            <b>Observations Générales :</b><br>
                                            <ul>
                                              <li>La matrice confirme les corrélations numériques faibles que nous avons calculées précédemment.</li>
                                              <li>Il n'y a pas de relation linéaire forte entre les variables quantitatives.</li>
                                              <li>Les variables sont relativement indépendantes les unes des autres, à l'exception de quelques tendances légères (par exemple, âge vs expérience).</li>
                                            </ul>
                                          </div>"
                                        )
                                       ),collapsible = T)
                                      )
                                     ),
                                     fluidRow(
                                      column(12,
                                          box(width=12, title="Analyse dynamique bivariée : Choisissez deux variables à analyser et le résultat se met à jour dynamiquement",
                                          HTML("<p style='color: red;> Cliquez sur le bouton 'Générer le graphique' pour voir les résultats</p>"),
                                          sidebarLayout(
                                            sidebarPanel(
                                              selectInput("var1", "Variable 1", choices = NULL),
                                              selectInput("var2", "Variable 2", choices = NULL),
                                              actionButton("plotBtn", "Générer le graphique"),
                                              p("CLIQUEZ SUR LE BOUTON POUR VOIR LES RESULTATS (les commentaires se mettent à jour). essayez par exemple Var1 = city_development_index et Var2 = age ou Var1 = age et Var2 = training_hours", style= "color: red;")
                                            ),
                                            mainPanel(
                                              plotOutput("bivariatePlot")
                                            )
                                          ),
                                          footer = textOutput("dynamic_footer_plot")
                                          )
                                        )
                                      ),
                                     fluidRow(
                                        column(12,
                                          box(width=12,
                                            column(6,
                                            sidebarLayout(
                                              sidebarPanel(
                                                checkboxGroupInput(
                                                "selected_vars", 
                                                "Variables numériques à inclure :",
                                                choices = NULL),
                                                selected = NULL
                                              ),
                                              actionButton("corBtn", "Afficher la matrice")
                                            )),
                                            column(6,
                                              selectInput("method", "Méthode de corrélation :", 
                                                          choices = c("Pearson", "Spearman"), 
                                                          selected = "Pearson"),
                                              mainPanel(
                                                plotOutput("correlationPlot")
                                              )
                                            ),
                                            footer = textOutput("dynamic_footer")
                                          )
                                        )
                                      ),
                                     h3("Quantitatif vs Qualitatif"),
                                     fluidRow(box(width = 12,
                                       column(6,
                                              box(
                                                width=12,
                                                plotOutput("box_experience_gender") %>% withSpinner(type = 5, color = "#FFD700"),
                                                footer = "Graphique (Boxplot) : Ce graphique montre la répartition des années d'expérience pour chaque genre. On peut noter que la médiane de l'expérience est légèrement plus élevée pour les femmes que pour les hommes et les autres genres.
                                                  Les boîtes ont à peu près la même largeur pour les trois groupes, ce qui indique une dispersion des données relativement similaire. Il n'y a pas de valeurs aberrantes très éloignées des boîtes, ce qui suggère que les données sont relativement homogènes.", collapsible = T
                                              )
                                       ),
                                       column(6,
                                          box(
                                            width=12,
                                            plotOutput("box_city_dev_education") %>% withSpinner(type = 7, color = "#FF00FF"),
                                            footer = "
                                            Les niveaux d'éducation 'High School', 'Primary School' et 'Masters' présentent des médianes presque similaires, indiquant que les candidats avec ces niveaux d'éducation tendent à vivre dans des villes avec un indice de développement un peu moins élevé par rapport au niveau Graduate.
Le niveau d'éducation 'Graduate' présente une médiane légèrement supérieure, suggérant que les candidats avec ce niveau d'éducation vivent dans des villes avec un indice de développement plus élévé.", collapsible = T
                                          )
                                        ),
                                       column(6,
                                              
                                        box(
                                            width=12,
                                            plotOutput("box_training_education") %>% withSpinner(type = 8, color = "#000080"),
                                            footer = 
                                              HTML("<h3>Heures de Formation :</h3>
                                                <ul>
                                                  <li>Les candidats avec un niveau d\'éducation 'Masters', 'Graduate' et 'High School' tendent à suivre un nombre médian d\'heures de formation similaire (environ 50 heures), mais avec une variabilité plus grande pour les diplômés de masters.</li>
                                                  <li>Les candidats avec un niveau d\'éducation 'Primary School' tendent à suivre un nombre médian d\'heures de formation très faible (environ 5 heures), avec une variabilité élévée</li>
                                                </ul>
                                                <h3>Variabilité :</h3>
                                                <ul>
                                                  <li>Il y a une variabilité notable dans les heures de formation pour tous les niveaux d\'éducation, avec des valeurs aberrantes indiquant que certains candidats suivent beaucoup plus d\'heures de formation que la médiane.</li>
                                                </ul>"
                                              )
                                            , collapsible = T
                                          )
                                        ),
                                       column(6,
                                              
                                        box(
                                            width=12,
                                            verbatimTextOutput("anova_results") %>% withSpinner(type = 2, color = "#C0C0C0"),
                                            footer = HTML(
                                              "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                                <b>Analyse des Tests Statistiques :</b><br>
                                                <ul>
                                                  <li><b>city_development_index vs gender :</b>
                                                    <ul>
                                                      <li><b>p-value</b> : 0.131</li>
                                                      <li>La p-value est supérieure à 0.05, ce qui suggère qu'il n'y a pas de relation statistiquement significative entre l'indice de développement urbain et le sexe.</li>
                                                    </ul>
                                                  </li>
                                                  <li><b>experience vs relevant_experience :</b>
                                                    <ul>
                                                      <li><b>p-value</b> : 1.03e-10</li>
                                                      <li>La p-value est très faible, ce qui indique une relation statistiquement significative entre l'expérience et l'expérience pertinente.</li>
                                                    </ul>
                                                  </li>
                                                  <li><b>training_hours vs education_level :</b>
                                                    <ul>
                                                      <li><b>p-value</b> : 0.88</li>
                                                      <li>La p-value est supérieure à 0.05, ce qui suggère qu'il n'y a pas de relation statistiquement significative entre les heures de formation et le niveau d'éducation.</li>
                                                    </ul>
                                                  </li>
                                                  <li><b>hours_per_week vs marital_status :</b>
                                                    <ul>
                                                      <li><b>p-value</b> : 1.23e-05</li>
                                                      <li>La p-value est très faible, ce qui indique une relation statistiquement significative entre le nombre d'heures travaillées par semaine et le statut marital.</li>
                                                    </ul>
                                                  </li>
                                                  <li><b>age vs workclass :</b>
                                                    <ul>
                                                      <li><b>p-value</b> : 0.000117</li>
                                                      <li>La p-value est très faible, ce qui indique une relation statistiquement significative entre l'âge et la catégorie socioprofessionnelle.</li>
                                                    </ul>
                                                  </li>
                                                </ul>
                                                <b>Résumé :</b>
                                                <ul>
                                                  <li>En résumé, seuls les tests impliquant <b>experience vs relevant_experience</b>, <b>hours_per_week vs marital_status</b> et <b>age vs workclass</b> montrent des relations statistiquement significatives, tandis que les autres comparaisons ne le sont pas.</li>
                                                </ul>
                                              </div>"
                                            )
                                            , collapsible = T
                                          )
                                        )
                                      )
                                     ),
                                     fluidRow(box(width = 12,
                                      h3("Corrélations entre Variables Qualitatives"),
                                       column(6, 
                                              box(
                                                width=12,
                                                verbatimTextOutput("contingency_gender_university") %>% withSpinner(type = 1, color = "#C0C0C0"),
                                                footer = "La table de contingence montre les effectifs par genre et par statut d'inscription à l'université (cours à temps plein, pas d'inscription, cours à temps partiel), on voit qu'il y a bien plus de personnes qui ne sont pas inscrites.", collapsible = T
                                              )
                                        ),
                                       column(6, 
                                          box(
                                            width=12,
                                            verbatimTextOutput("chi2_gender_university") ,
                                            footer = HTML(
                                              "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                                <b>Test du Chi Carré de Pearson :</b><br>
                                                <ul>
                                                  <li><b>Degrés de liberté (df)</b> : 4</li>
                                                  <li><b>p-value</b> : 0.2281</li>
                                                </ul>
                                                <b>Interprétation :</b>
                                                <ul>
                                                  <li>La p-value est supérieure à 0.05, ce qui indique qu'il n'y a pas de relation statistiquement significative entre le sexe (<b>gender</b>) et le statut d'inscription à une université (<b>enrolled_university</b>).</li>
                                                  <li>En d'autres termes, on ne peut pas rejeter l'hypothèse nulle selon laquelle les deux variables sont indépendantes.</li>
                                                </ul>
                                              </div>"
                                            ), collapsible = T
                                          )
                                        ),
                                       column(6, 
                                              box(
                                                width=12,
                                                verbatimTextOutput("contingency_gender_education") ,
                                                footer = "La table de contingence indique les effectifs par genre et niveau d'éducation (Graduate, Masters, Primary School). et on voit qu'il y a bien plus d'éffectifs ayant un niveau d'édcution 'graduate'", collapsible = T
                                              )
                                        ),
                                       column(6,
                                              box(
                                                width=12,
                                                verbatimTextOutput("chi2_gender_education") ,
                                                footer = HTML(
                                                  "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                                    <b>Test du Chi Carré de Pearson :</b><br>
                                                    <ul>
                                                      <li><b>Degrés de liberté (df)</b> : 6</li>
                                                      <li><b>p-value</b> : 0.7308</li>
                                                    </ul>
                                                    <b>Interprétation :</b>
                                                    <ul>
                                                      <li>La p-value est supérieure à 0.05, ce qui indique qu'il n'y a pas de relation statistiquement significative entre le sexe (<b>gender</b>) et le niveau d'éducation (<b>education_level</b>).</li>
                                                      <li>En d'autres termes, l'hypothèse nulle (selon laquelle les deux variables sont indépendantes) ne peut pas être rejetée, ce qui suggère que ces deux variables sont indépendantes l'une de l'autre dans notre échantillon de données.</li>
                                                    </ul>
                                                  </div>"
                                                )
                                                , collapsible = T
                                              )
                                       ),
                                       column(6,
                                            box(
                                              width=12,
                                              verbatimTextOutput("last_new_job_marital_status") ,
                                              footer = HTML(
                                                "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                                  <b>Test du Chi Carré de Pearson avec p-value simulée :</b><br>
                                                  <ul>
                                                    <li><b>p-value</b> : 0.5432</li>
                                                  </ul>
                                                  <b>Interprétation :</b>
                                                  <ul>
                                                    <li>La p-value est supérieure à 0.05, ce qui suggère qu'il n'y a pas de relation statistiquement significative entre le dernier emploi occupé (<b>last_new_job</b>) et le statut marital (<b>marital_status</b>).</li>
                                                    <li>Ainsi, l'hypothèse nulle (selon laquelle les deux variables sont indépendantes) ne peut pas être rejetée. Cela signifie que, dans notre jeu de données, le statut marital et le dernier emploi occupé sont indépendants l'un de l'autre.</li>
                                                  </ul>
                                                </div>"
                                              )
                                              , collapsible = T
                                            )
                                          ),
                                       column(6,
                                            box(
                                              width=12,
                                              verbatimTextOutput("workclass_marital_status") ,
                                              footer = HTML(
                                                "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                                  <b>Test du Chi Carré de Pearson avec p-value simulée :</b><br>
                                                  <ul>
                                                    <li><b>Degrés de liberté (df)</b> : Non spécifiés (NA), calculés de manière simulée.</li>
                                                    <li><b>p-value</b> : 0.04598</li>
                                                  </ul>
                                                  <b>Interprétation :</b>
                                                  <ul>
                                                    <li>La p-value est inférieure à 0.05, ce qui indique une relation statistiquement significative entre la catégorie socioprofessionnelle (<b>workclass</b>) et le statut marital (<b>marital_status</b>).</li>
                                                    <li>En d'autres termes, l'hypothèse nulle (selon laquelle les deux variables sont indépendantes) peut être rejetée. Cela suggère que ces deux variables sont liées dans notre jeu de données.</li>
                                                  </ul>
                                                </div>"
                                              )
                                              , collapsible = T
                                            )
                                          ),
                                       column(6,
                                            box(
                                              width=12,
                                              verbatimTextOutput("relevent_experience_workclass") ,
                                              footer = HTML(
                                                "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                                  <b>Test du Chi Carré de Pearson avec p-value simulée :</b><br>
                                                  <ul>
                                                    <li><b>Degrés de liberté (df)</b> : Non spécifiés (NA), calculés de manière simulée.</li>
                                                    <li><b>p-value</b> : 0.3033</li>
                                                  </ul>
                                                  <b>Interprétation :</b>
                                                  <ul>
                                                    <li>La p-value est supérieure à 0.05, ce qui indique qu'il n'y a pas de relation statistiquement significative entre l'expérience pertinente (<b>relevant_experience</b>) et la catégorie socioprofessionnelle (<b>workclass</b>).</li>
                                                    <li>L'hypothèse nulle (selon laquelle les deux variables sont indépendantes) ne peut pas être rejetée, ce qui suggère que ces deux variables sont indépendantes.</li>
                                                  </ul>
                                                </div>"
                                              )
                                              , collapsible = T
                                            )
                                          ),
                                       column(6,
                                            box(
                                              width=12,
                                              verbatimTextOutput("race_company_type") ,
                                              footer = HTML(
                                                "<div style='text-align: justify; font-size: 14px; line-height: 1.5;'>
                                                  <b>Test du Chi Carré de Pearson avec p-value simulée :</b><br>
                                                  <ul>
                                                    <li><b>Degrés de liberté (df)</b> : Non spécifiés (NA), calculés de manière simulée.</li>
                                                    <li><b>p-value</b> : 0.6442</li>
                                                  </ul>
                                                  <b>Interprétation :</b>
                                                  <ul>
                                                    <li>La p-value est supérieure à 0.05, ce qui indique qu'il n'y a pas de relation statistiquement significative entre la race (<b>race</b>) et le type d'entreprise (<b>company_type</b>).</li>
                                                    <li>L'hypothèse nulle (selon laquelle les deux variables sont indépendantes) ne peut pas être rejetée, ce qui suggère que ces deux variables sont indépendantes.</li>
                                                  </ul>
                                                </div>"
                                              )
                                              , collapsible = T
                                            )
                                          )
                                        )
                                      ),
                                     
                                     fluidRow(
                                        box(width=12,plotOutput("plot3",height = 300),
                                           footer="le but de ce graphe est d'esmiter le nombre de candidat ayant une experience pertinente et cela par leurs niveau d'education, on remarque qu'en globalite 
                                          pour chaque niveau d'etudes avoir une experience pertinente est plus frequent.",collapsible = T)
                                     )
                                    )
                                  )
                                )
                              ),
                              tabPanel(
                                "Prétraitement des données",
                                fluidRow(
                                  box(width = 12,
                                      #-----------------------------------------------#
                                      #--------------DEBUT AJOUT BOUTTONS A&R---------#
                                      #-----------------------------------------------#
                                        column(12,
                                          h4(style = "color: red;", "AFIN D'EVITER LES ALLERS-RETOURS LORSQU'ON FAIT DES ACTIONS DE PRETRAITEMENT SUR LE JEU DE DONNEES, NOUS ALLONS AFFICHER LE JEU DE DONNEES DIRECTEMENT DANS CETTE PAGE! CELA VOUS EVITE D'ALLER A L'ACCEUIL VOIR LES MISES A JOUR")
                                        ),
                                        br(),
                                        br(),
                                        column(12,
                                           box(width = 12 , title="Gérer les valeurs manquantes", column(12,
                                             # Bouton pour gérer les valeurs manquantes
                                             actionButton("dropAll" ,"Drop all missing values", icon("trash"),
                                                          class = "btn btn-sm btn-success"),
                                             
                                             actionButton("replaceAll" ,"Auto replace all missing values", icon("plus"),
                                                          class = "btn btn-sm btn-primary"),
                                             
                                             actionButton("resetAll" ,"Tout annuler", icon("sync"),
                                                          class = "btn btn-sm btn-danger"),
                                                          
                                             actionButton("dropAllOutliers" ,"Supprimier les Outliers", icon("trash"),
                                                          class = "btn btn-sm btn-info"),

                                             actionButton("reduceCenter" ,"Centrer et réduire", icon("sync"),
                                                          class = "btn btn-sm btn-warning")
                                              )
                                           )
                                        ),
                                      #-----------------------------------------------#
                                      #--------------FIN AJOUT BOUTTONS A&R-----------#
                                      #-----------------------------------------------#
                                      
                                      fluidRow(
                                        box(width = 12,dataTableOutput("table") %>% withSpinner(type = 2, color = "orange"),title="Data")
                                      )
                                  )
                                )
                              )
                            )
                          ),
                          tabPanel("Analyse factorielle et classification",
                           tabsetPanel(
                            tabPanel("ACP: anlyse en composantes principales",
                              fluidRow(box(width = 12,
                                 wellPanel(
                                  h5("Dans cette partie nous allons faire une analyse factorielle : nous allons implémenter une ACP et une CAH.",
                                    tags$br(),tags$br()
                                  ) 
                                 )
                                )
                              ),
                              
                              fluidRow(
                                column(12,
                                  column(7, 
                                    box("Importance des composantes principales issues de l'ACP",
                                      width=12,
                                      verbatimTextOutput("eigenvalues"),
                                      footer = HTML("
                                        <h3>Importance des composantes principales issues de l'ACP</h3>

                                        <h4>1. Interprétation des valeurs propres :</h4>
                                        <ul>
                                          <li>Les valeurs propres représentent l'inertie expliquée par chaque composante principale (<b>PC</b>).</li>
                                          <li>Une valeur propre supérieure à 1 indique qu'une composante explique plus de variance qu'une variable initiale unique.</li>
                                          <li>Ici, les trois premières composantes (<b>PC1</b>, <b>PC2</b>, et <b>PC3</b>) ont des valeurs propres > 1 :</li>
                                          <ul>
                                            <li><b>PC1</b> = 1.36</li>
                                            <li><b>PC2</b> = 1.11</li>
                                            <li><b>PC3</b> = 1.03</li>
                                          </ul>
                                          <li>Cela suggère qu'elles sont importantes pour résumer l'information du jeu de données.</li>
                                        </ul>

                                        <h4>2. Proportion de variance expliquée :</h4>
                                        <ul>
                                          <li>La colonne <b>Proportion_of_Variance</b> montre la part de variance totale expliquée par chaque composante :</li>
                                          <ul>
                                            <li><b>PC1</b> : 27,2 %</li>
                                            <li><b>PC2</b> : 22,1 %</li>
                                            <li><b>PC3</b> : 20,7 %</li>
                                          </ul>
                                          <li>Ces trois composantes expliquent ensemble environ 70 % de la variance totale.</li>
                                        </ul>

                                        <h4>3. Cumul de la variance expliquée :</h4>
                                        <ul>
                                          <li>La colonne <b>Cumulative_Proportion</b> montre le pourcentage cumulé de variance expliquée :</li>
                                          <ul>
                                            <li><b>PC1</b> + <b>PC2</b> + <b>PC3</b> = 70 % de la variance cumulée.</li>
                                            <li>Avec <b>PC4</b> et <b>PC5</b>, toute la variance est expliquée (100 %), mais elles ajoutent moins d'information significative.</li>
                                          </ul>
                                        </ul>

                                        <h4>4. Choix du nombre de composantes principales :</h4>
                                        <ul>
                                          <li>En général, on conserve les composantes qui :</li>
                                          <ul>
                                            <li>Expliquent environ 70 à 80 % de la variance cumulée.</li>
                                            <li>Ont des valeurs propres supérieures à 1.</li>
                                          </ul>
                                        </ul>

                                        <h4>Conclusion :</h4>
                                        <ul>
                                          <li>On peut justifier de conserver <b>PC1</b>, <b>PC2</b>, et <b>PC3</b> (avec une variance cumulative de 70 %).</li>
                                          <li>Elles contiennent l'essentiel de l'information.</li>
                                          <li>Réduire les dimensions à ces trois composantes est efficace et informatif.</li>
                                        </ul>
                                        "),
                                      collapsible = T
                                    )
                                  ),
                                  column(5,
                                    box("Pourcentage des variances expliquées par les composantes principales",
                                      width=12,
                                      verbatimTextOutput("kaiser_summary"),
                                      footer = "", collapsible = T
                                    ),
                                    box("Pourcentage des variances expliquées par les composantes principales",
                                      width=12,
                                      plotOutput("contribution_plot"),
                                      footer = "Cet histrogramme représente la visualisation graphique des pourcentages des variances expliquées par les quatres premières composantes principales.", collapsible = T
                                    )
                                  )
                                ),
                                column(12,
                                  column(4,
                                    # Sélecteur pour choisir une variable qualitative
                                    selectInput("qualitative_var",
                                      label = "Choisir une variable qualitative à inclure dans l'analyse",
                                      choices = NULL,
                                      selected = NULL
                                    ),
                                    box("Plans Factoriels",
                                      width=12,
                                      plotOutput("factorial_plan"),
                                      footer = HTML("
                                        <h3>1. Plans factorielles des individus</h3>

                                        <h4>Interprétation :</h4>
                                        <ul>
                                          <li>Ce graphique représente les individus projetés sur les deux premières dimensions principales (<b>Dim1</b> et <b>Dim2</b>).</li>
                                          <li>L’ellipse bleue montre la zone où se concentrent la majorité des individus.</li>
                                          <li>Les points correspondent aux individus, et leur position indique leur relation vis-à-vis des axes principaux.</li>
                                        </ul>

                                        <h4>Analyse :</h4>
                                        <ul>
                                          <li>Les individus proches du centre (origine) ont des valeurs moyennes sur toutes les variables.</li>
                                          <li>Les individus éloignés du centre dans une direction donnée sont fortement influencés par les variables corrélées à cette direction.</li>
                                        </ul>
                                        "),
                                      collapsible = T
                                    )
                                  ),
                                  column(4,
                                    box("Cercle de Corrélation", 
                                      width=12,
                                      plotOutput("correlation_circle") ,
                                      footer = HTML("
                                        <h3>2. Cercle de corrélation des variables</h3>

                                        <h4>Interprétation :</h4>
                                        <ul>
                                          <li>Ce graphique montre la contribution et la corrélation des variables d’origine avec les dimensions principales (<b>Dim1</b> et <b>Dim2</b>).</li>
                                          <li>Chaque flèche représente une variable, et sa longueur indique sa contribution.</li>
                                          <li>Les variables proches de la circonférence du cercle sont mieux représentées par les axes <b>Dim1</b> et <b>Dim2</b>.</li>
                                        </ul>

                                        <h4>Analyse :</h4>
                                        <ul>
                                          <li><b>Dim1 (27,2 %)</b> :</li>
                                          <ul>
                                            <li>Les variables fortement corrélées avec <b>Dim1</b> incluent probablement <i>city_development_index</i> et <i>experience</i>.</li>
                                            <li>Cela signifie que cette dimension peut refléter des aspects liés au développement urbain et à l'expérience.</li>
                                          </ul>
                                          <li><b>Dim2 (22,1 %)</b> :</li>
                                          <ul>
                                            <li><i>training_hours</i> semble contribuer davantage à cette dimension.</li>
                                            <li>Elle pourrait refléter un aspect lié à l'engagement dans la formation.</li>
                                          </ul>
                                          <li>Les flèches opposées (angles de 180°) indiquent des corrélations négatives. Par exemple, si <i>age</i> est opposé à <i>training_hours</i>, cela pourrait indiquer une relation négative entre ces deux variables.</li>
                                        </ul>
                                        "),
                                      collapsible = T
                                    )
                                  ),
                                  column(4,
                                    box("Graphiques des Individus et Variables", 
                                      width=12,
                                      plotOutput("individuals_variables") ,
                                      footer = HTML("
                                        <h3>3. Biplot</h3>

                                        <h4>Interprétation :</h4>
                                        <ul>
                                          <li>Ce graphique combine les deux précédents, superposant les individus et les variables.</li>
                                          <li>Il permet d'interpréter comment les variables influencent les regroupements des individus.</li>
                                        </ul>

                                        <h4>Analyse :</h4>
                                        <ul>
                                          <li>Les individus situés près d'une flèche (variable) sont fortement influencés par cette variable.</li>
                                          <li>Par exemple, des individus proches de la flèche de <i>training_hours</i> sont ceux ayant consacré plus de temps à leur formation.</li>
                                        </ul>
                                        "),
                                      collapsible = T
                                    )
                                  )
                                )
                              )
                            ),
                            tabPanel("Kmeans: Classification non hiérarchique",
                              fluidRow(box(width = 12, title = "  k means",
                                 wellPanel("",
                                  tags$h4("CLIQUEZ SUR LE BOUTON APPLIQUER K-MEANS POUR VOIR LES RESULTATS.", style= "color: red;")
                                 )
                                )
                              ),
                              fluidRow(title="Analyse K-means et Méthode du Coude",
                                  sidebarLayout(
                                    column(12,
                                      column(4,
                                        box(title="CLIQUEZ SUR LE BOUTON APPLIQUER K-MEANS POUR VOIR LES RESULTATS.", color = "red",
                                          width=12,
                                          sidebarPanel(width=12,
                                            sliderInput("max_k", "Nombre maximal de clusters (k) :", 
                                                        min = 2, max = 10, value = 10),
                                            actionButton("run_kmeans", "Appliquer K-means")
                                          ),
                                          footer = "Utiliser ce slider pour choisir le nombre de clusters à visualiser, puis cliquez sur appliquer K-means.", collapsible = T
                                        )
                                      ),
                                      column(8, 
                                        box(
                                          width=12,
                                          plotOutput("elbow_plot") %>% withSpinner(type = 1, color = "green"),
                                          footer = HTML("
                                            <h3>Méthode du coude</h3>
                                            <p>La courbe montre la somme des carrés intra-cluster (variance interne des clusters) en fonction du nombre de clusters <b>k</b>.</p>

                                            <h4>Interprétation du graphique</h4>
                                            <h5>Tendance générale :</h5>
                                            <ul>
                                              <li>Lorsque le nombre de clusters augmente, la variance interne diminue (c’est attendu, car plus il y a de clusters, plus les points sont proches de leurs centres).</li>
                                              <li>Cependant, cette diminution n’est pas linéaire : elle ralentit progressivement.</li>
                                            </ul>

                                            <h5>Identification du coude :</h5>
                                            <ul>
                                              <li>Le \"coude\" est le point où la diminution de la variance interne commence à ralentir significativement.</li>
                                              <li>Sur ce graphique, le coude semble se situer autour de <b>k = 3</b> ou <b>k = 4</b>, nous avons fait le choix de k = 3 qui fournit une meilleure répartition des groupes.</li>
                                              <li>Avant ce point, l'ajout de clusters réduit considérablement la variance interne.</li>
                                              <li>Après ce point, l'ajout de clusters apporte peu d'amélioration en termes de compacité.</li>
                                            </ul>

                                            <h4>Choix du nombre de clusters</h4>
                                            <p>Le coude est une bonne indication du nombre optimal de clusters, car il correspond à un équilibre entre la simplicité du modèle (moins de clusters) et sa capacité à expliquer la structure des données (compacité des clusters).</p>

                                            <h5>Justification :</h5>
                                            <ul>
                                              <li><b>k = 3</b> : Une réduction importante de la variance interne est encore observable, tout en maintenant une simplicité du modèle.</li>
                                              <li><b>k = 4</b> : Peut aussi être un choix acceptable, mais au-delà, les gains deviennent négligeables.</li>
                                            </ul>

                                            <h4>Conclusion</h4>
                                            <p>En utilisant la méthode du coude, nous pouvons justifier le choix de <b>k = 3</b> comme le nombre optimal de clusters pour notre analyse de K-means. Cela permet un bon équilibre entre la qualité des clusters et la simplicité du modèle. Si nécessaire, <b>k = 4</b> peut être exploré comme alternative pour affiner les clusters.</p>
                                            "),
                                          collapsible = T
                                        )
                                      )
                                    ),
                                    mainPanel(width=12,
                                      fluidRow(
                                        column(12,
                                          column(4,
                                            box(
                                              width=12,
                                              plotOutput("kmeans_viz") %>% withSpinner(type = 4, color = "yellow"),
                                              footer = HTML("
                                                <h3>Résultats de l'analyse K-means (k = 3)</h3>
                                                <p>Les résultats présentés dans les visualisations montrent les clusters obtenus avec <b>k = 3</b> lors de l'analyse K-means. Voici une explication détaillée :</p>

                                                <h4>Projection sur deux dimensions principales (Dim1 et Dim2) :</h4>
                                                <ul>
                                                  <li>Ce graphique montre les données projetées dans un espace réduit à deux dimensions principales, permettant de visualiser les clusters sur un plan.</li>
                                                  <li>Les clusters <b>1</b>, <b>2</b> et <b>3</b> sont représentés par des couleurs différentes (<span style='color:red;'>rouge</span>, <span style='color:green;'>vert</span>, <span style='color:blue;'>bleu</span>).</li>
                                                  <li>Chaque cluster est délimité par une enveloppe convexe qui contient les points appartenant à ce cluster.</li>
                                                </ul>

                                                <h4>Analyse des clusters :</h4>
                                                <ul>
                                                  <li><b>Cluster 1 (rouge)</b> : Concentré dans une région spécifique du graphe, indiquant des individus relativement homogènes dans leurs caractéristiques.</li>
                                                  <li><b>Cluster 2 (vert)</b> : Montre un groupe dense avec une certaine variabilité dans la dimension <b>Dim1</b>.</li>
                                                  <li><b>Cluster 3 (bleu)</b> : Bien séparé, suggérant que les individus partagent des similarités spécifiques qui les différencient des autres clusters.</li>
                                                </ul>

                                                <h4>Séparation des clusters :</h4>
                                                <ul>
                                                  <li>Les clusters sont bien séparés, indiquant une bonne segmentation des données par l'algorithme K-means.</li>
                                                  <li>Cependant, quelques chevauchements indiquent des observations limites (frontières entre clusters).</li>
                                                </ul>
                                                "),
                                              collapsible = T
                                            )
                                          ),
                                          column(4,
                                            box(width=12, title="Selectionner les variables pour visualuer les clusters",
                                                sidebarPanel(width=12,
                                                  selectInput("var_clust1", "Variable 1", choices = NULL),
                                                  selectInput("var_clust2", "Variable 2", choices = NULL)
                                                ),
                                              footer = "Ce bloc permet de voir la distribution des individus suivant les deux variables sélectionnées. Nous avont fait notre analyse suivant les variables age et training_hours"
                                            )
                                          ),
                                          column(4,
                                            box(
                                              width=12,
                                              plotOutput("kmeans_plot") %>% withSpinner(type = 2, color = "blue"),
                                              footer = HTML("
                                                <h3>Analyse de la distribution suivant les variables age et training_hours.</h3>
                                                <h5>Axes :</h5>
                                                <ul>
                                                  <li>Les variables sélectionnées sont <b>age</b> (axe <b>x</b>) et <b>training_hours</b> (axe <b>y</b>).</li>
                                                  <li>Cela permet de visualiser comment les individus se regroupent en fonction de ces deux variables spécifiques.</li>
                                                </ul>

                                                <h5>Clusters (codes couleurs) :</h5>
                                                <ul>
                                                  <li><b>Cluster 1 (rouge)</b> : Correspond aux individus ayant généralement un âge moyen et un nombre de <b>training_hours</b> élevé.</li>
                                                  <li><b>Cluster 2 (vert)</b> : Correspond à des individus plus jeunes avec des <b>training_hours</b> modérés.</li>
                                                  <li><b>Cluster 3 (bleu)</b> : Correspond à des individus plus âgés et avec un nombre plus faible de <b>training_hours</b>.</li>
                                                </ul>

                                                <h5>Interprétation des caractéristiques :</h5>
                                                <ul>
                                                  <li>Les clusters révèlent des groupes distincts d’individus en fonction de l’âge et du temps de formation.</li>
                                                  <li>Par exemple, les individus plus jeunes (<span style='color:green;'>vert</span>) ont tendance à avoir un profil distinct des individus plus âgés (<span style='color:blue;'>bleu</span>), ce qui correspond à différentes catégories dans les données, comme des niveaux d'expérience ou des priorités différentes.</li>
                                                </ul>
                                                "), 
                                              collapsible = T
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              ),
                              tabPanel("CAH: Classification Ascendante Hiérarchique",
                                fluidRow(box(width = 12,
                                  wellPanel("",
                                      verbatimTextOutput("perte0")     
                                  )
                                  )
                                ),
                                
                                fluidRow(
                                  column(6, 
                                  box(
                                    width=12,
                                    plotOutput("dendro")  %>% withSpinner(type = 8, color = "purple"),
                                    footer = HTML("
                                      <h3>Dendrogramme global de la CAH</h3>

                                      <h4>Structure hiérarchique :</h4>
                                      <ul>
                                        <li>Le dendrogramme illustre la hiérarchie entre les observations.</li>
                                        <li>Les observations (individus) sont représentées en bas, tandis que les regroupements successifs sont indiqués par des fusions verticales.</li>
                                        <li>L'axe des ordonnées (<b>Height</b>) représente la distance ou l'inertie nécessaire pour fusionner des clusters.</li>
                                      </ul>

                                      <h4>Interprétation :</h4>
                                      <ul>
                                        <li>En regardant le dendrogramme, on observe des niveaux où des regroupements importants se forment (grandes branches).</li>
                                        <li>Cela suggère que le jeu de données peut être divisé en groupes bien distincts à ces niveaux.</li>
                                      </ul>
                                      ")
                                      , 
                                    collapsible = T
                                  )
                                  ),
                                  column(6,
                                  box(
                                    width=12,
                                    plotOutput("cah_clusters") %>% withSpinner(type = 7, color = "orange"),
                                    footer = HTML("
                                      <h3>Dendrogramme découpé en 3 clusters</h3>

                                      <h4>Partition en trois clusters :</h4>
                                      <ul>
                                        <li>En coupant le dendrogramme à un certain niveau (lignes pointillées), les données sont divisées en <b>3 clusters</b> (<span style='color:red;'>rouge</span>, <span style='color:green;'>vert</span>, <span style='color:blue;'>bleu</span>).</li>
                                        <li>Chaque couleur représente un cluster distinct.</li>
                                      </ul>

                                      <h4>Séparation des clusters :</h4>
                                      <ul>
                                        <li>Les clusters sont bien séparés, avec des branches qui ne se chevauchent pas significativement.</li>
                                        <li>Le choix de <b>3 clusters</b> est cohérent, basé sur l'observation visuelle et les regroupements naturels des données.</li>
                                      </ul>

                                      <h4>Justification du choix de 3 clusters :</h4>
                                      <h5>Observation des distances :</h5>
                                      <ul>
                                        <li>Le choix du nombre de clusters est basé sur le \"saut\" des distances entre les fusions (coupure au niveau des grandes branches).</li>
                                        <li>Ici, couper le dendrogramme en <b>3</b> est optimal pour capturer les regroupements majeurs.</li>
                                      </ul>

                                      <h5>Cohérence avec K-means :</h5>
                                      <ul>
                                        <li>Ce découpage en <b>3 clusters</b> est cohérent avec les résultats obtenus précédemment via K-means, confirmant la robustesse de cette segmentation.</li>
                                      </ul>
                                      ")
                                      , collapsible = T
                                  )
                                  )
                                )
                              ),
                              # tabPanel("AFC: Analyse Factorielle des Correspondances",
                              #   fluidRow(box(width = 12, title = "Résultats de l'AFC",
                              #     wellPanel("eigenvalues", verbatimTextOutput("perte0"))
                              #     )
                              #   ),
                                
                              #   fluidRow(
                              #     column(6, 
                              #     box(
                              #       width=12,
                              #       plotOutput("eigenvalues_plot"),
                              #       footer = "perte5 ", collapsible = T
                              #     )
                              #     ),
                              #     column(6,
                              #     box(
                              #       width=12,
                              #       plotOutput("categories_plot") ,
                              #       footer = "perte6", collapsible = T
                              #     )
                              #     ),
                              #     column(6,
                              #     box(
                              #       width=12,
                              #       plotOutput("variables_plot") ,
                              #       footer = "perte7 .", collapsible = T
                              #     )
                              #     ),
                              #     column(6,
                              #     box(
                              #       width=12,
                              #       plotOutput("biplot") ,
                              #       footer = "perte8", collapsible = T
                              #     )
                              #     )
                              #   )
                              # )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
)

###########################################
#                FIN UI                   #
###########################################