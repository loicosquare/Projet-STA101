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
  "factoextra"
)

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
      menuItem("Gérer les données", tabName = "dash", icon = icon("fas fa-chart-bar"),
      tagList(
        fileInput(
          inputId = "file1",
          label = "Veuillez charger le jeu de données",
          multiple = TRUE,
          accept = c(
            "text/csv", ".xlsx", ".xls", ".tsv",
            "text/comma-separated-values,text/plain", ".csv", ".xlsx"
          ),
          placeholder = "Sélectionner un fichier"
        )
      )
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
                                    dataTableOutput("table_datahr") %>% withSpinner(color = "#0dc5c1"),title="Données consolidées pour déclaration",
                                    footer="Données sur les candidats",
                                    collapsible = T)
                                )
                              ),
                              tabPanel(
                                "Analyse exploratoire des données",
                                box(width = 12,
                                
                                    box(width = 12,
                                      wellPanel(
                                        h5("Cette section sera consacrée analyse exploratoire de données  basée sur le dataset HR Analytics: Job Change of Data Scientists",
                                           tags$br(),tags$br(),"Rappelons le contexte : une entreprise cherche à recruter des data scientists parmi ceux qui ont passé leurs examens/entretiens. 
        On cherche à savoir parmi ceux qui ont canditaté, quels sont les candidats qui souhaitent réellement intégrer l'entreprise ou ceux qui cherchent à rejoindre une autre entreprise. 
        Cette prédiction permettrait de réduire les coûts de formation 
                           et de planifier la catégorisation des candidats.",
                                        )
                                      
                                        # actionButton("btn_apply_explore_analysis" ,"Lancer l'analyse exploratoire des données", icon("check"),
                                        # style = "color: #FFFFFF; background-color: #08766b; border-color: #0d3c5e;")
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
                                      column(6, 
                                       box(
                                        width=12,
                                           plotOutput("hist_city_dev")  %>% withSpinner(type = 4, color = "blue"),
                                           footer = "L’indice de développement de la ville (city_development_index) montre une répartition assez concentrée autour des valeurs élevées, surtout entre 0,8 et 0,9, avec un pic marqué autour de 0,92. Cela indique que la majorité des candidats vivent dans des villes ayant un indice de développement relativement élevé.", collapsible = T
                                        )
                                      ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("hist_experience") %>% withSpinner(type = 5, color = "green"),
                                         footer = "La variable d'expérience montre une répartition avec un pic important à 20 ans, suggérant soit des candidats expérimentés soit une possible codification pour un niveau maximal d'expérience. Hormis ce pic, la distribution est relativement uniforme pour les autres années, sans forte concentration autour d'une valeur particulière", collapsible = T
                                       )
                                      ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("hist_training_hours") %>% withSpinner(type = 6, color = "red"),
                                         footer = "La distribution des heures de formation est fortement asymétrique avec une concentration importante vers les petites valeurs (autour de 20 heures) et quelques candidats avec des valeurs extrêmes jusqu’à 336 heures. Cette asymétrie pourrait indiquer que la plupart des candidats suivent une formation minimale, tandis qu'un petit nombre investit beaucoup plus de temps.", collapsible = T
                                       )
                                      ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("box_city_dev") %>% withSpinner(type = 7, color = "pink"),
                                         footer = "Le boxplot montre une répartition majoritairement élevée avec quelques valeurs atypiques (outliers) en dessous de 0,6. Cela confirme que les candidats viennent généralement de zones bien développées.", collapsible = T
                                       )
                                      ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("box_experience") %>% withSpinner(type = 8, color = "#333333"),
                                         footer = "Le boxplot de l'expérience montre que la majorité des candidats a entre 6 et 18 ans d'expérience. Les valeurs entre 0 et 5 ans ou supérieures à 18 ans pourraient être moins fréquentes ou représenter des cas spécifiques.", collapsible = T
                                       )
                                      ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("box_training_hours") ,
                                         footer = "Le boxplot met en évidence plusieurs valeurs atypiques (outliers) au-dessus de 100 heures, confirmant la présence de quelques candidats ayant suivi un grand nombre d'heures de formation, contrairement à la majorité.", collapsible = T
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
  Colonnes avec peu de valeurs manquantes: À l'inverse, d'autres colonnes comme enrallee_Id semblent avoir très peu, voire aucune, valeur manquante. Ces colonnes sont donc plus complètes et pourraient être plus fiables pour l'analyse.",collapsible = T)
                                     ),
                                     fluidRow(
                                       column(6,
                                        box(width=12,plotOutput("plot2",height = 300),
                                           footer="plus de 87% des candidats ont une experience pertinente.",collapsible = T)
                                       ),
                                      column(6,
                                       box(
                                         width=12,
                                         plotOutput("bar_gender") ,
                                         footer = "La majorité des individus de l'échantillon sont de genre masculin(plus de 90% des candidats de notre dataset.), ce qui montre un déséquilibre entre les genres. Cela pourrait indiquer que les métiers de la data science sont encore dominés par les hommes.
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
                                            footer = "Le plus grand groupe de candidats a un diplôme de niveau 'Graduate', suivi des titulaires d\'un 'Master', avec très peu de doctorats (PhD).
  La répartition montre que les postes en data science semblent majoritairement accessibles avec un diplôme de niveau licence ou master, tandis que le doctorat reste rare, ce qui pourrait être pertinent pour des postes plus spécialisés ou en recherche.", collapsible = T
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
                                           footer="73% des candidats ne sont pas actuellement inscrits a l'universite. on peut supposer que la plupart des candidats ont deja obtenu leur diplome universitaire. Les 27 % restants sont partages par des cours 
                      a temps plein et des cours a temps partiel.",
                                           collapsible = T)
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot5",height = 500),
                                           footer="Le graphe nous indique que plus de 60% des candidats ont un diplome de premier cycle,
                  les candidats ayant un master represente plus de 22% et ceux du doctorat represente seulement 2%. ce qui indique que 
                  85% ont poursuivi des etudes superieures.",collapsible = T)
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot6",height = 500),
                                           footer="les etudes lies au domaine d'etudes STEM ( sciences, technologie, ingenierie et mathematiques) est le domaine le plus populaire chez les candidats masculins et feminins, en effet 75% des cadidats ont choisit ce parcours pour leurs etudes.  ",collapsible = T)
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=6,plotOutput("plot17",height = 500),
                                           footer="Contrairement a toute autre variable, la taille de l'entreprise etait equitablement repartie. Cependant, il y a encore une petite tendance ou plus de candidats avaient un emploi dans une petite entreprise. La taille de l'entreprise de 50 ~ 99 detiennent la majorite avec 16 %. La taille de l'entreprise de 100 ~ 500 vient ensuite avec 13,4 %. 10000+ etait le troisieme avec 10,5% en resumé, On observe que la majorité des entreprises sont de petite ou moyenne taille (moins de 1000 employés). Les classes de taille les plus représentées sont celles des entreprises de 100 à 500 employés et de 10 à 49 employés.
  Décroissance de l'effectif avec la taille: Le nombre d'entreprises diminue à mesure que la taille augmente. Il y a beaucoup moins de très grandes entreprises (10 000+ employés) que de petites entreprises.
  Hétérogénéité des tailles: La distribution est assez hétérogène, avec des pics pour certaines classes de taille et des creux pour d'autres..
                                              ",collapsible = T),
                                       box(width=6,plotOutput("plot18",height = 500),
                                           footer="Pres de 75% des candidats travaillent ou ont travaille dans une SARL (Pvt Ltd). Le graphique nous indique qu'il n'y a que peu de personnes qui ont travaille ou travaillent dans une start-up.",collapsible = T),
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot19",height = 500),
                                           footer="Beaucoup de candidats ont deja plus de 20 ans d'experience. Cependant, un bon nombre de candidats  ont une experience qui varie entre 2 a 7 ans.",
                                           
                                           collapsible = T)
                                       
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot20",height = 500),
                                           footer="Plus de 44% des candidats ont un an d'ecart entre leur emploi precedent et leur emploi actuel. presque 22% des candidats ont une difference de plus de
                                                      4 ans entre leur emploi precedent et leur emploi actuel."
                                           ,collapsible = T)
                                       
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot21",height = 500),
                                           footer="La plupart des candidats sont issus d'une ville qui un indice de developpement d'aux alentours de 0,9."
                                           ,collapsible = T)
                                       
                                       
                                     ),
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot22",height = 500),
                                           footer = "ce graphe illustre la densite de candidats par le nombre d'heures de formation, Il semble que la plupart des candidats aient environ 0 a 60 heures de formation."
                                           ,
                                           collapsible = T)
                                       
                                       
                                     )
                                    ),
                                    
                                    
                                    tabPanel("Analyse Bivariée",
                                     fluidRow(box(width = 12, 
                                      h3("Corrélations entre Variables Quantitatives"),
                                       column(12, verbatimTextOutput("cor_matrix") %>% withSpinner(type = 4, color = "#FF5733")),
                                       column(12, plotOutput("pairs_plot") %>% withSpinner(type = 3, color = "#3CB371")),
                                       column(12, h5("Ce graphique en matrice de dispersion présente les relations entre les trois variables quantitatives du jeu de données : city_development_index, experience, et training_hours. Voici l'interprétation des relations entre chaque paire de variables :
  
  1. city_development_index vs. experience
  La distribution semble assez dispersée, avec une très légère tendance positive, confirmée par la corrélation modérée de 0,33. Cela pourrait indiquer qu'il y a une association positive entre le niveau de développement de la ville et l'expérience des individus, mais elle reste faible.
  2. city_development_index vs. training_hours
  Les points semblent distribués de manière assez aléatoire sans tendance apparente, ce qui est cohérent avec la très faible corrélation de -0,005. Cela indique qu’il n'y a pas de relation significative entre le niveau de développement de la ville et les heures de formation.
  3. experience vs. training_hours
  Bien que les points montrent une certaine densité dans les faibles valeurs d'expérience et d'heures de formation, il n'y a pas de tendance claire, ce qui est cohérent avec la corrélation quasi nulle de -0,002. Il semble que l'expérience et le nombre d'heures de formation ne soient pas significativement liés dans ce contexte.
  "))
                                      )
                                     ),
                                     
                                     fluidRow(
                                       box(width=12,plotOutput("plot23",height = 500) %>% withSpinner(type = 6, color = "#87CEEB"),
                                           footer = "Dans cette fenetre on presente notre matrice de correlation, qui servira a faire notre analyse bidimensionnelle des variables. Ce graphique de corrélation montre les relations entre les différentes variables de manière binaire et continue, codées par couleur pour indiquer la force et la direction de la corrélation (de -1 à 1). Voici quelques observations basées sur cette matrice :
  
  Corrélations positives fortes :
  
  Certaines catégories de variables catégorielles semblent être corrélées avec des sous-catégories. Par exemple, il pourrait y avoir des corrélations positives entre certains niveaux de education_level et des types de company_type, ce qui peut indiquer une certaine association entre ces catégories.
  Corrélations négatives :
  
  Il y a aussi quelques corrélations négatives significatives entre des sous-catégories, qui peuvent indiquer une opposition entre certaines caractéristiques. Par exemple, les catégories comme company_type_Public Sector et company_type_Pvt Ltd montrent une corrélation négative (car elles sont mutuellement exclusives).
  Variable cible (target) :
  
  La variable target ne semble pas montrer de corrélations très fortes avec les autres variables dans cette matrice, bien qu’il pourrait y avoir quelques relations plus faibles avec certaines catégories comme relevent_experience_Has relevent experience.
  Corrélation avec city_development_index :
  
  city_development_index ne semble pas avoir de corrélations marquées avec les autres variables binaires ou catégorielles, ce qui pourrait indiquer que cet indice est relativement indépendant par rapport aux autres attributs dans le contexte de ce dataset.
  Autres corrélations notables :
  
  Certaines corrélations faibles apparaissent, mais elles sont peu marquées. Par exemple, il peut y avoir une relation entre training_hours et les niveaux d'éducation ou les types d'enrôlement universitaire.
  En somme, la matrice indique des relations modérées et faibles, sans corrélations très marquées à exploiter immédiatement pour la variable cible target. Cela suggère que des méthodes avancées comme la régression logistique, les arbres de décision ou les modèles basés sur le machine learning pourraient être nécessaires pour capturer des relations non linéaires ou plus subtiles.Notez qu'en ammont une dummification des variables qualitatives a bien ete effectuée,
                                                        neanmoins dans un soucis de nettete quelques variables ne sont pas presente sur cette matrice.",
                                           
                                           collapsible = T)
                                     ),
                                     h3("Quantitatif vs Qualitatif"),
                                     fluidRow(box(width = 12, 
                                       column(6,
                                              
                                              box(
                                                width=12,
                                                plotOutput("box_experience_gender") %>% withSpinner(type = 5, color = "#FFD700"),
                                                footer = "Graphique (Boxplot) : Ce graphique montre la répartition des années d'expérience pour chaque genre. On peut noter que la médiane de l'expérience semble légèrement plus élevée pour les hommes que pour les femmes et les autres genres. Les intervalles interquartiles et les valeurs extrêmes sont également visibles.
  Interprétation : Il pourrait y avoir une différence dans l'expérience entre les genres, ce qui peut être lié à divers facteurs, tels que les secteurs ou les rôles où les individus travaillent.", collapsible = T
                                              )
                                       ),
                                       column(6,
                                          box(
                                            width=12,
                                            plotOutput("box_city_dev_education") %>% withSpinner(type = 7, color = "#FF00FF"),
                                            footer = "Graphique (Boxplot) : Ce graphique compare l'indice de développement de la ville en fonction des niveaux d'éducation (Graduate, Masters, PhD). On remarque que la médiane de l'indice est similaire pour les niveaux Graduate et Masters, avec une légère augmentation pour PhD. Cependant, la distribution présente des valeurs aberrantes pour chaque niveau.
  Interprétation : Il est possible que les individus ayant un niveau de doctorat (PhD) viennent de villes avec un indice de développement légèrement plus élevé. Cela pourrait être lié à l'accessibilité à l'éducation de haut niveau dans les grandes villes.", collapsible = T
                                          )
                                        ),
                                       column(6,
                                              
                                        box(
                                            width=12,
                                            plotOutput("box_training_education") %>% withSpinner(type = 8, color = "#000080"),
                                            footer = "Graphique (Boxplot) : Ce graphique présente la répartition des heures de formation en fonction des niveaux d'éducation. La médiane des heures de formation est similaire entre les trois groupes, mais il y a des valeurs aberrantes importantes, notamment pour les niveaux Masters et PhD.
  Interprétation : Bien que les heures de formation ne varient pas de manière significative en fonction du niveau d'éducation, il semble que certaines personnes, en particulier celles avec un niveau de doctorat, effectuent des heures de formation bien au-delà de la moyenne.", collapsible = T
                                          )
                                        ),
                                       column(6,
                                              
                                        box(
                                            width=12,
                                            verbatimTextOutput("anova_results") %>% withSpinner(type = 2, color = "#C0C0C0"),
                                            footer = "Le tableau montre une valeur de p très faible (0.00035), indiquant que le genre a un effet significatif sur l'indice de développement de la ville.
                                             deplus, la p-value de 0.19 indique que les différences dans les heures de formation entre les niveaux d'éducation ne sont pas significatives. Experience vs Relevant Experience : Avec un p-value de < 2e-16, ce test montre une forte association entre l'expérience et l'expérience pertinente, ce qui est intuitivement logique.
  Training Hours vs Education Level : Avec un p-value de 0.19, il n'y a pas de différence significative dans les heures de formation selon le niveau d'éducation. Variables significatives : Le gender et education_level montrent des différences significatives dans certaines variables (par exemple, city_development_index).
  Variables non significatives : Le niveau d'éducation n'a pas d'effet significatif sur les heures de formation, ce qui suggère que les heures de formation peuvent être influencées par d'autres facteurs indépendants du niveau d'éducation.", collapsible = T
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
                                                footer = "	Le test du khi-deux de Pearson (Chi-squared) pour cette association donne un X2=3.3484X^2 = 3.3484X2=3.3484, avec 4 degrés de liberté et une valeur p de 0.5013. La p-valeur (0.5013) est bien supérieure au seuil habituel de 0.05, indiquant qu'il n'y a pas de relation statistiquement significative entre le genre et le statut d'inscription universitaire. Autrement dit, le statut d'inscription à l'université ne dépend pas du genre dans ce jeu de données.
  ", collapsible = T
                                              )
                                        ),
                                       column(6, 
                                              box(
                                                width=12,
                                                verbatimTextOutput("contingency_gender_education") ,
                                                footer = "La table de contingence indique les effectifs par genre et niveau d'éducation (Graduate, Masters, PhD). et on voit qu'il y a bien plus d'éffectifs ayant un niveau d'édcution 'graduate'", collapsible = T
                                              )
                                        ),
                                       column(6,
                                              box(
                                                width=12,
                                                verbatimTextOutput("chi2_gender_education") ,
                                                footer = "Le test du khi-deux pour cette association montre un X2=25.601X^2 = 25.601X2=25.601, avec 4 degrés de liberté, et une p-valeur de 3.808e-05 (ou 0.00003808). o	La p-valeur est très faible, bien en dessous du seuil de 0.05, indiquant une relation statistiquement significative entre le genre et le niveau d'éducation. Cela suggère que la distribution des niveaux d'éducation diffère en fonction du genre.", collapsible = T
                                              )
                                       ),
                                       column(6,
                                            box(
                                              width=12,
                                              verbatimTextOutput("chisq_test") ,
                                              footer = "Le test de khi-deux avec valeur simulée (basée sur 2000 répétitions) confirme également cette signification avec une p-valeur de 0.0004998, soutenant le fait que le genre et le niveau d'éducation sont associés.
  En résumé :
  •	Genre et Inscription à l'Université : Pas de relation significative.
  •	Genre et Niveau d'Éducation : Relation significative.
  ", collapsible = T
                                            )
                                          )
                                        )
                                      ),
                                     
                                     fluidRow(
                                       
                                       box(width=12,plotOutput("plot3",height = 300),
                                           footer="le but de ce graphe est d'esmiter le nombre de candidat ayant une experience pertinante et cela par leurs niveau d'education, on remarque qu'en globalite 
                      pour chaque niveau d'etudes avoir une experience pertiante est plus frequent.",collapsible = T)
                                       
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
                                        column(4,
                                           box(width = 12 , title="Gérer les valeurs manquantes", column(12,
                                             # Bouton pour gérer les valeurs manquantes
                                             actionButton("dropAll" ,"Drop all missing values", icon("trash"),
                                                          class = "btn btn-sm btn-success"),
                                             
                                             actionButton("replaceAll" ,"Auto replace all missing values", icon("plus"),
                                                          class = "btn btn-sm btn-primary"),
                                             
                                             actionButton("resetAll" ,"Tout annuler", icon("sync"),
                                                          class = "btn btn-sm btn-danger")
                                              )
                                           )
                                        ),
                                        column(2,
                                               box(width = 12,title="Normalisation", column(12,
                                              # Bouton pour gérer les valeurs manquantes
                                              actionButton("normalize" ,"Normaliser", icon("filter"),
                                                           class = "btn btn-sm btn-warning"),
                                              
                                              actionButton("cancel" ,"Cancel", icon("sync"),
                                                           class = "btn btn-sm btn-default")
                                          )
                                         )
                                        ),
                                        column(2,
                                               box(width = 12,title="Dummification",
                                              #Liste des colonnes du dataset
                                              selectizeInput(inputId = "columns_select",
                                                             label = "Choose column",
                                                             choices = NULL, 
                                                             multiple = TRUE,
                                                             selected = NULL),
                                              
                                              # Bouton pour la dummification
                                              actionButton("dummify" ,"Dummify", icon("sync"),
                                                           class = "btn btn-sm btn-success"),
                                              # Bouton pour gérer les valeurs manquantes
                                              actionButton("remove_all" ,"Remove All", icon("trash"),
                                                           style = "color: #FFFFFF; background-color: #CA001B; border_color: #CA001B"),
                                              
                                              # Bouton pour gérer les valeurs manquantes
                                              #actionButton("add_all" ,"Add All", icon("minus"),
                                              #style = "color: #FFFFFF; background-color: #CA001B; border_color: #CA001B"),
                                              
                                            )
                                          ),
                                          column(4,  
                                            box(width = 12,title="Déséquilibre des classes", column(12,
                                                                                                 
                                               sliderInput("balance_level", "Régler le problème de déséquilibre à combien de %:",
                                                           min = 0, max = 100,
                                                           value = 0, step = 5,
                                                           animate = animationOptions(interval = 300, loop = TRUE)),
                                               
                                               # Bouton pour gérer les valeurs manquantes
                                               actionButton("balance_add" ,"Ajouter Classe Minoritaire", icon("plus"),
                                                            class = "btn btn-sm btn-success"),
                                               
                                               # Bouton pour gérer les valeurs manquantes
                                               actionButton("balance_delete" ,"Diminuer Classe Majoritaire", icon("minus"),
                                                            class = "btn btn-sm btn-danger"),
                                               
                                               
                                               actionButton("resetEquilibre" ,"Reset", icon("sync"),
                                               class = "btn btn-sm btn-danger")
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
                             tabPanel("Méthode d'anlyse factorielle",
                              fluidRow(box(width = 12, title = "identification des variables les plus importantes bien que n'étant pas une uné méthode d'analyse factorielle, nous prévoyons comparer les résultats à ceux de l'ACP dans le but de mieux commenter et critiquer les résultats de l'ACP!",
                                 h4("Sélection de variables : importance de chaque variable"),
                                 wellPanel(
                                   h5("Bienvenue dans la section basée sur le dataset HR Analytics: Job Change of Data Scientists",
                                      tags$br(),tags$br(),
                                      "Cette base de données, nettoyée et analysée précédemment dans les sections prédentes est composée de 8955 lignes et 14 colonnes",
                                      tags$br(),"Parmi les différentes colonnes, nous avons pu sélectionner les features les importantes qui sont affichées ci-dessous.",
                                   ),
                                   #plot feature selection, 
                                   #plotOutput("plot_feature"),
                                   # Charger l'image et l'afficher
                                   tags$img(src="feature.png", alt="Feature selection", style = "width: -webkit-fill-available;", deleteFile=FALSE),
                                   
                                   h5("On observe ainsi que la feature ayant le plus d'impact est le 'City development index' qui mesure le développement des villes, indicateur qui permet de classer les villes du monde.",
                                    tags$br(),
                                    "En identifiant les caractéristiques les plus importantes, le graphique permet de réduire la dimensionnalité du jeu de données en se concentrant sur les variables les plus influentes
                                    L'ACP vise également à réduire la dimensionnalité en projetant les données dans un espace de plus faible dimension, tout en conservant le maximum de variance.
                                    Notre objectif est d' établir un lien plus formel entre le graphique et l'ACP on va donc : ",
                                    tags$br(), 

"Comparer les résultats: Calculer les corrélations entre les variables initiales et les composantes principales. Les variables ayant les corrélations les plus élevées avec les premières composantes sont celles qui expliquent le plus de variance et devraient donc être similaires aux variables identifiées comme importantes dans ce graphique."
                                    
                                   )
                                 )
                                )
                              ),
                              
                              fluidRow(
                                column(6, 
                                 box("Analyse en composantes principales intéractive",
                                   width=12,
                                   plotOutput("perte1"),
                                   footer = "perte1 ", collapsible = T
                                 )
                                ),
                                column(6,
                                 box(
                                   width=12,
                                   #sliderInput("n_comp", "Nombre de composantes principales:", min = 1, max = ncol(data), value = n_comp_reactive()),
                                   #selectInput("vars", "Sélectionner les variables:", choices = "", multiple = TRUE),
                                   
                                   selectizeInput(inputId = "vars",
                                                  label = "Sélectionner les variables",
                                                  choices = NULL, 
                                                  multiple = TRUE,
                                                  selected = NULL),
                                   sliderInput("n_comp", "Nombre de composantes principales:", min = 1, max = 14, value = 2),
                                   plotOutput("circle") ,
                                   footer = "perte2", collapsible = T
                                 )
                                ),
                                column(6,
                                 box(
                                   width=12,
                                   plotOutput("perte3") ,
                                   footer = "perte3 .", collapsible = T
                                 )
                                ),
                                column(6,
                                 box(
                                   width=12,
                                   plotOutput("perte4") ,
                                   footer = "perte4", collapsible = T
                                 )
                                )
                              )
                             ),
                             tabPanel("Méthode de classification",
                              fluidRow(box(width = 12, title = "Centres mobiles",
                                 h4("Centres mobiles"),
                                 wellPanel("",
                                           verbatimTextOutput("perte0")     
                                 )
                                )
                              ),
                              
                              fluidRow(
                                column(6, 
                                 box(
                                   width=12,
                                   plotOutput("perte5"),
                                   footer = "perte5 ", collapsible = T
                                 )
                                ),
                                column(6,
                                 box(
                                   width=12,
                                   plotOutput("perte6") ,
                                   footer = "perte6", collapsible = T
                                 )
                                ),
                                column(6,
                                 box(
                                   width=12,
                                   plotOutput("perte7") ,
                                   footer = "perte7 .", collapsible = T
                                 )
                                ),
                                column(6,
                                 box(
                                   width=12,
                                   plotOutput("perte8") ,
                                   footer = "perte8", collapsible = T
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
        )
      )
    )
)

###########################################
#                FIN UI                   #
###########################################