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
  dashboardHeader(title="PROJET STAT 101"),
  dashboardSidebar
    (title = "Menu",
       sidebarMenu(
         #menuItem("Gérer les origines", tabName = "raw", icon = icon("table")),
         menuItem("Gérer les données", tabName = "dash", icon = icon("fas fa-chart-bar"))
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


#------DEBUT DU SERVER-------

#update upload params
options(shiny.maxRequestSize=30*1024^2)
server<-function(input,output,session){
  
  hr_data <- NULL
  #déclaration des variables
  dataset_replaced <-NULL             #dataset avec les valeurs NA remplacées
  initial_data <-NULL                 #dataset initialement chargé
  previous_dataset <-NULL
  current_dataset <-NULL
  categorial_columns_list <- c()      #Colonnes qualitatives         
  
  
  choosed_dataset_number <- 0 #if value == 0, then it's initial_data, if value == 1, it's dataset (whithout NA values), if value == 2 it's cleaned_dataset(replaced values)
  
  
  
  ###############################
  # DEBUT CHARGEMENT DES DONNEES#
  ###############################

  
  #get data uploaded from mouvement de stocks file
  data_hr<- eventReactive(input$file1, {
    
    #Read uploaded dataset
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    if(stringr::str_ends(input$file1$datapath, "csv")) {
      hr_data <<- read.csv(input$file1$datapath,header = TRUE, sep=",", stringsAsFactors = FALSE, na.strings = c("","NA"))
      
      applyAnalyseExoloratoire()
      applyAnalyseDsitribution()
      analyseUnivarieeEtBivariee(drop_na(hr_data))
      
      #Nous commentons la méthode pour la selection des features car, elle prend énormement de temps mais, on affiche le résultat sous forme d'image
      #selectFuture()
      
      # output$image <- renderImage({
      #   list(src = "www/feature.png", contentType = 'image/png', alt = "Feature Image")
      # }, deleteFile = FALSE)
      
      initial_data <<- hr_data
    } else if (stringr::str_ends(input$file1$datapath, "(xlsx|xls|tsv)")) {
      
    }
    hr_data
    data_summary(hr_data)
    
    #####################################################
    # INITIALISATION PRETRAITEMENT DE LA DONNEE DEBUT   #
    ####################################################
    #Initialize select
    if (!is.null(initial_data)) {
      
      # dplyr get all categorial columns
      df<-initial_data %>% summarise_all(funs(n_distinct))
      data_size<-length(df)
      
      for (i in 1:data_size ) {
        if (df[1,i] < 10 && df[1,i] > 2 && df[1,i]/dim(initial_data)[1] < 0.01 ) { #10 parce qu'on suppose que, on ne veut pas dummifier une variable qui contient plus de 10 valeurs distinctes, on ne la considère pas comme une variable quantitative
          categorial_columns_list <- c(categorial_columns_list, names(df)[i])
        }
      }
      
      #initialize select with all categorial variables
      updateSelectizeInput(session,"columns_select",choices=sort(unique(categorial_columns_list)), 
                           selected=NULL, options = list(placeholder="Please Select at Least One Column")
      )
      
      #initialize select with all categorial variables
      updateSelectizeInput(session,"vars",choices=sort(unique(colnames(data_hr))), 
                           selected=NULL, options = list(placeholder="Please Select at Least One Column")
      )
      
      generate_acp()
      
    }
    if (!is.null(initial_data)) {
      tabNa <<- which(is.na(initial_data),arr.ind=TRUE)
      previous_dataset <<- initial_data
      current_dataset <<- initial_data
    }
    ###################################################
    # INITIALISATION PRETRAITEMENT DE LA DONNEE FIN   #
    ##################################################
  })
  
  data_summary <- function(data){
    # Résultat de str() - structure des données
    output$str_data <- renderPrint({
      str(data)  # Affiche la structure des données
    })
    
    # Résultat de summary() - résumé statistique
    output$summary_data <- renderPrint({
      summary(data)  # Affiche le résumé statistique
    })
    
    # Calcul des fréquences des classes
    class_freq <- table(data$target)
    
    # Affichage du graphique
    output$barplot_target <- renderPlot({
      ggplot(data, aes(x = factor(target))) + 
        geom_bar(fill = "lightblue") + 
        labs(title = "Distribution des Classes", x = "Classe", y = "Fréquence") +
        theme_minimal()
    })
    
    # Affichage du tableau de fréquence des classes
    output$table_target <- renderTable({
      class_freq
    })
    
    # Affichage des proportions des classes
    output$proportion_target <- renderPrint({
      prop.table(class_freq)
    })
  }
  
  output$table_datahr<-DT::renderDataTable({
    tmp.dat <- data_hr()
    DT::datatable(
      tmp.dat, extensions = 'Buttons',
      options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = 
       list(list(
         extend = "collection",
         text = 'Show All',
         action = DT::JS("function ( e, dt, node, config ) {
        dt.page.len(-1);
        dt.ajax.reload();
      }")
     ),
     'copy', 'csv', list(
       extend = 'excel',
       filename = 'Hr analytics dataset',
       title = NULL,
       pageLength = 15,
       exportOptions = list(columns = c(1:length(hr_data)))
     ),list(extend = 'colvis'))),filter='top'
    )
  })
  
  
  output$table<-DT::renderDataTable({
    tmp.dat <- data_hr()
    DT::datatable(
      tmp.dat, extensions = 'Buttons',
      options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = 
       list(list(
         extend = "collection",
         text = 'Show All',
         action = DT::JS("function ( e, dt, node, config ) {
        dt.page.len(-1);
        dt.ajax.reload();
      }")
     ),
     'copy', 'csv', list(
       extend = 'excel',
       filename = 'Hr analytics dataset',
       title = NULL,
       pageLength = 15,
       exportOptions = list(columns = c(1:length(hr_data)))
     ),list(extend = 'colvis'))),filter='top'
    )
  })
  
  
  
  ###########################################
  #   ANALYSE EXPLORATOIRE DES DONNEES DEBUT#
  ###########################################
  
  # observeEvent(input$btn_apply_explore_analysis, {
  #   applyAnalyseExoloratoire()
  #   applyAnalyseDsitribution()
  #   analyseUnivarieeEtBivariee(drop_na(hr_data))
  # })
  
  
  
  applyAnalyseExoloratoire <- function(){
    
    if (!is.null(hr_data)) {
      updateProgressBar(session, id = "progressBar", value = 0, total = 100)
      
      d_2<-drop_na(hr_data)
      updateProgressBar(session, id = "progressBar", value = 10)
      
    
    output$plot1<-renderPlot({
      #dessiner le graphe des frequence pour chaque variable numerique
      #plot_num(data()[,-1])+
      #labs(title="xxxxxxxxxxxxxxxxxx")
      ggplot(d_2,aes(x=gender))+
        geom_bar(fill = "coral")+
        geom_text(stat='count', aes(label=..count..), vjust=-0.2)+
        ggtitle("Distribution Sur le Genre")+
        #theme(plot.title = element_text(hjust = 0.5))+
        theme_minimal() +
        labs(title = "Répartition des genres", x = "Genre", y = "Fréquence")
    }
    
    )
    
    output$plot2<-renderPlot({
      
      #dessiner le graphe des frequence pour chaque variable numerique
      ggplot(d_2,aes(x=relevent_experience))+
        geom_bar(fill = "purple")+
        geom_text(stat='count', aes(label=..count..), vjust=-0.2)+
        #theme(plot.title = element_text(hjust = 0.5))+
        theme_minimal() +
        labs(title = "Distribution des differentes expériences", x = "Relevent Experience", y = "Fréquence")
        #ggtitle("Relevent Experience")
    }
    )
    
    output$plot3<-renderPlot({
      ggplot(d_2,aes(x=relevent_experience))+
        geom_bar(fill = "skyblue")+
        facet_wrap(~education_level)+
        ggtitle("Relevent experience by education level")+
        theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
        theme(plot.title = element_text(hjust = 0.5))
    }
    )
    output$plot4<-renderPlot({
      ggplot(d_2,aes(x=enrolled_university,fill = relevent_experience))+
        geom_bar()+
        facet_wrap(~relevent_experience)+
        theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_text(stat='count', aes(label=..count..), vjust=-1)+
        ggtitle("Enrolled University")+
        xlab("Enrolled University")+
        ylab("Count")
      
    }
    )
    output$plot5<-renderPlot({
      ggplot(d_2,aes(x=education_level,fill=relevent_experience))+
        geom_bar()+
        theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
        geom_text(stat='count', aes(label=..count..), vjust=-1)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Education Background")+
        xlab("Education Level")+
        ylab("Count")
    }
    )
    
    output$plot6<-renderPlot({
      ggplot(d_2,aes(x=major_discipline,fill=relevent_experience))+
        geom_bar()+
        facet_wrap(~gender)+
        theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
        ggtitle("College major")+
        xlab("Major")+
        ylab("Count")
    }
    
    )
    output$plot17<-renderPlot({
      ggplot(d_2,aes(x=company_size))+
        geom_bar(fill = "blue")+
        geom_text(stat='count', aes(label=..count..), vjust=-1)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Company size")+
        xlab("Size")+
        ylab("Count")
      
    }
    
    )
    output$plot18<-renderPlot({
      ggplot(d_2,aes(x= company_type))+
        geom_bar(fill = "blue")+
        theme(axis.text.x = element_text(angle = 45, hjust =1, vjust = 1))+
        geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Company Type")+
        xlab("Type")+
        ylab("Count")
      
    }
    
    )
    output$plot19<-renderPlot({
      ggplot(d_2,aes(x=experience))+
        geom_bar(fill = "blue")+
        geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Experience")+
        xlab("Experience")+
        ylab("Count")
      
      
    }
    
    )
    output$plot20<-renderPlot({
      ggplot(d_2,aes(x=last_new_job))+
        geom_bar(fill = "blue")+
        geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Time gap")+
        xlab("Last job")+
        ylab("Count")
      
    }
    )
    
    output$plot21<-renderPlot({
      ggplot(d_2,aes(x=city_development_index,fill = relevent_experience))+
        geom_bar()+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("City development index")+
        xlab("City development")+
        ylab("Count")
    }
    )
    output$plot22<-renderPlot({
      ggplot(d_2,aes(x= training_hours,fill = relevent_experience))+
        ggtitle("Training hours")+
        geom_density()
    }
    )
    d_3 <- dummy_cols(d_2, select_columns = "gender")
    d_3 <- dummy_cols(d_3, select_columns = "relevent_experience")
    d_3 <- dummy_cols(d_3, select_columns = "enrolled_university")
    d_3 <- dummy_cols(d_3, select_columns = "education_level")
    #d_3 <- dummy_cols(d_3, select_columns = "major_discipline")
    d_3 <- dummy_cols(d_3, select_columns = "company_type")
    
    
    
    mcor <- cor(d_3[sapply(d_3,is.numeric)])
    
    output$plot23<-renderPlot({
      corrplot(mcor,method="color",type="upper",tl.col="black")
      
    }
    )
    
    
    output$plot24<-renderPlot({
      corrplot(mcor,method="number")
      
    })
    updateProgressBar(session, id = "progressBar", value = 100)
    
    
    shinyalert("Analyse exploratoire terminée !", "L \'analyse exploratoire est terminée, vous pouvez maintenant visualiser les graphiques", type = "success")
    }else{
      shinyalert("Veuillez charger un jeu de données !", "Le jeu de données n\a pas été trouvé", type = "error")
    }
    
    
    
  }
  
  
  
  
  
  ##########################################
  #   ANALYSE EXPLORATOIRE DES DONNEES FIN #
  ##########################################
  
  
  
  
  
  
  ##########################################
  #   ANALYSE UNIVARIEE ET BIVARIEE DEBUT  #
  ##########################################
  
  
  analyseUnivarieeEtBivariee <- function(data_hr){
    
    # Analyse univariée - Variables quantitatives
    output$hist_city_dev <- renderPlot({ hist(data_hr$city_development_index, main="City Development Index", xlab="Index", col="lightblue", breaks=20) })
    # Correction pour convertir la variable 'experience' en numérique
    data_hr <- data_hr %>%
      mutate(experience = case_when(
        experience == ">20" ~ 21,
        experience == "<1" ~ 0,
        TRUE ~ as.numeric(experience)
      ))
    
    # Histogramme de la variable 'experience'
    output$hist_experience <- renderPlot({ hist(data_hr$experience, main="Expérience", xlab="Années", col="lightgreen", breaks=20) })
    output$hist_training_hours <- renderPlot({ hist(data_hr$training_hours, main="Heures de Formation", xlab="Heures", col="lightcoral", breaks=20) })
    output$box_city_dev <- renderPlot({ boxplot(data_hr$city_development_index, main="Boxplot de City Development Index", col="lightblue", horizontal=TRUE) })
    output$box_experience <- renderPlot({ boxplot(data_hr$experience, main="Boxplot de l'expérience", col="lightgreen", horizontal=TRUE) })
    output$box_training_hours <- renderPlot({ boxplot(data_hr$training_hours, main="Boxplot des heures de formation", col="lightcoral", horizontal=TRUE) })
    
    # Analyse univariée - Variables qualitatives
    output$bar_gender <- renderPlot({ barplot(table(data_hr$gender), main="Genre", col="lightblue") })
    output$bar_university <- renderPlot({ barplot(table(data_hr$enrolled_university), main="Inscription à l'université", col="lightgreen") })
    output$bar_education <- renderPlot({ barplot(table(data_hr$education_level), main="Niveau d'éducation", col="lightcoral") })
    output$bar_major <- renderPlot({ barplot(table(data_hr$major_discipline), main="Discipline principale", col="lightyellow") })
    
    # Analyse bivariée - Corrélations quantitatives
    # output$cor_matrix <- renderPrint({ 
    #   quantitative_vars <- select(data_hr, city_development_index, experience, training_hours)
    #   cor(quantitative_vars, use="complete.obs") 
    # })
    # output$pairs_plot <- renderPlot({
    #   quantitative_vars <- select(data_hr, city_development_index, experience, training_hours)
    #   pairs(quantitative_vars, main="Relations entre Variables Quantitatives", pch=20, col="blue")
    # })
    
    # Analyse bivariée - Corrélations entre variables quantitatives
    output$cor_matrix <- renderPrint({ 
      # Sélectionner les colonnes quantitatives
      quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours)
      
      # Convertir les colonnes en numérique si nécessaire
      quantitative_vars <- mutate_if(quantitative_vars, is.character, as.numeric)
      
      # Calcul de la matrice de corrélation
      cor(quantitative_vars, use = "complete.obs")
    })
    
    output$pairs_plot <- renderPlot({
      quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours)
      quantitative_vars <- mutate_if(quantitative_vars, is.character, as.numeric)
      
      pairs(quantitative_vars, main = "Relations entre Variables Quantitatives", pch = 20, col = "blue")
    })
    
    # Analyse bivariée - Quantitatif vs Qualitatif
    output$box_experience_gender <- renderPlot({
      boxplot(experience ~ gender, data=data_hr, main="Expérience par Genre", col=c("lightblue", "lightgreen"))
    })
    output$box_city_dev_education <- renderPlot({
      boxplot(city_development_index ~ education_level, data=data_hr, main="City Development Index par Niveau d'éducation", col=c("lightcoral", "lightgreen", "lightblue"))
    })
    output$box_training_education <- renderPlot({
      boxplot(training_hours ~ education_level, data=data_hr, main="Heures de formation par Niveau d'éducation", col=c("lightpink", "lightblue", "lightyellow"))
    })
    
    # Analyse bivariée - Corrélations qualitatives
    output$contingency_gender_university <- renderPrint({ table(data_hr$gender, data_hr$enrolled_university) })
    output$chi2_gender_university <- renderPrint({ chisq.test(table(data_hr$gender, data_hr$enrolled_university)) })
    output$contingency_gender_education <- renderPrint({ table(data_hr$gender, data_hr$education_level) })
    output$chi2_gender_education <- renderPrint({ chisq.test(table(data_hr$gender, data_hr$education_level)) })
    
    # Analyse univariée - Variables quantitatives
    output$summary_quant <- renderPrint({
      quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours)
      summary(quantitative_vars)
    })
    
    # Analyse univariée - Variables qualitatives
    output$summary_qual <- renderPrint({
      qualitative_vars <- dplyr::select(data_hr, gender, relevent_experience, enrolled_university, education_level, major_discipline, company_size, company_type, last_new_job)
      summary(qualitative_vars)
    })
    
    # Histogrammes pour les variables quantitatives
    output$histograms <- renderPlot({
      quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours)
      
      par(mfrow = c(1, 3))  # Ajuster la disposition des graphiques
      hist(quantitative_vars$city_development_index, main = "City Development Index", xlab = "City Development Index", col = "lightblue")
      hist(as.numeric(quantitative_vars$experience), main = "Experience", xlab = "Years of Experience", col = "lightgreen")
      hist(quantitative_vars$training_hours, main = "Training Hours", xlab = "Training Hours", col = "lightcoral")
    })
    
    # Analyse bivariée - Corrélations entre variables quantitatives et qualitatives (ANOVA)
    output$anova_results <- renderPrint({
      anova_results <- list(
        "city_development_index vs gender" = summary(aov(city_development_index ~ gender, data = data_hr)),
        "experience vs relevent_experience" = summary(aov(as.numeric(experience) ~ relevent_experience, data = data_hr)),
        "training_hours vs education_level" = summary(aov(training_hours ~ education_level, data = data_hr))
      )
      anova_results
    })
    
    # Analyse bivariée - Corrélations entre variables qualitatives (test de Chi-deux)
    #output$chisq_test <- renderPrint({
      #chisq.test(table(data_hr$gender, data_hr$education_level), simulate.p.value = TRUE)
    #})
    output$chisq_test <- renderPrint({
      chisq.test(table(data_hr$gender, data_hr$education_level), simulate.p.value = TRUE)
    })
  
  }
  
  ##########################################
  #   ANALYSE UNIVARIEE ET BIVARIEE FIN    #
  ##########################################
  
  
  
  
  #########################################
  #   REGARD RAPIDE SUR LES DONNEES DEBUT #
  #########################################
  
  applyAnalyseDsitribution <- function(){
  
    if (!is.null(hr_data)) {
      d_2<-drop_na(hr_data)
      
    # 1. Analyse des variables catégorielles
    
    # Fonction pour obtenir les pourcentages des catégories les plus fréquentes
    cat_summary <- function(df, col) {
      round(prop.table(table(df[[col]])) * 100, 1)
    }
    
    cat_cols <- c("gender", "relevent_experience", "enrolled_university", "education_level", "major_discipline", "company_size", "company_type", "last_new_job")
    
    for (col in cat_cols) {
      print(col)
      print(cat_summary(hr_data, col))
    }
    
    # 2. Analyse des variables continues
    summary(hr_data[c("city_development_index", "experience", "training_hours")])
    
    # 4. Visualisation des distributions et des valeurs manquantes
    
    output$dist2<-renderPlot({
    # Répartition des valeurs manquantes par colonne
    data_na <- melt(is.na(hr_data))
    ggplot(data_na, aes(x = Var2, fill = value)) +
      geom_bar() +
      theme_minimal() +
      labs(title = "Répartition des valeurs manquantes par colonne", x = "Colonnes", y = "Nombre de valeurs manquantes") +
      scale_fill_manual(values = c("grey", "red"), name = "Valeurs Manquantes", labels = c("Non", "Oui"))
    })
  
  }
    
  }
  
  
  ###########################################
  #     REGARD RAPIDE SUR LES DONNEES FIN   #
  ###########################################
  
  
  
  

  
  

  
  
  
  
  
  ###########################################
  #     PRETRAITEMENT DES DONNEES DEBUT     #
  ###########################################
  #Drop all NA values
  observeEvent(input$dropAll, {
    if (!is.null(initial_data)) {
      #get all not empty values
      previous_dataset <<- current_dataset
      current_dataset <<- na.omit(initial_data)
      choosed_dataset_number <<- 2
      #print(dim(current_dataset))
      
      data_summary(current_dataset)
      
      #sync data
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
    }else{
      shinyalert("Oops!", "Veuillez charger un dataset", type = "error")
    }
  })
  
  #Replace all NA values
  observeEvent(input$replaceAll, {
    if (!is.null(initial_data)) {
      
      #get all cells where empty values are
      # tabNa<-which(is.na(initial_data),arr.ind=TRUE)
      
      previous_dataset <<- current_dataset
      
      #replace all other lines
      # for (i in 1:dim(current_dataset)[2]){
      #   if ( is.na(current_dataset[1,i]))
      #   {
      #     j<-2
      #     while(is.na(current_dataset[j,i])){
      #       j<-j+1
      #     }
      #     current_dataset[1,i]<<-current_dataset[j,i]
      #   }
      # }
      
      #replace all other lines
      # for (i in 1:(length(tabNa)/2)){
      #   ligne<-tabNa[i,1]
      #   colonne<-tabNa[i,2]
      #   if(ligne > 1){
      #     current_dataset[ligne,colonne]<<-current_dataset[ligne-1,colonne]
      #   }
      # }
      
      # Remplacer les valeurs de la variable 'experience' contenant "<1" par "0" et ">20" par "20"
      initial_data <- initial_data %>%
        mutate(
          experience = case_when(
            experience == "<1" ~ "0",
            experience == ">20" ~ "20",
            TRUE ~ experience  # Conserve les autres valeurs
          ),
          experience = as.numeric(experience)
        )
      
      # Calcul préalable des valeurs pour éviter la redondance
      mean_city_dev <- mean(initial_data$city_development_index, na.rm = TRUE)
      median_experience <- median(initial_data$experience, na.rm = TRUE)
      median_training <- median(initial_data$training_hours, na.rm = TRUE)
      
      # Prétraitement des données
      data_preprocessed <- initial_data %>%
        
        # 1. Gestion des valeurs manquantes
        mutate(
          city_development_index = ifelse(is.na(city_development_index), mean_city_dev, city_development_index),
          experience = ifelse(is.na(experience), median_experience, experience),
          training_hours = ifelse(is.na(training_hours), median_training, training_hours),
          gender = ifelse(is.na(gender), "Male", gender), #Male c'est la valeur la plus fréquente
          enrolled_university = ifelse(is.na(enrolled_university), "no_enrollment", enrolled_university),
          education_level = ifelse(is.na(education_level), "Graduate", education_level),
          major_discipline = ifelse(is.na(major_discipline), "STEM", major_discipline),
          company_size = ifelse(is.na(company_size), "50-99", company_size),
          company_type = ifelse(is.na(company_type), "Pvt Ltd", company_type),
          last_new_job = ifelse(is.na(last_new_job), "never", last_new_job)
        ) %>%
        
        # 2. Conversion des variables catégorielles en facteurs (avec ordre pour education_level si nécessaire)
        mutate(
          gender = factor(gender),
          enrolled_university = factor(enrolled_university),
          education_level = factor(education_level),
          major_discipline = factor(major_discipline),
          company_size = factor(company_size),
          company_type = factor(company_type),
          last_new_job = factor(last_new_job),
          target = factor(target)
        ) %>%
        
        # 3. Conversion des variables numériques (comme "experience" et "training_hours") en numériques
        mutate(
          #experience = as.numeric(experience),
          training_hours = as.numeric(training_hours)
        ) %>%
        # 
        # # 4. Gestion des valeurs aberrantes pour 'experience'
        # mutate(
        #   experience = ifelse(experience > 40, median_experience, experience)
        # )
        
        # 5. Créer une nouvelle variable binaire "is_experienced"
        mutate(
         is_experienced = ifelse(experience > 5, 1, 0)
        )
      
      current_dataset <<- data_preprocessed
      
      data_summary(current_dataset)
      
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
      choosed_dataset_number <<- 1
      
    }else{
      shinyalert("Oops!", "Veuillez d'abord charger un dataset", type = "error")
    }
    
  })
  
  #reset dataset
  observeEvent(input$resetAll, {
    if (!is.null(initial_data)) {
      #reset to initial dataset
      data<- eventReactive(input$file1, {
        initial_data
      })
      
      data_summary(initial_data)
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
      choosed_dataset_number <<- 0
    }
  })
  
  observeEvent(input$remove_all, {
    updateSelectizeInput(session,"columns_select",choices=sort(unique(categorial_columns_list)), 
                         selected=NULL, options = list(placeholder="Please Select at Least One Column")
    )
  })
  
  #observeEvent(input$add_all, {
  #updateSelectizeInput(session,"columns_select",choices=sort(unique(current_dataset)), selected=sort(unique(current_dataset)) )
  #})
  
  #Dummify
  observeEvent(input$dummify, {
    vector = c()
    if (choosed_dataset_number != 0) {
      if (!is.null(current_dataset)) {
        previous_dataset <<- current_dataset
        selected_values <- input$columns_select
        if (!is.null(selected_values)) {
          groups <- c(selected_values)
          selected_values_list <- as.list(paste(strsplit(selected_values, split = "[ ]+"))) #remove all spaces from string returned by the select
          
          for (v in selected_values_list)
            vector <- c(vector, v)
          
          #Delete selected column into choosed dataset before apply dummification
          #croped_data_set <- current_dataset[ , !(names(current_dataset) %in% selected_values_list)]
          
          #Dummification
          data_dummy <<- dummy_cols(current_dataset, remove_selected_columns = TRUE, select_columns = vector)
          
          #allow us to know on which dataset operating
          choosed_dataset_number <<- -1
          
          #syncing data
          data<- eventReactive(input$file1, {
            data_dummy
          })
          
          data_summary(data_dummy)
          
          #displaying on the screen
          output$table<-DT::renderDataTable({
            tmp.dat <- data()
            DT::datatable(tmp.dat, 
                          options = list(scrollX = TRUE),filter='top')
          })
        }else{
          shinyalert("Oops!", "Veuillez choisir au moins une colonne", type = "warning")
        }
      }
    }else{
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données.", type = "error")
    }
  })
  
  #Déséquilibre
  balance_level = NULL
  ajusted_data <- NULL
  occ0 = 0
  occ1 = 0
  difference = 0
  balance_value = 0
  
  makeReactiveBinding("balance_level")
  
  observeEvent(input$balance_level, {
    balance_level <<- input$balance_level
    balance_level <<- as.integer(balance_level)
    #print(balance_level)
  })
  
  
  #Balance data
  observeEvent(input$balance_add, {
    
    if (!is.null(current_dataset)) {
      
      previous_dataset <<- current_dataset
      
      #Count occurence
      occ0<<-sum(current_dataset$target == 0)
      occ1<<-sum(current_dataset$target == 1)
      
      
      # get only rows that have 0 on target column
      dataset_zero_values_on_target <- filter(current_dataset, target == 0)
      
      # get only rows that have 1 on target column
      dataset_one_values_on_target <- filter(current_dataset, target == 1)
      
      if (occ1 < occ0) {
        difference <<- occ0 - occ1
      }else{
        difference <<- occ1 - occ0
      }
      print("------ occurence de 1 ------")
      print(occ1)
      
      print("------ occurence de 0 ------")
      print(occ0)
      
      print("----- difference d'occurence de 1 et 0 -----")
      print(difference)
      
      #apply value on slider input
      balance_value = as.integer((balance_level * difference) / 100)  #this is the number of rows we are going to add into datased to make it balanced
      
      print("----- nombre de lignes à ajouter -----")
      print(balance_value)
      
      #if we have more 0 than 1
      if (occ1 < occ0) {
        #ajusted_data <- dataset_one_values_on_target[rep(seq_len(balance_value), each = 1), ]
        ajusted_data <<- dataset_one_values_on_target[sample(nrow(dataset_one_values_on_target), balance_value, replace = TRUE, prob = NULL), ]
      }else{ #if not
        #ajusted_data <<- sample_n(dataset_zero_values_on_target, balance_value)
        ajusted_data <<- dataset_zero_values_on_target[sample(nrow(dataset_zero_values_on_target), balance_value), ]
      }
      balanced_dataset <- rbind(current_dataset, ajusted_data)
      print(dim(balanced_dataset))
      
      print("***** Voir l'équilibre *****")
      print(sum(balanced_dataset$target == 0))
      print(sum(balanced_dataset$target == 1))
      
      current_dataset <<- balanced_dataset
      
      #Refreshing view
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      data_summary(current_dataset)
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
      
      
    }else{
      shinyalert("Oops!", "Veuillez d'abord gérer les valeurs manquantes.", type = "error")
    }
  })
  
  
  #Déséquilibre
  observeEvent(input$balance_delete, {
    if (!is.null(current_dataset)) {
      
      previous_dataset <<- current_dataset
      
      #Count occurence
      occ0<<-sum(current_dataset$target == 0)
      occ1<<-sum(current_dataset$target == 1)
      
      
      # get only rows that have 0 on target column
      dataset_zero_values_on_target <- filter(current_dataset, target == 0)
      
      # get only rows that have 1 on target column
      dataset_one_values_on_target <- filter(current_dataset, target == 1)
      
      print("dim")
      print(dim(dataset_one_values_on_target)[1])
      
      if (occ1 < occ0) {
        difference <<- occ0 - occ1
      }else{
        difference <<- occ1 - occ0
      }
      print("------ occurence de 1 ------")
      print(occ1)
      
      print("------ occurence de 0 ------")
      print(occ0)
      
      print("----- difference d'occurence de 1 et 0 -----")
      print(difference)
      
      #apply value on slider input
      balance_value = as.integer((balance_level * difference) / 100)  #this is the number of rows we are going to add into datased to make it balanced
      
      print("----- nombre de lignes à supprimer -----")
      print(balance_value)
      
      #if we have more 0 than 1
      if (occ1 < occ0) {
        ajusted_data <<- tail(dataset_zero_values_on_target, n=dim(dataset_zero_values_on_target)[1]-balance_value)
        balanced_dataset <- rbind(dataset_one_values_on_target, ajusted_data)
      }else{ #if not
        ajusted_data <<- tail(dataset_one_values_on_target, n=dim(dataset_one_values_on_target)[1]-balance_value)
        balanced_dataset <- rbind(dataset_zero_values_on_target, ajusted_data)
      }
      print(dim(balanced_dataset))
      current_dataset <<- balanced_dataset
      
      print("***** Voir l'équilibre *****")
      print(sum(balanced_dataset$target == 0))
      print(sum(balanced_dataset$target == 1))
      
      #Refreshing view
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      data_summary(current_dataset)
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
      
      
    }else{
      shinyalert("Oops!", "Veuillez d'abord gérer les valeurs manquantes.", type = "error")
    }
  })
  
  #Normalisation
  observeEvent(input$normalize, {
    if (choosed_dataset_number != 0) {
      if (!is.null(current_dataset)) {
        previous_dataset <<- current_dataset
        df<-sapply(current_dataset, class)
        df2<-current_dataset%>%summarise_all(funs(n_distinct))
        for (i in 1:dim(current_dataset)[2] ) {
          if ((df[i] == "integer" || df[i] == "double" || df[i] == "numeric") &&  df2[1,i] > 2 && df2[1,i]!=dim(current_dataset)[1]) {
            for (j in 1:dim(current_dataset)[1]){
              current_dataset[j,i]<<-current_dataset[j,i]/quantile(current_dataset[,i], 0.975)
            }
          }
        }
        print(df2[1,1]!=dim(current_dataset)[1])
        #syncing data
        data<- eventReactive(input$file1, {
          current_dataset
        })
        
        
        data_summary(current_dataset)
        
        #displaying on the screen
        output$table<-DT::renderDataTable({
          tmp.dat <- data()
          DT::datatable(tmp.dat, 
                        options = list(scrollX = TRUE),filter='top')
        })
      }else{
        shinyalert("Oops!", "Veuillez appliquer un traitement sur les données manquantes.", type = "error")
      }
    }else{
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données manquantes", type = "warning")
    }
  })
  
  observeEvent(input$cancel, {
    if (!is.null(current_dataset)) {
      current_dataset <<- previous_dataset
      #syncing data
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      data_summary(current_dataset)
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
    }else{
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données", type = "error")
    }
  })
  
  observeEvent(input$resetEquilibre, {
    if (!is.null(current_dataset)) {
      current_dataset <<- previous_dataset
      #syncing data
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      data_summary(current_dataset)
      
      #Je voulais réinitialiser le slider mais, apparemment ça ne marche pas.
      balance_level = NULL
      ajusted_data <- NULL
      occ0 = 0
      occ1 = 0
      difference = 0
      balance_value = 0
      
      makeReactiveBinding("balance_level")
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
    }else{
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données", type = "error")
    }
  })
  ###########################################
  #     PRETRAITEMENT DES DONNEES fin       #
  ###########################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########################################
  #        ANALYSE FACTIORIELLE DEBUT       #
  ###########################################
  
  selectFuture <- function(){
    if (!is.null(hr_data)) {
      
      d<-drop_na(hr_data)
      
      #_________________AFFICHAGE FEATURES
      ####fonction qui recupere l'ensemble du dataset et determine les features les + importantes
      #transformations des variables catégorielles en nombre
      d$enrolled_university  <- as.integer(as.factor(d$enrolled_university ))
      d$company_type <- as.integer(as.factor(d$company_type))
      d$city <- as.integer(as.factor(d$city))
      d$education_level <- as.integer(as.factor(d$education_level))
      d$last_new_job <- as.integer(as.factor(d$last_new_job))
      d$major_discipline <- as.integer(as.factor(d$major_discipline))
      d$training_hours <- as.integer(as.factor(d$training_hours))
      d$gender <- as.integer(as.factor(d$gender))
      d$experience <- as.integer(as.factor(d$experience))
      d$relevent_experience <- as.integer(as.factor(d$relevent_experience))
      d$company_size <- as.integer(as.factor(d$company_size))
      
      res_boruta<-Boruta(target~.,data=d,doTrace=0)
      
      
      output$plot_feature<-renderPlot({
        plot(res_boruta,cex.axis=.7,las=2,xlab="",main="Les Features les plus importantes")}
      )
      
      #----------------------------Fin affichage Features
      
    }
  }
  
  
  
  
  ####--------------ACP DEBUT------------####
  # Fonction pour réaliser l'ACP
  perform_pca <- function(data, n_comp) {
    # Centrage et réduction
    data.scaled <- scale(data)
    
    # ACP
    res.pca <- prcomp(data.scaled, scale = FALSE)
    
    # Sélection des n premières composantes
    res.pca <- res.pca[,1:n_comp]
    
    return(res.pca)
  }
  
  generate_acp <- function(){
    #pca_results <- reactive({
      selected_data <- data_hr[, input$vars]
      perform_pca(selected_data, input$n_comp)
    #})
    
    output$circle <- renderPlot({
      #fviz_pca_var(pca_results, col.var = "cos2")
      fviz_pca_var(perform_pca(selected_data, input$n_comp), col.var = "cos2")
    })
  }
  
  
  ####--------------ACP FIN------------######
  
  
  ###########################################
  #       ANALYSE FACTORIELLE FIN           #
  ###########################################


}
#------FIN DU SERVER-------

shinyApp(ui,server)
