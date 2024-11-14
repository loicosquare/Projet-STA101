#Inclusion des fichiers séparés
source('server.r')
source('ui.r')
source('ACP.r')

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

shinyApp(ui,server)
