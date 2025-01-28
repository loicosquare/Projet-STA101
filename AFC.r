###########################################
#        ANALYSE FACTIORIELLE DEBUT       #
###########################################

####--------------AFC DEBUT------------####


# Fonction pour effectuer l'AFC
generate_afc <- function(data, input, output) {

  # Supprimer les lignes avec au moins une valeur manquante
  data <- na.omit(data)

  # Sélectionner les variables qualitatives
  dataset_qualitatif <- data[, sapply(data, is.character)]

  print(dataset_qualitatif)

  # Créer un tableau de contingence
  contingency_table <- table(dataset_qualitatif)

  # Vérifier les dimensions du tableau de contingence
  if (length(dim(contingency_table)) != 2) {
    stop("Le tableau de contingence doit avoir exactement deux dimensions.")
  }
  
  # Réaliser l'AFC
  afc_result <- CA(contingency_table, graph = FALSE)
  
  # Afficher les valeurs propres (inerties des axes)
  output$eigenvalues <- renderPrint({
    afc_result$eig
  })
  
  # Visualisation des valeurs propres (inertie expliquée)
  output$eigenvalues_plot <- renderPlot({
    fviz_eig(afc_result, addlabels = TRUE, ylim = c(0, 100))
  })
  
  # Cercle de corrélation (pour les catégories)
  output$categories_plot <- renderPlot({
    fviz_ca_row(afc_result, repel = TRUE, col.row = "blue")
  })
  
  # Graphique des colonnes (pour les variables qualitatives)
  output$variables_plot <- renderPlot({
    fviz_ca_col(afc_result, repel = TRUE, col.col = "red")
  })
  
  # Graphique biplot (individus et variables)
  output$biplot <- renderPlot({
    fviz_ca_biplot(afc_result, repel = TRUE, col.row = "blue", col.col = "red")
  })
}



####--------------AFC FIN------------######



###########################################
#       ANALYSE FACTORIELLE FIN           #
###########################################