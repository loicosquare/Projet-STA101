###########################################
#        KMEANS DEBUT       #
###########################################

####--------------KMEANS DEBUT------------####


# Fonction pour effectuer la méthode du coude
generate_kmeans <- function(data, max_k=10) {
  # Centrer et réduire les données
      # Nettoyer les données

    # Correction pour convertir la variable 'experience' en numérique
    data <- data %>%
      mutate(experience = case_when(
        experience == ">20" ~ 21,
        experience == "<1" ~ 0,
        TRUE ~ as.numeric(experience)
      ))

    data <- drop_na(data)

    # Sélectionner les variables numériques pour l'ACP
    numeric_data <- data[, c("city_development_index", "training_hours", "age", "hours_per_week", "experience")]

    # Centrer et réduire les données
    data_scaled <- scale(numeric_data)

  # Calculer la somme des carrés intra-cluster (WCSS) pour différents k
  wcss <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    kmeans_result <- kmeans(data_scaled, centers = k, nstart = 25)
    wcss[k - 1] <- kmeans_result$tot.withinss
  }
  
  # Créer un graphique de la méthode du coude
  elbow_plot <- ggplot(data.frame(k = 2:max_k, WCSS = wcss), aes(x = k, y = WCSS)) +
    geom_line() +
    geom_point() +
    labs(title = "Méthode du Coude", x = "Nombre de Clusters (k)", y = "Somme des Carrés Intra-cluster") +
    theme_minimal()

  # Générer et afficher le graphique de la méthode du coude en fonction de max_k
  #output$elbow_plot <- renderPlot({
  #  kmeans_elbow(data, max_k = input$max_k)
  #})
  
  return(elbow_plot)
}

# Fonction pour effectuer K-means
perform_kmeans <- function(data, k, output) {
  # Nettoyer les données

  # Correction pour convertir la variable 'experience' en numérique
  data <- data %>%
    mutate(experience = case_when(
      experience == ">20" ~ 21,
      experience == "<1" ~ 0,
      TRUE ~ as.numeric(experience)
    ))
    
  data <- drop_na(data)

  # Sélectionner les variables numériques pour l'ACP
  numeric_data <- data[, c("city_development_index", "training_hours", "age", "hours_per_week", "experience")]

  # Centrer et réduire les données
  data_scaled <- scale(numeric_data)

  
  # Appliquer K-means avec le nombre de clusters choisi
  kmeans_result <- kmeans(data_scaled, centers = k, nstart = 25)

  output$kmeans_viz <- renderPlot({
      # Visualisation des clusters
      fviz_cluster(kmeans_result, data = data_scaled)
    })
  
  # Ajouter les résultats des clusters au dataframe d'origine
  data$cluster <- as.factor(kmeans_result$cluster)
  
  return(list(kmeans_result = kmeans_result, data = data))
}

udateKmeansPlotResult <- function(kmeans_result, label1, label2, output, input) {
  # Afficher le graphique des clusters
  output$kmeans_plot <- renderPlot({
    ggplot(kmeans_result$data, aes(x = .data[[input$var_clust1]], y = .data[[input$var_clust2]], color = cluster)) +
      geom_point() +
      labs(title = "Résultats de K-means", x = label1, y = label2) +
      theme_minimal() +
      scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "black", "brown", "pink", "grey", "yellow"))
  })
}


####--------------KMEANS FIN------------######



###########################################
#       KMEANS FIN           #
###########################################