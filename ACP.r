  ###########################################
  #        ANALYSE FACTIORIELLE DEBUT       #
  ###########################################

####--------------ACP DEBUT------------####


# Fonction pour réaliser l'ACP
generate_acp <- function(data, output, input) {
    #library(FactoMineR)
    #library(factoextra)
    #library(dplyr)
    #library(tidyr)

    # Nettoyer les données
    data <- drop_na(data)

    # Correction pour convertir la variable 'experience' en numérique
    data <- data %>%
      mutate(experience = case_when(
        experience == ">20" ~ 21,
        experience == "<1" ~ 0,
        TRUE ~ as.numeric(experience)
      ))

    # Sélectionner les variables numériques pour l'ACP
    numeric_data <- data[, c("city_development_index", "training_hours", "age", "hours_per_week", "experience")]

    # Centrer et réduire les données
    numeric_data_scaled <- scale(numeric_data)

    # Effectuer l'ACP
    acp <- prcomp(numeric_data_scaled, center = TRUE, scale. = TRUE)

    # 1. Valeurs propres et proportions de variance
    eigenvalues <- acp$sdev^2
    prop_var <- eigenvalues / sum(eigenvalues)
    cum_var <- cumsum(prop_var)

    # 2. Afficher les valeurs propres
    output$eigenvalues <- renderPrint({
        data.frame(
            Component = paste0("PC", 1:length(eigenvalues)),
            Eigenvalue = eigenvalues,
            Proportion_of_Variance = prop_var,
            Cumulative_Proportion = cum_var
        )
    })

    # 3. Appliquer la règle du coude
    output$scree_plot <- renderPlot({
        fviz_eig(acp, 
                 addlabels = TRUE, 
                 ylim = c(0, 100), 
                 barfill = "skyblue", 
                 barcolor = "blue") +
          ggtitle("Scree Plot (Règle du Coude)") +
          theme_minimal()
    })

    # 4. Critère de Kaiser (Valeurs propres > 1)
    kaiser_components <- sum(eigenvalues > 1)

    output$kaiser_summary <- renderPrint({
        paste("Nombre de composantes retenues selon le critère de Kaiser : ", kaiser_components)
    })

    # 5. Contributions des variables à chaque axe
    output$contribution_plot <- renderPlot({
        fviz_eig(acp)
    })

    # 6. Graphique du plan factoriel
    output$factorial_plan <- renderPlot({
        fviz_pca_ind(acp, 
            geom.ind = "point", 
            col.ind = if (is.null(input$qualitative_var) || input$qualitative_var == "Ne pas inclure de variable qualitative") "blue" else data[[input$qualitative_var]],
            addEllipses = TRUE)
    })

    # 7. Cercle de corrélation
    output$correlation_circle <- renderPlot({
        fviz_pca_var(acp, col.var = "red")
    })

    # 8. Graphiques des individus et des variables
    output$individuals_variables <- renderPlot({
        fviz_pca_biplot(acp, 
            geom.ind = "point", 
            geom.var = "arrow", 
            col.ind = "blue", 
            col.var = "red", 
            addEllipses = TRUE)
    })
}



####--------------ACP FIN------------######



  ###########################################
  #       ANALYSE FACTORIELLE FIN           #
  ###########################################