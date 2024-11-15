###########################################
#        ANALYSE FACTIORIELLE DEBUT       #
###########################################

selectFuture <- function() {
    if (!is.null(hr_data)) {
        d <- drop_na(hr_data)

        # _________________AFFICHAGE FEATURES

        #### fonction qui recupere l'ensemble du dataset et determine les features les + importantes

        # transformations des variables catégorielles en nombre
        d$enrolled_university <- as.integer(as.factor(d$enrolled_university))
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

        res_boruta <- Boruta(target ~ ., data = d, doTrace = 0)

        output$plot_feature <- renderPlot({
            plot(res_boruta, cex.axis = .7, las = 2, xlab = "", main = "Les Features les plus importantes")
        })

        #----------------------------Fin affichage Features
    }
}

#### --------------ACP DEBUT------------####


# Fonction pour réaliser l'ACP
perform_pca <- function(data, n_comp) {
    # Centrage et réduction
    data.scaled <- scale(data)

    # ACP
    res.pca <- prcomp(data.scaled, scale = FALSE)

    # Sélection des n premières composantes
    res.pca <- res.pca[, 1:n_comp]

    return(res.pca)
}

## generate_acp <- function(data_hr, input){
# pca_results <- reactive({
## selected_data <- data_hr[, input$vars]
# perform_pca(selected_data, input$n_comp)
# })

## output$circle <- renderPlot({
# fviz_pca_var(pca_results, col.var = "cos2")
## fviz_pca_var(perform_pca(selected_data, input$n_comp), col.var = "cos2")
## })
## }

#### --------------ACP FIN------------######



###########################################
#       ANALYSE FACTORIELLE FIN           #
###########################################
