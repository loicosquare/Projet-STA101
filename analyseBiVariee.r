##########################################
#          ANALYSE BIVARIEE DEBUT        #
##########################################

# Importation du fichier qui contient les fonctions partagées.
source("shared.r")

analyseBivariee <- function(data_hr, output) {
    # Correction pour convertir la variable 'experience' en numérique
    data_hr <- transform_experience(data_hr)

    # Analyse bivariée - Corrélations quantitatives
    # output$cor_matrix <- renderPrint({
    #   quantitative_vars <- select(data_hr, city_development_index, experience, training_hours)
    #   cor(quantitative_vars, use="complete.obs")
    # })
    # output$pairs_plot <- renderPlot({
    #   quantitative_vars <- select(data_hr, city_development_index, experience, training_hours,hours_per_week, age)
    #   pairs(quantitative_vars, main="Relations entre Variables Quantitatives", pch=20, col="blue")
    # })

    # Analyse bivariée - Corrélations entre variables quantitatives
    output$cor_matrix <- renderPrint({
        # Sélectionner les colonnes quantitatives
        quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours,hours_per_week, age)

        # Convertir les colonnes en numérique si nécessaire
        quantitative_vars <- mutate_if(quantitative_vars, is.character, as.numeric)

        # Calcul de la matrice de corrélation
        cor(quantitative_vars, use = "complete.obs")
    })

    output$pairs_plot <- renderPlot({
        quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours,hours_per_week, age)
        quantitative_vars <- mutate_if(quantitative_vars, is.character, as.numeric)

        pairs(quantitative_vars, main = "Relations entre Variables Quantitatives", pch = 20, col = "blue")
    })

    # Analyse bivariée - Quantitatif vs Qualitatif
    output$box_experience_gender <- renderPlot({
        boxplot(experience ~ gender, data = data_hr, main = "Expérience par Genre", col = c("lightblue", "lightgreen"))
    })
    output$box_city_dev_education <- renderPlot({
        boxplot(city_development_index ~ education_level, data = data_hr, main = "City Development Index par Niveau d'éducation", col = c("lightcoral", "lightgreen", "lightblue"))
    })
    output$box_training_education <- renderPlot({
        boxplot(training_hours ~ education_level, data = data_hr, main = "Heures de formation par Niveau d'éducation", col = c("lightpink", "lightblue", "lightyellow"))
    })

    # Analyse bivariée - Corrélations qualitatives
    output$contingency_gender_university <- renderPrint({
        table(data_hr$gender, data_hr$enrolled_university)
    })
    output$chi2_gender_university <- renderPrint({
        chisq.test(table(data_hr$gender, data_hr$enrolled_university))
    })
    output$contingency_gender_education <- renderPrint({
        table(data_hr$gender, data_hr$education_level)
    })
    output$chi2_gender_education <- renderPrint({
        chisq.test(table(data_hr$gender, data_hr$education_level))
    })

    # Histogrammes pour les variables quantitatives
    output$histograms <- renderPlot({
        quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours, hours_per_week, age)

        par(mfrow = c(1, 3)) # Ajuster la disposition des graphiques
        hist(quantitative_vars$city_development_index, main = "City Development Index", xlab = "City Development Index", col = "lightblue")
        hist(as.numeric(quantitative_vars$experience), main = "Experience", xlab = "Years of Experience", col = "lightgreen")
        hist(quantitative_vars$training_hours, main = "Training Hours", xlab = "Training Hours", col = "lightcoral")
    })

    # Analyse bivariée - Corrélations entre variables quantitatives et qualitatives (ANOVA)
    output$anova_results <- renderPrint({
        anova_results <- list(
            "city_development_index vs gender" = summary(aov(city_development_index ~ gender, data = data_hr)),
            "experience vs relevent_experience" = summary(aov(as.numeric(experience) ~ relevent_experience, data = data_hr)),
            "training_hours vs education_level" = summary(aov(training_hours ~ education_level, data = data_hr)),
            "hours_per_week vs marital_status" = summary(aov(hours_per_week ~ marital_status, data = data_hr)),
            "age vs workclass" = summary(aov(age ~ workclass, data = data_hr))
        )
        anova_results
    })

    # Analyse bivariée - Corrélations entre variables qualitatives (test de Chi-deux)
    # output$chisq_test <- renderPrint({
    # chisq.test(table(data_hr$gender, data_hr$education_level), simulate.p.value = TRUE)
    # })

}

##########################################
#              ANALYSE BIVARIEE          #
##########################################
