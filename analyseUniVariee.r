##########################################
#         ANALYSE UNIVARIEE DEBUT        #
##########################################

# Importation du fichier qui contient les fonctions partagées.
source("shared.r")

analyseUnivariee <- function(data_hr, output) {
  # Analyse univariée - Variables quantitatives
  output$hist_city_dev <- renderPlot({
    hist(data_hr$city_development_index,
      main = "City Development Index",
      xlab = "Index", col = "lightblue",
      breaks = 20
    )
  })

  # Correction pour convertir la variable 'experience' en numérique
  data_hr <- transform_experience(data_hr)

  # Histogramme de la variable 'experience'
  output$hist_experience <- renderPlot({
    hist(data_hr$experience, main = "Expérience", xlab = "Années", col = "lightgreen", breaks = 20)
  })
  output$hist_training_hours <- renderPlot({
    hist(data_hr$training_hours, main = "Heures de Formation", xlab = "Heures", col = "lightcoral", breaks = 20)
  })
  output$box_city_dev <- renderPlot({
    boxplot(data_hr$city_development_index, main = "Boxplot de City Development Index", col = "lightblue", horizontal = TRUE)
  })
  output$box_experience <- renderPlot({
    boxplot(data_hr$experience, main = "Boxplot de l'expérience", col = "lightgreen", horizontal = TRUE)
  })
  output$box_training_hours <- renderPlot({
    boxplot(data_hr$training_hours, main = "Boxplot des heures de formation", col = "lightcoral", horizontal = TRUE)
  })

  # Analyse univariée - Variables qualitatives
  output$bar_gender <- renderPlot({
    barplot(table(data_hr$gender), main = "Genre", col = "lightblue")
  })
  output$bar_university <- renderPlot({
    barplot(table(data_hr$enrolled_university), main = "Inscription à l'université", col = "lightgreen")
  })
  output$bar_education <- renderPlot({
    barplot(table(data_hr$education_level), main = "Niveau d'éducation", col = "lightcoral")
  })
  output$bar_major <- renderPlot({
    barplot(table(data_hr$major_discipline), main = "Discipline principale", col = "lightyellow")
  })


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
}

##########################################
#          ANALYSE UNIVARIEE FIN         #
##########################################
