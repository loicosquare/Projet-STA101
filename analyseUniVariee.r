  ##########################################
  #   ANALYSE UNIVARIEE ET BIVARIEE DEBUT  #
  ##########################################
  
  
  analyseUnivarieeEtBivariee <- function(data_hr, output){
    
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
    output$hist_age <- renderPlot({ hist(data_hr$age, main="Age", xlab="Age", col="lightgreen", breaks=20) })
    output$hist_hours_per_week <- renderPlot({ hist(data_hr$hours_per_week, main="Hours_per_week", xlab="Heures", col="lightgreen", breaks=20) })
    output$box_city_dev <- renderPlot({ boxplot(data_hr$city_development_index, main="Boxplot de City Development Index", col="lightblue", horizontal=TRUE) })
    output$box_experience <- renderPlot({ boxplot(data_hr$experience, main="Boxplot de l'expérience", col="lightgreen", horizontal=TRUE) })
    output$box_training_hours <- renderPlot({ boxplot(data_hr$training_hours, main="Boxplot des heures de formation", col="lightcoral", horizontal=TRUE) })
    output$box_age <- renderPlot({ boxplot(data_hr$age, main="Boxplot de Age", col="lightblue", horizontal=TRUE) })
    output$box_hours_per_week <- renderPlot({ boxplot(data_hr$hours_per_week, main="Boxplot de Hours_per_week", col="lightblue", horizontal=TRUE) })
    
    # Analyse univariée - Variables qualitatives
    output$bar_workclass <- renderPlot({ barplot(table(data_hr$workclass), main="Secteur", col="green") })
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
      quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours, hours_per_week, age)
      
      # Convertir les colonnes en numérique si nécessaire
      quantitative_vars <- mutate_if(quantitative_vars, is.character, as.numeric)
      
      # Calcul de la matrice de corrélation
      cor(quantitative_vars, use = "complete.obs")
    })
    
    output$pairs_plot <- renderPlot({
      quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours, hours_per_week, age)
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
      quantitative_vars <- dplyr::select(data_hr, city_development_index, experience, training_hours, age, hours_per_week)
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
        "training_hours vs education_level" = summary(aov(training_hours ~ education_level, data = data_hr)),
        "hours_per_week vs marital_status" = summary(aov(hours_per_week ~ marital_status, data = data_hr)),
        "age vs workclass" = summary(aov(age ~ workclass, data = data_hr))
      )
      anova_results
    })
    
    # Analyse bivariée - Corrélations entre variables qualitatives (test de Chi-deux)
    #output$chisq_test <- renderPrint({
      #chisq.test(table(data_hr$gender, data_hr$education_level), simulate.p.value = TRUE)
    #})
    #output$chisq_test <- renderPrint({
      #chisq.test(table(data_hr$gender, data_hr$education_level), simulate.p.value = TRUE)
    #})

    output$last_new_job_marital_status <- renderPrint({
        chisq.test(table(data_hr$last_new_job, data_hr$marital_status), simulate.p.value = TRUE)
    })

    output$workclass_marital_status <- renderPrint({
        chisq.test(table(data_hr$workclass, data_hr$marital_status), simulate.p.value = TRUE)
    })
    
    output$relevent_experience_workclass <- renderPrint({
        chisq.test(table(data_hr$relevent_experience, data_hr$workclass), simulate.p.value = TRUE)
    })

    output$race_company_type <- renderPrint({
        chisq.test(table(data_hr$race, data_hr$company_type), simulate.p.value = TRUE)
    })

    }
  
  ##########################################
  #   ANALYSE UNIVARIEE ET BIVARIEE FIN    #
  ##########################################