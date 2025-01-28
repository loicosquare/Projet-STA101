###########################################
#               DEBUT SERVER              #
###########################################

# Chargement des sources dans le server
source('analyseExploratoire.r')
source('analyseUniVariee.r')
source('analyseBivariee.r')

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

      hr_data$age <<- hr_data[!is.na(as.numeric(hr_data$age)), "age"]
      hr_data$hours_per_week <<- hr_data[!is.na(as.numeric(hr_data$hours_per_week)), "hours_per_week"]

      
      applyAnalyseExoloratoire(hr_data, session, output)
      applyAnalyseDsitribution()
      #analyseUnivarieeEtBivariee(drop_na(hr_data), output)
      analyseUnivarieeEtBivariee(hr_data, output)
      
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
      
      #for (i in 1:data_size ) {
        #if (df[1,i] < 10 && df[1,i] > 2 && df[1,i]/dim(initial_data)[1] < 0.01 ) { #10 parce qu'on suppose que, on ne veut pas dummifier une variable qui contient plus de 10 valeurs distinctes, on ne la considère pas comme une variable quantitative
          #categorial_columns_list <- c(categorial_columns_list, names(df)[i])
        #}
      #}
      
      #initialize select with all categorial variables
      #updateSelectizeInput(session,"columns_select",choices=sort(unique(categorial_columns_list)), 
                           #selected=NULL, options = list(placeholder="Please Select at Least One Column")
      #)
      
      #initialize select with all categorial variables
      updateSelectizeInput(session,"vars",choices=sort(unique(colnames(hr_data))), 
                           selected=NULL, options = list(placeholder="Please Select at Least One Column")
      )

      updateSelectizeInput(session,"var1",choices=sort(unique(colnames(hr_data))), 
                           selected=NULL, options = list(placeholder="Variable 1")
      )

      updateSelectizeInput(session,"var2",choices=sort(unique(colnames(hr_data))), 
                           selected=NULL, options = list(placeholder="Variable 2")
      )

      updateSelectizeInput(session,"var_clust1",choices=sort(unique(colnames(hr_data))), 
                           selected = "age", options = list(placeholder="Variable 1")
      )

      updateSelectizeInput(session,"var_clust2",choices=sort(unique(colnames(hr_data))), 
                           selected = "training_hours", options = list(placeholder="Variable 2")
      )
  
      #updateCheckboxGroupInput(session, "selected_vars", label = "Variables numériques à inclure :",
        #choices = names(hr_data[sapply(hr_data, function(x) !(is.character(x)))]), 
        #selected = names(hr_data[sapply(hr_data, function(x) !(is.character(x)))])
      #)

      updateCheckboxGroupInput(session, "selected_vars", label = "Variables numériques à inclure :",
        choices = c("city_development_index", "training_hours", "age", "hours_per_week", "experience"), 
        selected = c("city_development_index", "training_hours", "age", "hours_per_week", "experience")
      )

      updateSelectizeInput(session,"qualitative_var",choices=c("Ne pas inclure de variable qualitative", colnames(hr_data)[!colnames(hr_data) %in% c("city_development_index", "training_hours", "age", "hours_per_week", "experience")]), 
        selected=NULL, options = list(placeholder="Choisir une variable qualitative")
      )
      
      generate_acp(hr_data, output, input)
      #generate_afc(hr_data, output, input)

      # Générer et afficher le graphique de la méthode du coude en fonction de max_k
      output$elbow_plot <- renderPlot({
        generate_kmeans(hr_data, max_k = input$max_k)
      })

      # Appliquer K-means lorsqu'on clique sur le bouton "Appliquer K-means"
      observeEvent(input$run_kmeans, {
        # Effectuer le K-means avec le nombre de clusters choisi (par défaut à 3)
        kmeans_result <- perform_kmeans(hr_data, k = input$max_k, output)
        
        # Afficher le graphique des clusters
        output$kmeans_plot <- renderPlot({
          ggplot(kmeans_result$data, aes(x = age, y = training_hours, color = cluster)) +
            geom_point() +
            labs(title = "Résultats de K-means", x = "Age", y = "Heures de formation") +
            theme_minimal() +
            scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "black", "brown", "pink", "grey", "yellow"))
        })

        observeEvent(input$var_clust1, {
          udateKmeansPlotResult(kmeans_result, input$var_clust1, input$var_clust2, output, input)
        })

        observeEvent(input$var_clust2, {
          udateKmeansPlotResult(kmeans_result, input$var_clust1, input$var_clust2, output, input)
        })
        
        # Afficher un résumé du K-means
        output$kmeans_summary <- renderText({
          paste("Nombre de clusters: ", input$max_k, 
                "\nCentres des clusters:\n", 
                capture.output(kmeans_result$kmeans_result$centers), sep = "")
        })
      })

      # Calculate CAH result reactively
      cah_result <- reactive({
        generate_cah(hr_data)  # Pass the reactive data to the function
      })

      # Render the dendrogram
      output$dendro <- renderPlot({
        plot(cah_result(), main = "Dendrogramme de la CAH", xlab = "", sub = "")
      })

      # Render the clusters
      output$cah_clusters <- renderPlot({
        fviz_dend(cah_result(), k = 3, rect = TRUE, main = "Dendrogramme avec 3 clusters")
      })

      #generate_acp <- function(data_hr){
      #  #pca_results <- reactive({})
      #  selected_data <- data_hr[, input$vars]
      #  perform_pca(selected_data, input$n_comp)
        

      #  output$circle <- renderPlot({
      #      fviz_pca_var(perform_pca(selected_data, input$n_comp), col.var = "cos2")
      #  })
      #}
        
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
          exportOptions = list(columns = c(1:length(current_dataset)))
        ),list(extend = 'colvis'))),filter='top'
        )
      })
    }else{
      shinyalert("Oops!", "Veuillez charger un dataset", type = "error")
    }
  })
  
  #Replace all NA values
  observeEvent(input$replaceAll, {
    if (!is.null(initial_data)) {
      
      previous_dataset <<- current_dataset
      
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
          exportOptions = list(columns = c(1:length(current_dataset)))
        ),list(extend = 'colvis'))),filter='top'
        )
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
          exportOptions = list(columns = c(1:length(initial_data)))
        ),list(extend = 'colvis'))),filter='top'
        )
      })
      choosed_dataset_number <<- 0
    }
  })

  #Drop all outliers
  observeEvent(input$dropAllOutliers, {
    if (!is.null(current_dataset)) {
      previous_dataset <<- current_dataset
      data <- current_dataset

      for (col in names(data)) {
        if (is.numeric(data[[col]])) {
          Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
          Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1
          lower_bound <- Q1 - 1.5 * IQR
          upper_bound <- Q3 + 1.5 * IQR
          data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound | is.na(data[[col]]), ]
        }
      }

      current_dataset <<- data
      data_summary(current_dataset)
      
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        
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
          exportOptions = list(columns = c(1:length(current_dataset)))
        ),list(extend = 'colvis'))),filter='top'
        )
      })
    }else{
      shinyalert("Oops!", "Veuillez d'abord charger un dataset", type = "error")
    }
  })

  #reduceCenter

  observeEvent(input$reduceCenter, {
    if (!is.null(current_dataset)) {
      previous_dataset <<- current_dataset
      data <- current_dataset

      for (col in names(data)) {
        if (is.numeric(data[[col]])) {
          # Calculer la moyenne et l'écart-type de la colonne
          mean_col <- mean(data[[col]], na.rm = TRUE)
          sd_col <- sd(data[[col]], na.rm = TRUE)
          
          # Appliquer le centrage et la réduction
          data[[col]] <- (data[[col]] - mean_col) / sd_col
        }
      }

      current_dataset <<- data
      data_summary(current_dataset)
      
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        
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
          exportOptions = list(columns = c(1:length(current_dataset)))
        ),list(extend = 'colvis'))),filter='top'
        )
      })
    }else{
      shinyalert("Oops!", "Veuillez d'abord charger un dataset", type = "error")
    }
  })
  
  observeEvent(input$remove_all, {
    updateSelectizeInput(session,"columns_select",choices=sort(unique(categorial_columns_list)), 
                         selected=NULL, options = list(placeholder="Please Select at Least One Column")
    )
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
  ###########################################
  #     PRETRAITEMENT DES DONNEES fin       #
  ###########################################





















  ############################bivar###########################
    observeEvent(input$plotBtn, {
      output$bivariatePlot <- renderPlot({
      var1 <- input$var1
      var2 <- input$var2
      output$dynamic_footer_plot <- renderText({
            "Vous pouvez choisir var 1 = city_development_index et var 2 = age par exemple, ou var 1 = age et var 2 = training_hours pour voir lire les commentaires de notre analyse"
          })
      
      # Identifier les types de variables
      var1_type <- ifelse(is.numeric(hr_data[[var1]]), "numeric", "categorical")
      var2_type <- ifelse(is.numeric(hr_data[[var2]]), "numeric", "categorical")
      
      # Analyse en fonction des types
      if (var1_type == "categorical" & var2_type == "categorical") {
          ggplot(as.data.frame(table(hr_data[[var1]], hr_data[[var2]])),
              aes(Var1, Freq, fill = Var2)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = var1, y = "Fréquence", fill = var2) +
          theme_minimal()
      } else if (var1_type == "numeric" & var2_type == "categorical") {
          ggplot(hr_data, aes_string(x = var2, y = var1, fill = var2)) +
          geom_boxplot() +
          labs(x = var2, y = var1, title = paste("Boxplot de", var1, "par", var2)) +
          theme_minimal()
      } else if (var1_type == "numeric" & var2_type == "numeric") {
        if ( var1 == "city_development_index" && var2 == "age") {
          output$dynamic_footer_plot <- renderText({
            "Dans ce graphique on observe que certaines zones ont plus de points et ces zones influencent davantage la pente de la droite, de plus la corrélation est de 0.02 qui est presque nulle. 
            Cela indique qu’il n’existe pas de relation linéaire significative entre les deux variables. "
          })
        }

        if ( var1 == "age" && var2 == "training_hours") {
          output$dynamic_footer_plot <- renderText({
            "
            Analyse et interprétation :<br><br>

            1. Force de la relation (corrélation) :<br>
              - Le coefficient de corrélation de 0.14 indique une relation très faible et positive entre l'âge et les heures de formation.<br>
              - En d'autres termes, il existe une légère tendance montrant que les heures de formation augmentent légèrement avec l'âge, mais cette relation est loin d'être significative.<br><br>

            2. Répartition des données :<br>
              - La majorité des points se concentrent dans les jeunes tranches d'âge (moins de 40 ans).<br>
              - Les heures de formation varient considérablement pour tous les âges, avec certains individus ayant jusqu'à 300 heures de formation, indépendamment de leur âge.<br><br>

            3. Ligne de tendance :<br>
              - La pente de la ligne de régression est faible, ce qui confirme que l'âge a un impact minime sur les heures de formation.<br>
              - L'intervalle de confiance (zone grisée) montre une incertitude importante dans la prédiction des heures de formation en fonction de l'âge.<br>
            "
          })
        }
          correlation <- cor(hr_data[[var1]], hr_data[[var2]], use = "complete.obs")
          ggplot(hr_data, aes_string(x = var1, y = var2)) +
          geom_point(alpha = 0.7, color = "blue") +
          geom_smooth(method = "lm", color = "red") +
          labs(
              x = var1, y = var2,
              title = paste("Scatterplot avec corrélation :", round(correlation, 2))
          ) +
          theme_minimal()
        } else {
          ggplot() + labs(title = "Type de graphique non pris en charge") + theme_void()
        }
      })
    })

    # Matrice de Corrélation
  observeEvent(input$corBtn, {
    output$correlationPlot <- renderPlot({
      # Filtrer les variables numériques sélectionnées
      selected_data <- hr_data[, input$selected_vars, drop = FALSE]

      selected_data <- selected_data %>%
      mutate(experience = case_when(
        experience == ">20" ~ 21,
        experience == "<1" ~ 0,
        TRUE ~ as.numeric(experience)
      ))
      
      # Calcul de la matrice de corrélation
      cor_matrix <- cor(selected_data, method = tolower(input$method), use = "complete.obs")
      #cor_matrix <- cor(selected_data, method = tolower("spearman"), use = "complete.obs")
      
      # Transformation pour ggplot
      cor_melt <- melt(cor_matrix)

      if (tolower(input$method) == "pearson") {
        output$dynamic_footer <- renderText({
          HTML(
          "experience vs age (0.21) : Une faible corrélation positive, ce qui est logique, car plus une personne est âgée, plus elle a accumulé d'expérience professionnelle.
          Autres relations (comme experience vs training_hours, hours_per_week, etc.) montrent des coefficients très faibles (proches de 0), indiquant une absence de relation significative.
          age vs training_hours (0.08) : Une corrélation positive très faible, cohérente avec le scatterplot précédemment analysé.
          age vs hours_per_week (0.16) : Une faible corrélation positive, suggérant que les heures travaillées par semaine augmentent légèrement avec l'âge.
          age vs experience (0.21) : Déjà commenté, mais reste la relation la plus notable avec age.
          Les relations entre le city_development_index et d'autres variables sont faibles ou proches de 0, montrant que cet indice n'a pas d'impact direct sur des variables comme l'expérience ou les heures de travail.
          Absence de relations significatives :

          Les variables comme training_hours, hours_per_week et city_development_index ne montrent pas de fortes corrélations entre elles ou avec d'autres variables.
          Aucune des variables ne présente une corrélation forte avec une autre. Cela suggère que ces variables sont relativement indépendantes les unes des autres.
          Les relations observées (par exemple, entre age et experience) sont attendues et intuitives, mais restent faibles.
          "
          )
        })
      }

      if (tolower(input$method) == "spearman") {
        output$dynamic_footer <- renderText({
          HTML(
          "Les corrélations sont globalement faibles (valeurs proches de 0), ce qui suggère une faible relation linéaire ou monotone entre les variables.
          Une corrélation légèrement positive est observée entre age et experience (0.21), ce qui est attendu car l'expérience tend à augmenter avec l'âge.
          Les autres corrélations sont négligeables (< 0.2), notamment entre city_development_index et les autres variables."
          )
        })
      }
      
      # Affichage de la matrice
      ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), color = "white") +
        scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
        labs(title = paste("Matrice de corrélation - Méthode :", input$method),
             x = "", y = "", fill = "Corrélation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })


}

###########################################
#                FIN SERVER               #
###########################################