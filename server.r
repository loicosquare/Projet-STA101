###########################################
#               DEBUT SERVER              #
###########################################

# Chargement des sources dans le server
source("analyseExploratoire.r")
source("analyseUniVariee.r")
source("analyseBivariee.r")

# update upload params
options(shiny.maxRequestSize = 30 * 1024^2)
server <- function(input, output, session) {
  hr_data <- NULL
  # déclaration des variables
  dataset_replaced <- NULL # dataset avec les valeurs NA remplacées
  initial_data <- NULL # dataset initialement chargé
  previous_dataset <- NULL
  current_dataset <- NULL
  categorial_columns_list <- c() # Colonnes qualitatives


  choosed_dataset_number <- 0 # if value == 0, then it's initial_data, if value == 1, it's dataset (whithout NA values), if value == 2 it's cleaned_dataset(replaced values)



  ###############################
  # DEBUT CHARGEMENT DES DONNEES#
  ###############################


  # get data uploaded from mouvement de stocks file
  data_hr <- eventReactive(input$file1, {
    # Read uploaded dataset
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }

    if (stringr::str_ends(input$file1$datapath, "csv")) {
      hr_data <<- read.csv(input$file1$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))

      #applyAnalyseExoloratoire(hr_data, session, output)
      #applyAnalyseDsitribution()
      analyseUnivariee(drop_na(hr_data), output)
      #analyseBivariee(drop_na(hr_data), output)

      # Nous commentons la méthode pour la selection des features car, elle prend énormement de temps mais, on affiche le résultat sous forme d'image
      # selectFuture()

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
    # Initialize select
    if (!is.null(initial_data)) {
      # dplyr get all categorial columns
      df <- initial_data %>% summarise_all(funs(n_distinct))
      data_size <- length(df)

      for (i in 1:data_size) {
        if (df[1, i] < 10 && df[1, i] > 2 && df[1, i] / dim(initial_data)[1] < 0.01) { # 10 parce qu'on suppose que, on ne veut pas dummifier une variable qui contient plus de 10 valeurs distinctes, on ne la considère pas comme une variable quantitative
          categorial_columns_list <- c(categorial_columns_list, names(df)[i])
        }
      }

      # initialize select with all categorial variables
      updateSelectizeInput(session, "columns_select",
        choices = sort(unique(categorial_columns_list)),
        selected = NULL, options = list(placeholder = "Please Select at Least One Column")
      )

      # initialize select with all categorial variables
      updateSelectizeInput(session, "vars",
        choices = sort(unique(colnames(data_hr))),
        selected = NULL, options = list(placeholder = "Please Select at Least One Column")
      )

      # generate_acp(data_hr)

      generate_acp <- function(data_hr) {
        # pca_results <- reactive({})
        selected_data <- data_hr[, input$vars]
        perform_pca(selected_data, input$n_comp)


        output$circle <- renderPlot({
          fviz_pca_var(perform_pca(selected_data, input$n_comp), col.var = "cos2")
        })
      }
    }
    if (!is.null(initial_data)) {
      tabNa <<- which(is.na(initial_data), arr.ind = TRUE)
      previous_dataset <<- initial_data
      current_dataset <<- initial_data
    }
    ###################################################
    # INITIALISATION PRETRAITEMENT DE LA DONNEE FIN   #
    ##################################################
  })

  data_summary <- function(data) {
    # Résultat de str() - structure des données
    output$str_data <- renderPrint({
      str(data) # Affiche la structure des données
    })

    # Résultat de summary() - résumé statistique
    output$summary_data <- renderPrint({
      summary(data) # Affiche le résumé statistique
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

  output$table_datahr <- DT::renderDataTable({
    tmp.dat <- data_hr()
    DT::datatable(
      tmp.dat,
      extensions = "Buttons",
      options = list(
        dom = "Blfrtip", scrollX = TRUE, buttons =
          list(
            list(
              extend = "collection",
              text = "Show All",
              action = DT::JS("function ( e, dt, node, config ) {
        dt.page.len(-1);
        dt.ajax.reload();
      }")
            ),
            "copy", "csv", list(
              extend = "excel",
              filename = "Hr analytics dataset",
              title = NULL,
              pageLength = 15,
              exportOptions = list(columns = c(1:length(hr_data)))
            ), list(extend = "colvis")
          )
      ), filter = "top"
    )
  })


  output$table <- DT::renderDataTable({
    tmp.dat <- data_hr()
    DT::datatable(
      tmp.dat,
      extensions = "Buttons",
      options = list(
        dom = "Blfrtip", scrollX = TRUE, buttons =
          list(
            list(
              extend = "collection",
              text = "Show All",
              action = DT::JS("function ( e, dt, node, config ) {
        dt.page.len(-1);
        dt.ajax.reload();
      }")
            ),
            "copy", "csv", list(
              extend = "excel",
              filename = "Hr analytics dataset",
              title = NULL,
              pageLength = 15,
              exportOptions = list(columns = c(1:length(hr_data)))
            ), list(extend = "colvis")
          )
      ), filter = "top"
    )
  })




  #########################################
  #   REGARD RAPIDE SUR LES DONNEES DEBUT #
  #########################################

  applyAnalyseDsitribution <- function() {
    if (!is.null(hr_data)) {
      d_2 <- drop_na(hr_data)

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

      output$dist2 <- renderPlot({
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
  # Drop all NA values
  observeEvent(input$dropAll, {
    if (!is.null(initial_data)) {
      # get all not empty values
      previous_dataset <<- current_dataset
      current_dataset <<- na.omit(initial_data)
      choosed_dataset_number <<- 2
      # print(dim(current_dataset))

      data_summary(current_dataset)

      # sync data
      data <- eventReactive(input$file1, {
        current_dataset
      })

      # displaying on the screen
      output$table <- DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat,
          options = list(scrollX = TRUE), filter = "top"
        )
      })
    } else {
      shinyalert("Oops!", "Veuillez charger un dataset", type = "error")
    }
  })

  # Replace all NA values
  observeEvent(input$replaceAll, {
    if (!is.null(initial_data)) {
      # get all cells where empty values are
      # tabNa<-which(is.na(initial_data),arr.ind=TRUE)

      previous_dataset <<- current_dataset

      # replace all other lines
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

      # replace all other lines
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
            TRUE ~ experience # Conserve les autres valeurs
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
          gender = ifelse(is.na(gender), "Male", gender), # Male c'est la valeur la plus fréquente
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
          # experience = as.numeric(experience),
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

      data <- eventReactive(input$file1, {
        current_dataset
      })

      # displaying on the screen
      output$table <- DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat,
          options = list(scrollX = TRUE), filter = "top"
        )
      })
      choosed_dataset_number <<- 1
    } else {
      shinyalert("Oops!", "Veuillez d'abord charger un dataset", type = "error")
    }
  })

  # reset dataset
  observeEvent(input$resetAll, {
    if (!is.null(initial_data)) {
      # reset to initial dataset
      data <- eventReactive(input$file1, {
        initial_data
      })

      data_summary(initial_data)

      # displaying on the screen
      output$table <- DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat,
          options = list(scrollX = TRUE), filter = "top"
        )
      })
      choosed_dataset_number <<- 0
    }
  })

  observeEvent(input$remove_all, {
    updateSelectizeInput(session, "columns_select",
      choices = sort(unique(categorial_columns_list)),
      selected = NULL, options = list(placeholder = "Please Select at Least One Column")
    )
  })

  # observeEvent(input$add_all, {
  # updateSelectizeInput(session,"columns_select",choices=sort(unique(current_dataset)), selected=sort(unique(current_dataset)) )
  # })

  # Dummify
  observeEvent(input$dummify, {
    vector <- c()
    if (choosed_dataset_number != 0) {
      if (!is.null(current_dataset)) {
        previous_dataset <<- current_dataset
        selected_values <- input$columns_select
        if (!is.null(selected_values)) {
          groups <- c(selected_values)
          selected_values_list <- as.list(paste(strsplit(selected_values, split = "[ ]+"))) # remove all spaces from string returned by the select

          for (v in selected_values_list) {
            vector <- c(vector, v)
          }

          # Delete selected column into choosed dataset before apply dummification
          # croped_data_set <- current_dataset[ , !(names(current_dataset) %in% selected_values_list)]

          # Dummification
          data_dummy <<- dummy_cols(current_dataset, remove_selected_columns = TRUE, select_columns = vector)

          # allow us to know on which dataset operating
          choosed_dataset_number <<- -1

          # syncing data
          data <- eventReactive(input$file1, {
            data_dummy
          })

          data_summary(data_dummy)

          # displaying on the screen
          output$table <- DT::renderDataTable({
            tmp.dat <- data()
            DT::datatable(tmp.dat,
              options = list(scrollX = TRUE), filter = "top"
            )
          })
        } else {
          shinyalert("Oops!", "Veuillez choisir au moins une colonne", type = "warning")
        }
      }
    } else {
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données.", type = "error")
    }
  })

  # Déséquilibre
  balance_level <- NULL
  ajusted_data <- NULL
  occ0 <- 0
  occ1 <- 0
  difference <- 0
  balance_value <- 0

  makeReactiveBinding("balance_level")

  observeEvent(input$balance_level, {
    balance_level <<- input$balance_level
    balance_level <<- as.integer(balance_level)
    # print(balance_level)
  })


  # Balance data
  observeEvent(input$balance_add, {
    if (!is.null(current_dataset)) {
      previous_dataset <<- current_dataset

      # Count occurence
      occ0 <<- sum(current_dataset$target == 0)
      occ1 <<- sum(current_dataset$target == 1)


      # get only rows that have 0 on target column
      dataset_zero_values_on_target <- filter(current_dataset, target == 0)

      # get only rows that have 1 on target column
      dataset_one_values_on_target <- filter(current_dataset, target == 1)

      if (occ1 < occ0) {
        difference <<- occ0 - occ1
      } else {
        difference <<- occ1 - occ0
      }
      print("------ occurence de 1 ------")
      print(occ1)

      print("------ occurence de 0 ------")
      print(occ0)

      print("----- difference d'occurence de 1 et 0 -----")
      print(difference)

      # apply value on slider input
      balance_value <- as.integer((balance_level * difference) / 100) # this is the number of rows we are going to add into datased to make it balanced

      print("----- nombre de lignes à ajouter -----")
      print(balance_value)

      # if we have more 0 than 1
      if (occ1 < occ0) {
        # ajusted_data <- dataset_one_values_on_target[rep(seq_len(balance_value), each = 1), ]
        ajusted_data <<- dataset_one_values_on_target[sample(nrow(dataset_one_values_on_target), balance_value, replace = TRUE, prob = NULL), ]
      } else { # if not
        # ajusted_data <<- sample_n(dataset_zero_values_on_target, balance_value)
        ajusted_data <<- dataset_zero_values_on_target[sample(nrow(dataset_zero_values_on_target), balance_value), ]
      }
      balanced_dataset <- rbind(current_dataset, ajusted_data)
      print(dim(balanced_dataset))

      print("***** Voir l'équilibre *****")
      print(sum(balanced_dataset$target == 0))
      print(sum(balanced_dataset$target == 1))

      current_dataset <<- balanced_dataset

      # Refreshing view
      data <- eventReactive(input$file1, {
        current_dataset
      })

      data_summary(current_dataset)

      # displaying on the screen
      output$table <- DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat,
          options = list(scrollX = TRUE), filter = "top"
        )
      })
    } else {
      shinyalert("Oops!", "Veuillez d'abord gérer les valeurs manquantes.", type = "error")
    }
  })


  # Déséquilibre
  observeEvent(input$balance_delete, {
    if (!is.null(current_dataset)) {
      previous_dataset <<- current_dataset

      # Count occurence
      occ0 <<- sum(current_dataset$target == 0)
      occ1 <<- sum(current_dataset$target == 1)


      # get only rows that have 0 on target column
      dataset_zero_values_on_target <- filter(current_dataset, target == 0)

      # get only rows that have 1 on target column
      dataset_one_values_on_target <- filter(current_dataset, target == 1)

      print("dim")
      print(dim(dataset_one_values_on_target)[1])

      if (occ1 < occ0) {
        difference <<- occ0 - occ1
      } else {
        difference <<- occ1 - occ0
      }
      print("------ occurence de 1 ------")
      print(occ1)

      print("------ occurence de 0 ------")
      print(occ0)

      print("----- difference d'occurence de 1 et 0 -----")
      print(difference)

      # apply value on slider input
      balance_value <- as.integer((balance_level * difference) / 100) # this is the number of rows we are going to add into datased to make it balanced

      print("----- nombre de lignes à supprimer -----")
      print(balance_value)

      # if we have more 0 than 1
      if (occ1 < occ0) {
        ajusted_data <<- tail(dataset_zero_values_on_target, n = dim(dataset_zero_values_on_target)[1] - balance_value)
        balanced_dataset <- rbind(dataset_one_values_on_target, ajusted_data)
      } else { # if not
        ajusted_data <<- tail(dataset_one_values_on_target, n = dim(dataset_one_values_on_target)[1] - balance_value)
        balanced_dataset <- rbind(dataset_zero_values_on_target, ajusted_data)
      }
      print(dim(balanced_dataset))
      current_dataset <<- balanced_dataset

      print("***** Voir l'équilibre *****")
      print(sum(balanced_dataset$target == 0))
      print(sum(balanced_dataset$target == 1))

      # Refreshing view
      data <- eventReactive(input$file1, {
        current_dataset
      })

      data_summary(current_dataset)

      # displaying on the screen
      output$table <- DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat,
          options = list(scrollX = TRUE), filter = "top"
        )
      })
    } else {
      shinyalert("Oops!", "Veuillez d'abord gérer les valeurs manquantes.", type = "error")
    }
  })

  # Normalisation
  observeEvent(input$normalize, {
    if (choosed_dataset_number != 0) {
      if (!is.null(current_dataset)) {
        previous_dataset <<- current_dataset
        df <- sapply(current_dataset, class)
        df2 <- current_dataset %>% summarise_all(funs(n_distinct))
        for (i in 1:dim(current_dataset)[2]) {
          if ((df[i] == "integer" || df[i] == "double" || df[i] == "numeric") && df2[1, i] > 2 && df2[1, i] != dim(current_dataset)[1]) {
            for (j in 1:dim(current_dataset)[1]) {
              current_dataset[j, i] <<- current_dataset[j, i] / quantile(current_dataset[, i], 0.975)
            }
          }
        }
        print(df2[1, 1] != dim(current_dataset)[1])
        # syncing data
        data <- eventReactive(input$file1, {
          current_dataset
        })


        data_summary(current_dataset)

        # displaying on the screen
        output$table <- DT::renderDataTable({
          tmp.dat <- data()
          DT::datatable(tmp.dat,
            options = list(scrollX = TRUE), filter = "top"
          )
        })
      } else {
        shinyalert("Oops!", "Veuillez appliquer un traitement sur les données manquantes.", type = "error")
      }
    } else {
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données manquantes", type = "warning")
    }
  })

  observeEvent(input$cancel, {
    if (!is.null(current_dataset)) {
      current_dataset <<- previous_dataset
      # syncing data
      data <- eventReactive(input$file1, {
        current_dataset
      })

      data_summary(current_dataset)

      # displaying on the screen
      output$table <- DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat,
          options = list(scrollX = TRUE), filter = "top"
        )
      })
    } else {
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données", type = "error")
    }
  })

  observeEvent(input$resetEquilibre, {
    if (!is.null(current_dataset)) {
      current_dataset <<- previous_dataset
      # syncing data
      data <- eventReactive(input$file1, {
        current_dataset
      })

      data_summary(current_dataset)

      # Je voulais réinitialiser le slider mais, apparemment ça ne marche pas.
      balance_level <- NULL
      ajusted_data <- NULL
      occ0 <- 0
      occ1 <- 0
      difference <- 0
      balance_value <- 0

      makeReactiveBinding("balance_level")

      # displaying on the screen
      output$table <- DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat,
          options = list(scrollX = TRUE), filter = "top"
        )
      })
    } else {
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données", type = "error")
    }
  })
  ###########################################
  #     PRETRAITEMENT DES DONNEES fin       #
  ###########################################
}

###########################################
#                FIN SERVER               #
###########################################