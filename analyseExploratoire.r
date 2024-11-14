  ###########################################
  #   ANALYSE EXPLORATOIRE DES DONNEES DEBUT#
  ###########################################
  
  # observeEvent(input$btn_apply_explore_analysis, {
  #   applyAnalyseExoloratoire()
  #   applyAnalyseDsitribution()
  #   analyseUnivarieeEtBivariee(drop_na(hr_data))
  # })
  
  
  
  applyAnalyseExoloratoire <- function(hr_data, session, output){
    
    if (!is.null(hr_data)) {
      updateProgressBar(session, id = "progressBar", value = 0, total = 100)
      
      d_2<-drop_na(hr_data)
      updateProgressBar(session, id = "progressBar", value = 10)
      
    
    output$plot1<-renderPlot({
      #dessiner le graphe des frequence pour chaque variable numerique
      #plot_num(data()[,-1])+
      #labs(title="xxxxxxxxxxxxxxxxxx")
      ggplot(d_2,aes(x=gender))+
        geom_bar(fill = "coral")+
        geom_text(stat='count', aes(label=..count..), vjust=-0.2)+
        ggtitle("Distribution Sur le Genre")+
        #theme(plot.title = element_text(hjust = 0.5))+
        theme_minimal() +
        labs(title = "Répartition des genres", x = "Genre", y = "Fréquence")
    }
    
    )
    
    output$plot2<-renderPlot({
      
      #dessiner le graphe des frequence pour chaque variable numerique
      ggplot(d_2,aes(x=relevent_experience))+
        geom_bar(fill = "purple")+
        geom_text(stat='count', aes(label=..count..), vjust=-0.2)+
        #theme(plot.title = element_text(hjust = 0.5))+
        theme_minimal() +
        labs(title = "Distribution des differentes expériences", x = "Relevent Experience", y = "Fréquence")
        #ggtitle("Relevent Experience")
    }
    )
    
    output$plot3<-renderPlot({
      ggplot(d_2,aes(x=relevent_experience))+
        geom_bar(fill = "skyblue")+
        facet_wrap(~education_level)+
        ggtitle("Relevent experience by education level")+
        theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
        theme(plot.title = element_text(hjust = 0.5))
    }
    )
    output$plot4<-renderPlot({
      ggplot(d_2,aes(x=enrolled_university,fill = relevent_experience))+
        geom_bar()+
        facet_wrap(~relevent_experience)+
        theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_text(stat='count', aes(label=..count..), vjust=-1)+
        ggtitle("Enrolled University")+
        xlab("Enrolled University")+
        ylab("Count")
      
    }
    )
    output$plot5<-renderPlot({
      ggplot(d_2,aes(x=education_level,fill=relevent_experience))+
        geom_bar()+
        theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
        geom_text(stat='count', aes(label=..count..), vjust=-1)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Education Background")+
        xlab("Education Level")+
        ylab("Count")
    }
    )
    
    output$plot6<-renderPlot({
      ggplot(d_2,aes(x=major_discipline,fill=relevent_experience))+
        geom_bar()+
        facet_wrap(~gender)+
        theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
        ggtitle("College major")+
        xlab("Major")+
        ylab("Count")
    }
    
    )
    output$plot17<-renderPlot({
      ggplot(d_2,aes(x=company_size))+
        geom_bar(fill = "blue")+
        geom_text(stat='count', aes(label=..count..), vjust=-1)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Company size")+
        xlab("Size")+
        ylab("Count")
      
    }
    
    )
    output$plot18<-renderPlot({
      ggplot(d_2,aes(x= company_type))+
        geom_bar(fill = "blue")+
        theme(axis.text.x = element_text(angle = 45, hjust =1, vjust = 1))+
        geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Company Type")+
        xlab("Type")+
        ylab("Count")
      
    }
    
    )
    output$plot19<-renderPlot({
      ggplot(d_2,aes(x=experience))+
        geom_bar(fill = "blue")+
        geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Experience")+
        xlab("Experience")+
        ylab("Count")
      
      
    }
    
    )
    output$plot20<-renderPlot({
      ggplot(d_2,aes(x=last_new_job))+
        geom_bar(fill = "blue")+
        geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("Time gap")+
        xlab("Last job")+
        ylab("Count")
      
    }
    )
    
    output$plot21<-renderPlot({
      ggplot(d_2,aes(x=city_development_index,fill = relevent_experience))+
        geom_bar()+
        theme(plot.title = element_text(hjust = 0.5))+
        ggtitle("City development index")+
        xlab("City development")+
        ylab("Count")
    }
    )
    output$plot22<-renderPlot({
      ggplot(d_2,aes(x= training_hours,fill = relevent_experience))+
        ggtitle("Training hours")+
        geom_density()
    }
    )
    d_3 <- dummy_cols(d_2, select_columns = "gender")
    d_3 <- dummy_cols(d_3, select_columns = "relevent_experience")
    d_3 <- dummy_cols(d_3, select_columns = "enrolled_university")
    d_3 <- dummy_cols(d_3, select_columns = "education_level")
    #d_3 <- dummy_cols(d_3, select_columns = "major_discipline")
    d_3 <- dummy_cols(d_3, select_columns = "company_type")
    
    
    
    mcor <- cor(d_3[sapply(d_3,is.numeric)])
    
    output$plot23<-renderPlot({
      corrplot(mcor,method="color",type="upper",tl.col="black")
      
    }
    )
    
    
    output$plot24<-renderPlot({
      corrplot(mcor,method="number")
      
    })
    updateProgressBar(session, id = "progressBar", value = 100)
    
    
    shinyalert("Analyse exploratoire terminée !", "L \'analyse exploratoire est terminée, vous pouvez maintenant visualiser les graphiques", type = "success")
    }else{
      shinyalert("Veuillez charger un jeu de données !", "Le jeu de données n\a pas été trouvé", type = "error")
    }
    
    
    
  }
  
  
  
  
  
  ##########################################
  #   ANALYSE EXPLORATOIRE DES DONNEES FIN #
  ##########################################