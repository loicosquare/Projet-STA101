#Inclusion des fichiers séparés
source('server.r')
source('ui.r')
source('ACP.r')
source('AFC.r')
source('CAH.r')
source('Kmeans.r')

shinyApp(ui,server)
