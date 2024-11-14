#Inclusion des fichiers séparés
source('server.r')
source('ui.r')
source('ACP.r')

shinyApp(ui,server)
