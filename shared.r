###########################################
#               SHARED DEBUT              #
#   FICHIER POUR LES FONCTION PARTAGEES   #
###########################################

# Définir une fonction qui transforme l'expérience
transform_experience <- function(data) {
    data %>%
        mutate(experience = case_when(
            experience == ">20" ~ 21,
            experience == "<1" ~ 0,
            TRUE ~ as.numeric(experience)
        ))
}

###########################################
#               SHARED FIN                #
#   FICHIER POUR LES FONCTION PARTAGEES   #
###########################################
