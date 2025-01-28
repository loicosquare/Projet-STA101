###########################################
#        ANALYSE FACTIORIELLE DEBUT       #
###########################################

####--------------cah DEBUT------------####


generate_cah <- function(data) {
  # Handle potential NAs in experience during conversion
  data <- data %>%
    mutate(experience = case_when(
      experience == ">20" ~ 21,
      experience == "<1" ~ 0,
      TRUE ~ as.numeric(experience)
    )) %>%
    filter(!is.na(experience))  # Remove rows with NAs in experience

  # Select numeric variables for PCA
  numeric_data <- data[, c("city_development_index", "training_hours", "age", "hours_per_week", "experience")]

  # Center and scale the data
  numeric_data_scaled <- scale(numeric_data)

  # Calculate distance matrix
  dist_matrix <- dist(numeric_data_scaled, method = "euclidean")

  # Perform hierarchical clustering
  cah_result <- hclust(dist_matrix, method = "ward.D2")

  # Return the clustering result
  return(cah_result)
}




####--------------CAH FIN------------######



###########################################
#       ANALYSE FACTORIELLE FIN           #
###########################################