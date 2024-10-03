# As functions
library(palmerpenguins)
library(tidyverse)
get_island_data <- function(island_name = "Biscoe") {
  
  this_island_data <- penguins |>
    filter(island == island_name) |>
    filter(!is.na(sex),
           !is.na(body_mass_g))
  
  return(this_island_data)
  
}

get_predicted_body_masses <- function(island_data) {
  
  # Fits a linear model of body_mass_g ~ sex + species + sex:species
  island_lm <- lm(body_mass_g ~ sex * species,
                  data = island_data)
  
  # Extracting the predicted values from the linear model
  island_lm_predictions <- island_data |>
    select(island, sex, species) |>
    distinct() 
  
  island_lm_predictions <- island_lm_predictions |>
    mutate(predicted = predict(island_lm, newdata = island_lm_predictions))
  
  # Return the predictions
  
  return(island_lm_predictions)
  
}

# Workflow as functions

biscoe_data <- get_island_data(island_name = "Biscoe")
biscoe_predictions <- get_predicted_body_masses(biscoe_data)

torgersen_predictions <- 
  get_island_data(island_name = "Torgersen") |>
  get_predicted_body_masses()

dream_predictions <- 
  get_island_data("Dream") |> 
  get_predicted_body_masses()


islands <- c("Biscoe", "Dream", "Torgersen")

island_predictions_df <- 
  islands |>
  map(get_island_data) |>
  map(get_predicted_body_masses) |>
  bind_rows()

islands_data <- list()

for(an_island in c("Biscoe", "Dream", "Torgersen")) {
  islands_data[[an_island]] <- get_island_data(island_name = an_island)
}

### purrr::map to get the predictions

islands_predictions <- purrr::map(islands_data, get_predicted_body_masses)

island_predictions_df <- dplyr::bind_rows(islands_predictions)
