#' Convert population data from wide to long and interpolate missing data for 
#' years where data is unavailable.
#' 
#' @description
#' Takes in wide incomplete population data and turns it into 
#' long form with interpolated data along a range. 
#' @import dplyr tidyr stringr
#' @param pop_data Wide population data from census with only certain years provided 
#' @param start.year an integer for the first year of data to begin interpolation
#' @param end.year  an integer for the last year of data to end interpolation
#' @returns A tibble with variables `state`, `county`, `year`, `pop`
interpolate_pop <- function(pop_data, start.year, end.year){

  pop_data <- us_pop
  start.year <- 2010
  end.year <- 2030
  
  #convert wide to long and clean up year data
  our_data<-pivot_longer(pop_data, starts_with("pop"), names_to = "year", names_prefix = "pop") |> 
    mutate(
      year = str_replace(year, "_", ""), 
      year = as.numeric(year)
    )
  #state, county, year, pop
  
  #fit a linear model for population data with variations for year and county
  model <- lm(value ~ year + county, data = our_data)
  
  #create an empty dataset with all of our years of interest and all georgia counties
  simulated.data <- expand_grid(
    "county" = unique(our_data$county), 
    "year" = start.year:end.year
  )
  
  #predict from the linear model for other years of data
  simulated.data$prediction <- predict(model, newdata = simulated.data)
  
}
