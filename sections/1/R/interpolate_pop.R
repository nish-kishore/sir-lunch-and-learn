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

}
