#' Takes in a plot object and returns a ggplot map with 
#' npafp rate data attached. 
#' 
#' @description
#' Takes in a plot object adds npafp data and returns a facet wrapped
#' plot object depending on start and end year. 
#' @param input_plot a ggplot object to be passed in. In this case it is base_plot
#' @param spatial_data a sf object with variables to join in data from pop
#' @param afp_data a tibble object loading afp information
#' @param pop_data a tibble object with pop data in long format
#' @param start.year an integer for the first year of data to begin interpolation
#' @param end.year an integer for the last year of data to end interpolation
#' @returns a ggplot object 
add_npafp_data <- function(input_plot, spatial_data, afp_data, pop_data, start.year, end.year){

  int_data <- afp_data |> 
    dplyr::filter(classification == "NPAFP") |>
    dplyr::mutate(year = lubridate::year(date)) |> 
    dplyr::group_by(year, state, county) |> 
    dplyr::summarise(count = n()) |> 
    dplyr::left_join(pop_data, by = c("year", "state", "county")) |> 
    dplyr::mutate(npafp_rate = count/pop*100000) |> 
    dplyr::select(year, state, county, npafp_rate) |> 
    filter(year >= start.year & year <= end.year)
  
  return(plot)
  
}