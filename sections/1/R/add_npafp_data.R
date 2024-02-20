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

  #subset afp data so that is it specific for the years that I'm interested in 
  int_data <- afp_data |> 
    mutate(year = lubridate::year(date)) |> 
    filter(year >= start.year & year <= end.year)
  
  #subset afp data so that it's only NPAFP
  int_data <- int_data |> 
    filter(classification == "NPAFP")
  
  #count total incidence of NPAFP per Year
  int_data <- int_data |> 
    group_by(state, county, year) |> 
    summarise(count = n())
  
  #subset pop data so that it's only for our years of interest
  int_pop <- pop_data |> 
    filter(year >= start.year & year <= end.year)
  
  #merge pop and afp data together
  int_data <- left_join(int_pop, int_data, by = c("state", "county", "year")) |> 
    replace_na(list("count" = 0))
  
  #calculate the NPAFP rate 
  int_data <- int_data |> 
    mutate(npafp_rate = count/pop*100000)
  
  #merge NPAFP rate into spatial file 
  int_spatial_data <- left_join(spatial_data, int_data, by = c("NAMELSAD" = "county"))
  
  #create my map
  plot <- input_plot +
    geom_sf(data = int_spatial_data, aes(fill = npafp_rate)) +
    facet_wrap(~year)
  
  return(plot)
  
}
