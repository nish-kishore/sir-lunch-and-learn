#' Takes in a plot object and returns a ggplot map with ES sites added to it. 
#' 
#' @description
#' Takes in ES data and merges it to spatial files. Filters ES data 
#' based off of having at least a minimum number of collections and portrayed
#' on the map with different symbols for each type and only the years requested
#' @import dplyr
#' @param input_plot a ggplot object to be passed in. In this case it is base_plot
#' @param es_sites a spatial points dataset with information about sites
#' @param es_data a tibble with information on sites years and # of collections 
#' @param min.coll an integer describing the minimum number of collections 
#' required to be able to include a site in the map
#' @param start.year an integer for the first year of data to begin interpolation
#' @param end.year an integer for the last year of data to end interpolation
#' @returns a ggplot object 
add_es_sites <- function(input_plot, es_sites, es_data, min.coll, start.year, end.year){
  
  long_es_sites <- lapply(start.year:end.year, function(x) mutate(es_sites, year = x)) |> 
    dplyr::bind_rows()
  
  int_data <- es_data |> 
    dplyr::filter(n_collections >= min.coll) |> 
    dplyr::filter(year >= start.year & year <= end.year)
  
  int_spatial <- left_join(long_es_sites, int_data, by = c("site_id" ,"type", "year")) |> 
    filter(!is.na(n_collections))
  
  plot <- input_plot + 
    ggplot2::geom_sf(data = int_spatial, aes(color = n_collections, shape = type)) + 
    ggplot2::facet_wrap(~year)
  
  return(plot)
  
}