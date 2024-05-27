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

  # load population data
  pop_data <- read_rds("sections/1/data/ex_long_pop.rds")
  
  input_plot <- base_plot
  spatial_data <- ga_counties 
  afp_data <- afp_data
  pop_data <- pop_data
  start.year <- 2020
  end.year <- 2023
  
  npafp_Rate <-
      afp_data |>
        # add a column for the year
        mutate(year = year(date)) |>
        #filter to only include NPAFP
        filter(classification == "NPAFP") |>
        #summarize data by year
        group_by(county, year) |>
        summarise(count = n()) |>
        #merge in population data 
        left_join(y = pop_data, by = c("county", "year")) |>
        #calculate npafp rate - # of NPAFP cases / pop * 100000
        mutate( npafp_rate = (count / pop) * 100000 )
    #merge with spatial data (ga counties)
    
  spatial_npafp <- 
        left_join(spatial_data, npafp_Rate, by = c("NAMELSAD" = "county"))
    #plot 
  ggplot() +
        geom_sf(data = spatial_npafp, aes(fill = npafp_rate), color = "black") + 
        #scale_fill_distiller(palette = "Reds") + 
        scale_fill_viridis_c() +
        facet_wrap("year") +
        theme_bw()
  
  return(plot)
  
}



# add_npafp_data(base_plot, ga_counties, afp_data, pop_data, us_pop, start.year = 2020, end.year = 2022)
