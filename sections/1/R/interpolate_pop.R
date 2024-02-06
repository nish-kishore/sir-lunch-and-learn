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
  us_pop |> 
    #convert wide to long
    tidyr::pivot_longer(dplyr::starts_with("pop"), names_to = "year", names_prefix = "pop") |> 
    #this only contains a few years of data so we can linearly extrapolate
    #to fill in the rest 
    dplyr::mutate(
      year = stringr::str_replace(year, "_", ""), 
      year = as.numeric(year)
    ) %>%
    #everything inide the {} below references the output above as '.'
    {
      #bind full years of data with fitted full years
      dplyr::bind_cols(
        #create the "fake" dataset
        tidyr::expand_grid(
          state = "Georgia", 
          county = unique(.$county),
          year = start.year:end.year), 
        "fit_pop" = predict.lm(
          #linear model specific for  year and county
          object = lm(value ~ year + county, data = .), 
          #producing the same newdata as above
          newdata = tidyr::expand_grid(
            state = "Georgia", 
            county = unique(.$county),
            year = start.year:end.year)
        )
      )
    } |> 
    dplyr::mutate(pop = round(fit_pop, 0))
}
