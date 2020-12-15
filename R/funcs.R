
# functions ---------------------------------------------------------------

#' get census tidy version
#'
#' @param ... fields to pass to censusapi::getCensus
#'
#' @return
#' @export
#' @importFrom tibble as_tibble
#' @importFrom censusapi getCensus
tidy_get_census <- function(...) {
  censusapi::getCensus(
    ...
  ) %>% 
    as_tibble()
}

#' Turn year quarter into date data
#'
#' @param df dataframe
#' @param variable field with data in format "2020-Q1"
#' @param format format to put date into
#'
#' @return
#' @export
#' @importFrom dplyr mutate
as_date_year_qtr <- function(df, variable, format = "%Y-Q%q") {
  df %>% 
    mutate( 
      year_qtr  = zoo::as.yearqtr({{variable}}, format = format),
      date      = zoo::as.Date(year_qtr) 
    )
}


#' Get QWI metrics
#'
#' @param name name of census api
#' @param vars vars to select from api
#' @param region the level of geo (state, county)
#' @param regionin limiting the region to explore
#' @param time the time period for data
#' @param ... addtional arguments passed to tidy_get_census
#'
#' @return
#' @export
#'
get_qwi_metrics <- function(name = "timeseries/qwi/se", 
                            vars = c("Emp", "HirA", "HirAEndR", "SepBeg", "SepBegR", "EarnHirNS", "EarnS"), 
                            region = "county:*", 
                            regionin = "state:06",
                            time = "from 2019 to 2022", ...) {
  
  if (name == "timeseries/qwi/se"){
    vars = c(vars, c("education", "sex"))
  } else if (name == "timeseries/qwi/sa"){
    vars = c(vars, c("sex", "agegrp"))
  } else if (name == "timeseries/qwi/rh") {
    vars = c(vars, c("race", "ethnicity"))
  }
  
  tidy_get_census(
    name = name,
    vars = vars,
    region = region,
    regionin = regionin,
    time = time,
    ...
  )
}

#' Safely get QWI metrics
#'
#' @param ... fields to pass to get_qwi_metrics
#' @return
#' @importFrom purrr safely
#' @export
safely_get_qwi_metrics <- safely(get_qwi_metrics)


