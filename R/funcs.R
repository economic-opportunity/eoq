
# functions ---------------------------------------------------------------

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
tidy_get_census <- function(...) {
  getCensus(
    ...
  ) %>% 
    as_tibble()
}

#' Title
#'
#' @param df 
#' @param variable 
#' @param format 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
as_date_year_qtr <- function(df, variable, format = "%Y-Q%q", ...) {
  df %>% 
    mutate( 
      year_qtr  = zoo::as.yearqtr({{variable}}, format = format),
      date      = zoo::as.Date(year_qtr) 
    )
}


#' Title
#'
#' @param name 
#' @param vars 
#' @param region 
#' @param regionin 
#' @param time 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
get_qwi_metrics <- function(name = "timeseries/qwi/se", 
                            vars = c(qwi_numeric_variables, "education"), 
                            region = "county:*", 
                            regionin = "state:06",
                            time = "from 2000 to 2018", ...) {
  tidy_get_census(
    name = name,
    vars = vars,
    region = region,
    regionin = regionin,
    time = time,
    ...
  )
}

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
safely_get_qwi_metrics <- function(...){
  purrr::safely(get_qwi_metrics(...)) 
}


