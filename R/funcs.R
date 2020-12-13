
# functions ---------------------------------------------------------------

#' get census tidy version
#'
#' @param ... 
#'
#' @return
#' @export
#' @importFrom tibble as_tibble
#' @importFrom censusapi getCensus
#' @examples
tidy_get_census <- function(...) {
  censusapi::getCensus(
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
#' @importFrom dplyr mutate
#' @examples
as_date_year_qtr <- function(df, variable, format = "%Y-Q%q", ...) {
  df %>% 
    mutate( 
      year_qtr  = zoo::as.yearqtr({{variable}}, format = format),
      date      = zoo::as.Date(year_qtr) 
    )
}


#' Get QWI metrics
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
                            vars = c("Emp", "HirA", "HirAEndR", "SepBeg", "SepBegR", "EarnHirNS", "EarnS", "education", "sex"), 
                            region = "county:*", 
                            regionin = "state:06",
                            time = "from 2018 to 2022", ...) {
  
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
#' @param ... 
#'
#' @return
#' @importFrom purrr safely
#' @export
#' @examples
safely_get_qwi_metrics <- purrr::safely(get_qwi_metrics)


