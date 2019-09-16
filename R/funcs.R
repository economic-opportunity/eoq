
# functions ---------------------------------------------------------------

tidy_get_census <- function(...) {
  getCensus(
    ...
  ) %>% 
    as_tibble()
}

as_date_year_qtr <- function(df, variable, format = "%Y-Q%q", ...) {
  df %>% 
    mutate( 
      year_qtr  = zoo::as.yearqtr({{variable}}, format = format),
      date      = zoo::as.Date(year_qtr) 
    )
}


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

safely_get_qwi_metrics <- safely(get_qwi_metrics)

