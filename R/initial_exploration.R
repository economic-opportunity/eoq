library(tidyverse)
library(censusapi)
library(skimr)


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


# apis --------------------------------------------------------------------

apis <- listCensusApis() %>% 
  as_tibble()

View(apis)
skim(apis)

# data --------------------------------------------------------------------

# get info about columns available in QWI for SE (sex/education) endpoint
qwi_metadata <- listCensusMetadata("timeseries/qwi/se")

# get all numeric variables for coercing to numeric from character later
qwi_numeric_variables <- qwi_metadata %>% 
  filter(concept == "Quarterly Workforce Indicators (QWI)") %>% 
  pull(name)

# get all variables by sex and education from census API
raw_qwi_se <- tidy_get_census(
  name = "timeseries/qwi/se", 
  vars = c(qwi_numeric_variables, "education"), 
  region = "county:*", # get all counties
  regionin = "state:06", # CA
  time = "from 2000 to 2018", # get data from 2000 to 2018
  sex = "1", # get both male and female
  sex = "2" # get both male and female
  ) 

#clean raw QWI SE data
clean_qwi_se <- raw_qwi_se %>% 
  # format date variable to useful formats
  as_date_year_qtr(time) %>% 
  # convert numeric variables from characters
  mutate_at(vars(qwi_numeric_variables), as.numeric) 
 
# quick exploration of QWI SE dataset
glimpse(clean_qwi_se)
skim(clean_qwi_se)
