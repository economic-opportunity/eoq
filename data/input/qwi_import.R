
# load libraries ---------------------------------------------------------
library(tidyverse) # the one and only tidyverse
library(censusapi) # access Census data via API
library(skimr) # summarize data
library(janitor) # cleaning data and column names

# source funcs ------------------------------------------------------------

source("R/funcs.R")

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
write_csv(clean_qwi_se, "~/Google Drive/EOQ/Data/QWI/qwi_se_quarterly.csv")

# get industry breakdown --------------------------------------------------

# get all variables by sex and education from census API
# raw_qwi_se_industry_51 <- tidy_get_census(
#   name = "timeseries/qwi/se", 
#   vars = c(qwi_numeric_variables, "education"), 
#   region = "county:*", # get all counties
#   regionin = "state:06", # CA
#   time = "from 2000 to 2018", # get data from 2000 to 2018
#   sex = "1", # get both male and female
#   sex = "2", # get both male and female
#   industry = 51
# ) 
# 
# raw_qwi_se_industry_51 <- get_qwi_metrics(
#   sex = "1", sex = "2",
#   industry = 51
# )

# https://www.census.gov/eos/www/naics/2017NAICS/2017_NAICS_Structure_Summary_Table.xlsx
industries <- tibble::tribble(
  ~Sector,                                                                      ~Name,
  "11",                               "Agriculture, Forestry, Fishing and Hunting",
  "21",                            "Mining, Quarrying, and Oil and Gas Extraction",
  "22",                                                                "Utilities",
  "23",                                                             "Construction",
  "31-33",                                                            "Manufacturing",
  "42",                                                          "Wholesale Trade",
  "44-45",                                                             "Retail Trade",
  "48-49",                                           "Transportation and Warehousing",
  "51",                                                              "Information",
  "52",                                                    "Finance and Insurance",
  "53",                                       "Real Estate and Rental and Leasing",
  "54",                         "Professional, Scientific, and Technical Services",
  "55",                                  "Management of Companies and Enterprises",
  "56", "Administrative and Support and Waste Management and Remediation Services",
  "61",                                                     "Educational Services",
  "62",                                        "Health Care and Social Assistance",
  "71",                                      "Arts, Entertainment, and Recreation",
  "72",                                          "Accommodation and Food Services",
  "81",                            "Other Services (except Public Administration)",
  "92",                                                    "Public Administration"
) %>% 
  clean_names() %>% 
  mutate(sector = parse_number(sector))

qwi_by_industry <- industries %>% 
  mutate(
    df = map(sector, ~ {
      # Print intial collection status
      print(glue::glue("Collecting data for {x}", x = .x))
      # Get QWI metrics for given sector, or catch error
      safely_get_qwi_metrics(industry = .x, sex = "1", sex = "2")
      # Print finished status
      print(glue::glue("Finshed collecting data for {x}", x = .x))
      # 10 second break b/t calls for responsible api usage
      Sys.sleep(10)
    }
    )
  ) %>% 
  compact()

valid_qwi_by_industry <- qwi_by_industry %>% 
  # unnesting list column
  unnest(df) %>% 
  # filtering on actual returned tibbles (not errors or null values)
  filter(df %>% map(is_tibble) %>% map_lgl(any)) %>% 
  # unnesting list column again
  unnest(df)

write_csv(valid_qwi_by_industry, "~/Google Drive/EOQ/Data/QWI/qwi_se_by_industry.csv.gz")
