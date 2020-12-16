

# libs --------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(janitor)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(rjson)
library(pivotr)
library(eoq)

# load --------------------------------------------------------------------

# post processing to make acs and cps smaller
acs_dropbox_link <- "https://www.dropbox.com/s/sg2pjcbr0iyzzvw/ACS_Cleaned.zip?dl=1"
cps_dropbox_link <- "https://www.dropbox.com/s/zgqsb2ckw69putb/CPS_Cleaned.zip?dl=1"
bls_dropbox_link <- "https://www.dropbox.com/s/agt6mj16d52flhj/lau_unemp_max_month.csv?dl=1"

tmp_dir <- tempdir()
acs_tmp <- file.path(tmp_dir, "asc.csv.zip")
cps_tmp <- file.path(tmp_dir, "cps.csv.zip")

download.file(acs_dropbox_link, acs_tmp)
download.file(cps_dropbox_link, cps_tmp)

cps <- read_csv(cps_tmp) %>%
  janitor::clean_names()

acs <- read_csv(acs_tmp) %>%
  janitor::clean_names()

# exploration ---------------------------------------------------------------


acs_wage <- acs %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  make_percentiles(totalwage, age_bucket, education, race_ethnicity, is_male) %>% 
  ungroup()

acs_employment <- acs %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  calc_unemployment_rate(employmentstatus, age_bucket, education, race_ethnicity, is_male) %>% 
  ungroup()

cps_wage <- cps %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  make_percentiles(hourlywage, age_bucket, education, race_ethnicity, is_male) %>% 
  ungroup()


cps_employment <-  cps %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  calc_unemployment_rate(employmentstatus, age_bucket, education, race_ethnicity, is_male) %>% 
  ungroup()

cps_fips_soc <- cps %>% 
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  group_by(countyfips, soc_group_description) %>% 
  summarize(wage = mean(hourlywage, na.rm = TRUE),
            unemployment_rate = sum(employmentstatus == "unemployed", na.rm = TRUE) / sum(employmentstatus %in% c("unemployed", "employed"), na.rm = TRUE),
            wage_n = sum(!is.na(hourlywage)),
            emp_n = sum(!is.na(employmentstatus))) %>% 
  ungroup()
  
cps_fips_naics <- cps %>% 
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  group_by(countyfips, naics_2digit_label) %>% 
  summarize(wage = mean(hourlywage, na.rm = TRUE),
            unemployment_rate = sum(employmentstatus == "unemployed", na.rm = TRUE) / sum(employmentstatus %in% c("unemployed", "employed"), na.rm = TRUE),
            wage_n = sum(!is.na(hourlywage)),
            emp_n = sum(!is.na(employmentstatus))) %>% 
  ungroup()


cps_fips_soc_naics <- cps %>% 
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  group_by(countyfips, soc_group_description, naics_2digit_label) %>% 
  summarize(wage = mean(hourlywage, na.rm = TRUE),
            unemployment_rate = sum(employmentstatus == "unemployed", na.rm = TRUE) / sum(employmentstatus %in% c("unemployed", "employed"), na.rm = TRUE),
            wage_n = sum(!is.na(hourlywage)),
            emp_n = sum(!is.na(employmentstatus))) %>% 
  ungroup()





# actual important ones ---------------------------------------------------

acs_wage <- acs %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  clean_sex(is_male) %>% 
  group_by(age_bucket, education, race_ethnicity, sex) %>% 
  mutate(percentile = ntile(totalwage, 100)) %>% 
  pivotr::cube(groups = c(age_bucket, education, race_ethnicity, sex, percentile), 
               mean = mean(totalwage, na.rm = TRUE),
               n = n(),
               .totals = "All") %>% 
  ungroup()

acs_unemployment <- acs %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  clean_sex(is_male) %>% 
  pivotr::cube(groups = c(age_bucket, education, race_ethnicity, sex), 
               unemp_rate = sum(employmentstatus == 1, na.rm = TRUE) / sum(employmentstatus %in% c(1,2), na.rm = TRUE),
               n = n(),
               .totals = "All") %>% 
  ungroup()

cps_hours <- cps %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  clean_sex(is_male) %>% 
  pivotr::cube(groups = c(age_bucket, education, race_ethnicity, sex),
               p25 = quantile(hoursperweek, .25, na.rm = TRUE),
               p75 = quantile(hoursperweek, .75, na.rm = TRUE),
               p99 = quantile(hoursperweek, .99, na.rm = TRUE),
               .totals = "All") %>% 
  ungroup()

fips_soc_naics <- acs %>% 
  make_age_buckets(age) %>%
  clean_education(education) %>%
  pivotr::cube(groups = c(age_bucket, education, countyfips), 
               wage = mean(totalwage, na.rm = TRUE),
               unemp_rate = sum(employmentstatus == 1, na.rm = TRUE) / sum(employmentstatus %in% c(1,2), na.rm = TRUE),
               n = n(),
               .totals = "All") %>% 
  ungroup()
  
  

# write -------------------------------------------------------------------

dropbox_data_filepath <- "~/Dropbox/Economic Opportunity Project/Data/Comparison to Peers/Outputs"
acs_wage_filepath <- file.path(dropbox_data_filepath, "acs_wage.csv")
acs_unemployment_filepath <- file.path(dropbox_data_filepath, "acs_unemployment.csv")
cps_hours_filepath <- file.path(dropbox_data_filepath, "cps_hours.csv")

write_csv(acs_wage, acs_wage_filepath)
write_csv(acs_unemployment, acs_unemployment_filepath)
write_csv(cps_hours, cps_hours_filepath)
