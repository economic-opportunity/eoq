

# libs --------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(janitor)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(rjson)

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

# transform ---------------------------------------------------------------


acs_wage <- acs %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  make_percentiles(totalwage, age_bucket, education, race_ethnicity, is_male)

acs_employment <- acs %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  calc_unemployment_rate(employmentstatus, age_bucket, education, race_ethnicity, is_male, industry)

cps_wage <- cps %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  make_percentiles(hourlywage, age_bucket, education, race_ethnicity, is_male)

cps_hours <- cps %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  make_percentiles(hoursperweek, age_bucket, education, race_ethnicity, is_male)

cps_employment <-  cps %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  calc_unemployment_rate(employmentstatus, age_bucket, education, race_ethnicity, is_male)

cps_fips_soc <- cps %>% 
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  group_by(countyfips, soc_group_description) %>% 
  summarize(wage = mean(hourlywage, na.rm = TRUE),
            unemployment_rate = sum(employmentstatus == "unemployed", na.rm = TRUE) / sum(employmentstatus %in% c("unemployed", "employed"), na.rm = TRUE)) %>% 
  ungroup()
  
cps_fips_naics <- cps %>% 
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>%
  group_by(countyfips, naics_2digit_label) %>% 
  summarize(wage = mean(hourlywage, na.rm = TRUE),
            unemployment_rate = sum(employmentstatus == "unemployed", na.rm = TRUE) / sum(employmentstatus %in% c("unemployed", "employed"), na.rm = TRUE)) %>% 
  ungroup()


