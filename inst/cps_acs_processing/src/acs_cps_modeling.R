# libs --------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(tidymodels)
library(janitor)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(rjson)
library(pivotr)
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results

# load --------------------------------------------------------------------

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


# clean -------------------------------------------------------------------

acs_clean <- acs %>%
  make_age_buckets(age) %>%
  clean_race_ethnicity(racehispanic) %>%
  clean_education(education) %>% 
  clean_sex(is_male) %>%
  ungroup()


# viz ---------------------------------------------------------------------

acs_clean %>% 
  ggplot(aes(totalwage, age, size = is_male, color = education)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE)

# model -------------------------------------------------------------------



lm_mod <- linear_reg() %>% 
  set_engine("lm")

lm_fit <- lm_mod %>% 
  fit(
    totalwage ~ is_male + age_bucket + education + racehispanic,
    data = acs_clean
    )
