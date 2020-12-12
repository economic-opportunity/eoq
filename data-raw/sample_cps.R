## code to prepare `sample_cps` dataset goes here

# libs --------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(janitor)
library(shinythemes)
library(shinyWidgets)

# load --------------------------------------------------------------------

dropbox_data_filepath <- "~/Dropbox/Economic Opportunity Project/Data/Comparison to Peers/Outputs"
cps_dropbox_filepath <- file.path(dropbox_data_filepath, "CPS_Cleaned.zip")


cps <- read_csv(cps_dropbox_filepath) %>%
  janitor::clean_names()


# transform ---------------------------------------------------------------

sample_cps <- cps %>% sample_frac(0.03)

# write -------------------------------------------------------------------

usethis::use_data(sample_cps, overwrite = TRUE)
