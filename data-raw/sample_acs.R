## code to prepare `sample_acs` dataset goes here

# libs --------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(janitor)
library(shinythemes)
library(shinyWidgets)
library(eoq)

# load --------------------------------------------------------------------

dropbox_data_filepath <- "~/Dropbox/Economic Opportunity Project/Data/Comparison to Peers/Outputs"
acs_dropbox_filepath <- file.path(dropbox_data_filepath, "ACS_Cleaned.zip")

acs <- read_csv(acs_dropbox_filepath) %>%
  janitor::clean_names()

# transform ---------------------------------------------------------------

sample_acs <- acs %>% sample_frac(0.01)

# write -------------------------------------------------------------------

usethis::use_data(sample_acs, overwrite = TRUE)
