

# load libraries ---------------------------------------------------------
library(tidyverse) # the one and only tidyverse
library(censusapi) # access Census data via API
library(skimr) # summarize data
library(janitor) # cleaning data and column names
library(purrr) # working with list columns
library(eoq)

# source  -----------------------------------------------------------------

source("inst/analysis/data/input/datapasta_tribbles.R")

# apis --------------------------------------------------------------------


df_list <- list()

for (i in seq_along(state_fips$fips)){
  
  df_list[[i]] <- safely_get_qwi_metrics(regionin = str_c("state:", state_fips$fips[[i]]))
  
}

df_list_backup <- df_list

# df_list[sapply(df_list, is.null)] <- NULL

results <-  map_dfr(df_list, "result")

results <- results %>% 
  clean_names() %>% 
  mutate(fips = str_c(state, county))

write_csv(results, "~/Dropbox/Economic Opportunity Project/Data/QWI/Outputs/qwi_se_18-20.csv")
