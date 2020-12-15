

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


df_list_se <- list()

for (i in seq_along(state_fips$fips)){
  
  # Print intial collection status
  print(glue::glue("Collecting data for {x}", x = state_fips$fips[[i]]))
  # 1 second break b/t calls for responsible api usage
  
  df_list_se[[i]] <- safely_get_qwi_metrics(regionin = str_c("state:", state_fips$fips[[i]]))
  
  # Print finished status
  print(glue::glue("Finished collecting data for {x}", x = state_fips$fips[[i]]))
  
  Sys.sleep(1)
  
}

df_list_backup_se <- df_list_se

# df_list[sapply(df_list, is.null)] <- NULL

results_se <-  map_dfr(df_list_se, "result")

results_se <- results_se %>% 
  clean_names() %>% 
  mutate(fips = str_c(state, county))

write_csv(results_se, "~/Dropbox/Economic Opportunity Project/Data/QWI/Outputs/qwi_se_18-20.csv")



# sa ----------------------------------------------------------------------


df_list_sa <- list()

for (i in seq_along(state_fips$fips)){
  # Print intial collection status
  print(glue::glue("Collecting data for {x}", x = state_fips$fips[[i]]))
  # 1 second break b/t calls for responsible api usage
  
  df_list_sa[[i]] <- safely_get_qwi_metrics(name = "timeseries/qwi/sa",
                                            regionin = str_c("state:", state_fips$fips[[i]]))
  # Print finished status
  print(glue::glue("Finished collecting data for {x}", x = state_fips$fips[[i]]))
  
  Sys.sleep(1)
}


df_list_backup_sa <- df_list_sa

results_sa <-  map_dfr(df_list_sa, "result")

results_sa <- results_sa %>% 
  clean_names() %>% 
  mutate(fips = str_c(state, county))

write_csv(results_sa, "~/Dropbox/Economic Opportunity Project/Data/QWI/Outputs/qwi_sa_19-20.csv")

# rh ----------------------------------------------------------------------


df_list_rh <- list()

for (i in seq_along(state_fips$fips)){
  # Print intial collection statu
  print(glue::glue("Collecting data for {x}", x = state_fips$fips[[i]]))
  # 1 second break b/t calls for responsible api usage
  
  df_list_rh[[i]] <- safely_get_qwi_metrics(name = "timeseries/qwi/rh",
                                            regionin = str_c("state:", state_fips$fips[[i]]))
  # Print finished status
  print(glue::glue("Finished collecting data for {x}", x = state_fips$fips[[i]]))
  
  Sys.sleep(1)
}


df_list_backup_rh <- df_list_rh

results_rh <-  map_dfr(df_list_rh, "result")

results_rh <- results_rh %>% 
  clean_names() %>% 
  mutate(fips = str_c(state, county))

write_csv(results_rh, "~/Dropbox/Economic Opportunity Project/Data/QWI/Outputs/qwi_rh_19-20.csv")


