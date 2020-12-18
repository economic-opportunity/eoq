############################################-
# Script to consolidate business employment dynamics
############################################-



############################################-
# Section 0: Packages, set up ####
############################################-

# packages
library(tidyverse)
library(data.table)



############################################-
# Section 1: Consolidate data
############################################-



# Section 1.1: Read data ----------------------------------#

# load series attributes
bd_attr <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.series')

# load series data
bd_data <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.data.1.AllItems')


# load various mapping files (below)

# state/national codes
xwalk_state <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.state')

# seasonal/not seasonal
xwalk_seasonal <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.seasonal')

# rate level
xwalk_rate <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.ratelevel')

# periodicity
xwalk_periodicity <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.periodicity')

# onwnership
xwalk_ownership <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.ownership')

# industry
xwalk_industry <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.industry')

# data class
xwalk_data_c <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.dataclass')

# data element
xwalk_data_e <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.dataelement')

# size class
xwalk_size_c <- readr::read_tsv('https://download.bls.gov/pub/time.series/bd/bd.sizeclass')



# Section 1.2: Join relevant columns to the data -------------------------#


# map attributes to the data
bd_data_2 <- dplyr::left_join(bd_data,
                              bd_attr,
                              by = 'series_id')


# now do one big, piped left join to map all the other columns
bd_data_3 <- bd_data_2 %>%
  dplyr::left_join(.,
                   select(xwalk_data_c, 
                          dataclass_code, 
                          dataclass_name),
                   by = 'dataclass_code') %>%
  dplyr::left_join(.,
                   select(xwalk_data_e, 
                          dataelement_code,
                          dataelement_name),
                   by = 'dataelement_code') %>%
  dplyr::left_join(.,
                   select(xwalk_industry,
                          industry_code,
                          industry_name),
                   by = 'industry_code') %>%
  dplyr::left_join(.,
                   xwalk_ownership,
                   by = 'ownership_code') %>%
  dplyr::left_join(.,
                   xwalk_periodicity,
                   by = 'periodicity_code') %>%
  dplyr::left_join(.,
                   xwalk_rate,
                   by = 'ratelevel_code') %>%
  dplyr::left_join(.,
                   xwalk_seasonal,
                   by = c('seasonal' = 'seasonal_code')) %>%
  dplyr::left_join(.,
                   xwalk_size_c,
                   by = 'sizeclass_code') %>%
  dplyr::left_join(.,
                   xwalk_state,
                   by = 'state_code')


# select relevant columns and create final table
bd_data_final <- dplyr::select(bd_data_3,
                               year,
                               state = state_name,
                               quarter = period,
                               value,
                               series_title,
                               data_class = dataclass_name,
                               data_element = dataelement_name,
                               industry = industry_name,
                               ownership = ownership_name,
                               periodicity = periodicity_name,
                               rate_level = ratelevel_name,
                               seasonal = seasonal_text,
                               size_class = sizeclass_name) %>%
  arrange(desc(year), desc(quarter))


# # to csv
# readr::write_csv(bd_data_final,
#                  '../outputs/bd_1992_2020.csv')
  