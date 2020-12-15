
############################################-
# Script to consolidate local area unemployment rate
############################################-



############################################-
# Section 0: Packages, set up ####
############################################-

# packages
library(tidyverse)
library(data.table)



############################################-
# Section 1: Consolidate unemployment by state and fips
############################################-



# Section 1.1: Read data ----------------------------------#



# all series attributes (LAU: local area unemployment)
lau_attr <- readr::read_tsv('https://download.bls.gov/pub/time.series/la/la.series')


# All LAUS areas, not seasonally adjusted, monthly and annual averages, 2015-forward
lau_series_15_19 <- readr::read_tsv(
  'https://download.bls.gov/pub/time.series/la/la.data.0.CurrentU15-19')


# All LAUS areas, not seasonally adjusted, monthly and annual averages, 2020-forward
lau_series_20 <- readr::read_tsv(
  'https://download.bls.gov/pub/time.series/la/la.data.0.CurrentU20-24')


# stack lau_series_15_19 and lau_series_20
lau_series <- base::rbind(lau_series_15_19, lau_series_20)


# read measures codes xwalk (e.g., 03: unemployment rate, 04: unemployment)
lau_measures <- readr::read_tsv('https://download.bls.gov/pub/time.series/la/la.measure')


# read state fips <-> state name xwalk
state_xwalk <- readr::read_tsv(
  'https://download.bls.gov/pub/time.series/la/la.state_region_division')


# read period <-> month xwalk
period_xwalk <- readr::read_tsv('https://download.bls.gov/pub/time.series/la/la.period')


# read area xwalk
area_type <- readr::read_tsv('https://download.bls.gov/pub/time.series/la/la.area')


# read area type code xwalk
area_type_code <- readr::read_tsv('https://download.bls.gov/pub/time.series/la/la.area_type')



# Section 1.2: Join relevant columns to the data -------------------------#



# join attributes to series data
lau_series_2 <- dplyr::left_join(lau_series,
                                 dplyr::select(lau_attr,
                                               series_id,
                                               area_code,
                                               measure_code,
                                               seasonal,
                                               srd_code,
                                               series_title),
                                 by = 'series_id')


# join area type (contains name -- e.g., California)
lau_series_3 <- dplyr::left_join(lau_series_2,
                                 area_type,
                                 by = 'area_code')


# join area type text (states if record is state, metropolitan area, county, etc)
lau_series_4 <- dplyr::left_join(lau_series_3,
                                 area_type_xwalk,
                                 by = 'area_type_code')


# join measure name
lau_series_5 <- dplyr::left_join(lau_series_4,
                                 period_xwalk,
                                 by = 'period')


# join state name
lau_series_6 <- dplyr::left_join(lau_series_5,
                                 state_xwalk,
                                 by = 'srd_code')


# join measure type (e.g. 03: unemployment rate, 04: unemployment)
lau_series_7 <- dplyr::left_join(lau_series_6,
                                 lau_measures,
                                 by = 'measure_code')
                                 

# Section 1.3: Filter data and polish columns -------------------------#


# select and rename columns, and filter to specific geographic levels, in one go
lau_series_final <- dplyr::select(
  lau_series_7, 
  year,
  period_name,
  state = srd_text,
  location = area_text,
  level = areatype_text,
  measure = measure_text,
  value,
  area_code) %>%
  dplyr::filter(
    level %in% c('Cities and towns above 25,000 population',
                 'Counties and equivalents',
                 'Metropolitan areas',
                 'Statewide'))
  
# if the level is 'Counties and equivalents', extract the fips from 'area_code'
lau_series_final$fips <- dplyr::case_when(
  lau_series_final$level %in% 'Counties and equivalents' ~ base::substr(
    lau_series_final$area_code, 3, 7),
  TRUE ~ NA_character_)


# remove 'area_code' -- not really useful anymor
lau_series_final$area_code <- NULL



############################################-
# Section 2: Write to csv
############################################-


# # write to csv
# readr::write_csv(lau_series_final,
#                  '../outputs/lau_15-20.csv')

############################################-
# Section 3: Write smaller dataset to csv
############################################-

lau_series_final %>% 
  # convert month name to number
  mutate(month_no = match(period_name, month.name)) %>% 
  filter(
    # filter to the latest year 
    year == max(year),
    # filter to unemployment rate
    measure == "unemployment rate",
    # filter to county fips
    !is.na(fips)
    ) %>% 
  filter( 
    #filter to max month in the max year 
    month_no == max(month_no)
    ) %>% 
  write_csv('../outputs/lau_unemp_max_month.csv')
