
##################
# BLS API Import #
##################

# Purpose: Import tables from the BLS containing data on California's
# employment metrics in since 2010. This script will performn the 
# imports at the state and county (FIPS) level

# ======================================================================
# Section 1: Packages, small scale tests, setup

# packages
library(blsAPI) # package simplifying interaction with bls api
library(rjson) # process json
library(listviewer) # helps view nested list
library(tidyverse) # the one and only tidyverse
library(data.table) # package for interacting with data.frames as data.table

# create small mock tibble to save direct to google drive
test <- tribble(
  ~A, ~B, ~C,
  1,2,3,
  4,5,6
)

# assign path to google drive on my machine
# the ../../ component goes back 2 folders to then be able to map to eoq_google_drive
gdrive_dir <- "../../eoq_google_drive/EOQ/data/BLS/"

# save to google drive
# the test file below was subsequently deleted from the google drive to avoid confusion
#fwrite(test, paste0(gdrive_dir, "Test.csv")) # it works!

# series to pull
california_unemployment_series <- 'LASST060000000000003'

# define key before requesting data from the bls api
reg_key = '445b6e1510f04d2bb4547570722e975a'

# create payload; this will be sent as part of request to the bls api
payload <- list(
  'seriesid' = california_unemployment_series,
  'startyear' = 2010,
  'endyear' = 2019,
  'registrationKey'= reg_key)

# ping the api
response <- blsAPI(payload, api_version = 2)
json <- rjson::fromJSON(response)

# neat package to view nested list
listviewer::jsonedit(json)

# converting the first request to data.frame
# I want to be able to customize a bit, so I added the function below
#test = blsAPI(payload, api_version = 2, return_data_frame = TRUE)

# ======================================================================
# Section 2: Define a function to convert JSON to tabular table
#   The data comes in quite the nested (gross..) format
#   This function will take the json as an input and 
#   output a tidy table

# function to create table
make_table <- function(data, series_num, variable_name, sub_state_area_name = NA) {
  # bind all the individual json elements into rows
  tmp_df = tibble::enframe(base::unlist(data[["Results"]][["series"]][[series_num]][["data"]]))
  
  # create an id column so data.table::dcast can cast the df
  tmp_df$id = stats::ave(tmp_df$name, tmp_df$name, FUN = base::seq_along)
  
  # cast the dataframe
  tmp_dt = data.table::dcast(data.table::setDT(tmp_df), id ~ name, value.var = "value")
  
  # add the state
  tmp_dt$state = "California"
  
  # for ordering purposes, it would be nice to have the month number
  # code below will take month name (mmmm) and match to number using R built in functionality
  tmp_dt$month_num = base::match(tmp_dt$periodName, month.name)
  
  # arrange table by year and month, descending
  tmp_dt = tmp_dt %>% arrange(desc(year), desc(month_num))
  
  # if there is a value for area name..
  if (!base::is.na(sub_state_area_name)) {
    # add a column with the area name
    tmp_dt$area_name = sub_state_area_name
    # arrange the columns in a specific order
    tmp_dt = dplyr::select(tmp_dt, state, area_name, year, period, periodName, value)
  } else {
    # arrange the columns in a specific order
    tmp_dt = dplyr::select(tmp_dt, state, year, period, periodName, value)
  }
  
  # rename "value" column to the subject of the table (e.g., "unemployment_rate")
  names(tmp_dt)[names(tmp_dt)=="value"] = variable_name
  
  # return the table
  return(tmp_dt)
}

# first series - California's unemployment rate
ca_unempl_rate <- make_table(data=json, series_num=1, variable_name="state_unemployment_rate")

# write to outputs repo
fwrite(ca_unempl_rate, paste0(gdrive_dir, "California_Statewide_Unemployment_Rate_2010-2019.csv"))

# ======================================================================
# Section 3: Define California (CA) areas for which to query BLS API

# series info (last 2 digits of series)
# 06 labor force
# 05 employment
# 04 unemployment
# 03 unemployment rate (above, we got this for California, state-wide)

# codes for areas of interest (F should be the predominant focus for this section)
# (B could be of interest as well - e.g., 
#       MT0631080000000	Los Angeles-Long Beach-Anaheim, CA Metropolitan Statistical Area)
# A: statewide
# B: metropolitan areas
# C: metropolitan divisions
# D: Micropolitan areas
# E: combined areas (what is this???)
# F: counties and equivalents
# G: Cities and towns above 25,000 population
# H: Cities and towns below 25,000 population in New England
# I	Parts of cities that cross county boundaries
# J	Multi-entity small labor market areas
# K	Intrastate parts of interstate areas
# L	Balance of state areas
# M	Census regions
# N	Census divisions

# here are California's metropolitan areas (B above)
# copied from https://download.bls.gov/pub/time.series/la/la.area
ca_metro_areas <- ("B	MT0612540000000	Bakersfield, CA Metropolitan Statistical Area	0	T	379
    B	MT0617020000000	Chico, CA Metropolitan Statistical Area	0	T	380
    B	MT0620940000000	El Centro, CA Metropolitan Statistical Area	0	T	381
    B	MT0623420000000	Fresno, CA Metropolitan Statistical Area	0	T	382
    B	MT0625260000000	Hanford-Corcoran, CA Metropolitan Statistical Area	0	T	383
    B	MT0631080000000	Los Angeles-Long Beach-Anaheim, CA Metropolitan Statistical Area	0	T	384
    B	MT0631460000000	Madera, CA Metropolitan Statistical Area	0	T	385
    B	MT0632900000000	Merced, CA Metropolitan Statistical Area	0	T	386
    B	MT0633700000000	Modesto, CA Metropolitan Statistical Area	0	T	387
    B	MT0634900000000	Napa, CA Metropolitan Statistical Area	0	T	388
    B	MT0637100000000	Oxnard-Thousand Oaks-Ventura, CA Metropolitan Statistical Area	0	T	389
    B	MT0639820000000	Redding, CA Metropolitan Statistical Area	0	T	390
    B	MT0640140000000	Riverside-San Bernardino-Ontario, CA Metropolitan Statistical Area	0	T	391
    B	MT0640900000000	Sacramento--Roseville--Arden-Arcade, CA Metropolitan Statistical Area	0	T	392
    B	MT0641500000000	Salinas, CA Metropolitan Statistical Area	0	T	393
    B	MT0641740000000	San Diego-Carlsbad, CA Metropolitan Statistical Area	0	T	394
    B	MT0641860000000	San Francisco-Oakland-Hayward, CA Metropolitan Statistical Area	0	T	395
    B	MT0641940000000	San Jose-Sunnyvale-Santa Clara, CA Metropolitan Statistical Area	0	T	396
    B	MT0642020000000	San Luis Obispo-Paso Robles-Arroyo Grande, CA Metropolitan Statistical Area	0	T	397
    B	MT0642100000000	Santa Cruz-Watsonville, CA Metropolitan Statistical Area	0	T	398
    B	MT0642200000000	Santa Maria-Santa Barbara, CA Metropolitan Statistical Area	0	T	399
    B	MT0642220000000	Santa Rosa, CA Metropolitan Statistical Area	0	T	400
    B	MT0644700000000	Stockton-Lodi, CA Metropolitan Statistical Area	0	T	401
    B	MT0646700000000	Vallejo-Fairfield, CA Metropolitan Statistical Area	0	T	402
    B	MT0647300000000	Visalia-Porterville, CA Metropolitan Statistical Area	0	T	403
    B	MT0649700000000	Yuba City, CA Metropolitan Statistical Area	0	T	404")

# convert text above into tibble that is easier to reference
# since the text above is tab (\t) separated, we'll use readr::read_tsv
ca_metro_areas_df <- readr::read_tsv(ca_metro_areas
                                     , col_names = c("area_type_code", "area_code", "metropolitan_area"
                                              , "display_level", "selectable", "sort_sequence")
                                     # code below so T not read as logical TRUE
                                     , col_types = cols(
                                       selectable = col_character()
                                     ))


# here are California's counties (F area code)
# https://download.bls.gov/pub/time.series/la/la.area
ca_counties <- ("F	CN0600100000000	Alameda County, CA	0	T	425
    F	CN0600300000000	Alpine County, CA	0	T	426
    F	CN0600500000000	Amador County, CA	0	T	427
    F	CN0600700000000	Butte County, CA	0	T	428
    F	CN0600900000000	Calaveras County, CA	0	T	429
    F	CN0601100000000	Colusa County, CA	0	T	430
    F	CN0601300000000	Contra Costa County, CA	0	T	431
    F	CN0601500000000	Del Norte County, CA	0	T	432
    F	CN0601700000000	El Dorado County, CA	0	T	433
    F	CN0601900000000	Fresno County, CA	0	T	434
    F	CN0602100000000	Glenn County, CA	0	T	435
    F	CN0602300000000	Humboldt County, CA	0	T	436
    F	CN0602500000000	Imperial County, CA	0	T	437
    F	CN0602700000000	Inyo County, CA	0	T	438
    F	CN0602900000000	Kern County, CA	0	T	439
    F	CN0603100000000	Kings County, CA	0	T	440
    F	CN0603300000000	Lake County, CA	0	T	441
    F	CN0603500000000	Lassen County, CA	0	T	442
    F	CN0603700000000	Los Angeles County, CA	0	T	443
    F	CN0603900000000	Madera County, CA	0	T	444
    F	CN0604100000000	Marin County, CA	0	T	445
    F	CN0604300000000	Mariposa County, CA	0	T	446
    F	CN0604500000000	Mendocino County, CA	0	T	447
    F	CN0604700000000	Merced County, CA	0	T	448
    F	CN0604900000000	Modoc County, CA	0	T	449
    F	CN0605100000000	Mono County, CA	0	T	450
    F	CN0605300000000	Monterey County, CA	0	T	451
    F	CN0605500000000	Napa County, CA	0	T	452
    F	CN0605700000000	Nevada County, CA	0	T	453
    F	CN0605900000000	Orange County, CA	0	T	454
    F	CN0606100000000	Placer County, CA	0	T	455
    F	CN0606300000000	Plumas County, CA	0	T	456
    F	CN0606500000000	Riverside County, CA	0	T	457
    F	CN0606700000000	Sacramento County, CA	0	T	458
    F	CN0606900000000	San Benito County, CA	0	T	459
    F	CN0607100000000	San Bernardino County, CA	0	T	460
    F	CN0607300000000	San Diego County, CA	0	T	461
    F	CN0607500000000	San Francisco County/city, CA	0	T	462
    F	CN0607700000000	San Joaquin County, CA	0	T	463
    F	CN0607900000000	San Luis Obispo County, CA	0	T	464
    F	CN0608100000000	San Mateo County, CA	0	T	465
    F	CN0608300000000	Santa Barbara County, CA	0	T	466
    F	CN0608500000000	Santa Clara County, CA	0	T	467
    F	CN0608700000000	Santa Cruz County, CA	0	T	468
    F	CN0608900000000	Shasta County, CA	0	T	469
    F	CN0609100000000	Sierra County, CA	0	T	470
    F	CN0609300000000	Siskiyou County, CA	0	T	471
    F	CN0609500000000	Solano County, CA	0	T	472
    F	CN0609700000000	Sonoma County, CA	0	T	473
    F	CN0609900000000	Stanislaus County, CA	0	T	474
    F	CN0610100000000	Sutter County, CA	0	T	475
    F	CN0610300000000	Tehama County, CA	0	T	476
    F	CN0610500000000	Trinity County, CA	0	T	477
    F	CN0610700000000	Tulare County, CA	0	T	478
    F	CN0610900000000	Tuolumne County, CA	0	T	479
    F	CN0611100000000	Ventura County, CA	0	T	480
    F	CN0611300000000	Yolo County, CA	0	T	481
    F	CN0611500000000	Yuba County, CA	0	T	482")

# extract CA county area codes from the block of string above
ca_counties_df <- readr::read_tsv(ca_counties
                                  , col_names = c("area_type_code", "area_code", "county"
                                        , "display_level", "selectable", "sort_sequence")
                                  # code below so F and T not read as logicals
                                  , col_types = cols(
                                    area_type_code = col_character(),
                                    selectable = col_character()
                                  ))

# ======================================================================
# Section 4: Build rest of series and subject request inputs
#   We have the area codes, but we still need to build the rest of the
#   series to include local unemployment data tables, unemployment
#   rate, labor force, etc.

# all the data will come from Local Area Unemployment Statistics
# so we'll start there
prefix <- "LA"

# next comes the seasonal adjustment code
s_adj_code <- "U"

# we already have area code (from above section)

# finally, measure code
measure_codes <- tribble(
  ~measure_code, ~measure_text,
  "03", "unemployment_rate",
  "04", "unemployment",
  "05", "employment",
  "06", "labor_force"
)


# next, we want to combine elements into a full series that can be queried
# we'll just make it a new column in the existing tables for metropolitan
# areas and counties
for (i in 1:nrow(measure_codes)) {
  ca_counties_df[[measure_codes$measure_text[i]]] <- paste0(
    prefix, s_adj_code, ca_counties_df$area_code, measure_codes$measure_code[i]
  )
}

# repeat the above for metropolitan areas
for (i in 1:nrow(measure_codes)) {
  ca_metro_areas_df[[measure_codes$measure_text[i]]] <- paste0(
    prefix, s_adj_code, ca_metro_areas_df$area_code, measure_codes$measure_code[i]
  )
}

# ======================================================================
# Section 5: Apply function created in section 2 to create data tables
#   This section will send a decent number of queries to the BLS API.
#   A few notes to keep in mind re: what we can do with a registered API key:
#     - Daily query limit: 500
#     - Series per query limit: 50
#     - Years per query limit: 20

# the ca_counties_df consists of 58 series per measure
# in the for loop for ca_counties_df, we will go a bit inception on the data
# by running a second nested for loop within the main for loop to split the query

# below, we'll build a for loop to run each query and output to an appropriately
# named csv file

# metro areas for loop ---------------------------------------------------
#!!! important note: this for loop was run, and I have the data stored locally
#    on my machine but I have not uploaded it to the cloud since it looks like
#    we will not be using it

# define columns of interest
ca_metro_areas_measures <- colnames(ca_metro_areas_df)[7:10]

# this for loop will ping the bls api to acquire the json input
# then run each batch of series 
for (measure in ca_metro_areas_measures) {
  # extract all the series for the measure (e.g., unemployment_rate)
  series_to_pass = ca_metro_areas_df[[measure]]
  
  # define an iterator for the nested for loop below
  iterator = 1:length(ca_metro_areas_df[[measure]])
  
  # define the payload
  loop_payload <- list(
    'seriesid' = series_to_pass,
    'startyear' = 2010,
    'endyear' = 2019,
    'registrationKey' = reg_key
  )
  
  # ping the api and collect json response
  loop_response = blsAPI(loop_payload, api_version = 2)
  loop_json = rjson::fromJSON(loop_response)
  
  # nested for loop
  for (i in iterator) {
    # extract the metro area name
    sub_state_area_name = ca_metro_areas_df$metropolitan_area[i]
    
    # create a temporary table containing one metro area's data
    tmp_table = make_table(data = loop_json, series_num = i
                           , variable_name = measure
                           , sub_state_area_name = sub_state_area_name)
    
    # define name for csv; the metro area names are long, so 
    # we'll remove ", CA Metropolitan Statistical Area" (34 characters) from each
    length_of_name = base::nchar(sub_state_area_name)
    csv_metro_name = base::substr(sub_state_area_name, 1, length_of_name-34)
    
    # write to metro area to csv
    # note: this path is to my local machine and does not output the CSVs to google drive
    data.table::fwrite(tmp_table, paste0("../outputs/", csv_metro_name, "_", measure, ".csv"))
    
  }
  # print if successfully
  print(paste(measure, "successfully written to csv"))
  
  # sleep 5 seconds before moving to next measure
  base::Sys.sleep(5)
  
}

# county areas for loop -------------------------------------------------
#!!! important note: we decided on pulling data only for unemployment_rate for counties

# define columns of interest
ca_counties_measures <- colnames(ca_counties_df)[7] # just get unemployment rate (7th column)

# similar to above loop, with one difference - we need to split the query since BLS caps at 50 series
# not the most elegant, but we'll run this twice (once with first 50 items in vector and a second 
# time with the remaining 8 items)
for (measure in ca_counties_measures) {
  # extract all the series for the measure (e.g., unemployment_rate)
  # we'll split the 58 series to two objects so we don't exceed bls api query limit of 50 series
  series_to_pass1 = ca_counties_df[[measure]][1:50]
  series_to_pass2 = ca_counties_df[[measure]][51:58]
  
  # define an iterator for the nested for loops below
  iterator1 = 1:length(series_to_pass1)
  iterator2 = 1:length(series_to_pass2)
  
  # define the first payload
  loop_payload1 <- list(
    'seriesid' = series_to_pass1,
    'startyear' = 2010,
    'endyear' = 2019,
    'registrationKey' = reg_key
  )
  
  # define the second payload
  loop_payload2 <- list(
    'seriesid' = series_to_pass2,
    'startyear' = 2010,
    'endyear' = 2019,
    'registrationKey' = reg_key
  )
  
  # ping the api and collect json response
  # first payload (series 1 through 50)
  loop_response1 = blsAPI(loop_payload1, api_version = 2)
  loop_json1 = rjson::fromJSON(loop_response1)
  # second payload (series 51 through 58)
  loop_response2 = blsAPI(loop_payload2, api_version = 2)
  loop_json2 = rjson::fromJSON(loop_response2)
  
  # nested for loop
  for (i in iterator1) {
    # extract the county name
    sub_state_area_name = ca_counties_df$county[i]
    
    # define name for csv
    # remove the last 4 characters ", CA"
    length_of_name = base::nchar(sub_state_area_name)
    csv_county_name = base::substr(sub_state_area_name, 1, length_of_name-4)
    
    # if there is something like a "/" (e.g., San Francisco County/city), there will be an error
    # when creating the file path, so we'll remove it here
    csv_county_name = gsub("/", "_", csv_county_name)
    
    # create a temporary table containing one county's data
    tmp_table = make_table(data = loop_json1, series_num = i
                           , variable_name = measure
                           , sub_state_area_name = csv_county_name)
    
    # write to metro area to csv
    data.table::fwrite(tmp_table, paste0(gdrive_dir, csv_county_name, "_", measure, ".csv"))
    
  }

  # print if successful
  print(paste(measure, "successfully written loop 1 items to csv :-)"))
  
  # sleep 5 seconds before moving to next measure
  base::Sys.sleep(5)
  
  # nested for loop
  for (i in iterator2) {
    # extract the county name
    sub_state_area_name = ca_counties_df$county[i]
    
    # define name for csv
    # remove the last 4 characters ", CA"
    length_of_name = base::nchar(sub_state_area_name)
    csv_county_name = base::substr(sub_state_area_name, 1, length_of_name-4)
    
    # if there is something like a "/" (e.g., San Francisco County/city), there will be an error
    # when creating the file path, so we'll remove it here
    csv_county_name = gsub("/", "_", csv_county_name)
    
    # create a temporary table containing one county's data
    tmp_table = make_table(data = loop_json1, series_num = i
                           , variable_name = measure
                           , sub_state_area_name = csv_county_name)
    
    # write to metro area to csv
    data.table::fwrite(tmp_table, paste0(gdrive_dir, csv_county_name, "_", measure, ".csv"))
    
  }
  
  # print if successful
  print(paste(measure, "successfully written loop 2 items to csv :-)"))
  
}

# ======================================================================
# Section 6: Combine all the CSVs into one big CSV for convenience
#   - rbind every csv into one big csv containing the county unemployment rate
#   - since 2010 for every county in California

# combine all csvs into one table
combined <-
  base::list.files(path = gdrive_dir,
             pattern = "*rate.csv",
             full.names = TRUE) %>%
  purrr::map_df(~fread(.))

# write the combined output to csv
fwrite(combined, paste0(gdrive_dir, "/California_All_Counties_Unemployment_Rate_2010-2019.csv"))































