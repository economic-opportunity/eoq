# Purpose:
# Read data extract from IPUMS and prep for R Shiny app Seamus is creating

#! Join crosswalks
#! 

########################################################################-
#### Packages and file imports ####
########################################################################-

# packages
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(haven)
library(skimr)
#library(ipumsr)
#library(devtools)

#devtools::install_github("economic-opportunity/shinyeoq")

# read acs data
acs <- data.table::fread('../inputs/acs_10.25.2020.csv',
                         colClasses = list(character = c('CBSERIAL', 'CLUSTER', 'STRATA')))

# read the cps data
cps <- data.table::fread('../inputs/cps_09.29.2020.csv',
                         colClasses = list(character = c('CPSID', 'CPSIDP')))

# ind1990 crosswak
ind1990 <- data.table::fread('../inputs/ind1990_naics_xwalk.csv')

# occ2010 crosswalk
occ2010 <- data.table::fread('../inputs/occ2010_soc_xwalk.csv')

########################################################################-
#### Legacy import code ####
########################################################################-

# the code below was used when reading .dat file; I prefer .csv
# acs_dat_xml <- ipumsr::read_ipums_ddi('../inputs/acs_10.22.2020.xml')
# acs_dat_xml$file_name <- "acs_10.22.2020.dat"
# acs_dat <- ipumsr::read_ipums_micro(acs_dat_xml)

# read cps data
# cps_04 <- data.table::fread('../inputs/cps_00004.csv', 
#                             colClasses = list(character = c('CPSID', 'CPSIDP')))
#cps_29 <- data.table::fread('../inputs/cps_00029.csv', colClasses = 'character')

########################################################################-
#### Clean the CPS data ####
########################################################################-

# let's update all the columns in one fell swoop
cps_cleaned <- cps %>%
  transmute(
    
    # is_male
    is_male = dplyr::case_when(
      SEX == 1 ~ 1,
      SEX == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # age
    age = AGE,
    
    # education
    education = as_factor(
      haven::labelled(
        dplyr::case_when(
          EDUC >= 10 &  EDUC <= 72 ~ 1,
          #cps_04[['EDUC']]>=73 &  cps_04[['EDUC']]>=122 &  cps_04[['EDUC']]!=111 ~ 2,
          EDUC >= 73 & EDUC <= 122 & EDUC != 111 ~ 2,
          EDUC == 111 ~ 3,
          #cps_04[['EDUC']]==124 | cps_04[['EDUC']]==125 ~ 54,
          EDUC == 124 | EDUC == 125 ~ 4,
          TRUE ~ NA_real_ ),
        c("less than high school" = 1,"high school" = 2,
          "bachelor's degree" = 3, "advanced degree" = 4)), 
                          levels = "labels"),
    
    # county
    countyfips = stringr::str_pad(COUNTY, side = 'left', width = '5', pad = '0'),
    
    # occupation
    occupation = dplyr::case_when(
      !OCC2010 %in% c(9999, 9920) ~ as.character(OCC2010),
      TRUE ~ NA_character_
    ),
    
    # racehispanic
    racehispanic = as_factor(
      labelled(
        dplyr::case_when(
          HISPAN >= 100 &  HISPAN >= 612 ~ 3,
          RACE == 100 ~ 1,
          RACE == 200 ~ 2,
          RACE == 651 ~ 4,
          RACE >= 100 &  RACE < 999 ~ 5,
          TRUE ~ NA_real_),
        c("white"=1,"black"=2,"Hispanic"=3,"Asian"=4,"other"=5)), 
              levels = "labels"),
    
    # industry
    industry = dplyr::case_when(
      #! change the line below from 999 to 998; cps cbk refers to 998 as "Unknown" industry
      #! separately, many IND1990 values are '0', which don't map to an industry in cps cbk
      IND1990 != 998 ~ as.character(IND1990),
      TRUE ~ NA_character_),
    
    # hourlywage
    hourlywage = dplyr::case_when(
      
      # #condition 1
      # ((ELIGORG == 1) & (PAIDHOUR == 2) & (!UHRSWORKORG %in% c(999, 998)) &
      #    (HOURWAGE != 999.99) & (EARNWEEK != 9999.99)) ~ HOURWAGE,
      # 
      # #condition 2
      # #! ELIGORG added
      # ((ELIGORG == 1) & (PAIDHOUR != 2) & (!UHRSWORKORG %in% c(999, 998)) &
      #    (HOURWAGE != 999.99) & (EARNWEEK != 9999.99)) ~ EARNWEEK / UHRSWORKORG,
      
      #! new condition
      ((ELIGORG == 1) & (PAIDHOUR == 2) & (HOURWAGE != 999.99)) ~ HOURWAGE,
    
      TRUE ~ NA_real_
    ),
    
    # flag for hourly wage
    hourlywage_flag = dplyr::case_when(
      hourlywage == NA_real_ ~ NA_real_,
      ((!hourlywage %in% 999.99) & (!is.na(hourlywage)) & (!UHRSWORKORG %in% c(999, 998))) ~ 0,
      ((!hourlywage %in% 999.99) & (!is.na(hourlywage)) & (UHRSWORKORG %in% c(999, 998))) ~ 1
    ),
    
    # employmentstatus
    employmentstatus = as_factor(
      labelled(
        dplyr::case_when(
          EMPSTAT >=30 & EMPSTAT <=36 ~ 0,
          EMPSTAT >=20 & EMPSTAT <=22 ~ 1,
          EMPSTAT ==10 | EMPSTAT ==12 ~ 2,
          EMPSTAT ==01 ~ 3,
          TRUE ~ NA_real_ ),
        c("not in labor force" = 0, "unemployed" = 1, "employed" = 2, "in military" = 3)),
      levels = "labels"),
    
    # hoursperweek
    hoursperweek = dplyr::case_when(
      UHRSWORKORG !=998 & UHRSWORKORG !=999 ~ as.numeric(UHRSWORKORG),
      TRUE ~ NA_real_)
  )

########################################################################-
#### Join industry & occupation crosswalks to cps_cleaned ####
########################################################################-

# occupation ---------------------------------------------

# pad the 'occupation' column in cps_cleaned. this will allow for join below
cps_cleaned$occupation <- str_pad(cps_cleaned$occupation, width = 4, side = 'left', pad = '0')

# check how many occ2010 values from xwalk in cps_cleaned
sum(occ2010$occ2010 %in% cps_cleaned$occupation) # ans: 422

# check how many unique occupation values from cps_cleaned *NOT* in occ2010
sum(!unique(cps_cleaned$occupation) %in% occ2010$occ2010) # ans: 21 unique occupation values

# what are the 21 values ID'd above?
missing_cps_occ_1 <- unique(cps_cleaned$occupation)[
  !unique(cps_cleaned$occupation) %in% occ2010$occ2010]

# join occupation/industry crosswalk to the data
cps_cleaned_final <- dplyr::left_join(
  cps_cleaned,
  occ2010,
  by = c('occupation' = 'occ2010')
)

# industry ---------------------------------------------

# check how many ind1990 values from xwalk in cps_cleaned
sum(ind1990$ind1990 %in% cps_cleaned$industry) # ans: 221

# check how many unique industry values from cps_cleaned *NOT* in ind1990
sum(!unique(cps_cleaned$industry) %in% ind1990$ind1990) # ans: 1

# what is the 1 value ID'd above
missing_cps_ind_1 <- unique(cps_cleaned$industry)[
  !unique(cps_cleaned$industry) %in% ind1990$ind1990] # ans: "0"

# convert ind1990 in ind1990 table to character to allow for join below
ind1990$ind1990 <- as.character(ind1990$ind1990)

# join industry crosswalk to the data
cps_cleaned_final <- dplyr::left_join(
  cps_cleaned_final,
  ind1990,
  by = c('industry' = 'ind1990')
)

# # write to outputs
# fwrite(cps_cleaned_final, '../outputs/CPS_Cleaned.csv')

########################################################################-
#### Clean the ACS data ####
########################################################################-


# let's update all the columns in one fell swoop
acs_cleaned <- acs %>%
  
  transmute(
    
    # is_male
    is_male = ifelse(SEX == 2, 0, SEX),
    
    # age
    age = AGE,
    
    # education
    education = dplyr::case_when(
      EDUCD %in% 1 ~ NA_character_,
      (EDUCD >= 2) & (EDUCD <= 61) ~ "less than high school",
      #EDUCD %in% c(62, 63, 64, 65,) ~ "high school",
      (EDUCD >= 62) & (EDUCD <= 100) ~ "high school",
      EDUCD %in% 101 ~ "bachelor's degree",
      EDUCD %in% c(114, 115, 116) ~ "advanced degree"
    ),
    
    # countyfips
    countyfips = paste0(
      str_pad(STATEFIP, side = 'left', width = 2, pad = '0'),
      str_pad(COUNTYFIP, side = 'left', width = 3, pad = '0')),
    
    # occupation
    occupation = OCC2010, #! contains '0'; documentation unclear on missing/blank
    
    # racehispanic
    racehispanic = dplyr::case_when(
      HISPAN %in% c(1,2,3,4) ~ 3,
      RACE == 1 ~ 1,
      RACE == 2 ~ 2,
      RACE %in% c(4,5,6) ~ 4,
      RACE %in% c(3,7,8,9) ~ 5 #! note this - we include 'two major races', 'three or more
                              #! major races', 'american indian or alaska native' in "Other"
    ),
    
    # industry
    industry = IND1990, #! contains '0'; documentation unclear on missing/blank
    
    # totalincome
    inctot = ifelse(INCTOT == 9999999, NA, INCTOT),
    
    # poverty
    below_poverty = dplyr::case_when(
      POVERTY <= 100 ~ 1,
      POVERTY > 100 ~ 0
    ),
    
    # totalwage
    totalwage = ifelse(INCWAGE == 999999, NA, INCWAGE),
    
    # employmentstatus
    employmentstatus = dplyr::case_when(
      EMPSTATD %in% c(13, 14, 15) ~ 3, # armed forces
      EMPSTAT == 3 ~ 0, # not in labor force
      EMPSTAT == 2 ~ 1, # unemployed
      EMPSTAT == 1 ~ 2, # employed
      EMPSTAT == 0 ~ NA_real_
    )#,
    
    # hoursperweek
    #hoursperweek = UHRSWORK # there is no UHRSWORK column in ACS
  )

#######################################################################-
#### Join industry & occupation crosswalks to acs_cleaned ####
########################################################################-

# occupation ---------------------------------------------

# pad the 'occupation' column in cps_cleaned. this will allow for join below
acs_cleaned$occupation <- str_pad(acs_cleaned$occupation, width = 4, side = 'left', pad = '0')

# check how many occ2010 values from xwalk in acs_cleaned
sum(occ2010$occ2010 %in% acs_cleaned$occupation) # ans: 406

# check how many unique occupation values from acs_cleaned *NOT* in occ2010
sum(!unique(acs_cleaned$occupation) %in% occ2010$occ2010) # ans: 21 unique occupation values

# what are the 21 values ID'd above?
missing_acs_occ_1 <- unique(acs_cleaned$occupation)[
  !unique(acs_cleaned$occupation) %in% occ2010$occ2010]

# join occupation/industry crosswalk to the data
acs_cleaned_final <- dplyr::left_join(
  acs_cleaned,
  occ2010,
  by = c('occupation' = 'occ2010')
)

# industry ---------------------------------------------

# check how many ind1990 values from xwalk in acs_cleaned
sum(ind1990$ind1990 %in% acs_cleaned$industry) # ans: 221

# check how many unique industry values from acs_cleaned *NOT* in ind1990
sum(!unique(acs_cleaned$industry) %in% ind1990$ind1990) # ans: 2

# what is the 1 value ID'd above
missing_acs_ind_1 <- unique(acs_cleaned$industry)[
  !unique(acs_cleaned$industry) %in% ind1990$ind1990] # ans: c(0, 992)

# convert ind1990 in acs_cleaned_final table to character to allow for join below
acs_cleaned_final$industry <- as.character(acs_cleaned_final$industry)

# join industry crosswalk to the data
acs_cleaned_final <- dplyr::left_join(
  acs_cleaned_final,
  ind1990,
  by = c('industry' = 'ind1990')
)


# # write to outputs
# fwrite(acs_cleaned, '../outputs/ACS_Cleaned.csv')
