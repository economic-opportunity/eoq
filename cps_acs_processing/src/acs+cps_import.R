# Purpose:
# Read data extract from IPUMS and prep for R Shiny app Seamus is creating

########################################################################-
#### Packages and file imports ####
########################################################################-

# packages
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(haven)

# read acs data
acs <- data.table::fread('../inputs/acs_09.26.2020.csv',
                         colClasses = list(character = c('CBSERIAL', 'CLUSTER', 'STRATA')))

# read cps data
cps_04 <- data.table::fread('../inputs/cps_00004.csv', 
                            colClasses = list(character = c('CPSID', 'CPSIDP')))
#cps_29 <- data.table::fread('../inputs/cps_00029.csv', colClasses = 'character')

########################################################################-
#### Clean the CPS data ####
########################################################################-

# let's update all the columns in one fell swoop
cps_04_cleaned <- cps_04 %>%
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
      !OCC2010 %in% c(9999, 9920) ~ as.numeric(OCC2010),
      TRUE ~ NA_real_
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
      IND1990 != 999 ~ as.numeric(IND1990),
      TRUE ~ NA_real_),
    
    # hourlywage
    hourlywage = dplyr::case_when(
      
      ((ELIGORG == 1) & (PAIDHOUR == 2) & (!UHRSWORKORG %in% c(999, 998)) & 
         (HOURWAGE != 999.99) & (EARNWEEK != 9999.99)) ~ HOURWAGE,
      
      #! ELIGORG added
      ((ELIGORG == 1) & (PAIDHOUR != 2) & (!UHRSWORKORG %in% c(999, 998)) &
         (HOURWAGE != 999.99) & (EARNWEEK != 9999.99)) ~ EARNWEEK / UHRSWORKORG,
    
      TRUE ~ NA_real_
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

# # write to outputs
# fwrite(cps_04_cleaned, '../outputs/CPS_Cleaned.csv')

########################################################################-
#### Clean the ACS data ####
########################################################################-

acs_cleaned <- acs %>%
  
  transmute(
    
    # is_male
    is_male = ifelse(SEX == 2, 0, SEX),
    
    # age
    age = AGE,
    
    # education
    education = dplyr::case_when(
      # less than high school
      EDUC < 63 ~ "less than high school",
      EDUC %in% c(62, 63, 64) ~ "high school",
      EDUC %in% 101 ~ "bachelor's degree",
      EDUC %in% c(114, 115, 116) ~ "advanced degree"
    ),
    
    # countyfips
    countyfips = paste0(
      str_pad(STATEFIP, side = 'left', width = 2, pad = '0'),
      str_pad(COUNTYFIP, side = 'left', width = 3, pad = '0')),
    
    # occupation
    occupation = OCC, #! contains '0'; documentation unclear on missing/blank
    
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
    industry = IND, #! contains '0'; documentation unclear on missing/blank
    
    # totalincome
    totalincome = ifelse(INCTOT == 9999999, NA, INCTOT),
    
    # totalwage
    totalwage = ifelse(INCWAGE == 999999, NA, INCWAGE),
    
    # employmentstatus
    employmentstatus = dplyr::case_when(
      EMPSTATD %in% c(13, 14, 15) ~ 3, # armed forces
      EMPSTAT == 3 ~ 0, # not in labor force
      EMPSTAT == 2 ~ 1, # unemployed
      EMPSTAT == 1 ~ 2, # employed
      EMPSTAT == 0 ~ NA_real_
    ),
    
    # hoursperweek
    hoursperweek = UHRSWORK
  )

# a<-select(filter(acs_cleaned, is.na(racehispanic)), RACE, RACED, HISPAN, HISPAND)
# count(a, RACE)

# # write to outputs
# fwrite(acs_cleaned, '../outputs/ACS_Cleaned.csv')
