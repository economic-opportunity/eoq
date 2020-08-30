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

# read csv1
#df <- data.table::fread('../inputs/personal_income_and_edu.csv')
incwage <- data.table::fread('../inputs/income_wages_plus_edu.csv')

########################################################################-
#### Remove missing data ####
########################################################################-

# remove records where 'EDUCD' is missing or less than first grade
incwage_2 <- dplyr::filter(incwage, !EDUCD %in% c(0, 1, 2, 10, 11, 12, 999))

########################################################################-
#### Create 'education_clean' et al columns ####
########################################################################-

# SAMPLE col values to '2018 ACS'
incwage_2[['SAMPLE']] <- '2018 ACS'

# create a new column that aligns 'EDUCD' in IPUMS to 'educ_cat' in Matt's 'cps_clean.csv' file
incwage_2[['education_clean']] <- dplyr::case_when(
  incwage_2[['EDUCD']] >= 13 & incwage_2[['EDUCD']] <= 61 ~ 1, # did not finish high school
  (incwage_2[['EDUCD']] >= 62 & incwage_2[['EDUCD']] <= 113) &
    (incwage_2[['EDUCD']] != 101) ~ 2, # at least high school (includes college but not bach degree)
  incwage_2[['EDUCD']] == 101 ~ 3, # bachelor's degree
  incwage_2[['EDUCD']] >= 114 & incwage_2[['EDUCD']] <= 116 ~ 4, # graduate degree
  TRUE ~ -1 # anything not captured by above logic gets a -1
)

# add text description to make life easier
incwage_2[['education_clean_description']] <- dplyr::case_when(
  incwage_2[['education_clean']] %in% 1 ~ 'Less than high school',
  incwage_2[['education_clean']] %in% 2 ~ "High school or some college",
  incwage_2[['education_clean']] %in% 3 ~ "Bachelor's Degree",
  incwage_2[['education_clean']] %in% 4 ~ "Graduate Degree"
)

# curious how many 'EDUCD' values aligned to 'education_clean' 2 are exactly high school grad
counts <- incwage_2 %>%
  filter(education_clean %in% 2) %>%
  count(EDUCD, sort = TRUE)

# the above code returned the following
# 1    63 602,113 --> regular high school diploma
# 2    71 381,192 --> 1 or more years of college credit, no degree
# 3    81 213,684 --> Associate's degree, type not specified
# 4    65 190,344 --> Some college, but less than 1 year
# 5    64 103,161 --> GED or alternative credential

########################################################################-
#### Remove INCWAGE '999999' (indicates NA) ####
########################################################################-

# remove records where 'INCWAGE' == 999999
incwage_3 <- filter(incwage_2, INCWAGE != 999999)

# count rows removed
nrow(incwage_2) - nrow(incwage_3) # removed 334,782 rows (2.6 million remain)

# create quick hist to confirm data
ggplot(data = filter(incwage_3, INCWAGE < 200000)) + # data filtered
  geom_histogram(mapping = aes(x = INCWAGE))

# show median income by 'education_clean' value
ggplot(incwage_3) +
  geom_bar(aes(education_clean, INCWAGE), stat = 'summary', fun.y = 'median')

########################################################################-
#### Select a few columns and write to csv ####
########################################################################-

# select a few columns
incwage_slim <- select(incwage_3, YEAR, SAMPLE, INCWAGE, education_clean,
                       education_clean_description)

# write to csv
#! commented out to avoid re-writing to csv
#fwrite(incwage_slim, "../outputs/ipums_edu_incwage_clean.csv")

########################################################################-
#### For later (when doing deep dive): Explore the data ####
########################################################################-

# placeholder
