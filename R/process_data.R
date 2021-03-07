#' Make percentiles for chart
#'
#' @param data tibble
#' @param var variable to take the mean and median of
#' @param n number of ntiles, defaults to 100 for percentiles
#' @param ... grouping variables, optional
#'
#' @return tibble
#' @export
#' @importFrom dplyr group_by mutate summarize ungroup ntile
#' @importFrom stats median
#' @examples
#' \dontrun{
#' eoq::sample_acs %>% make_percentiles(totalwage)
#' eoq::sample_cps %>% make_percentiles(hourlywage, education)
#' }
make_percentiles <- function(data, var, ..., n = 100) {
  
  data %>%
    group_by(...) %>%
    mutate(percentile = ntile({{ var }}, n)) %>%
    group_by(..., percentile) %>%
    summarize(mean = mean( {{var}} , na.rm = TRUE ),
              median = median( {{var}} , na.rm = TRUE)) %>%
    ungroup()
  
}


#' Make age buckets
#'
#' @param data tibble
#' @param var age variable
#'
#' @return tibble
#' @export
#' @importFrom dplyr case_when
#' @examples
#' \dontrun{
#' eoq::sample_cps %>% make_age_buckets(age)
#' eoq::sample_acs %>% make_age_buckets(age)
#' }
make_age_buckets <- function(data, var){
  
  data %>%
    mutate(
      age_bucket = case_when(
        {{ var }} >= 0  & {{ var }} < 25 ~ "Under 25",
        {{ var }} >= 25 & {{ var }} < 35 ~ "25-34",
        {{ var }} >= 35 & {{ var }} < 45 ~ "35-44",
        {{ var }} >= 45 & {{ var }} < 55 ~ "45-54",
        {{ var }} >= 55 & {{ var }} < 65 ~ "55-64",
        {{ var }} >= 65                  ~ "Over 65"
      )
    )
}


#' Convert race / ethnicity to standard set
#'
#' @param data tibble
#' @param var race variable
#'
#' @return tibble
#' @export
#' @importFrom dplyr case_when
#' @importFrom stringr str_to_title
#' @examples
#' \dontrun{
#' eoq::sample_cps %>% clean_race_ethnicity(racehispanic)
#' eoq::sample_acs %>% clean_race_ethnicity(racehispanic)
#' }
clean_race_ethnicity <- function(data, var) {
  
  data %>%
    mutate(
      race_ethnicity = case_when(
        {{ var }} == 1 ~ "White",
        {{ var }} == 2 ~ "Black",
        {{ var }} == 3 ~ "Hispanic",
        {{ var }} == 4 ~ "Asian",
        {{ var }} == 5 ~ "Other",
        is.character( {{ var }} ) ~ str_to_title( {{ var }} ),
        TRUE ~ NA_character_
      )
    )
}


#' Convert education data to title case
#'
#' @param data tibble
#' @param var education variable
#'
#' @return tibble
#' @export
#' @importFrom stringr str_to_title
#' @examples
#' \dontrun{
#' eoq::sample_cps %>% clean_education(education)
#' eoq::sample_acs %>% clean_education(education)
#' }
clean_education <- function(data, var) {
  data %>%
    mutate(
      education = str_to_title( {{ var }} )
    )
}

#' Clean unemployment variable
#'
#' @param data a tibble
#' @param var employment variable name, typically `employmentstatus``
#'
#' @return a tibble
#' @export
#' @importFrom dplyr pull case_when
#' @importFrom stringr str_to_sentence
#' @examples
#' \dontrun{
#' eoq::sample_cps %>% clean_employment(employmentstatus)
#' eoq::sample_acs %>% clean_employment(employmentstatus)
#' }
clean_employment <- function(data, var) {
  
  if(data %>% pull({{ var }}) %>% is.numeric()){
    data %>%
      mutate(
        employmentstatus = case_when(
          {{ var }} == 0 ~ "Not in labor force",
          {{ var }} == 1 ~ "Unemployed",
          {{ var }} == 2 ~ "Employed",
          {{ var }} == 3 ~ "In military",
          TRUE           ~ NA_character_
        )
        
      )
  } else {
    
    data %>%
      mutate(employmentstatus = str_to_sentence({{ var }}))
    
  }
  
}

#' Clean sex variable
#'
#' @param data a tibble
#' @param var an is_male variable
#'
#' @return a tibble
#' @export
#'
clean_sex <- function(data, var) {
  data %>% 
    mutate(sex = ifelse({{ var }} == 1, "Male", "Female"))
}



#' Process cleaned ACS/CPS data for shiny app
#'
#' @param data tibble
#' @param input input from shiny
#'
#' @return tibble
#' @export
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' # only works inside a shiny app
#' eoq::sample_cps %>% process_comp_data(input)
#' eoq::sample_acs %>% process_comp_data(input)
#' }
process_comp_data <- function(data, input) {
  data %>%
    filter(
      education      == input$comp_edu,
      race_ethnicity == input$comp_race,
      age_bucket     == input$comp_age,
      sex            == input$comp_sex
    )
  
}

#' Process individual data
#'
#' @param data a tibble
#' @param input input from shiny
#'
#' @return tibble
#' @export
#'
process_individual_data <- function(data, input) {
  data %>%
    clean_sex(is_male) %>% 
    mutate(
      sex = if_else(is_male == 1, "Male", "Female"),
      age_bucket = age_group,
      industry = naics_2digit_label
    ) %>%
    clean_education(education) %>%
    filter(
      education      == input$individual_edu,
      # race_ethnicity == input$comp_race,
      age_bucket     == input$individual_age,
      sex            == input$individual_sex
    )
}

#' Calculate unemployment metric
#'
#' @param data tibble
#' @param var unemployment variable
#' @param ... grouping variables
#' @param na.rm whether to remove null values from calculation, defaults to true
#' @return tibble
#' @export
#' @importFrom dplyr pull group_by summarize
#' @examples
#' \dontrun{
#' eoq::sample_cps %>% calc_unemployment_rate(employmentstatus)
#' eoq::sample_acs %>% calc_unemployment_rate(employmentstatus)
#' }
calc_unemployment_rate <- function(data, var, ..., na.rm = TRUE) {
  
  if(data %>% pull({{ var }}) %>% is.numeric()){
    data %>%
      group_by(...) %>%
      summarize(
        unemployment_rate = sum({{ var }} == 1, na.rm = na.rm) /
          sum({{ var }} %in% c(1,2), na.rm = na.rm)
      )
  } else {
    data %>%
      group_by(...) %>%
      summarize(
        unemployment_rate = sum({{ var }} == "unemployed", na.rm = na.rm) /
          sum({{ var }} %in% c("unemployed", "employed"), na.rm = na.rm)
      )
  }
  
}



