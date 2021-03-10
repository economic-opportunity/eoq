test_that("make_percentiles returns percentiles", {
  expect_equal(
    tibble::tibble(x = 1:1000, y = 1:1000) %>%
      make_percentiles(y) %>%
      dplyr::pull(percentile) %>%
      length(),
    100)
})

test_that("make_percentiles calculates mean properly", {
  expect_equal(
    tibble::tibble(x = 1:1000, y = 1:1000) %>%
      make_percentiles(y) %>%
      dplyr::pull(mean) %>%
      mean(),
    500.5)
})


test_that("age_buckets returns 6 buckets", {
  expect_equal(
    tibble::tibble(x = seq.int(20, 70, 10)) %>%
      make_age_buckets(x) %>%
      dplyr::pull(age_bucket),
    c("Under 25", "25-34", "35-44",
      "45-54", "55-64", "Over 65")
  )
})

test_that("clean_race_ethnicity returns character", {
  expect_type(clean_race_ethnicity(tibble::tibble(x = 0:4), x)$race_ethnicity, "character")
})

test_that("clean_unemployment returns character", {
  expect_type(clean_employment(tibble::tibble(x = 0:4), x)$employmentstatus, "character")
})

test_that("clean_education returns title case",{
  expect_equal(clean_education(tibble::tibble(x = letters[0:4]), x)$education, LETTERS[0:4])
})

