context("District Value Add Files")

library(readr)
library(dplyr)
library(purrr)

district_va_absenteeism <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_absenteeism_va.csv")
district_va_elpa <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_elpa_va.csv")
district_va_grad <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_grad_va.csv")

district_va_absenteeism_am <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_absenteeism_va_AM.csv")
district_va_elpa_am <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_elpa_va_AM.csv")
district_va_grad_am <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_grad_va_AM.csv")

test_that("Matching District Value Add", {

    absenteeism_diff_df <- setdiff(district_va_absenteeism, district_va_absenteeism_am) %>%
        bind_rows(setdiff(district_va_absenteeism_am, district_va_absenteeism)) %>%
        arrange(subgroup, system) %>%
        filter(!is.na(value_add_metric)) # Filtering out NAs due to the method use for district_va_absenteeism_am

    elpa_diff_df <- setdiff(district_va_elpa, district_va_elpa_am) %>%
        bind_rows(setdiff(district_va_elpa_am, district_va_elpa)) %>%
        arrange(subgroup, system)

    grad_diff_df <- setdiff(district_va_grad, district_va_grad_am) %>%
        bind_rows(setdiff(district_va_grad_am, district_va_grad)) %>%
        arrange(subgroup, system)

    expect_equal(nrow(absenteeism_diff_df), 0)
    expect_equal(nrow(elpa_diff_df), 0)
    expect_equal(nrow(elpa_diff_df), 0)

})
