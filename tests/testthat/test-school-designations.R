context("School Designations")

library(readr)
library(dplyr)
library(purrr)

school_grading_grades <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv") %>%
    select(system:designation_ineligible, score_achievement:final_average) %>%
    mutate(targeted_support_Native = as.numeric(targeted_support_Native), targeted_support_HPI = as.numeric(targeted_support_HPI))

school_grading_grades_am <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades_AM.csv") %>%
    mutate(targeted_support_Native = as.numeric(targeted_support_Native), targeted_support_HPI = as.numeric(targeted_support_HPI))

test_that("Matching Grades", {

    difference_df_grades <- setdiff( school_grading_grades , school_grading_grades_am) %>% #  %>% select(-percentile)
        bind_rows(setdiff(school_grading_grades_am, school_grading_grades)) %>%
        arrange(system, school)

    expect_equal(nrow(difference_df_grades), 0)
})

atsi_option_1 <- read_csv("N:/ORP_accountability/projects/Josh/school-designations/output/atsi-subgroup-option-1.csv") %>%
    group_by(system, school, pool, designation_ineligible) %>%
    summarise(
        additional_targeted_support = max(atsi, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        additional_targeted_support = if_else(additional_targeted_support == -Inf | is.na(additional_targeted_support), 0, additional_targeted_support)
    )
atsi_ap <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv") %>%
    select(system:designation_ineligible, additional_targeted_support) %>%
    mutate(additional_targeted_support = if_else(is.na(additional_targeted_support), 0, additional_targeted_support))

test_that("Matching ATSI", {

    difference_df_atsi <- setdiff( atsi_option_1 , atsi_ap) %>% #  %>% select(-percentile)
        bind_rows(setdiff(atsi_ap, atsi_option_1)) %>%
        arrange(system, school)

    expect_equal(nrow(difference_df_grades), 0)
})

