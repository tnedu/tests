context("School Designations")

library(readr)
library(dplyr)
library(purrr)
library(tidyr)


school_grading_grades <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv") %>%
    select(system:designation_ineligible, score_achievement:targeted_support_White, targeted_support) %>%
    mutate(targeted_support_Native = as.numeric(targeted_support_Native), targeted_support_HPI = as.numeric(targeted_support_HPI))

school_grading_grades_am <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades_AM.csv") %>%
    select(-final_average, - targeted_support_subgroups) %>%
    mutate(targeted_support_Native = as.numeric(targeted_support_Native), targeted_support_HPI = as.numeric(targeted_support_HPI))

test_that("Matching Grades", {

    difference_df_grades <- setdiff( school_grading_grades , school_grading_grades_am) %>% #  %>% select(-percentile)
        bind_rows(setdiff(school_grading_grades_am, school_grading_grades)) %>%
        arrange(system, school)

    expect_equal(nrow(difference_df_grades), 0)
})

atsi_jc <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/atsi-subgroup-option-1.csv")

atsi_option_1 <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/atsi-subgroup-option-1.csv") %>%
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

atsi_ap_subgroups <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv") %>%
    select(system:designation_ineligible, additional_targeted_support, additional_targeted_support_BHN:additional_targeted_support_White)

test_that("Matching ATSI Schools", {
    # Matching ATSI Schools
    difference_df_atsi <- setdiff( atsi_option_1 , atsi_ap) %>% #  %>% select(-percentile)
        bind_rows(setdiff(atsi_ap, atsi_option_1)) %>%
        arrange(system, school)

    expect_equal(nrow(difference_df_atsi), 0)
})

# Matching ATSI Subgroups
atsi_jc_subgroups<- read_csv("N:/ORP_accountability/projects/2019_school_accountability/atsi-subgroup-option-1.csv") %>%
    filter(!subgroup %in% c('All Students', 'Super Subgroup')) %>%
    mutate(
        atsi = if_else(elig_tsi == 0, NA_real_, atsi)
    ) %>%
    select(system, school, subgroup, atsi) %>%
    mutate(
        subgroup = case_when(
            subgroup == 'Black/Hispanic/Native American' ~ 'additional_targeted_support_BHN',
            subgroup == 'Economically Disadvantaged' ~ 'additional_targeted_support_ED',
            subgroup == 'Students with Disabilities' ~ 'additional_targeted_support_SWD',
            subgroup == 'English Learners with Transitional 1-4' ~ 'additional_targeted_support_EL',
            subgroup == 'American Indian or Alaska Native' ~ 'additional_targeted_support_Native',
            subgroup == 'Asian' ~ 'additional_targeted_support_Asian',
            subgroup == 'Black or African American' ~ 'additional_targeted_support_Black',
            subgroup == 'Hispanic' ~ 'additional_targeted_support_Hispanic',
            subgroup == 'Native Hawaiian or Other Pacific Islander' ~ 'additional_targeted_support_HPI',
            subgroup == 'White' ~ 'additional_targeted_support_White'
        )
    ) %>%
    spread(subgroup, atsi)

atsi_ap_subgroups <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv") %>%
    select(system, school, additional_targeted_support_BHN:additional_targeted_support_White) %>%
    mutate(additional_targeted_support_Native = as.numeric(additional_targeted_support_Native),
           additional_targeted_support_HPI = as.numeric(additional_targeted_support_HPI))

test_that("Matching ATSI Subgroups", {
    # Matching ATSI Schools
    difference_df_atsi_subgroups <- setdiff( atsi_jc_subgroups , atsi_ap_subgroups) %>% #  %>% select(-percentile)
        bind_rows(setdiff(atsi_ap_subgroups, atsi_jc_subgroups)) %>%
        arrange(system, school)

    expect_equal(nrow(difference_df_atsi_subgroups), 0)
})

















