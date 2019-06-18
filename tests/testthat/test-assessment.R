context("Assessment Files")

library(readr)
library(dplyr)
library(purrr)

school_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv")
district_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file.csv")
state_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/state_assessment_file.csv")

school_assessment_am <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file_AM.csv")
district_assessment_am <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file_AM.csv")
state_assessment_am <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/state_assessment_file_AM.csv")

test_that("Check Uniqueness", {
    expect_true(
        nrow(school_assessment) == nrow(distinct(school_assessment, year, system, school, test, subject, grade, subgroup)),
        label = "School assessment file unique by year, system, school, test, subject, grade, subgroup"
    )
    expect_true(
        nrow(district_assessment) == nrow(distinct(district_assessment, year, system, test, subject, grade, subgroup)),
        label = "District assessment file unique by year, system, test, subject, grade, subgroup"
    )
    expect_true(
        nrow(state_assessment) == nrow(distinct(state_assessment, year, test, subject, grade, subgroup)),
        label = "State assessment file unique by year, test, subject, grade, subgroup"
    )
})

test_that("No Missingness", {

    count_missing <- function(x) sum(is.na(x))

    expect_true(
        school_assessment %>%
            select(year, system, school, test, subject, grade, subgroup) %>%
            map_lgl(~ count_missing(.) == 0) %>%
            all(),
        label = "No missingness in school assessment file")
    expect_true(
        district_assessment %>%
            select(year, system, test, subject, grade, subgroup) %>%
            map_lgl(~ count_missing(.) == 0) %>%
            all(),
        label = "No missingness in the district assessment file"
    )
    expect_true(
        district_assessment %>%
            select(year, system, test, subject, grade, subgroup) %>%
            map_lgl(~ count_missing(.) == 0) %>%
            all(),
        label = "No missingness in the state assessment file"
    )
})

test_that("All Tests", {

    tests <- c("EOC", "TNReady", "MSAA/Alt-Social Studies")

    expect_setequal(school_assessment$test, tests)
    expect_setequal(district_assessment$test, tests)
    expect_setequal(state_assessment$test, tests)

})

test_that("All Subjects", {

    subjects <- c("Math", "ELA", "Algebra I", "Algebra II", "English I", "English II", "Geometry",
      "Integrated Math I", "Integrated Math II", "Integrated Math III", "US History")

    expect_setequal(school_assessment$subject, subjects)
    expect_setequal(district_assessment$subject, subjects)
    expect_setequal(state_assessment$subject, subjects)

})

test_that("All Subgroups", {

    subgroups <- c("All Students", "Asian", "Black or African American", "Black/Hispanic/Native American",
        "Economically Disadvantaged", "English Learners", "English Learner Transitional 1-4",
        "English Learners with Transitional 1-4", "Native Hawaiian or Other Pacific Islander",
        "Hispanic", "American Indian or Alaska Native", "Non-Black/Hispanic/Native American",
        "Non-Economically Disadvantaged", "Non-English Learners/Transitional 1-4", "Non-Students with Disabilities",
        "Super Subgroup", "Students with Disabilities", "White", "Male", "Female", "Migrant", "Gifted")

    expect_setequal(unique(state_assessment$subgroup), subgroups)
    expect_setequal(unique(district_assessment$subgroup), subgroups)
    expect_setequal(unique(school_assessment$subgroup), subgroups)

})

test_that("Matching", {

    difference_df_state <- (state_assessment %>% select(year:n_mastered)) %>%
        setdiff(state_assessment_am %>% select(year:n_mastered)) %>%
        bind_rows(setdiff(state_assessment_am %>% select(year:n_mastered), state_assessment %>% select(year:n_mastered))) %>%
        arrange(system, test, subject, grade, subgroup, -year)

    difference_df_district <- (district_assessment %>% select(year:n_mastered)) %>%
        setdiff(district_assessment_am %>% select(year:n_mastered)) %>%
        bind_rows(setdiff(district_assessment_am %>% select(year:n_mastered), district_assessment %>% select(year:n_mastered))) %>%
        arrange(system, test, subject, grade, subgroup, -year)

    difference_df_school <- (school_assessment %>% select(year:n_mastered)) %>%
        setdiff(school_assessment_am %>% select(year:n_mastered)) %>%
        bind_rows(setdiff(school_assessment_am %>% select(year:n_mastered), school_assessment %>% select(year:n_mastered))) %>%
        arrange(system, school, test, subject, grade, subgroup, -year)

    expect_equal(nrow(difference_df_state), 0)
    expect_equal(nrow(difference_df_district), 0)
    expect_equal(nrow(difference_df_school), 0)

})


