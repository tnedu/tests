context("Absenteeism")

library(readr)
library(dplyr)
library(purrr)

state <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/state_chronic_absenteeism_Jun17.csv")
district <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jun17.csv")
school <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jun17.csv")
student <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/student_chronic_absenteeism_Jun17.csv")

test_that("Check Uniqueness", {

    expect_true(
        nrow(state) == nrow(distinct(state, subgroup, grade_band))
    )

    expect_true(
        nrow(district) == nrow(distinct(district, system, subgroup, grade_band))
    )

    expect_true(
        nrow(school) == nrow(distinct(school, system, school, subgroup))
    )

})

test_that("Test Split", {

    district_split <- dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "District", full.names = TRUE) %>%
        map_dfr(read_csv)

    expect_equal(nrow(bind_rows(setdiff(district, district_split), setdiff(district_split, district))), 0)

    school_split <- dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "School", full.names = TRUE) %>%
        map_dfr(read_csv)

    expect_equal(nrow(bind_rows(setdiff(school, school_split), setdiff(school_split, school))), 0)

    student_split <- dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "Student", full.names = TRUE) %>%
        map_dfr(read_csv)

    expect_equal(nrow(bind_rows(setdiff(student, student_split), setdiff(student_split, student))), 0)

})

test_that("No Empty Split Files", {

    expect_true(
        dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "District", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(nrow) %>%
            all(. != 0)
    )

    expect_true(
        dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "School", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(nrow) %>%
            all(. != 0)
    )

    expect_true(
        dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "Student", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(nrow) %>%
            all(. != 0)
    )

})

test_that("One District Per Split File", {

    expect_true(
        dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "District", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(~ length(unique(.$system))) %>%
            all(. == 1)
    )

    expect_true(
        dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "School", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(~ length(unique(.$system))) %>%
            all(. == 1)
    )

    expect_true(
        dir("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", pattern = "Student", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(~ length(unique(.$system))) %>%
            all(. == 1)
    )

})
