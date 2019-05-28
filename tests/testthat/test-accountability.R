context("Accountability Files")

library(readr)
library(dplyr)
library(purrr)

school_accountability <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv")
district_accountability <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_accountability_file.csv")

test_that("Check Uniqueness", {
    expect_true(
        nrow(school_accountability) == nrow(distinct(school_accountability, system, school, indicator, subgroup)),
        label = "School accountability file unique by system, school, indicator, subgroup"
    )
    expect_true(
        nrow(district_accountability) == nrow(distinct(district_accountability, system, grade, indicator, subgroup)),
        label = "District accountability file unique by system, grade, indicator, subgroup"
    )
})

test_that("All Indicators", {
    expect_setequal(
        unique(school_accountability$indicator),
        c("Achievement", "Growth", "Chronic Absenteeism", "Graduation Rate", "Ready Graduates", "ELPA Growth Standard")
    )
    expect_setequal(
        unique(district_accountability$indicator),
        c("Achievement", "Chronic Absenteeism", "Graduation Rate", "ELPA Growth Standard")
    )
})

test_that("All Subgroups", {
    expect_setequal(
        unique(district_accountability$subgroup),
        c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
          "English Learners with Transitional 1-4", "Students with Disabilities")
    )
    expect_setequal(
        unique(school_accountability$subgroup),
        c("All Students", "American Indian or Alaska Native", "Asian", "Black or African American",
          "Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4", "Hispanic",
          "Native Hawaiian or Other Pacific Islander", "Super Subgroup", "Students with Disabilities", "White")
    )
})
