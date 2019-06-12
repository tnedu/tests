library(readr)
library(dplyr)
library(purrr)

student_ap <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student.csv")
student_am <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student_AM.csv")

test_that("match_student", {

    expect_equal(
        0,
        bind_rows(setdiff(student_ap, student_am), setdiff(student_am, student_ap)) %>%
            nrow()
    )

})

state_ap <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_state.csv")
state_am <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_state_AM.csv") %>%
    rename(literacy_average = literacy_avg, composite_average = composite_avg)

test_that("match_state", {

    expect_equal(
        0,
        bind_rows(setdiff(state_ap, state_am), setdiff(state_ap, state_am)) %>%
            nrow()
    )

})

district_ap <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district.csv")
district_am <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district_AM.csv") %>%
    rename(literacy_average = literacy_avg, composite_average = composite_avg)

test_that("match_district", {

    expect_equal(
        0,
        bind_rows(setdiff(district_ap, district_am), setdiff(district_ap, district_am)) %>%
            nrow()
    )

})

school_ap <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school.csv")
school_am <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school_AM.csv") %>%
    rename(literacy_average = literacy_avg, composite_average = composite_avg)

test_that("match_school", {

    expect_equal(
        0,
        bind_rows(setdiff(school_ap, school_am), setdiff(school_ap, school_am)) %>%
            nrow()
    )

})
