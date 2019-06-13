library(readr)
library(dplyr)
library(purrr)

student_ap <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student.csv")
student_am <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student_AM.csv")

test_that("Match Student", {

    expect_equal(
        0,
        bind_rows(setdiff(student_ap, student_am), setdiff(student_am, student_ap)) %>%
            nrow()
    )

})

state_ap <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_state.csv")
state_am <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_state_AM.csv") %>%
    rename(literacy_average = literacy_avg, composite_average = composite_avg)

test_that("Match State", {

    expect_equal(
        0,
        bind_rows(setdiff(state_ap, state_am), setdiff(state_ap, state_am)) %>%
            nrow()
    )

})

district_ap <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district.csv")
district_am <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district_AM.csv") %>%
    rename(literacy_average = literacy_avg, composite_average = composite_avg)

test_that("Match District", {

    expect_equal(
        0,
        bind_rows(setdiff(district_ap, district_am), setdiff(district_ap, district_am)) %>%
            nrow()
    )

})

school_ap <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school.csv")
school_am <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school_AM.csv") %>%
    rename(literacy_average = literacy_avg, composite_average = composite_avg)

test_that("Match School", {

    expect_equal(
        0,
        bind_rows(setdiff(school_ap, school_am), setdiff(school_ap, school_am)) %>%
            nrow()
    )

})

test_that("Test Split", {

    district_split <- dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "District", full.names = TRUE) %>%
        map_dfr(read_csv)

    expect_equal(
        0,
        bind_rows(setdiff(district_ap, district_split), setdiff(district_split, district_ap)) %>%
            nrow()
    )

    school_split <- dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "School", full.names = TRUE) %>%
       map_dfr(read_csv)

    expect_equal(
       0,
       bind_rows(setdiff(school_ap, school_split), setdiff(school_split, school_ap)) %>%
           nrow()
   )

    student_split <- dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "Student", full.names = TRUE) %>%
        map_dfr(read_csv)

    expect_equal(
        0,
        bind_rows(setdiff(student_ap, student_split), setdiff(student_split, student_ap)) %>%
            nrow()
    )
})

test_that("No Empty Split Files", {

    expect_true(
        dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "District", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(nrow) %>%
            all(. != 0)
    )

    expect_true(
        dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "School", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(nrow) %>%
            all(. != 0)
    )

    expect_true(
        dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "Student", full.names = TRUE) %>%
            map(read_csv) %>%
            map_int(nrow) %>%
            all(. != 0)
    )

})

test_that("One District Per Split File", {

    expect_true(
        dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "District", full.names = TRUE) %>%
            map(read_csv) %>%
            map(~ length(unique(.$system))) %>%
            all(. == 1)
    )

    expect_true(
        dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "School", full.names = TRUE) %>%
            map(read_csv) %>%
            map(~ length(unique(.$system))) %>%
            all(. == 1)
    )

    expect_true(
        dir("N:/ORP_accountability/data/2019_ELPA/split/", pattern = "Student", full.names = TRUE) %>%
            map(read_csv) %>%
            map(~ length(unique(.$system))) %>%
            all(. == 1)
    )

})
