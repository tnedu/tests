context("Ready Grad")

library(readr)
library(dplyr)
library(purrr)

state_ap <- read_csv("N:\\ORP_accountability\\projects\\2019_ready_graduate\\Data\\ready_graduate_state.csv")
state_am <- read_csv("N:\\ORP_accountability\\projects\\2019_ready_graduate\\Data\\ready_graduate_state_AM.csv")

test_that("Match State", {

    expect_equal(
        0,
        bind_rows(setdiff(state_ap %>% select(subgroup:n_ready_grad), state_am %>% select(subgroup:n_ready_grad)),
                  setdiff(state_ap %>% select(subgroup:n_ready_grad), state_am %>% select(subgroup:n_ready_grad))) %>%
            nrow()
    )

})

district_ap <- read_csv("N:\\ORP_accountability\\projects\\2019_ready_graduate\\Data\\ready_graduate_district.csv")
district_am <- read_csv("N:\\ORP_accountability\\projects\\2019_ready_graduate\\Data\\ready_graduate_district_AM.csv")

test_that("Match District", {

    expect_equal(
        0,
        bind_rows(setdiff(district_ap, district_am), setdiff(district_ap, district_am)) %>%
            nrow()
    )

})

school_ap <- read_csv("N:\\ORP_accountability\\projects\\2019_ready_graduate\\Data\\ready_graduate_school.csv")
school_am <- read_csv("N:\\ORP_accountability\\projects\\2019_ready_graduate\\Data\\ready_graduate_school.csv")

test_that("Match School", {

    expect_equal(
        0,
        bind_rows(setdiff(school_ap, school_am), setdiff(school_ap, school_am)) %>%
            nrow()
    )

})
