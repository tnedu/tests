context("Student Level File")

library(readr)
library(dplyr)
library(purrr)

student <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv")
student_am <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_AM.csv")

test_that("Check Uniqueness", {
    expect_equal(nrow(student), nrow(distinct(student, state_student_id, original_subject, test)))
})

test_that("No Missingness", {

    count_missing <- function(x) sum(is.na(x))

    expect_equal(count_missing(student$gender), 0)
    expect_equal(count_missing(student$reported_race), 0)
    expect_equal(count_missing(student$economically_disadvantaged), 0)
    expect_equal(count_missing(student$special_ed), 0)
    expect_equal(count_missing(student$el), 0)

})

test_that("All Tests", {
    expect_setequal(student$test, c("EOC", "TNReady", "MSAA", "Alt-Social Studies"))
})

test_that("All Subjects", {
    expect_setequal(
        student$original_subject,
        c("Math", "ELA", "Algebra I", "Algebra II", "English I", "English II", "Geometry",
          "Integrated Math I", "Integrated Math II", "Integrated Math III", "US History")
    )
})


test_that("Matching", {

    difference_df_student <- setdiff( student , student_am) %>% #  %>% select(-percentile)
        bind_rows(setdiff(student_am, student)) %>%
        arrange(system, school, state_student_id, original_subject)

    expect_equal(nrow(difference_df_student), 0)
})














