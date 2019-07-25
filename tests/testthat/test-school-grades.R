context('School Grades')

library(readr)
library(dplyr)
library(purrr)

school_grading_metrics_ap <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_metrics.csv") %>%
    select(system:subgroup, score_achievement, score_growth, score_grad, score_ready_grad, score_absenteeism,
           score_elpa, total_weight, subgroup_average) %>%
    mutate(score_elpa = as.numeric(score_elpa)) %>%
    filter(subgroup != 'Subgroups')
school_grading_metrics_am <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_metrics_AM.csv") %>%
    select(system:subgroup, score_achievement, score_growth, score_grad, score_ready_grad, score_absenteeism,
           score_elpa, total_weight, subgroup_average)

school_grading_grades_ap <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv")
school_grading_grades_am <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades_AM.csv")

test_that("Matching Metrics", {

    diff_df_metrics <- setdiff(school_grading_metrics_ap, school_grading_metrics_am) %>% #  %>% select(-total_weight, -subgroup_average)
        bind_rows(setdiff(school_grading_metrics_am, school_grading_metrics_ap)) %>%
        #filter(is.na(pool), designation_ineligible == 0) %>%
        arrange(system, school, subgroup)

    expect_equal(nrow(diff_df_metrics), 0)
})
