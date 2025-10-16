suppressPackageStartupMessages({library(testthat); library(here); library(yaml)})
source(here::here("R","simulate_individuals.R"))

cfg <- yaml::read_yaml(here::here("config","config.yml"))
indiv <- sim_individuals(cfg)

cols_expected <- c("participant_id","game","branch","software","attendance",
                   "tier","years_coding","treatment","reproduction_i",
                   "minor_errors_i","major_errors_i","referee_app_human_i",
                   "referee_score_human_i","referee_app_ai_i","referee_score_ai_i",
                   "good_checks_i","two_good_checks_i","implemented_check_1_i",
                   "implemented_check_2_i")

test_that("participant data have expected columns", {
  expect_true(all(cols_expected %in% names(indiv)))
  expect_equal(nrow(indiv), cfg$n_participants)
  expect_true(all(indiv$reproduction_i %in% c(0,1)))
})

