suppressPackageStartupMessages({library(testthat); library(here); library(yaml)})
source(here::here("R","simulate_individuals.R"))

cfg <- yaml::read_yaml(here::here("config","config.yml"))
indiv <- sim_individuals(cfg)
main  <- aggregate_to_team(indiv)


test_that("individuals have expected columns", {
  expect_true(all(c("team_id","indiv_id","branch","software","attendance") %in% names(indiv)))
  expect_true(all(indiv$reproduction_i %in% c(0,1)))
})

test_that("aggregation yields team-level rows", {
  expect_true(all(!duplicated(main$team_id)))
  expect_true(all(main$number_teammates >= cfg$individuals_per_team_min))
  expect_true(all(main$minor_errors >= 0 & main$major_errors >= 0))
})

