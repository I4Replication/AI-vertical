# Cleaning step for AI vertical: simulate individuals and aggregate to team-level
suppressPackageStartupMessages({
  library(here); library(yaml); library(dplyr)
})
source(here::here("R","simulate_individuals.R"))

cfg <- yaml::read_yaml(here::here("config","config.yml"))
indiv <- sim_individuals(cfg)

# Save dataset for subsequent scripts (individual-level only)
sim_save_indiv(indiv)
