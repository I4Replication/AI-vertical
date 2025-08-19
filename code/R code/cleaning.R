# Cleaning step for AI vertical: simulate individuals and aggregate to team-level
suppressPackageStartupMessages({
  library(here); library(yaml); library(dplyr)
})
source(here::here("R","simulate_individuals.R"))

cfg <- yaml::read_yaml(here::here("config","config.yml"))
indiv <- sim_individuals(cfg)
main <- aggregate_to_team(indiv)

# Cast factors similar to AI paper
main <- main %>% mutate(
  game2 = factor(game2, levels = c("Toronto","Ottawa","Sheffield","Cornell","Bogota","Tilburg","Virtual","Virtual 2025")),
  branch = factor(branch, levels = c("Human-Only","AI-Assisted","AI-Led")),
  software = factor(software, levels = c("Stata","R")),
  attendance = factor(attendance, levels = c("Virtual","In-Person")),
  paper = factor("Synthetic", levels = c("Synthetic")),
  max_gpt = factor(NA, levels = cfg$gpt_levels),
  min_gpt = factor(NA, levels = cfg$gpt_levels),
  max_coding = factor(NA, levels = cfg$coding_levels),
  min_coding = factor(NA, levels = cfg$coding_levels)
)

# Save dataset for subsequent scripts
sim_save_main(main)
