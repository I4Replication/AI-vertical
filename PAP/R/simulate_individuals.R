# Synthetic individual-level generator for AI vertical
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(purrr); library(readr); library(yaml)
})

sim_read_config <- function(){
  cfg_path <- here::here("config","config.yml")
  yaml::read_yaml(cfg_path)
}

sim_individuals <- function(cfg){
  set.seed(cfg$seed %||% 1234)
  games <- cfg$games
  branches <- cfg$branches
  softwares <- cfg$softwares
  attendance <- cfg$attendance
  skills <- cfg$skills
  gpt_levels <- cfg$gpt_levels
  coding_levels <- cfg$coding_levels

  # PAP tiers (expertise ladder)
  tiers <- c("UG","MA","PhD","PD","P")

  teams <- tibble(
    team_id = seq_len(cfg$n_teams),
    game = factor(sample(games, cfg$n_teams, replace = TRUE), levels = games),
    branch = factor(sample(branches, cfg$n_teams, replace = TRUE), levels = branches),
    software = factor(sample(softwares, cfg$n_teams, replace = TRUE), levels = softwares),
    attendance = factor(sample(attendance, cfg$n_teams, replace = TRUE), levels = attendance),
    number_teammates = sample(cfg$individuals_per_team_min:cfg$individuals_per_team_max, cfg$n_teams, replace = TRUE),
    max_skill = factor(sample(skills, cfg$n_teams, replace = TRUE), levels = skills),
    min_skill = factor(sample(skills, cfg$n_teams, replace = TRUE), levels = skills)
  ) |> mutate(
    game2 = game
  )

  # expand to individuals
  indiv <- teams |> 
    rowwise() |> 
    mutate(indiv_ids = list(seq_len(number_teammates))) |> 
    unnest(indiv_ids) |> 
    ungroup() |> 
    transmute(
      team_id, game, game2, branch, software, attendance,
      number_teammates,
      max_skill, min_skill,
      indiv_id = paste(team_id, indiv_ids, sep = "_")
    ) |> 
    mutate(
      # simple article assignment per event (3 per event)
      article = factor(paste0(as.character(game), "_A", sample(1:3, n(), replace = TRUE))),
      # PAP: expertise tiers (random across individuals)
      tier = factor(sample(tiers, n(), replace = TRUE), levels = tiers),
      years_coding = pmax(0,
        ifelse(tier=="UG", rnorm(n(), 2, 1),
        ifelse(tier=="MA", rnorm(n(), 3.5, 1.5),
        ifelse(tier=="PhD", rnorm(n(), 5, 2),
        ifelse(tier=="PD", rnorm(n(), 6, 2), rnorm(n(), 8, 3))))
      )),
      preferred_software = factor(sample(c("R","Stata","Python"), n(), replace = TRUE), levels = c("R","Stata","Python")),
      prior_gpt_familiarity = factor(sample(c("None","Some","Heavy"), n(), replace = TRUE), levels = c("None","Some","Heavy")),
      # PAP: treatment assignment 1:1 within tier (approximate at team expansion stage)
      treatment = rbinom(n(), 1, 0.5),
      max_gpt = factor(sample(gpt_levels, n(), replace = TRUE), levels = gpt_levels),
      min_gpt = factor(sample(gpt_levels, n(), replace = TRUE), levels = gpt_levels),
      max_coding = factor(sample(coding_levels, n(), replace = TRUE), levels = coding_levels),
      min_coding = factor(sample(coding_levels, n(), replace = TRUE), levels = coding_levels)
    )

  # prompts/files/images/words at individual level (Poisson-ish)
  lam <- pmax(0.1, rnorm(nrow(indiv), cfg$prompts_intensity_mean, cfg$prompts_intensity_sd))
  indiv <- indiv |> mutate(
    prompts_i = rpois(n(), lam),
    files_i   = rpois(n(), lam/4),
    images_i  = rpois(n(), lam/6),
    words_i   = rpois(n(), lam*20)
  )

  # PAP: outcome generation with treatment x tier compression
  # Baseline UG control mean and tier deltas
  base_rep_ug <- cfg$pap_base_rep_ug %||% 0.40
  tier_delta <- case_when(
    indiv$tier=="MA" ~ 0.03,
    indiv$tier=="PhD" ~ 0.07,
    indiv$tier=="PD" ~ 0.10,
    indiv$tier=="P" ~ 0.15,
    TRUE ~ 0
  )
  # Treatment main effect at UG and compression (negative interactions for higher tiers)
  trt_main <- cfg$pap_trt_main_ug %||% 0.06
  comp_rate <- cfg$pap_compression_rate %||% 0.40
  # Probability under control for each tier
  p0 <- pmin(0.98, pmax(0.02, base_rep_ug + tier_delta))
  # Treatment effect by tier: UG gets trt_main; higher tiers get reduced by compression*delta
  d_tier <- trt_main - comp_rate * pmax(0, tier_delta)
  p1 <- pmin(0.98, pmax(0.02, p0 + d_tier))
  p_rep <- ifelse(indiv$treatment==1, p1, p0)
  # Prior familiarity and prompts nudge
  p_rep <- p_rep + 0.01*(indiv$prior_gpt_familiarity=="Heavy") + 0.0005*indiv$prompts_i
  p_rep <- pmin(0.98, pmax(0.02, p_rep + rnorm(nrow(indiv), 0, 0.05)))

  indiv <- indiv |> mutate(
    reproduction_i = rbinom(n(), 1, p_rep),
    minor_errors_i = rpois(n(), lambda = cfg$error_minor_rate * (1 + (tier %in% c("UG","MA"))*0.1)),
    major_errors_i = rpois(n(), lambda = cfg$error_major_rate * (1 + (tier %in% c("UG"))*0.1))
  )

  # Referee report outcomes (human and AI assessments)
  # Binary appropriateness (LPM target) and 0-5 scores (continuous)
  # Human assessment: average of three judges (simulated as single scalar outcome here)
  lin_h <- -0.5 + 0.25*(indiv$treatment==1) + 0.06*log1p(indiv$years_coding)
  p_app_h <- plogis(lin_h)
  ref_app_human_i  <- rbinom(nrow(indiv), 1, p_app_h)
  ref_score_human_i <- pmin(5, pmax(0, 2.8 + 0.5*(indiv$treatment==1) + 0.1*log1p(indiv$years_coding) + rnorm(nrow(indiv), 0, 0.8)))

  # AI assessment: correlated with human plus small independent noise
  lin_ai <- lin_h + rnorm(nrow(indiv), 0, 0.2)
  p_app_ai <- plogis(lin_ai)
  ref_app_ai_i  <- rbinom(nrow(indiv), 1, p_app_ai)
  ref_score_ai_i <- pmin(5, pmax(0, ref_score_human_i + rnorm(nrow(indiv), 0, 0.3)))

  indiv <- indiv |> mutate(
    referee_app_human_i  = ref_app_human_i,
    referee_score_human_i = ref_score_human_i,
    referee_app_ai_i     = ref_app_ai_i,
    referee_score_ai_i   = ref_score_ai_i
  )

  # Robustness checks and clarity
  indiv <- indiv |> mutate(
    good_checks_i = rbinom(n(), 1, plogis(-1 + 0.4*(treatment==1) + 0.05*years_coding)),
    two_good_checks_i = rbinom(n(), 1, plogis(-2 + 0.3*(treatment==1) + 0.05*years_coding)),
    ran_any_check_i = rbinom(n(), 1, plogis(-0.5 + 0.3*(treatment==1) + 0.03*years_coding)),
    clarity_score_i = pmin(5, pmax(0, rnorm(n(), 3 + 0.2*(treatment==1) + 0.05*log1p(years_coding), 0.8)))
  )

  # approximate times since 9:00 base
  base_time <- as.POSIXct("1899-12-31 09:00:00", tz = "UTC")
  indiv <- indiv |> mutate(
    time2_reproduction_i = ifelse(reproduction_i==1, pmax(1, rnorm(n(), cfg$start_time_mean - 8*(treatment==1), cfg$start_time_sd)), NA),
    time2_first_minor_i  = ifelse(minor_errors_i>0, pmax(1, rnorm(n(), cfg$start_time_mean - 4*(treatment==1), cfg$start_time_sd)), NA),
    time2_first_major_i  = ifelse(major_errors_i>0, pmax(1, rnorm(n(), cfg$start_time_mean - 4*(treatment==1), cfg$start_time_sd)), NA),
    # Censor at 420 minutes for minutes-to-success as per PAP
    minutes_to_success_i = pmin(420, ifelse(is.na(time2_reproduction_i), 420, time2_reproduction_i))
  )

  indiv
}

# Aggregate individuals -> team-level main like AI paper
aggregate_to_team <- function(indiv){
  indiv |> group_by(team_id, game, game2, branch, software, attendance, number_teammates, max_skill, min_skill) |> 
    summarise(
      reproduction = mean(reproduction_i, na.rm = TRUE),
      time2_reproduction = suppressWarnings(min(time2_reproduction_i, na.rm = TRUE)),
      minor_errors = sum(minor_errors_i, na.rm = TRUE),
      time2_first_minor = suppressWarnings(min(time2_first_minor_i, na.rm = TRUE)),
      major_errors = sum(major_errors_i, na.rm = TRUE),
      time2_first_major = suppressWarnings(min(time2_first_major_i, na.rm = TRUE)),
      prompts = sum(prompts_i, na.rm = TRUE),
      files   = sum(files_i,   na.rm = TRUE),
      images  = sum(images_i,  na.rm = TRUE),
      words   = sum(words_i,   na.rm = TRUE),
      .groups = "drop"
    ) |> mutate(
      time2_reproduction = ifelse(is.infinite(time2_reproduction), NA, time2_reproduction),
      time2_first_minor  = ifelse(is.infinite(time2_first_minor), NA, time2_first_minor),
      time2_first_major  = ifelse(is.infinite(time2_first_major), NA, time2_first_major)
    )
}

# Save main dataset (team-level)
sim_save_main <- function(main_df){
  dir.create(here::here("data"), showWarnings = FALSE, recursive = TRUE)
  saveRDS(main_df, file = here::here("data","AI games.rds"))
}

# Save individual-level dataset for PAP analyses
sim_save_indiv <- function(indiv_df){
  dir.create(here::here("data"), showWarnings = FALSE, recursive = TRUE)
  saveRDS(indiv_df, file = here::here("data","AI individuals.rds"))
}
