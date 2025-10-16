# Synthetic individual-level generator for AI vertical
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(purrr); library(yaml)
})

sim_read_config <- function(){
  cfg_path <- here::here("config","config.yml")
  yaml::read_yaml(cfg_path)
}

sim_individuals <- function(cfg){
  set.seed(cfg$seed %||% 1234)
  n_participants <- cfg$n_participants %||% 300
  games <- cfg$games
  branches <- cfg$branches
  softwares <- cfg$softwares
  attendance <- cfg$attendance
  skills <- cfg$skills
  gpt_levels <- cfg$gpt_levels
  coding_levels <- cfg$coding_levels

  tiers <- c("UG","MA","PhD","PD","P")

  indiv <- tibble(
    participant_id = seq_len(n_participants),
    game = factor(sample(games, n_participants, replace = TRUE), levels = games),
    branch = factor(sample(branches, n_participants, replace = TRUE), levels = branches),
    software = factor(sample(softwares, n_participants, replace = TRUE), levels = softwares),
    attendance = factor(sample(attendance, n_participants, replace = TRUE), levels = attendance)
  ) %>%
    mutate(
      game2 = game,
      article = factor(paste0(as.character(game), "_A", sample(1:3, n(), replace = TRUE))),
      tier = factor(sample(tiers, n(), replace = TRUE), levels = tiers),
      years_coding = pmax(0,
        ifelse(tier=="UG", rnorm(n(), 2, 1),
        ifelse(tier=="MA", rnorm(n(), 3.5, 1.5),
        ifelse(tier=="PhD", rnorm(n(), 5, 2),
        ifelse(tier=="PD", rnorm(n(), 6, 2), rnorm(n(), 8, 3)))))
      ),
      preferred_software = factor(sample(c("R","Stata","Python"), n(), replace = TRUE), levels = c("R","Stata","Python")),
      prior_gpt_familiarity = factor(sample(c("None","Some","Heavy"), n(), replace = TRUE), levels = c("None","Some","Heavy")),
      treatment = rbinom(n(), 1, 0.5),
      max_gpt = factor(sample(gpt_levels, n(), replace = TRUE), levels = gpt_levels),
      min_gpt = factor(sample(gpt_levels, n(), replace = TRUE), levels = gpt_levels),
      max_coding = factor(sample(coding_levels, n(), replace = TRUE), levels = coding_levels),
      min_coding = factor(sample(coding_levels, n(), replace = TRUE), levels = coding_levels)
    )

  base_rep_ug <- cfg$pap_base_rep_ug %||% 0.40
  tier_delta <- case_when(
    indiv$tier=="MA" ~ 0.03,
    indiv$tier=="PhD" ~ 0.07,
    indiv$tier=="PD" ~ 0.10,
    indiv$tier=="P" ~ 0.15,
    TRUE ~ 0
  )
  trt_main <- cfg$pap_trt_main_ug %||% 0.06
  comp_rate <- cfg$pap_compression_rate %||% 0.40
  p0 <- pmin(0.98, pmax(0.02, base_rep_ug + tier_delta))
  d_tier <- trt_main - comp_rate * pmax(0, tier_delta)
  p1 <- pmin(0.98, pmax(0.02, p0 + d_tier))
  p_rep <- ifelse(indiv$treatment==1, p1, p0)
  p_rep <- p_rep + 0.01*(indiv$prior_gpt_familiarity=="Heavy")
  p_rep <- pmin(0.98, pmax(0.02, p_rep + rnorm(nrow(indiv), 0, 0.05)))

  indiv <- indiv %>% mutate(
    reproduction_i = rbinom(n(), 1, p_rep),
    minor_errors_i = rpois(n(), lambda = cfg$error_minor_rate * (1 + (tier %in% c("UG","MA"))*0.1)),
    major_errors_i = rpois(n(), lambda = cfg$error_major_rate * (1 + (tier %in% c("UG"))*0.1))
  )

  lin_h <- -0.5 + 0.25*(indiv$treatment==1) + 0.06*log1p(indiv$years_coding)
  p_app_h <- plogis(lin_h)
  ref_app_human_i  <- rbinom(nrow(indiv), 1, p_app_h)
  ref_score_human_i <- pmin(5, pmax(0, 2.8 + 0.5*(indiv$treatment==1) + 0.1*log1p(indiv$years_coding) + rnorm(nrow(indiv), 0, 0.8)))

  lin_ai <- lin_h + rnorm(nrow(indiv), 0, 0.2)
  p_app_ai <- plogis(lin_ai)
  ref_app_ai_i  <- rbinom(nrow(indiv), 1, p_app_ai)
  ref_score_ai_i <- pmin(5, pmax(0, ref_score_human_i + rnorm(nrow(indiv), 0, 0.3)))

  indiv <- indiv %>% mutate(
    referee_app_human_i  = ref_app_human_i,
    referee_score_human_i = ref_score_human_i,
    referee_app_ai_i     = ref_app_ai_i,
    referee_score_ai_i   = ref_score_ai_i
  )

  indiv <- indiv %>% mutate(
    good_checks_i = rbinom(n(), 1, plogis(-1 + 0.4*(treatment==1) + 0.05*years_coding)),
    two_good_checks_i = rbinom(n(), 1, plogis(-2 + 0.3*(treatment==1) + 0.05*years_coding)),
    ran_any_check_i = rbinom(n(), 1, plogis(-0.5 + 0.3*(treatment==1) + 0.03*years_coding)),
    clarity_score_i = pmin(5, pmax(0, rnorm(n(), 3 + 0.2*(treatment==1) + 0.05*log1p(years_coding), 0.8)))
  )

  indiv <- indiv %>% mutate(
    implemented_check_1_i = pmin(1, good_checks_i * rbinom(n(), 1, 0.75)),
    implemented_check_2_i = pmin(1, two_good_checks_i * rbinom(n(), 1, 0.65))
  )

  indiv
}

sim_save_indiv <- function(indiv_df){
  dir.create(here::here("data"), showWarnings = FALSE, recursive = TRUE)
  saveRDS(indiv_df, file = here::here("data","AI individuals.rds"))
}
