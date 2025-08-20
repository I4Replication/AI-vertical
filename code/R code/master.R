# ===========================================
# Master R Script for AI Vertical (synthetic)
# ===========================================
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
here::i_am("code/R code/master.R")

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  here, yaml, dplyr, stringr, tidyr, forcats, janitor, lubridate,
  fixest, purrr, broom, tibble, car, margins,
  sandwich, lmtest, multcomp, kableExtra,
  ggplot2, patchwork, modelsummary, ggsurvfit, survRM2, xtable, glue
)

# logging
log_file <- here::here("output", "master_log_R.log")
dir.create(here::here("output"), showWarnings = FALSE, recursive = TRUE)
sink(log_file, split = TRUE)
cat("=== MASTER LOG START (vertical) ===\n")

# 1. Cleaning (simulate + aggregate)
cat("\n--- Cleaning / simulate synthetic data ---\n")
source(here::here("code","R code","cleaning.R"))
rm(list = ls())

# 2. Figures: prompt distribution, reproduction and errors
cat("\n--- Figures: Reproduction rates ---\n")
source(here::here("code","R code","reproduction rates.R"))
rm(list = ls())

cat("\n--- Figures: Prompt distribution ---\n")
source(here::here("code","R code","prompt distribution.R"))
rm(list = ls())

cat("\n--- Tables: Error shares ---\n")
source(here::here("code","R code","error shares.R"))
rm(list = ls())

# New: time-to-first figures
cat("\n--- Figures: Time to first events ---\n")
source(here::here("code","R code","time to first.R"))
rm(list = ls())

# New: text-derived figures (wordcloud and markov-like heatmaps)
cat("\n--- Figures: Text corpus visualizations ---\n")
source(here::here("code","R code","text figures.R"))
rm(list = ls())

# New: synthetic tables to mirror reference outputs
cat("\n--- Tables: Synthetic set to mirror reference ---\n")
source(here::here("code","R code","tables_synthetic.R"))
rm(list = ls())

# New: PAP-compliant analyses (individual-level)
cat("\n--- PAP analyses: main and secondary ---\n")
source(here::here("code","R code","pap_analyses.R"))
rm(list = ls())

cat("\n=== MASTER LOG END ===\n")
sink()
