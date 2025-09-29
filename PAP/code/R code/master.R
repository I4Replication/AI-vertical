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

# 1. Cleaning (simulate individuals)
cat("\n--- Cleaning / simulate synthetic data (individual-level) ---\n")
source(here::here("code","R code","cleaning.R"))
rm(list = ls())

# 2. Individual-level figures
cat("\n--- Figures: Prompt usage distributions (AI arm) ---\n")
source(here::here("code","R code","prompt distribution.R"))
rm(list = ls())

cat("\n--- Figures: Text corpus visualizations ---\n")
source(here::here("code","R code","text figures.R"))
rm(list = ls())

# 3. PAP-compliant analyses (individual-level)
cat("\n--- PAP analyses: main, secondary, appendix ---\n")
source(here::here("code","R code","pap_analyses.R"))
rm(list = ls())

cat("\n=== MASTER LOG END ===\n")
sink()
