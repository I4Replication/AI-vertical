AI Vertical (Synthetic Individual-Level) — Analysis Pipeline

This project implements an individual‑level analysis pipeline aligned with the PAP “Reproducing with AI Across the Expertise Ladder.” The unit of observation is the individual. We simulate individual data and analyze treatment effects of ChatGPT+ on outcomes and gaps across expertise tiers; all outputs, tables, and figures are computed at the individual level. X‑axis labels for event‑indexed figures now use generic “Game 1–5” ticks to reflect the five‑game design in the PAP.


## Overview
- Purpose: Implement PAP-specified analyses for individual participants (no teams).
- Data model: Individuals are simulated across events, tiers, and software; analyses use these individual records directly.
- Reproducibility: A single master script orchestrates all steps; configuration lives in `config/config.yml` for easy tuning.


## Repository Layout
- `code/R code/`
  - `master.R`: Orchestrates the pipeline and logging.
  - `cleaning.R`: Simulates individual‑level data and saves `AI individuals.rds`.
  - `prompt distribution.R`: Densities of prompts/files/images/words (AI arm, individual-level).
  - `text figures.R`: Word frequency “cloud” and Markov heatmaps from `data/corpus`.
  - `pap_analyses.R`: Main and secondary regressions specified in the PAP, plus KM plot.
- `R/`
  - `simulate_individuals.R`: Synthetic individual generator (and legacy aggregation helpers, unused by default).
  - `reproduction rates.R`: Generates reproduction and error figures used as mock‑ups in the PAP.
- `config/`
  - `config.yml`: Parameters that govern the simulation and labeling.
- `data/`
  - `AI individuals.rds`: Generated individual‑level dataset used by all analyses.
  - `corpus/`: Optional `.txt` files used to build text‑based figures.
- `output/`
  - `figures/`: Generated figures (PDF/PNG).
  - `tables/`: Generated LaTeX tables for the PAP.
  - `master_log_R.log`: R pipeline log.
  - `master_log_stata.log`: Placeholder for historical parity only.
- `tests/`: Placeholder for future tests.


## How To Run
1. Open a terminal at the project root.
2. Run: `Rscript "code/R code/master.R"`
3. Outputs appear under `output/figures` and `output/tables`; a log is written to `output/master_log_R.log`.

Notes
- The master script uses `pacman::p_load(...)` to install/load required packages automatically if missing.
- If you prefer, open an interactive R session and pre‑install packages listed under “Dependencies”.


## Quick Start (copy/paste)

From the shell:

```
Rscript "code/R code/master.R"
```

From an interactive R session:

```
# Optionally install pacman for convenient loading
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Run the master pipeline (assumes working directory is project root)
source("code/R code/master.R")

# Outputs will be in output/figures and output/tables
```

Regenerate outputs from a clean slate (optional):

```
unlink("output/figures", recursive = TRUE, force = TRUE)
unlink("output/tables",  recursive = TRUE, force = TRUE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
Rscript "code/R code/master.R"
```


## Dependencies
Core R packages (loaded via `pacman::p_load` in `master.R`):
- Project utilities: `here`, `yaml`, `glue`
- Data wrangling: `dplyr`, `tidyr`, `stringr`, `forcats`, `janitor`, `lubridate`, `tibble`, `purrr`
- Modeling & stats: `fixest`, `sandwich`, `lmtest`, `car`, `margins`, `multcomp`, `broom`
- Tables & reporting: `kableExtra`, `modelsummary`, `xtable`
- Survival (RMST compatibility): `ggsurvfit`, `survRM2`
- Visualization: `ggplot2`, `patchwork`

The scripts will run with a recent R (≥ 4.2 recommended) and recent package versions. Some ggplot2 warnings about removed rows (due to missing values at aggregation) are expected and harmless.


## Configuration (`config/config.yml`)
Key parameters you can tune:
- `seed`: RNG seed for reproducibility.
- Categorical levels: `softwares`, `skills`, `gpt_levels`, `coding_levels`, `games`, and simulated tiers.
- Prompt intensity: `prompts_intensity_mean`, `prompts_intensity_sd` (drives prompts/files/images/words at the individual level).
- Error and reproduction process: `error_minor_rate`, `error_major_rate`, `reproduction_base_prob`, compression parameters.
- Time modeling (minutes): `start_time_mean`, `start_time_sd` for minutes‑to‑success and related timing.

Edit `config.yml` to change data characteristics, then re‑run the master script.


## PAP Document
- Source: `pre_analysis_plan.Rmd` (renders to `pre_analysis_plan.pdf`).
- Rendering: Knit in RStudio or run `rmarkdown::render('pre_analysis_plan.Rmd')` from R.
- Figures: The Rmd includes existing mock‑ups from `output/figures`; it creates sanitized copies in `output/figures_sanitized` for LaTeX.
- Labels: Event‑indexed figures use generic `Game 1` … `Game 5` ticks (non‑mapped events are omitted).


## Data Flow
1. Simulation (`cleaning.R` + `R/simulate_individuals.R`)
   - Simulates individual records across events, tiers, and software.
   - Outcomes: prompts/files/images/words; success in reproduction; minor/major errors; minutes‑to‑success (censored); robustness and clarity.

2. Figures and tables
   - Prompt Distribution (`prompt distribution.R`): densities for prompts/files/images/words (AI arm, individual-level).
   - Text Figures (`text figures.R`): top word bar chart; word/bigram transition heatmaps (uses `data/corpus`).
   - PAP Analyses (`pap_analyses.R`):
     - OLS (success, minutes) and Poisson (minor/major) with event/article FE; SEs clustered by event×software.
     - Years-of-coding interaction (linear main; quadratic appendix).
     - Within-AI usage models (prompts/files/images/words), including tier-split appendices.
     - Kaplan–Meier survival plot and log‑rank summary.

3. Logging
   - R log: `output/master_log_R.log`.
   - Stata log: `output/master_log_stata.log` is kept only for historical parity.


## Outputs Produced
- Figures (`output/figures/`):
  - `reproduction rates (raw).pdf`, `major errors (raw).pdf`, `minor errors (raw).pdf` (levels, by branch)
  - `prompt distribution.pdf`
  - `wordcloud_focus_groups.png`, `markov_words_focus_groups.png`, `markov_bigrams_focus_groups.png`
  - `pap_km_success.pdf`
- Tables (`output/tables/`):
  - `pap_main.tex`, `pap_secondary_years.tex`, `pap_secondary_years_alt.tex`, `pap_secondary_learning.tex`
  - `pap_secondary_usage_all.tex`, `pap_usage_success_by_tier.tex`, `pap_usage_minutes_by_tier.tex`
  - `pap_robustness_clarity.tex`, `pap_robustness_clarity_appendix.tex`, `pap_robustness_clarity_both.tex`
- Logs: `master_log_R.log`, `master_log_stata.log` (placeholder)


## Figure Previews

These PNGs render directly in most viewers; PDF figures are in `output/figures`.

![Wordcloud](output/figures/wordcloud_focus_groups.png)

![Word transitions](output/figures/markov_words_focus_groups.png)

![Bigram transitions](output/figures/markov_bigrams_focus_groups.png)


## Customizing & Extending
- Tune `config/config.yml` to change distributions, sizes, and label sets, then re‑run the master script.
- Replace or add `.txt` files in `data/corpus` to alter the text‑based figures.
- Swap in a real individual‑level dataset: adapt `simulate_individuals.R` functions to read your data and keep the aggregation schema stable so downstream scripts remain unchanged.
- Add new outputs: follow the pattern in `master.R` to source additional scripts and save to `output/figures` or `output/tables` as needed.


## Troubleshooting
- Package install prompts: the scripts try to install missing packages; if behind a proxy/firewall, pre‑install packages in an interactive R session.
- ggplot2 warnings about “Removed rows”: expected when some games/branches lack values due to simulation or S1 filtering; visuals remain valid.
- File permissions: ensure the project has write access to `output/`.
- Stata outputs: only a placeholder log is produced; no `.do` files are run in this project.


## Provenance
- This project implements an individual‑level PAP. Earlier references to team-level outputs have been removed.
- X‑axis labels on event‑indexed figures updated to generic “Game 1–5” to match the preregistered scope.

## PAP Outputs Index

Main tables (PAP):
- `output/tables/pap_main.tex`: OLS (success, minutes) and Poisson (minor/major) with FE and clustered SEs.
- `output/tables/pap_secondary_years.tex`: Linear dose-response (years of coding × treatment).
- `output/tables/pap_secondary_learning.tex`: Learning (treatment × event order).

Secondary and appendix:
- `output/tables/pap_secondary_usage_all.tex`: Within-AI usage regressions including prompts, files, images, words.
- `output/tables/pap_usage_success_by_tier.tex`: Within-AI usage → success, separate models per tier.
- `output/tables/pap_usage_minutes_by_tier.tex`: Within-AI usage → minutes, separate models per tier.
- `output/tables/pap_robustness_clarity.tex`: Robustness and clarity outcomes (with FE).
- `output/tables/pap_robustness_clarity_appendix.tex`: Robustness and clarity (no FE) specification check.
- `output/tables/pap_robustness_clarity_both.tex`: Combined table with FE and no-FE columns side-by-side.
- `output/tables/pap_secondary_years_alt.tex`: Years of coding with quadratic term (appendix).

Figures (PAP):
- `output/figures/pap_km_success.pdf`: Kaplan–Meier survival by treatment (minutes to success).
