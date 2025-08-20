############################################################
# PAP-compliant analyses (individual-level)
############################################################
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(fixest)
  library(ggplot2); library(ggsurvfit); library(survival); library(modelsummary)
})

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

ind <- readRDS("data/AI individuals.rds")

# Prepare variables
ind <- ind %>%
  mutate(
    event = factor(game),
    article = factor(article),
    software3 = factor(preferred_software, levels = c("R","Stata","Python")),
    tier = factor(tier, levels = c("UG","MA","PhD","PD","P")),
    cluster_es = interaction(event, software3, drop = TRUE),
    event_order = as.integer(factor(event, levels = unique(event)))
  )

# Outcomes
y_success   <- ind$reproduction_i
y_minutes   <- ind$minutes_to_success_i
y_minor_cnt <- ind$minor_errors_i
y_major_cnt <- ind$major_errors_i

# Main specification: OLS for binary/time, Poisson for counts
f_ols <- reproduction_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_min <- minutes_to_success_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_pmn <- minor_errors_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_pmj <- major_errors_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)

m_success <- feols(f_ols, data = ind, vcov = ~cluster_es)
m_minutes <- feols(f_min, data = ind, vcov = ~cluster_es)
m_minor   <- fepois(f_pmn, data = ind, vcov = ~cluster_es)
m_major   <- fepois(f_pmj, data = ind, vcov = ~cluster_es)

msummary(list("Success (OLS)" = m_success,
              "Minutes (OLS)" = m_minutes,
              "Minor errors (Poisson)" = m_minor,
              "Major errors (Poisson)" = m_major),
         output = "output/tables/pap_main.tex")

# Robustness and clarity outcomes (OLS per PAP for binary & continuous)
f_good <- good_checks_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_two  <- two_good_checks_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_ran  <- ran_any_check_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_clar <- clarity_score_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)

m_good <- feols(f_good, data = ind, vcov = ~cluster_es)
m_two  <- feols(f_two,  data = ind, vcov = ~cluster_es)
m_ran  <- feols(f_ran,  data = ind, vcov = ~cluster_es)
m_clar <- feols(f_clar, data = ind, vcov = ~cluster_es)

msummary(list("Good check (>=1)" = m_good,
              ">=2 good checks" = m_two,
              "Ran any check" = m_ran,
              "Clarity score" = m_clar),
         output = "output/tables/pap_robustness_clarity.tex")

# Appendix: robustness & clarity without event/article FE (specification check)
f_good_nofe <- good_checks_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity
f_two_nofe  <- two_good_checks_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity
f_ran_nofe  <- ran_any_check_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity
f_clar_nofe <- clarity_score_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity
m_good_nofe <- feols(f_good_nofe, data = ind, vcov = ~cluster_es)
m_two_nofe  <- feols(f_two_nofe,  data = ind, vcov = ~cluster_es)
m_ran_nofe  <- feols(f_ran_nofe,  data = ind, vcov = ~cluster_es)
m_clar_nofe <- feols(f_clar_nofe, data = ind, vcov = ~cluster_es)
msummary(list("Good check (no FE)" = m_good_nofe,
              ">=2 good checks (no FE)" = m_two_nofe,
              "Ran any check (no FE)" = m_ran_nofe,
              "Clarity score (no FE)" = m_clar_nofe),
         output = "output/tables/pap_robustness_clarity_appendix.tex")

# Combined table: FE vs no-FE side-by-side in one table
msummary(list(
  "Good (FE)" = m_good, "Good (no FE)" = m_good_nofe,
  ">=2 (FE)" = m_two, ">=2 (no FE)" = m_two_nofe,
  "Ran any (FE)" = m_ran, "Ran any (no FE)" = m_ran_nofe,
  "Clarity (FE)" = m_clar, "Clarity (no FE)" = m_clar_nofe
),
  output = "output/tables/pap_robustness_clarity_both.tex")

# Secondary 1: replace tier with continuous years of coding
f_ols2 <- update(f_ols, . ~ . - tier - treatment:tier + years_coding:treatment)
f_min2 <- update(f_min, . ~ . - tier - treatment:tier + years_coding:treatment)
f_pmn2 <- update(f_pmn, . ~ . - tier - treatment:tier + years_coding:treatment)
f_pmj2 <- update(f_pmj, . ~ . - tier - treatment:tier + years_coding:treatment)

m_success2 <- feols(f_ols2, data = ind, vcov = ~cluster_es)
m_minutes2 <- feols(f_min2, data = ind, vcov = ~cluster_es)
m_minor2   <- fepois(f_pmn2, data = ind, vcov = ~cluster_es)
m_major2   <- fepois(f_pmj2, data = ind, vcov = ~cluster_es)

msummary(list("Success (yrs x trt)" = m_success2,
              "Minutes (yrs x trt)" = m_minutes2,
              "Minor (yrs x trt)" = m_minor2,
              "Major (yrs x trt)" = m_major2),
         output = "output/tables/pap_secondary_years.tex")

# Alternative specifications for years-of-coding (appendix)
f_ols2_q <- update(f_ols2, . ~ . + I(years_coding^2))
f_min2_q <- update(f_min2, . ~ . + I(years_coding^2))
f_pmn2_q <- update(f_pmn2, . ~ . + I(years_coding^2))
f_pmj2_q <- update(f_pmj2, . ~ . + I(years_coding^2))

m_success2_q <- feols(f_ols2_q, data = ind, vcov = ~cluster_es)
m_minutes2_q <- feols(f_min2_q, data = ind, vcov = ~cluster_es)
m_minor2_q   <- fepois(f_pmn2_q, data = ind, vcov = ~cluster_es)
m_major2_q   <- fepois(f_pmj2_q, data = ind, vcov = ~cluster_es)

msummary(list("Success (yrs+sq)" = m_success2_q,
              "Minutes (yrs+sq)" = m_minutes2_q,
              "Minor (yrs+sq)" = m_minor2_q,
              "Major (yrs+sq)" = m_major2_q),
         output = "output/tables/pap_secondary_years_alt.tex")

# Secondary 2: within AI arm, regress outcomes on prompts (use log(1+p))
ai <- ind %>% filter(treatment == 1)
# usage models including files, images, and words (and prompts for completeness)
f_ai_success <- reproduction_i ~ log1p(prompts_i) + log1p(files_i) + log1p(images_i) + log1p(words_i) + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_ai_minutes <- minutes_to_success_i ~ log1p(prompts_i) + log1p(files_i) + log1p(images_i) + log1p(words_i) + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
m_ai_success <- feols(f_ai_success, data = ai, vcov = ~cluster_es)
m_ai_minutes <- feols(f_ai_minutes, data = ai, vcov = ~cluster_es)
msummary(list("AI: Success ~ usage" = m_ai_success,
              "AI: Minutes ~ usage" = m_ai_minutes),
         output = "output/tables/pap_secondary_usage_all.tex")

# Appendix: usage effects within AI arm by tier (separate models per tier)
tiers <- c("UG","MA","PhD","PD","P")
mods_s <- list(); mods_m <- list()
for (tt in tiers) {
  dat <- ai %>% filter(tier == tt)
  if (nrow(dat) >= 5) {
    mods_s[[tt]] <- feols(reproduction_i ~ log1p(prompts_i) + log1p(files_i) + log1p(images_i) + log1p(words_i)
                           + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article),
                           data = dat, vcov = ~cluster_es)
    mods_m[[tt]] <- feols(minutes_to_success_i ~ log1p(prompts_i) + log1p(files_i) + log1p(images_i) + log1p(words_i)
                           + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article),
                           data = dat, vcov = ~cluster_es)
  }
}
mods_s <- Filter(Negate(is.null), mods_s)
mods_m <- Filter(Negate(is.null), mods_m)
if (length(mods_s) > 0) msummary(mods_s, output = "output/tables/pap_usage_success_by_tier.tex")
if (length(mods_m) > 0) msummary(mods_m, output = "output/tables/pap_usage_minutes_by_tier.tex")

# Secondary 3: learning across events (treatment x event order)
f_learn <- reproduction_i ~ treatment*event_order + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
m_learn <- feols(f_learn, data = ind, vcov = ~cluster_es)
msummary(list("Success ~ trt x order" = m_learn),
         output = "output/tables/pap_secondary_learning.tex")

# Kaplan–Meier survival curves (time to success)
survdat <- ind %>% transmute(time = minutes_to_success_i,
                             status = as.integer(reproduction_i==1),
                             treatment = factor(ifelse(treatment==1, "ChatGPT+", "Control"),
                                                levels = c("Control","ChatGPT+")))
fit <- survfit(Surv(time, status) ~ treatment, data = survdat)
g <- ggsurvfit(fit) +
  labs(x = "Minutes", y = "Survival (not yet reproduced)", color = "Arm",
       title = "Time to reproduction: Kaplan–Meier by arm") +
  theme_minimal(base_size = 13)
ggsave("output/figures/pap_km_success.pdf", g, width = 7, height = 4)

# Log-rank test
if (length(unique(survdat$treatment)) >= 2) {
  lr <- survdiff(Surv(time, status) ~ treatment, data = survdat)
  p_lr <- 1 - pchisq(lr$chisq, df = length(lr$n) - 1)
  writeLines(sprintf("Log-rank chi2 = %.3f, df = %d, p = %.4f", lr$chisq, length(lr$n)-1, p_lr),
             con = "output/tables/pap_km_logrank.txt")
} else {
  writeLines("Log-rank test unavailable (only one arm present).", con = "output/tables/pap_km_logrank.txt")
}
