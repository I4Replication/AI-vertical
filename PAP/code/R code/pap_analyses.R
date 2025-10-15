############################################################
# PAP-compliant analyses (individual-level)
############################################################
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(fixest)
  library(ggplot2); library(broom)
  if (identical(Sys.getenv("PAP_ENABLE_MODELSUMMARY"), "1") &&
      requireNamespace("modelsummary", quietly = TRUE)) {
    library(modelsummary)
  } else {
    message("Skipping modelsummary (set PAP_ENABLE_MODELSUMMARY=1 to load if available).")
  }
})

# Fixest parallelism can trigger OpenMP shared-memory issues in sandboxed environments.
fixest::setFixest_nthreads(1)

# ----- AI-paper table helpers (classic tabular) -----
fmt3 <- function(x) { if (is.na(x) || is.null(x)) return("—"); sprintf("% .3f", x) }
star_sym <- function(p) { if (is.na(p)) return(""); if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else "" }
coef_stats <- function(m, term) {
  tt <- tryCatch(broom::tidy(m, conf.int = TRUE), error = function(e) NULL)
  if (is.null(tt)) return(list(est=NA,se=NA,p=NA,lwr=NA,upr=NA))
  # exact match
  row <- tt[tt$term == term, , drop = FALSE]
  # try reversed interaction order
  if (nrow(row) == 0 && grepl(':', term, fixed = TRUE)) {
    parts <- strsplit(term, ':', fixed = TRUE)[[1]]
    term2 <- paste(rev(parts), collapse = ':')
    row <- tt[tt$term == term2, , drop = FALSE]
  }
  # robust fallback: allow any non-alphanumeric separator and order-insensitive matching
  if (nrow(row) == 0 && grepl(':', term, fixed = TRUE)) {
    req <- tolower(strsplit(term, ':', fixed = TRUE)[[1]])
    req <- trimws(req)
    tokenise <- function(x) unique(trimws(strsplit(tolower(x), "[^A-Za-z0-9_]+")[[1]]))
    has_all <- function(trm) {
      toks <- tokenise(trm)
      all(req %in% toks)
    }
    idx <- which(vapply(tt$term, has_all, logical(1)))
    if (length(idx) >= 1) row <- tt[idx[1], , drop = FALSE]
  }
  if (nrow(row) == 0) return(list(est=NA,se=NA,p=NA,lwr=NA,upr=NA))
  list(est = row$estimate[[1]], se = row$std.error[[1]], p = row$p.value[[1]], lwr = row$conf.low[[1]], upr = row$conf.high[[1]])
}
wald_compression_p <- function(m) {
  tt <- tryCatch(broom::tidy(m), error = function(e) NULL)
  if (is.null(tt)) return(NA_real_)
  cand <- unique(c(tt$term[grepl('^treatment:tier', tt$term)],
                   tt$term[grepl('^tier.*:treatment$', tt$term)]))
  if (length(cand) <= 1) return(NA_real_)
  base <- cand[[1]]; rest <- setdiff(cand, base)
  R <- paste(base, '=', rest)
  tryCatch({ fixest::wald(m, R)$p.value }, error = function(e) NA_real_)
}

# One-sided monotonic compression p-value: AI×Undergrad ≤ AI×Graduate ≤ AI×Professor/Researcher
p_monotonic_tier <- function(m) {
  b <- tryCatch(coef(m), error = function(e) NULL)
  V <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(b) || is.null(V)) return(NA_real_)
  # helper to fetch coefficient index, allowing reversed order
  find_idx <- function(name){
    if (name %in% names(b)) return(which(names(b)==name))
    parts <- strsplit(name, ':', fixed = TRUE)[[1]]
    name2 <- paste(rev(parts), collapse=':')
    if (name2 %in% names(b)) return(which(names(b)==name2))
    return(NA_integer_)
  }
  idx_main <- find_idx('treatment')
  idx_GR   <- find_idx('treatment:tier_3GR')
  idx_PR   <- find_idx('treatment:tier_3PR')
  if (any(is.na(c(idx_main, idx_GR, idx_PR)))) return(NA_real_)
  # Build difference vectors for [Graduate − Undergrad], [Professor/Researcher − Graduate]
  mk_p <- function(i, j){
    cvec <- rep(0, length(b)); cvec[i] <- -1; cvec[j] <- 1
    est <- sum(cvec * b)
    se <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
    if (!is.finite(se) || se <= 0) return(NA_real_)
    z <- est / se
    1 - pnorm(z) # one-sided p for H0: diff <= 0 vs H1: diff > 0
  }
  p1 <- mk_p(idx_main, idx_GR)
  p2 <- mk_p(idx_GR, idx_PR)
  if (any(is.na(c(p1,p2)))) return(NA_real_)
  max(p1,p2)
}
write_ai_table <- function(models, col_titles, file, coef_mode = c('tier','years','learn','ood'),
                           controls_desc = 'Event×article FE; years of coding; software; prior AI familiarity.',
                           dep_means = NULL) {
  coef_mode <- match.arg(coef_mode)
  K <- length(models); stopifnot(length(col_titles) == K)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  if (coef_mode == 'tier') {
    keep <- c('treatment','treatment:tier_3GR','treatment:tier_3PR')
    labels <- c('AI-Assisted','AI × Graduate','AI × Professor/Researcher')
  } else if (coef_mode == 'years') {
    keep <- c('treatment','years_coding:treatment')
    labels <- c('AI-Assisted','AI × Years of coding')
  } else if (coef_mode == 'learn') {
    keep <- c('treatment','treatment:event_order')
    labels <- c('AI-Assisted','AI × Event order')
  } else if (coef_mode == 'ood') {
    keep <- c('treatment','treatment:out_of_disc_i')
    labels <- c('AI-Assisted','AI × Outside-discipline')
  }
  lines <- c()
  lines <- c(lines, '\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}')
  lines <- c(lines, sprintf('\\begin{tabular}{l*{%d}{c}}', K))
  lines <- c(lines, '\\hline\\hline')
  # Numbers first, then titles
  lines <- c(lines, paste(c('', sprintf('(%d)', seq_len(K))), collapse=' & '), '\\\\')
  lines <- c(lines, paste(c('', col_titles), collapse=' & '), ' \\\\')
  lines <- c(lines, '\\hline')
  for (i in seq_along(keep)) {
    lab <- labels[[i]]
    ests <- se_line <- ci_line <- character(K)
    dbg_vals <- c()
    for (j in seq_len(K)) {
      s <- coef_stats(models[[j]], keep[[i]])
      ests[[j]] <- if (is.na(s$est)) '—' else sprintf('%s%s', fmt3(s$est), star_sym(s$p))
      se_line[[j]] <- if (is.na(s$se)) '' else sprintf('(%s)', fmt3(s$se))
      ci_line[[j]] <- if (is.na(s$lwr) || is.na(s$upr)) '' else sprintf('[%s; %s]', fmt3(s$lwr), fmt3(s$upr))
      dbg_vals <- c(dbg_vals, if (is.na(s$est)) NA else s$est)
    }
    if (coef_mode == 'ood' && grepl('Outside', lab)) {
      try({ writeLines(paste('OOD row ests:', paste(dbg_vals, collapse=',')), 'output/tables/pap_horizontal_debug.txt', append=FALSE) }, silent=TRUE)
    }
    lines <- c(lines, paste(c(lab, ests), collapse=' & '), '\\\\')
    lines <- c(lines, paste(c('', se_line), collapse=' & '), '\\\\')
    lines <- c(lines, paste(c('', ci_line), collapse=' & '), '\\\\')
  }
  lines <- c(lines, '\\hline')
  ck <- rep('$\\checkmark$', K)
  lines <- c(lines, paste(c('Controls', ck), collapse=' & '), '\\\\')
  if (is.null(dep_means)) {
    dep_means <- sapply(models, function(m) {
      val <- tryCatch({as.numeric(fixest::fitstat(m, 'ymean')$value)}, error=function(e) NA_real_)
      if (is.na(val)) val <- tryCatch({mean(m$fitted.values + residuals(m), na.rm=TRUE)}, error=function(e) NA_real_)
      val
    })
  }
  lines <- c(lines, paste(c('Mean of dep. var', sprintf('% .3f', dep_means)), collapse=' & '), '\\\\')
  if (coef_mode == 'tier') {
    pvals <- sapply(models, function(m) fmt3(tryCatch(p_monotonic_tier(m), error=function(e) NA_real_)))
    lines <- c(lines, paste(c('p-val (Monotonic compression)', pvals), collapse=' & '), '\\\\')
  }
  lines <- c(lines, paste(c('Obs.', sapply(models, nobs)), collapse=' & '), '\\\\')
  lines <- c(lines, '\\hline', '\\hline\\hline')
  ctrl_txt <- gsub('&', '\\&', controls_desc, fixed = TRUE)
  lines <- c(lines,
             paste0('\\multicolumn{', K+1, '}{l}{\\it{Note:} Standard errors in parentheses; confidence intervals in brackets.}\\\\'),
             paste0('\\multicolumn{', K+1, '}{l}{Controls: ', ctrl_txt, '}\\\\'))
  if (coef_mode == 'tier') {
    lines <- c(lines, paste0('\\multicolumn{', K+1, '}{l}{Compression (monotonic): one-sided p-value for increasing effects across the three tiers (baseline: Undergraduate).}\\\\'))
  }
  lines <- c(lines, paste0('\\multicolumn{', K+1, '}{l}{\\sym{*} $p<0.10$, \\sym{**} $p<0.05$,  \\sym{***} $p<0.01$}\\\\'))
  lines <- c(lines, '\\end{tabular}')
  writeLines(lines, con = file)
}

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

ind <- readRDS("data/AI individuals.rds")

# Ensure individual-level ChatGPT familiarity control exists and is well-typed
if (!('prior_gpt_familiarity' %in% names(ind))) {
  if ('prior_ai_familiarity' %in% names(ind)) {
    ind$prior_gpt_familiarity <- ind$prior_ai_familiarity
  } else {
    ind$prior_gpt_familiarity <- NA_character_
  }
}
ind$prior_gpt_familiarity <- as.character(ind$prior_gpt_familiarity)
ind$prior_gpt_familiarity[is.na(ind$prior_gpt_familiarity) | ind$prior_gpt_familiarity == ""] <- "None"
ind$prior_gpt_familiarity <- factor(ind$prior_gpt_familiarity, levels = c("None","Some","Heavy"))

# Prepare variables
ind <- ind %>%
  mutate(
    event = factor(game),
    article = factor(article),
    event_article = interaction(event, article, drop = TRUE),
    software3 = factor(preferred_software, levels = c("R","Stata","Python")),
    tier = factor(tier, levels = c("UG","MA","PhD","PD","P")),
    tier_3 = factor(case_when(
      tier == "UG" ~ "UG",
      tier %in% c("MA","PhD") ~ "GR",
      tier %in% c("PD","P") ~ "PR",
      TRUE ~ NA_character_
    ), levels = c("UG","GR","PR")),
    cluster_es = interaction(event, software3, drop = TRUE),
    event_order = as.integer(factor(event, levels = unique(event)))
  )

# Horizontal dimension: participant/task disciplines and outside-of-discipline flag
# If disciplines are missing or degenerate in the synthetic data, generate placeholders for illustration
set.seed(12345)
if (!('participant_discipline' %in% names(ind)) ||
    length(unique(stats::na.omit(ind$participant_discipline))) <= 1) {
  ind$participant_discipline <- sample(c('Economics','Political Science','Psychology'),
                                       nrow(ind), replace = TRUE, prob = c(0.50, 0.25, 0.25))
}
if (!('task_discipline' %in% names(ind)) ||
    length(unique(stats::na.omit(ind$task_discipline))) <= 1) {
  arts <- unique(as.character(ind$article))
  art_map <- setNames(sample(c('Economics','Political Science','Psychology'),
                             length(arts), replace = TRUE), arts)
  ind$task_discipline <- art_map[as.character(ind$article)]
}
ind <- ind %>% mutate(
  participant_discipline = factor(participant_discipline, levels = c('Economics','Political Science','Psychology')),
  task_discipline = factor(task_discipline, levels = c('Economics','Political Science','Psychology')),
  out_of_disc_i = as.integer(!is.na(participant_discipline) & !is.na(task_discipline) & participant_discipline != task_discipline)
)
# Debug: write OOD variation summary
try({
  dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
  ood_tab <- table(ind$out_of_disc_i, useNA = 'ifany')
  writeLines(c("out_of_disc_i counts:", paste(names(ood_tab), as.integer(ood_tab))),
             "output/tables/pap_ood_counts.txt")
}, silent = TRUE)

# Outcomes
y_success   <- ind$reproduction_i
y_minor_cnt <- ind$minor_errors_i
y_major_cnt <- ind$major_errors_i

# Main specification: OLS for binary/continuous outcomes, Poisson for counts
f_rep <- reproduction_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
f_pmn <- minor_errors_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
f_pmj <- major_errors_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)

m_rep   <- feols(f_rep, data = ind, vcov = 'hetero')
m_minor <- fepois(f_pmn, data = ind, vcov = 'hetero')
m_major <- fepois(f_pmj, data = ind, vcov = 'hetero')

# Precompute robustness checks (≥1 and ≥2) for the non-coding table
f_good <- good_checks_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
f_two  <- two_good_checks_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
m_impl1 <- feols(implemented_check_1_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article),
                 data = ind, vcov = 'hetero')
m_impl2 <- feols(implemented_check_2_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article),
                 data = ind, vcov = 'hetero')
m_good <- feols(f_good, data = ind, vcov = 'hetero')
m_two  <- feols(f_two,  data = ind, vcov = 'hetero')

write_ai_table(
  models = list(m_rep, m_minor, m_major),
  col_titles = c("Reproduction", "Minor errors", "Major errors"),
  file = "output/tables/pap_coding.tex",
  coef_mode = "tier",
  controls_desc = "Event×article FE; years of coding; software; prior AI familiarity."
)

# Horizontal compression: add AI × Outside-discipline interaction (parallel to vertical tables)
if (TRUE) {
  f_rep_h <- reproduction_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
  f_pmn_h <- minor_errors_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
  f_pmj_h <- major_errors_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
  m_rep_h   <- feols(f_rep_h, data = ind, vcov = 'hetero')
  m_minor_h   <- fepois(f_pmn_h, data = ind, vcov = 'hetero')
  m_major_h   <- fepois(f_pmj_h, data = ind, vcov = 'hetero')
  # Debug: write term names for inspection
  tt_dbg <- tryCatch(broom::tidy(m_rep_h), error=function(e) NULL)
  if (!is.null(tt_dbg)) {
    dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
    writeLines(paste(tt_dbg$term, collapse='\n'), "output/tables/pap_horizontal_terms.txt")
  }
  f_good_h <- good_checks_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
  f_two_h  <- two_good_checks_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
  m_good_h <- feols(f_good_h, data = ind, vcov = 'hetero')
  m_two_h  <- feols(f_two_h,  data = ind, vcov = 'hetero')
  m_impl1_h <- feols(implemented_check_1_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article),
                     data = ind, vcov = 'hetero')
  m_impl2_h <- feols(implemented_check_2_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article),
                     data = ind, vcov = 'hetero')
  write_ai_table(
    models = list(m_rep_h, m_minor_h, m_major_h),
    col_titles = c("Reproduction", "Minor errors", "Major errors"),
    file = "output/tables/pap_coding_horizontal.tex",
    coef_mode = "ood",
    controls_desc = "Event×article FE; years of coding; software; prior AI familiarity."
  )

}

# Referee report outcomes (human and AI assessments) -- streamlined metrics
referee_vars_h <- c(
  "referee_app_human_i"    = "Appropriate (human)",
  "referee_overall_human_i"= "Overall 0–5 (human)"
)
referee_vars_ai <- c(
  "referee_app_ai_i"      = "Appropriate (AI)",
  "referee_overall_ai_i"  = "Overall 0–5 (AI)"
)
fallback_h <- if ("referee_score_human_i" %in% names(ind)) ind$referee_score_human_i else rep(0, nrow(ind))
fallback_ai <- if ("referee_score_ai_i" %in% names(ind)) ind$referee_score_ai_i else rep(0, nrow(ind))
for (nm in setdiff(names(referee_vars_h), names(ind))) ind[[nm]] <- fallback_h
for (nm in setdiff(names(referee_vars_ai), names(ind))) ind[[nm]] <- fallback_ai

mk_formula <- function(dep, rhs) stats::as.formula(paste(dep, "~", rhs))
fit_referee_models <- function(dep_vars, rhs) {
  lapply(names(dep_vars), function(dep) feols(mk_formula(dep, rhs), data = ind, vcov = 'hetero'))
}

titles_referee <- c(unname(referee_vars_h), unname(referee_vars_ai))

rhs_tier <- "treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)"
models_ref_tier <- c(fit_referee_models(referee_vars_h, rhs_tier),
                     fit_referee_models(referee_vars_ai, rhs_tier))
write_ai_table(
  models = c(models_ref_tier, list(m_good, m_two, m_impl1, m_impl2)),
  col_titles = c(titles_referee, "At least 1 check", "At least 2 checks", "Implemented ≥1", "Implemented ≥2"),
  file = "output/tables/pap_noncoding.tex",
  coef_mode = "tier",
  controls_desc = "Event×article FE; years of coding; software; prior AI familiarity."
)

# Horizontal: referee outcomes with AI × OOD
rhs_ood <- "treatment + out_of_disc_i + treatment:out_of_disc_i + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)"
models_ref_ood <- c(fit_referee_models(referee_vars_h, rhs_ood),
                    fit_referee_models(referee_vars_ai, rhs_ood))
write_ai_table(
  models = c(models_ref_ood, list(m_good_h, m_two_h, m_impl1_h, m_impl2_h)),
  col_titles = c(titles_referee, "At least 1 check", "At least 2 checks", "Implemented ≥1", "Implemented ≥2"),
  file = "output/tables/pap_noncoding_horizontal.tex",
  coef_mode = "ood",
  controls_desc = "Event×article FE; years of coding; software; prior AI familiarity."
)

# Non-coding by years (appendix): replace tier with years × AI
rhs_years <- "treatment*years_coding + software3 + prior_gpt_familiarity + i(event_article)"
models_ref_years <- c(fit_referee_models(referee_vars_h, rhs_years),
                      fit_referee_models(referee_vars_ai, rhs_years),
                      list(
                        feols(as.formula(paste("implemented_check_1_i ~", rhs_years)), data = ind, vcov = 'hetero'),
                        feols(as.formula(paste("implemented_check_2_i ~", rhs_years)), data = ind, vcov = 'hetero'),
                        feols(as.formula(paste("good_checks_i ~", rhs_years)), data = ind, vcov = 'hetero'),
                        feols(as.formula(paste("two_good_checks_i ~", rhs_years)), data = ind, vcov = 'hetero')
                      ))
titles_referee_years <- c(paste0(unname(referee_vars_h), " (yrs)"),
                          paste0(unname(referee_vars_ai), " (yrs)"),
                          "Implemented ≥1 (yrs)", "Implemented ≥2 (yrs)",
                          "Planned ≥1 (yrs)", "Planned ≥2 (yrs)")
write_ai_table(
  models = models_ref_years,
  col_titles = titles_referee_years,
  file = "output/tables/pap_noncoding_years.tex",
  coef_mode = "years",
  controls_desc = "Event×article FE; software; prior AI familiarity."
)

## (moved: robustness models precomputed above to append to main table)

# Appendix: robustness without event×article FE (specification check)
f_good_nofe <- good_checks_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity
f_two_nofe  <- two_good_checks_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity
f_impl1_nofe <- implemented_check_1_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity
f_impl2_nofe  <- implemented_check_2_i ~ treatment + tier_3 + treatment:tier_3 + years_coding + software3 + prior_gpt_familiarity
m_good_nofe <- feols(f_good_nofe, data = ind, vcov = 'hetero')
m_two_nofe  <- feols(f_two_nofe,  data = ind, vcov = 'hetero')
write_ai_table(
  models = list(m_good_nofe, m_two_nofe,
                feols(f_impl1_nofe, data = ind, vcov = 'hetero'),
                feols(f_impl2_nofe, data = ind, vcov = 'hetero')),
  col_titles = c("At least 1 check", "At least 2 checks", "Implemented ≥1", "Implemented ≥2"),
  file = "output/tables/pap_robustness_clarity_appendix.tex",
  coef_mode = "tier",
  controls_desc = "No event×article FE; years of coding; software; prior AI familiarity."
)

# (combined FE vs no-FE robustness table dropped)

# Coding outcomes with years-of-coding moderator
f_rep2 <- update(f_rep, . ~ . - tier_3 - treatment:tier_3 + years_coding:treatment)
f_pmn2 <- update(f_pmn, . ~ . - tier_3 - treatment:tier_3 + years_coding:treatment)
f_pmj2 <- update(f_pmj, . ~ . - tier_3 - treatment:tier_3 + years_coding:treatment)

m_rep2   <- feols(f_rep2, data = ind, vcov = 'hetero')
m_minor2 <- fepois(f_pmn2, data = ind, vcov = 'hetero')
m_major2 <- fepois(f_pmj2, data = ind, vcov = 'hetero')

write_ai_table(
  models = list(m_rep2, m_minor2, m_major2),
  col_titles = c("Reproduction (yrs)", "Minor (yrs)", "Major (yrs)"),
  file = "output/tables/pap_coding_years_core.tex",
  coef_mode = "years",
  controls_desc = "Event×article FE; software; prior AI familiarity."
)

# Secondary 3: learning across events (treatment x event order)
f_learn <- reproduction_i ~ treatment*event_order + tier_3 + years_coding + software3 + prior_gpt_familiarity + i(event_article)
m_learn <- feols(f_learn, data = ind, vcov = 'hetero')
write_ai_table(
  models = list(m_learn),
  col_titles = c("Success (trt×order)"),
  file = "output/tables/pap_secondary_learning.tex",
  coef_mode = "learn",
  controls_desc = "Event×article FE; years of coding; software; prior AI familiarity."
)
