############################################################
# PAP-compliant analyses (individual-level)
############################################################
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(fixest)
  library(ggplot2); library(ggsurvfit); library(survival); library(modelsummary)
  library(broom)
})

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

# One-sided monotonic compression p-value: AI×Professor < AI×Postdoc < AI×PhD < AI×Master's
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
  idx_P   <- find_idx('treatment:tierP')
  idx_PD  <- find_idx('treatment:tierPD')
  idx_PhD <- find_idx('treatment:tierPhD')
  idx_MA  <- find_idx('treatment:tierMA')
  if (any(is.na(c(idx_P, idx_PD, idx_PhD, idx_MA)))) return(NA_real_)
  # Build difference vectors for [PD-P], [PhD-PD], [MA-PhD]
  mk_p <- function(i, j){
    cvec <- rep(0, length(b)); cvec[i] <- -1; cvec[j] <- 1
    est <- sum(cvec * b)
    se <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
    if (!is.finite(se) || se <= 0) return(NA_real_)
    z <- est / se
    1 - pnorm(z) # one-sided p for H0: diff <= 0 vs H1: diff > 0
  }
  p1 <- mk_p(idx_P, idx_PD)
  p2 <- mk_p(idx_PD, idx_PhD)
  p3 <- mk_p(idx_PhD, idx_MA)
  if (any(is.na(c(p1,p2,p3)))) return(NA_real_)
  max(p1,p2,p3)
}
write_ai_table <- function(models, col_titles, file, coef_mode = c('tier','years','usage','learn','prompts','ood'),
                           controls_desc = 'Event & article FE; years of coding; software; prior AI familiarity.',
                           dep_means = NULL) {
  coef_mode <- match.arg(coef_mode)
  K <- length(models); stopifnot(length(col_titles) == K)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  if (coef_mode == 'tier') {
    keep <- c('treatment','treatment:tierMA','treatment:tierPhD','treatment:tierPD','treatment:tierP')
    labels <- c('AI-Assisted','AI × Master\'s','AI × PhD','AI × Postdoc','AI × Professor')
  } else if (coef_mode == 'years') {
    keep <- c('treatment','years_coding:treatment')
    labels <- c('AI-Assisted','AI × Years of coding')
  } else if (coef_mode == 'usage') {
    keep <- c('log1p(prompts_i)','log1p(files_i)','log1p(images_i)','log1p(words_i)')
    labels <- c('log(1+prompts)','log(1+files)','log(1+images)','log(1+words)')
  } else if (coef_mode == 'learn') {
    keep <- c('treatment','treatment:event_order')
    labels <- c('AI-Assisted','AI × Event order')
  } else if (coef_mode == 'prompts') {
    keep <- c('log1p(prompts_i)')
    labels <- c('log(1+prompts)')
  } else if (coef_mode == 'software') {
    keep <- c('treatment','software3R','software3Python','treatment:software3R','treatment:software3Python')
    labels <- c('AI-Assisted','R','Python','AI × R','AI × Python')
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
    lines <- c(lines, paste0('\\multicolumn{', K+1, '}{l}{Compression (monotonic): one-sided p-value for increasing effects across tiers (baseline: Undergraduate).}\\\\'))
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
    software3 = factor(preferred_software, levels = c("R","Stata","Python")),
    tier = factor(tier, levels = c("UG","MA","PhD","PD","P")),
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

# Precompute robustness checks (≥1 and ≥2) to append to the main results table
f_good <- good_checks_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_two  <- two_good_checks_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
m_good <- feols(f_good, data = ind, vcov = ~cluster_es)
m_two  <- feols(f_two,  data = ind, vcov = ~cluster_es)

write_ai_table(
  models = list(m_success, m_minutes, m_minor, m_major, m_good, m_two),
  col_titles = c("Reproduction", "Minutes", "Minor", "Major", "At least 1 check", "At least 2 checks"),
  file = "output/tables/pap_main.tex",
  coef_mode = "tier",
  controls_desc = "Event & article FE; years of coding; software; prior AI familiarity."
)

# Horizontal compression: add AI × Outside-discipline interaction (parallel to vertical tables)
if (TRUE) {
  f_ols_h <- reproduction_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  f_min_h <- minutes_to_success_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  f_pmn_h <- minor_errors_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  f_pmj_h <- major_errors_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  m_success_h <- feols(f_ols_h, data = ind, vcov = ~cluster_es)
  m_minutes_h <- feols(f_min_h, data = ind, vcov = ~cluster_es)
  m_minor_h   <- fepois(f_pmn_h, data = ind, vcov = ~cluster_es)
  m_major_h   <- fepois(f_pmj_h, data = ind, vcov = ~cluster_es)
  # Debug: write term names for inspection
  tt_dbg <- tryCatch(broom::tidy(m_success_h), error=function(e) NULL)
  if (!is.null(tt_dbg)) {
    dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
    writeLines(paste(tt_dbg$term, collapse='\n'), "output/tables/pap_horizontal_terms.txt")
  }
  f_good_h <- good_checks_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  f_two_h  <- two_good_checks_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  m_good_h <- feols(f_good_h, data = ind, vcov = ~cluster_es)
  m_two_h  <- feols(f_two_h,  data = ind, vcov = ~cluster_es)
  write_ai_table(
    models = list(m_success_h, m_minutes_h, m_minor_h, m_major_h, m_good_h, m_two_h),
    col_titles = c("Reproduction", "Minutes", "Minor", "Major", "At least 1 check", "At least 2 checks"),
    file = "output/tables/pap_main_horizontal.tex",
    coef_mode = "ood",
    controls_desc = "Event & article FE; years of coding; software; prior AI familiarity."
  )

}

# Referee report outcomes (human and AI assessments)
f_ref_app_h  <- referee_app_human_i  ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_ref_score_h<- referee_score_human_i~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_ref_app_ai <- referee_app_ai_i     ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_ref_score_ai<-referee_score_ai_i   ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)

m_ref_app_h   <- feols(f_ref_app_h,   data = ind, vcov = ~cluster_es)
m_ref_score_h <- feols(f_ref_score_h, data = ind, vcov = ~cluster_es)
m_ref_app_ai  <- feols(f_ref_app_ai,  data = ind, vcov = ~cluster_es)
m_ref_score_ai<- feols(f_ref_score_ai,data = ind, vcov = ~cluster_es)

write_ai_table(
  models = list(m_ref_app_h, m_ref_score_h, m_ref_app_ai, m_ref_score_ai),
  col_titles = c("Appropriate (human)", "Score (human)", "Appropriate (AI)", "Score (AI)"),
  file = "output/tables/pap_referee.tex",
  coef_mode = "tier",
  controls_desc = "Event & article FE; years of coding; software; prior AI familiarity."
)

# Horizontal: referee outcomes with AI × OOD
if (TRUE) {
  f_ref_app_h_o   <- referee_app_human_i   ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  f_ref_score_h_o <- referee_score_human_i ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  f_ref_app_ai_o  <- referee_app_ai_i      ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  f_ref_score_ai_o<- referee_score_ai_i    ~ treatment + out_of_disc_i + treatment:out_of_disc_i + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
  m_ref_app_h_o    <- feols(f_ref_app_h_o,    data = ind, vcov = ~cluster_es)
  m_ref_score_h_o  <- feols(f_ref_score_h_o,  data = ind, vcov = ~cluster_es)
  m_ref_app_ai_o   <- feols(f_ref_app_ai_o,   data = ind, vcov = ~cluster_es)
  m_ref_score_ai_o <- feols(f_ref_score_ai_o, data = ind, vcov = ~cluster_es)
  write_ai_table(
    models = list(m_ref_app_h_o, m_ref_score_h_o, m_ref_app_ai_o, m_ref_score_ai_o),
    col_titles = c("Appropriate (human)", "Score (human)", "Appropriate (AI)", "Score (AI)"),
    file = "output/tables/pap_referee_horizontal.tex",
    coef_mode = "ood",
    controls_desc = "Event & article FE; years of coding; software; prior AI familiarity."
  )
}

# Referee by years (appendix): replace tier with years × AI
f_ref_app_h_y   <- referee_app_human_i   ~ treatment*years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_ref_score_h_y <- referee_score_human_i ~ treatment*years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_ref_app_ai_y  <- referee_app_ai_i      ~ treatment*years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_ref_score_ai_y<- referee_score_ai_i    ~ treatment*years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
m_ref_app_h_y    <- feols(f_ref_app_h_y,    data = ind, vcov = ~cluster_es)
m_ref_score_h_y  <- feols(f_ref_score_h_y,  data = ind, vcov = ~cluster_es)
m_ref_app_ai_y   <- feols(f_ref_app_ai_y,   data = ind, vcov = ~cluster_es)
m_ref_score_ai_y <- feols(f_ref_score_ai_y, data = ind, vcov = ~cluster_es)
write_ai_table(
  models = list(m_ref_app_h_y, m_ref_score_h_y, m_ref_app_ai_y, m_ref_score_ai_y),
  col_titles = c("Appropriate (yrs)", "Score (yrs)", "Appropriate (AI, yrs)", "Score (AI, yrs)"),
  file = "output/tables/pap_referee_years.tex",
  coef_mode = "years",
  controls_desc = "Event & article FE; software; prior AI familiarity."
)

## (moved: robustness models precomputed above to append to main table)

# Appendix: robustness without event/article FE (specification check)
f_good_nofe <- good_checks_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity
f_two_nofe  <- two_good_checks_i ~ treatment + tier + treatment:tier + years_coding + software3 + prior_gpt_familiarity
m_good_nofe <- feols(f_good_nofe, data = ind, vcov = ~cluster_es)
m_two_nofe  <- feols(f_two_nofe,  data = ind, vcov = ~cluster_es)
write_ai_table(
  models = list(m_good_nofe, m_two_nofe),
  col_titles = c("At least 1 check", "At least 2 checks"),
  file = "output/tables/pap_robustness_clarity_appendix.tex",
  coef_mode = "tier",
  controls_desc = "No event/article FE; years of coding; software; prior AI familiarity."
)

# (combined FE vs no-FE robustness table dropped)

# Secondary 1: replace tier with continuous years of coding
f_ols2 <- update(f_ols, . ~ . - tier - treatment:tier + years_coding:treatment)
f_min2 <- update(f_min, . ~ . - tier - treatment:tier + years_coding:treatment)
f_pmn2 <- update(f_pmn, . ~ . - tier - treatment:tier + years_coding:treatment)
f_pmj2 <- update(f_pmj, . ~ . - tier - treatment:tier + years_coding:treatment)

m_success2 <- feols(f_ols2, data = ind, vcov = ~cluster_es)
m_minutes2 <- feols(f_min2, data = ind, vcov = ~cluster_es)
m_minor2   <- fepois(f_pmn2, data = ind, vcov = ~cluster_es)
m_major2   <- fepois(f_pmj2, data = ind, vcov = ~cluster_es)

write_ai_table(
  models = list(m_success2, m_minutes2, m_minor2, m_major2),
  col_titles = c("Reproduction (yrs)", "Minutes (yrs)", "Minor (yrs)", "Major (yrs)"),
  file = "output/tables/pap_main_years_core.tex",
  coef_mode = "years",
  controls_desc = "Event & article FE; software; prior AI familiarity."
)

# Prepare AI-arm subset for prompts-only and usage-by-tier analyses
ai <- ind %>% filter(treatment == 1)

# (prompts-only table removed from PAP and code)

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
if (length(mods_s) > 0) {
  write_ai_table(
    models = unname(mods_s),
    col_titles = {
      nm <- names(mods_s);
      rec <- c(UG = "Undergraduate", MA = "Master's", PhD = "PhD", PD = "Postdoc", P = "Professor");
      unname(ifelse(nm %in% names(rec), rec[nm], nm))
    },
    file = "output/tables/pap_usage_success_by_tier.tex",
    coef_mode = "usage",
    controls_desc = "Event & article FE; years of coding; software; prior AI familiarity (AI arm only)."
  )
}
if (length(mods_m) > 0) {
  write_ai_table(
    models = unname(mods_m),
    col_titles = {
      nm <- names(mods_m);
      rec <- c(UG = "Undergraduate", MA = "Master's", PhD = "PhD", PD = "Postdoc", P = "Professor");
      unname(ifelse(nm %in% names(rec), rec[nm], nm))
    },
    file = "output/tables/pap_usage_minutes_by_tier.tex",
    coef_mode = "usage",
    controls_desc = "Event & article FE; years of coding; software; prior AI familiarity (AI arm only)."
  )
}

# Secondary 3: learning across events (treatment x event order)
f_learn <- reproduction_i ~ treatment*event_order + tier + years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
m_learn <- feols(f_learn, data = ind, vcov = ~cluster_es)
write_ai_table(
  models = list(m_learn),
  col_titles = c("Success (trt×order)"),
  file = "output/tables/pap_secondary_learning.tex",
  coef_mode = "learn",
  controls_desc = "Event & article FE; years of coding; software; prior AI familiarity."
)

# Secondary 1: years of coding as moderator (replace tier)
f_ols2 <- reproduction_i ~ treatment*years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_min2 <- minutes_to_success_i ~ treatment*years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_pmn2 <- minor_errors_i ~ treatment*years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
f_pmj2 <- major_errors_i ~ treatment*years_coding + software3 + prior_gpt_familiarity + i(event) + i(article)
m_success2 <- feols(f_ols2, data = ind, vcov = ~cluster_es)
m_minutes2 <- feols(f_min2, data = ind, vcov = ~cluster_es)
m_minor2   <- fepois(f_pmn2, data = ind, vcov = ~cluster_es)
m_major2   <- fepois(f_pmj2, data = ind, vcov = ~cluster_es)
write_ai_table(
  models = list(m_success2, m_minutes2, m_minor2, m_major2),
  col_titles = c("Success (yrs×AI)", "Minutes (yrs×AI)", "Minor (yrs×AI)", "Major (yrs×AI)"),
  file = "output/tables/pap_secondary_years.tex",
  coef_mode = "years",
  controls_desc = "Event & article FE; software; prior AI familiarity."
)

 

 

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
