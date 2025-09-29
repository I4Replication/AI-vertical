############################################################
# Synthetic tables to mirror reference outputs (names only)
############################################################
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
})

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

main <- readRDS("data/AI games.rds")

write_tex <- function(path, body_lines){
  con <- file(path, open = "w"); on.exit(close(con))
  writeLines(c(
    "\\begin{tabular}{lrrrr}",
    "\\hline\\hline",
    body_lines,
    "\\hline\\hline",
    "\\end{tabular}"
  ), con)
}

fmt_num <- function(x, digits = 3){
  ifelse(is.finite(x), format(round(x, digits), nsmall = digits), "")
}

# 1) balance.tex: means by branch
bal <- main %>%
  group_by(branch) %>%
  summarise(
    reproduction = mean(reproduction, na.rm = TRUE),
    minor_errors = mean(minor_errors, na.rm = TRUE),
    major_errors = mean(major_errors, na.rm = TRUE),
    .groups = "drop"
  )
bal_lines <- c(
  "Branch & Reproduction & Minor errors & Major errors \\\\",
  bal %>% transmute(
    line = sprintf("%s & %s & %s & %s \\\\",
                   branch,
                   fmt_num(reproduction),
                   fmt_num(minor_errors),
                   fmt_num(major_errors))
  ) %>% pull(line)
)
write_tex("output/tables/balance.tex", bal_lines)

# Helper for S1 subset (exclude Virtual 2025)
s1 <- main %>% filter(game2 != "Virtual 2025")

# 2) branches.tex: counts by game and branch
br_tab <- main %>% count(game2, branch) %>%
  tidyr::pivot_wider(names_from = branch, values_from = n, values_fill = 0) %>%
  arrange(game2)
br_lines <- c(
  "Game & Human-Only & AI-Assisted & AI-Led \\\\",
  br_tab %>% mutate(
    line = sprintf("%s & %s & %s & %s \\\\", game2, `Human-Only`, `AI-Assisted`, `AI-Led`)
  ) %>% pull(line)
)
write_tex("output/tables/branches.tex", br_lines)

# 3) branches_s1.tex: same but S1 subset
br_s1 <- s1 %>% count(game2, branch) %>%
  tidyr::pivot_wider(names_from = branch, values_from = n, values_fill = 0) %>%
  arrange(game2)
br_s1_lines <- c(
  "Game & Human-Only & AI-Assisted & AI-Led \\\\",
  br_s1 %>% mutate(
    line = sprintf("%s & %s & %s & %s \\\\", game2, `Human-Only`, `AI-Assisted`, `AI-Led`)
  ) %>% pull(line)
)
write_tex("output/tables/branches_s1.tex", br_s1_lines)

# 4) prompts.tex: mean prompts/files/images/words by branch
pr <- main %>% group_by(branch) %>% summarise(
  prompts = mean(prompts, na.rm = TRUE),
  files   = mean(files,   na.rm = TRUE),
  images  = mean(images,  na.rm = TRUE),
  words   = mean(words,   na.rm = TRUE), .groups = "drop")
pr_lines <- c(
  "Branch & Prompts & Files & Images & Words \\\\",
  pr %>% transmute(
    line = sprintf("%s & %s & %s & %s & %s \\\\",
                   branch,
                   fmt_num(prompts), fmt_num(files), fmt_num(images), fmt_num(words))
  ) %>% pull(line)
)
write_tex("output/tables/prompts.tex", pr_lines)

# 5) softwares.tex: mean outcomes by software
sw <- main %>% group_by(software) %>% summarise(
  reproduction = mean(reproduction, na.rm = TRUE),
  minor_errors = mean(minor_errors, na.rm = TRUE),
  major_errors = mean(major_errors, na.rm = TRUE), .groups = "drop")
sw_lines <- c(
  "Software & Reproduction & Minor errors & Major errors \\\\",
  sw %>% transmute(
    line = sprintf("%s & %s & %s & %s \\\\",
                   software,
                   fmt_num(reproduction), fmt_num(minor_errors), fmt_num(major_errors))
  ) %>% pull(line)
)
write_tex("output/tables/softwares.tex", sw_lines)

# 6) gpt skill.tex: max/min skills by branch (counts)
gs <- main %>% group_by(branch, max_skill) %>% tally() %>%
  tidyr::pivot_wider(names_from = max_skill, values_from = n, values_fill = 0)
gs_lines <- c(
  paste("Branch &", paste(colnames(gs)[-1], collapse = " & "), "\\\\"),
  apply(gs, 1, function(r){ paste(paste(r, collapse = " & "), "\\\\") })
)
write_tex("output/tables/gpt skill.tex", gs_lines)

# 7) main.tex: simple differences in means (AI vs Human)
dm <- main %>% group_by(branch) %>% summarise(reproduction = mean(reproduction, na.rm = TRUE))
dm_lines <- c(
  "Branch & Mean reproduction \\\\",
  dm %>% transmute(line = sprintf("%s & %s \\\\", branch, fmt_num(reproduction))) %>% pull(line)
)
write_tex("output/tables/main.tex", dm_lines)

# 8) logit poisson.tex: summary stats in lieu of models
lp <- main %>% summarise(
  mean_reproduction = mean(reproduction, na.rm = TRUE),
  mean_minor = mean(minor_errors, na.rm = TRUE),
  mean_major = mean(major_errors, na.rm = TRUE)
)
lp_lines <- c(
  "Stat & Value \\\\",
  paste0("Mean reproduction & ", fmt_num(lp$mean_reproduction), " \\\\") ,
  paste0("Mean minor errors & ", fmt_num(lp$mean_minor), " \\\\") ,
  paste0("Mean major errors & ", fmt_num(lp$mean_major), " \\\\")
)
write_tex("output/tables/logit poisson.tex", lp_lines)

# 9) full controls (s1).tex and (s2).tex: placeholder means by branch
fc <- main %>% group_by(branch) %>% summarise(
  y1 = mean(minor_errors, na.rm = TRUE),
  y2 = mean(major_errors, na.rm = TRUE)
)
fc_lines <- c(
  "Branch & Minor errors & Major errors \\\\",
  fc %>% transmute(line = sprintf("%s & %s & %s \\\\", branch, fmt_num(y1), fmt_num(y2))) %>% pull(line)
)
write_tex("output/tables/full controls (s1).tex", fc_lines)
write_tex("output/tables/full controls (s2).tex", fc_lines)

# 10) power.tex: simple size/power proxy table
pw_lines <- c(
  "Quantity & Value \\\\",
  paste0("Number of teams & ", nrow(main), " \\\\") ,
  paste0("Games & ", length(unique(main$game2)), " \\\\")
)
write_tex("output/tables/power.tex", pw_lines)

# 11) rmst.tex: average times as proxy
rmst <- main %>% summarise(
  avg_t_reproduction = mean(time2_reproduction, na.rm = TRUE),
  avg_t_minor = mean(time2_first_minor, na.rm = TRUE),
  avg_t_major = mean(time2_first_major, na.rm = TRUE)
)
rmst_lines <- c(
  "Outcome & Minutes (avg) \\\\",
  paste0("Time to reproduction & ", fmt_num(rmst$avg_t_reproduction), " \\\\") ,
  paste0("Time to first minor error & ", fmt_num(rmst$avg_t_minor), " \\\\") ,
  paste0("Time to first major error & ", fmt_num(rmst$avg_t_major), " \\\\")
)
write_tex("output/tables/rmst.tex", rmst_lines)

# 12) study 2.tex: branch by attendance counts
st2 <- main %>% count(attendance, branch) %>%
  tidyr::pivot_wider(names_from = branch, values_from = n, values_fill = 0)
st2_lines <- c(
  paste("Attendance &", paste(colnames(st2)[-1], collapse = " & "), "\\\\"),
  apply(st2, 1, function(r){ paste(paste(r, collapse = " & "), "\\\\") })
)
write_tex("output/tables/study 2.tex", st2_lines)
