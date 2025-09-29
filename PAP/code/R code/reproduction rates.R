suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

# --- 1. Load data ---
main <- readRDS("data/AI games.rds")

# --- 2. Collapse by game and branch (averages) ---
df_summary <- main %>%
  group_by(game2, branch) %>%
  summarise(
    reproduction = mean(reproduction, na.rm = TRUE),
    minor_errors = mean(minor_errors, na.rm = TRUE),
    major_errors = mean(major_errors, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. Wide reshape ---
df_wide <- df_summary %>%
  pivot_wider(
    names_from = branch,
    values_from = c(reproduction, minor_errors, major_errors),
    names_sep = ""
  )

# --- 4. Create difference variables ---
df_wide <- df_wide %>%
  mutate(
    rep_machine   = `reproductionHuman-Only` - `reproductionAI-Led`,
    rep_cyorg     = `reproductionHuman-Only` - `reproductionAI-Assisted`, 
    minor_machine = `minor_errorsHuman-Only` - `minor_errorsAI-Led`,
    minor_cyorg   = `minor_errorsHuman-Only` - `minor_errorsAI-Assisted`,
    major_machine = `major_errorsHuman-Only` - `major_errorsAI-Led`,
    major_cyorg   = `major_errorsHuman-Only` - `major_errorsAI-Assisted`
  )

# --- 5. Labels for the x-axis (generic five-game design) ---
# Only the first five events are labeled; others drop (NA) in plots
game2_labels <- c(
  "Toronto" = "Game 1",
  "Ottawa" = "Game 2",
  "Sheffield" = "Game 3",
  "Cornell" = "Game 4",
  "Bogota" = "Game 5"
)
df_wide$game2 <- factor(
  as.character(df_wide$game2),
  levels = names(game2_labels),
  labels = unname(game2_labels)
)
df_wide$game2 <- as.character(df_wide$game2)
df_wide$game2[is.na(df_wide$game2)] <- "Game 6"
df_wide$game2 <- factor(df_wide$game2,
                        levels = c("Game 1","Game 2","Game 3","Game 4","Game 5","Game 6"))

# --- 6. Reproduction plot ---
p1 <- ggplot(df_wide, aes(x = game2)) +
  geom_line(aes(y = rep_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = rep_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2) +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.5, 0, 0.5, 1), limits = c(-0.5, 1)) +
  labs(
    y = "Difference in reproduction rate",
    x = "AI game",
    color = "",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("output/figures/reproduction rates.pdf", p1, width = 8, height = 4)


p1_s1 <- ggplot(
  df_wide %>% filter(!is.na(game2)),
  aes(x = game2)
) +
  geom_line(aes(y = rep_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = rep_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2) +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.5, 0, 0.5, 1), limits = c(-0.5, 1)) +
  labs(
    y = "Difference in reproduction rate",
    x = "AI game",
    color = "",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("output/figures/reproduction rates (s1).pdf", p1, width = 8, height = 4)

# --- 7. Minor errors plot ---
p2 <- ggplot(df_wide, aes(x = game2)) +
  geom_line(aes(y = minor_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = minor_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  labs(
    y = "Differences in the number of\nminor errors detected",
    x = "AI game",
    color = "",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("output/figures/minor errors.pdf", p2, width = 8, height = 4)


p2_s1 <- ggplot(
  df_wide %>% filter(!is.na(game2)),
  aes(x = game2)
) +
  geom_line(aes(y = minor_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = minor_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  labs(
    y = "Differences in the number of\nminor errors detected",
    x = "AI game",
    color = "",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("output/figures/minor errors (s1).pdf", p2_s1, width = 8, height = 4)

# --- 8. Major errors plot ---
p3 <- ggplot(df_wide, aes(x = game2)) +
  geom_line(aes(y = major_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = major_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2) +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  labs(
    y = "Differences in the number of\nmajor errors detected",
    x = "AI game",
    color = "",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("output/figures/major errors.pdf", p3, width = 8, height = 4)


p3_s1 <- ggplot(
  df_wide %>% filter(!is.na(game2)),
  aes(x = game2)
) +
  geom_line(aes(y = major_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = major_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2) +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  labs(
    y = "Differences in the number of\nmajor errors detected",
    x = "AI game",
    color = "",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("output/figures/major errors (s1).pdf", p3_s1, width = 8, height = 4)

# --- A. Harmonise 'game2' labels in the long (summary) data ---
df_summary$game2 <- factor(
  as.character(df_summary$game2),
  levels = names(game2_labels),
  labels = unname(game2_labels)
)
df_summary$game2 <- as.character(df_summary$game2)
df_summary$game2[is.na(df_summary$game2)] <- "Game 6"
df_summary$game2 <- factor(df_summary$game2,
                           levels = c("Game 1","Game 2","Game 3","Game 4","Game 5","Game 6"))

# --- B. Ensure tidy branch labels and colours (PAP: two-arm only) --
# Filter out AI-Led for PAP figures and standardize palette to two colors
df_summary <- df_summary %>% filter(branch %in% c("Human-Only","AI-Assisted"))
df_summary$branch <- factor(
  df_summary$branch,
  levels = c("Human-Only", "AI-Assisted")
)
branch_cols <- c("Human-Only"   = "#333333",
                 "AI-Assisted"  = "#1f77b4")

# --- C. Helper: one function to avoid repetition --------------
make_raw_plot <- function(data, yvar, ylab, percent = FALSE) {
  data <- data %>% filter(branch %in% c("Human-Only","AI-Assisted"))
  ggplot(data, aes(x = game2, y = .data[[yvar]],
                   group = branch, colour = branch)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_colour_manual(values = branch_cols, name = "") +
    {if (percent) scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                     limits = c(0, 1)) else NULL} +
    labs(y = ylab, x = "AI game", title = "", subtitle = "", caption = "") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA),
          legend.position = "top",
          legend.title = element_blank())
}

# --- D. Reproduction rate (percentage) ------------------------
p1_raw  <- make_raw_plot(df_summary,
                         yvar = "reproduction",
                         ylab = "Reproduction rate",
                         percent = TRUE)
ggsave("output/figures/reproduction rates (raw).pdf",
       p1_raw, width = 8, height = 4)

p1_raw_s1 <- make_raw_plot(
  df_summary %>% filter(!is.na(game2)),
  yvar  = "reproduction",
  ylab  = "Reproduction rate",
  percent = TRUE
)
ggsave("output/figures/reproduction rates (raw, s1).pdf",
       p1_raw_s1, width = 8, height = 4)

# --- E. Minor errors (counts) ---------------------------------
p2_raw <- make_raw_plot(df_summary,
                        yvar = "minor_errors",
                        ylab = "Number of minor errors")
ggsave("output/figures/minor errors (raw).pdf",
       p2_raw, width = 8, height = 4)

p2_raw_s1 <- make_raw_plot(
  df_summary %>% filter(!is.na(game2)),
  yvar = "minor_errors",
  ylab = "Number of minor errors")
ggsave("output/figures/minor errors (raw, s1).pdf",
       p2_raw_s1, width = 8, height = 4)

# --- F. Major errors (counts) ---------------------------------
p3_raw <- make_raw_plot(df_summary,
                        yvar = "major_errors",
                        ylab = "Number of major errors")
ggsave("output/figures/major errors (raw).pdf",
       p3_raw, width = 8, height = 4)

p3_raw_s1 <- make_raw_plot(
  df_summary %>% filter(!is.na(game2)),
  yvar = "major_errors",
  ylab = "Number of major errors")
ggsave("output/figures/major errors (raw, s1).pdf",
       p3_raw_s1, width = 8, height = 4)
