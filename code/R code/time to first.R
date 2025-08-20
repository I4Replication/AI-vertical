############################################################
# Time-to-first figures (synthetic individual-based aggregate)
############################################################
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2)
})

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

main <- readRDS("data/AI games.rds")

# Prepare labels for games
game2_labels <- c(
  "Toronto" = "Toronto\n(Feb)",
  "Ottawa" = "Ottawa\n(May)",
  "Sheffield" = "Sheffield\n(Jun)",
  "Cornell" = "Cornell\n(Aug)",
  "Bogota" = "Bogota\n(Oct)",
  "Tilburg" = "Tilburg\n(Oct)",
  "Virtual" = "Virtual\n(Nov)",
  "Virtual 2025" = "Virtual\n(2025)"
)

to_label <- function(x){
  factor(as.character(x), levels = names(game2_labels), labels = unname(game2_labels))
}

# Helper to create line plots by branch for a given y variable
make_plot <- function(df, yvar, ylab){
  ggplot(df, aes(x = game2, y = .data[[yvar]], group = branch, colour = branch)) +
    geom_line(size = 1.2) + geom_point(size = 2) +
    scale_colour_manual(values = c("Human-Only" = "#009E73",
                                   "AI-Assisted" = "#D55E00",
                                   "AI-Led" = "#0072B2"), name = "") +
    labs(x = "AI game", y = ylab) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA),
          legend.position = "top",
          legend.title = element_blank())
}

summ_by_game_branch <- main %>%
  group_by(game2, branch) %>%
  summarise(
    time2_first_major = mean(time2_first_major, na.rm = TRUE),
    time2_first_minor = mean(time2_first_minor, na.rm = TRUE),
    time2_reproduction = mean(time2_reproduction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(game2 = to_label(game2))

# All games
p_major  <- make_plot(summ_by_game_branch, "time2_first_major",  "Time to first major error (min)")
p_minor  <- make_plot(summ_by_game_branch, "time2_first_minor",  "Time to first minor error (min)")
p_reprod <- make_plot(summ_by_game_branch, "time2_reproduction", "Time to reproduction (min)")

ggsave("output/figures/time2_first_major.pdf",  p_major,  width = 8, height = 4)
ggsave("output/figures/time2_first_minor.pdf",  p_minor,  width = 8, height = 4)
ggsave("output/figures/time2_reproduction.pdf", p_reprod, width = 8, height = 4)

# Subset excluding Virtual 2025
summ_s1 <- summ_by_game_branch %>% filter(game2 != "Virtual\n(2025)")

p_major_s1  <- make_plot(summ_s1, "time2_first_major",  "Time to first major error (min)")
p_minor_s1  <- make_plot(summ_s1, "time2_first_minor",  "Time to first minor error (min)")
p_reprod_s1 <- make_plot(summ_s1, "time2_reproduction", "Time to reproduction (min)")

ggsave("output/figures/time2_first_major (s1).pdf",  p_major_s1,  width = 8, height = 4)
ggsave("output/figures/time2_first_minor (s1).pdf",  p_minor_s1,  width = 8, height = 4)
ggsave("output/figures/time2_reproduction (s1).pdf", p_reprod_s1, width = 8, height = 4)

