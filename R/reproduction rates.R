# ============================================================
#  Figures: Reproduction rates & Error counts (AI games)
#  Adaptado a fake_data (Human-Only vs AI-Assisted)
# ============================================================

# ---- 0. Paquetes --------------------------------------------------------------
required <- c("tidyverse", "scales")
for (p in required) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(required, library, character.only = TRUE))

# ---- 1. Cargar datos y armonizar nombres --------------------------------------
df <- readRDS("data/fake_data.rds") |>
  mutate(
    # Unificamos EU y NA en un solo evento “Virtual”
    game2 = case_when(
      event %in% c("Virtual_Europe", "Virtual_North_America") ~ "Virtual",
      event == "Virtual_2025"                                 ~ "Virtual_2025",
      TRUE                                                    ~ event
    ),
    branch = case_when(
      treatment == "human_only"  ~ "Human-Only",
      treatment == "ai_assisted" ~ "AI-Assisted",
      TRUE                       ~ treatment           # por si aparece algo más
    ),
    reproduction = reproduction_success                  # para mantener sintaxis original
  )

# ---- 2. Colapsar por juego y rama ------------------------------------------------
df_summary <- df |>
  group_by(game2, branch) |>
  summarise(
    reproduction = mean(reproduction, na.rm = TRUE),
    minor_errors = mean(minor_errors, na.rm = TRUE),
    major_errors = mean(major_errors, na.rm = TRUE),
    .groups      = "drop"
  )

# ---- 3. Tabla ancha y diferencias ------------------------------------------------
df_wide <- df_summary |>
  pivot_wider(
    names_from  = branch,
    values_from = c(reproduction, minor_errors, major_errors),
    names_sep   = "__"           # separador seguro
  ) |>
  mutate(
    rep_diff   = .data[["reproduction__Human-Only"]]  - .data[["reproduction__AI-Assisted"]],
    minor_diff = .data[["minor_errors__Human-Only"]]  - .data[["minor_errors__AI-Assisted"]],
    major_diff = .data[["major_errors__Human-Only"]]  - .data[["major_errors__AI-Assisted"]]
  )

# ---- 4. Etiquetas bonitas para el eje X ----------------------------------------
game2_labels <- c(
  "Toronto"      = "Toronto\n(Feb)",
  "Ottawa"       = "Ottawa\n(May)",
  "Sheffield"    = "Sheffield\n(Jun)",
  "Cornell"      = "Cornell\n(Aug)",
  "Bogota"       = "Bogota\n(Oct)",
  "Tilburg"      = "Tilburg\n(Oct)",
  "Virtual"      = "Virtual\n(Nov)",   # EU + NA 2024
  "Virtual_2025" = "Virtual\n(2025)"
)

df_wide$game2    <- factor(df_wide$game2,
                           levels = names(game2_labels),
                           labels = unname(game2_labels))
df_summary$game2 <- factor(df_summary$game2,
                           levels = names(game2_labels),
                           labels = unname(game2_labels))

# ---- 5. Colores y helpers -------------------------------------------------------
cols <- c("Human-Only vs AI-Assisted" = "#D55E00")
branch_cols <- c("Human-Only"  = "#009E73",
                 "AI-Assisted" = "#D55E00")

make_diff_plot <- function(data, yvar, ylab, percent = FALSE) {
  p <- ggplot(data, aes(x = game2, y = .data[[yvar]])) +
    geom_line(aes(group = 1, colour = "Human-Only vs AI-Assisted"), size = 1.2) +
    geom_point(aes(colour = "Human-Only vs AI-Assisted"), size = 2) +
    scale_colour_manual(values = cols, name = "") +
    labs(y = ylab, x = "AI game") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.background  = element_rect(fill = "white", colour = NA),
          legend.position  = "top",
          legend.title     = element_blank())
  if (percent) {
    p <- p + scale_y_continuous(labels = percent_format(accuracy = 1),
                                limits = c(-1, 1))
  }
  p
}

make_raw_plot <- function(data, yvar, ylab, percent = FALSE) {
  p <- ggplot(data, aes(x = game2, y = .data[[yvar]],
                        group = branch, colour = branch)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_colour_manual(values = branch_cols, name = "") +
    labs(y = ylab, x = "AI game") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.background  = element_rect(fill = "white", colour = NA),
          legend.position  = "top",
          legend.title     = element_blank())
  if (percent) {
    p <- p + scale_y_continuous(labels = percent_format(accuracy = 1),
                                limits = c(0, 1))
  }
  p
}

# ---- 6. Crear carpeta de salida -------------------------------------------------
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# ---- 7. Figuras “diferencia” ----------------------------------------------------
ggsave("output/figures/reproduction rates.pdf",
       make_diff_plot(df_wide, "rep_diff",
                      "Difference in reproduction rate", percent = TRUE),
       width = 8, height = 4)

ggsave("output/figures/minor errors.pdf",
       make_diff_plot(df_wide, "minor_diff",
                      "Difference in the number of\nminor errors detected"),
       width = 8, height = 4)

ggsave("output/figures/major errors.pdf",
       make_diff_plot(df_wide, "major_diff",
                      "Difference in the number of\nmajor errors detected"),
       width = 8, height = 4)

# ---- 8. Figuras “raw” -----------------------------------------------------------
ggsave("output/figures/reproduction rates (raw).pdf",
       make_raw_plot(df_summary, "reproduction",
                     "Reproduction rate", percent = TRUE),
       width = 8, height = 4)

ggsave("output/figures/minor errors (raw).pdf",
       make_raw_plot(df_summary, "minor_errors",
                     "Number of minor errors"),
       width = 8, height = 4)

ggsave("output/figures/major errors (raw).pdf",
       make_raw_plot(df_summary, "major_errors",
                     "Number of major errors"),
       width = 8, height = 4)