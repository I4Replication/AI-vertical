# ============================================================
#  Figure: Prompt distribution (AI-Assisted only)
# ============================================================

required <- c("tidyverse", "patchwork")
for (p in required) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(required, library, character.only = TRUE))

# ---- 1. Leer datos y filtrar rama AI-Assisted ---------------
df <- readRDS("data/fake_data.rds") |>
  filter(treatment == "ai_assisted")

# ---- 2. Variables a graficar --------------------------------
vars <- c("prompts", "files", "images", "words")

# ---- 3. Densidades tipo Stata -------------------------------
density_plots <- lapply(
  vars,
  \(v) ggplot(df, aes(x = .data[[v]])) +
    geom_density(na.rm = TRUE, adjust = 1) +   # ‘adjust’≈ bandwidth scalar de Stata
    labs(x = v, y = "Density") +
    theme_minimal(base_size = 13)
)

# ---- 4. 2×2 grid con patchwork ------------------------------
combo <- (density_plots[[1]] | density_plots[[2]]) /
  (density_plots[[3]] | density_plots[[4]])

# ---- 5. Carpeta y guardado ----------------------------------
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
ggsave("output/figures/prompt distribution.pdf", combo,
       width = 8, height = 4)