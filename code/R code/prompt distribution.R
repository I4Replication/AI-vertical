############################################################
# Density plots – individual-level (AI arm)
############################################################
# ---- 1. Read data ----
ind <- readRDS("data/AI individuals.rds")
ind <- ind %>% filter(treatment == 1)

# ---- 2. Variables for which we want densities ----
vars <- c("prompts_i", "files_i", "images_i", "words_i")
labels <- c(prompts_i = "prompts", files_i = "files", images_i = "images", words_i = "words")

# ---- 3. Make one ggplot2 density plot per variable ----
density_plots <- lapply(
  vars,
  function(v) {
    ggplot(ind, aes(x = .data[[v]])) +
      geom_density(na.rm = TRUE, adjust = 1) +
      labs(x = labels[[v]], y = "Density") +
      theme_minimal(base_size = 13)
  }
)

# ---- 4. Display the four plots in a 2x2 grid ----
(density_plots[[1]] | density_plots[[2]]) /
  (density_plots[[3]] | density_plots[[4]])

ggsave("output/figures/prompt distribution.pdf", (density_plots[[1]] | density_plots[[2]]) /
         (density_plots[[3]] | density_plots[[4]]), width = 8, height = 4)
