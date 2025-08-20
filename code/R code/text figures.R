############################################################
# Text-derived figures using local corpus (synthetic)
# - wordcloud_focus_groups.png
# - markov_words_focus_groups.png
# - markov_bigrams_focus_groups.png
############################################################
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(tidyr); library(ggplot2)
})

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# Load corpus: all .txt files from data/corpus
corpus_dir <- "data/corpus"
files <- if (dir.exists(corpus_dir)) list.files(corpus_dir, pattern = "\\.txt$", full.names = TRUE) else character(0)
texts <- if (length(files)) sapply(files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n"), USE.NAMES = FALSE) else ""
text  <- paste(texts, collapse = "\n")

clean_tokens <- function(txt){
  parts <- txt |>
    tolower() |>
    str_replace_all("[^a-z\n\t\r ]+", " ") |>
    str_squish() |>
    str_split(" ", simplify = FALSE) |>
    unlist()
  parts[nchar(parts) > 0]
}

tokens <- clean_tokens(text)

# 1) "Wordcloud": use a simple top-words bar chart (no extra deps)
word_freq <- tibble(word = tokens) |>
  count(word, sort = TRUE) |>
  slice_head(n = 30) |>
  arrange(n)

p_cloud <- ggplot(word_freq, aes(x = n, y = reorder(word, n))) +
  geom_col(fill = "#0072B2") +
  labs(x = "Frequency", y = NULL, title = "Top words (synthetic corpus)") +
  theme_minimal(base_size = 12)
ggsave("output/figures/wordcloud_focus_groups.png", p_cloud, width = 9, height = 6, dpi = 200)

# Helper to compute transitions for consecutive tokens
lagged_pairs <- function(vec){
  if (length(vec) < 2) return(tibble(from = character(), to = character()))
  tibble(from = vec[-length(vec)], to = vec[-1])
}

# 2) Word-level transitions heatmap
transitions <- lagged_pairs(tokens) |>
  count(from, to, sort = TRUE)

top_words <- unique(c(head(transitions$from, 20), head(transitions$to, 20)))
heat_words <- transitions |>
  filter(from %in% top_words, to %in% top_words)

p_heat_words <- ggplot(heat_words, aes(x = from, y = to, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Word t", y = "Word t+1", title = "Word transitions (top set)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/figures/markov_words_focus_groups.png", p_heat_words, width = 10, height = 8, dpi = 200)

# 3) Bigram transitions: build bigrams from tokens, then transitions between bigrams
if (length(tokens) >= 3) {
  bigrams <- paste(tokens[-length(tokens)], tokens[-1])
  big_pairs <- lagged_pairs(bigrams) |>
    count(from, to, sort = TRUE)
  top_bigrams <- unique(c(head(big_pairs$from, 20), head(big_pairs$to, 20)))
  heat_bigrams <- big_pairs |>
    filter(from %in% top_bigrams, to %in% top_bigrams)

  p_heat_bi <- ggplot(heat_bigrams, aes(x = from, y = to, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(option = "C") +
    labs(x = "Bigram t", y = "Bigram t+1", title = "Bigram transitions (top set)") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("output/figures/markov_bigrams_focus_groups.png", p_heat_bi, width = 12, height = 9, dpi = 200)
} else {
  # Fallback empty plot if not enough tokens
  p_empty <- ggplot() + theme_void() + labs(title = "Insufficient text for bigram plot")
  ggsave("output/figures/markov_bigrams_focus_groups.png", p_empty, width = 8, height = 6, dpi = 200)
}
