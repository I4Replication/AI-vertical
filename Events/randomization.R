#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(openxlsx)
  library(purrr)
})

# --- configuration ---------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
seed <- suppressWarnings(as.integer(args[1]))
if (length(args) == 0 || is.na(seed)) {
  seed <- suppressWarnings(as.integer(Sys.getenv("RANDOMIZATION_SEED", "")))
}
if (is.na(seed)) {
  seed <- 20241009L
}
set.seed(seed)

# Resolve the directory that holds this script (the Events folder)
resolve_events_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  scr <- cmd_args[grepl(file_arg, cmd_args)]
  if (length(scr) == 0) {
    return(normalizePath("."))
  }
  script_path <- sub(file_arg, "", scr[length(scr)])
  normalizePath(dirname(script_path))
}

events_dir <- resolve_events_dir()
repo_dir <- normalizePath(file.path(events_dir, ".."))

papers_path <- file.path(repo_dir, "Papers", "papers.xlsx")
if (!file.exists(papers_path)) {
  stop("Could not find Papers/papers.xlsx at ", papers_path)
}

# Load and tag the paper catalogue ------------------------------------------------
papers <- read_excel(papers_path, guess_max = 5000) |>
  mutate(
    Journal = str_squish(Journal),
    paper_discipline = case_when(
      str_detect(Journal, regex("psych", ignore_case = TRUE)) ~ "Psychology",
      str_detect(Journal, regex("ajps", ignore_case = TRUE)) ~ "Political Science",
      TRUE ~ "Economics"
    ),
    paper_title = str_squish(`Article Title`),
    paper_uid = paste0(paper_discipline, " | ", paper_title),
    paper_url = `Replication Package URL`
  ) |>
  select(paper_title, paper_discipline, Journal, paper_url, paper_uid)

if (anyNA(papers$paper_discipline)) {
  stop("Some papers could not be assigned to a discipline. Please update the journal-to-discipline mapping.")
}

# Ensure we have at least one paper per discipline
missing_disc <- setdiff(c("Economics", "Political Science", "Psychology"), unique(papers$paper_discipline))
if (length(missing_disc) > 0) {
  stop("Missing replication papers for disciplines: ", paste(missing_disc, collapse = ", "))
}

# Helper: draw a single paper given participant discipline and alignment ------------
draw_paper <- function(primary, alignment, catalogue) {
  stopifnot(alignment %in% c("Inside", "Outside"))
  if (is.na(primary) || !primary %in% c("Economics", "Political Science", "Psychology")) {
    stop("Unexpected discipline for participant: ", primary)
  }
  pool <- if (alignment == "Inside") {
    dplyr::filter(catalogue, paper_discipline == primary)
  } else {
    dplyr::filter(catalogue, paper_discipline != primary)
  }
  if (nrow(pool) == 0) {
    stop("No papers available for alignment=", alignment, " and discipline=", primary)
  }
  pool[sample.int(nrow(pool), 1), , drop = FALSE]
}

# Helper: clean participation roster -----------------------------------------------
clean_participants <- function(roster_df) {
  col_lookup <- function(df, pattern, label) {
    matches <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(matches) == 0) {
      stop("Could not find column for ", label, " (pattern: ", pattern, ")")
    }
    matches[[1]]
  }

  col_participate <- col_lookup(roster_df, "would you like to participate", "participation question")
  col_email <- col_lookup(roster_df, "what is your email address", "email")
  col_name <- col_lookup(roster_df, "what is your name", "name")
  col_position <- col_lookup(roster_df, "best describes your current position", "position")
  col_discipline <- col_lookup(roster_df, "academic discipline", "discipline")
  col_response <- col_lookup(roster_df, "^response id$", "response id")
  col_modified <- col_lookup(roster_df, "date modified", "date modified")
  col_first <- if (any(str_detect(names(roster_df), regex("^first name$", ignore_case = TRUE)))) {
    col_lookup(roster_df, "^first name$", "first name")
  } else NA_character_
  col_last <- if (any(str_detect(names(roster_df), regex("^last name$", ignore_case = TRUE)))) {
    col_lookup(roster_df, "^last name$", "last name")
  } else NA_character_

  roster_df |>
    mutate(
      participation_flag = str_to_lower(str_squish(.data[[col_participate]])) == "yes",
      email_clean = str_to_lower(str_squish(.data[[col_email]])),
      email_clean = if_else(is.na(email_clean) | email_clean == "", NA_character_, email_clean),
      date_modified = suppressWarnings(as.POSIXct(.data[[col_modified]], origin = "1970-01-01")),
      response_id = .data[[col_response]],
      name_entry = str_squish(.data[[col_name]])
    ) |>
    mutate(
      fallback_name = if (!is.na(col_first) && !is.na(col_last)) {
        str_squish(paste(.data[[col_first]], .data[[col_last]]))
      } else NA_character_,
      participant_name = coalesce(na_if(name_entry, ""), na_if(fallback_name, ""), email_clean, paste0("Response_", response_id)),
      position_raw = str_squish(.data[[col_position]]),
      discipline_raw = str_squish(.data[[col_discipline]])
    ) |>
    filter(participation_flag) |>
    mutate(dedupe_key = coalesce(email_clean, paste0("response:", response_id))) |>
    group_by(dedupe_key) |>
    slice_max(order_by = date_modified, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      tier = case_when(
        str_detect(str_to_lower(position_raw), "undergraduate") ~ "Undergraduate",
        str_detect(str_to_lower(position_raw), "master") ~ "Master's",
        str_detect(str_to_lower(position_raw), "phd") ~ "PhD",
        str_detect(str_to_lower(position_raw), "postdoc|post-doctoral|postdoctoral|researcher") ~ "Postdoc/Researcher",
        str_detect(str_to_lower(position_raw), "professor") ~ "Professor",
        TRUE ~ NA_character_
      ),
      primary_discipline = case_when(
        is.na(discipline_raw) | discipline_raw == "" ~ NA_character_,
        str_to_lower(discipline_raw) == "political science" ~ "Political Science",
        str_to_lower(discipline_raw) == "psychology" ~ "Psychology",
        TRUE ~ "Economics"
      )
    ) |>
    mutate(
      participant_name = str_replace_all(participant_name, "\\s+", " "),
      participant_name = str_squish(participant_name),
      tier_2 = case_when(
        tier == "Undergraduate" ~ "Undergraduate",
        tier %in% c("Master's", "PhD") ~ "Graduate",
        tier %in% c("Postdoc/Researcher", "Professor") ~ "Professor/Researcher",
        TRUE ~ NA_character_
      )
    )
}

# Helper: assign treatment within tiers --------------------------------------------
assign_treatment <- function(df) {
  n <- nrow(df)
  if (n == 0) {
    df$treatment_arm <- character(0)
    return(df)
  }
  n_ai <- floor(n / 2)
  n_human <- floor(n / 2)
  if (n %% 2 == 1) {
    if (runif(1) < 0.5) {
      n_ai <- n_ai + 1L
    } else {
      n_human <- n_human + 1L
    }
  }
  labels <- c(rep("AI-Assisted", n_ai), rep("Human-Only", n_human))
  df$treatment_arm <- sample(labels)
  df
}

# Helper: assign inside/outside balance within discipline x treatment --------------
assign_alignment_global <- function(df, target_share = 0.30) {
  n <- nrow(df)
  if (n == 0) {
    df$discipline_alignment <- character(0)
    return(df)
  }
  target_outside <- as.integer(round(n * target_share))
  eligible_idx <- which(df$tier != "Undergraduate")
  if (length(eligible_idx) == 0) {
    df$discipline_alignment <- rep("Inside", n)
    return(df)
  }
  max_outside <- length(eligible_idx)
  target_outside <- min(target_outside, max_outside)
  outside_idx <- if (target_outside > 0) sample(eligible_idx, target_outside) else integer(0)
  alignment <- rep("Inside", n)
  alignment[outside_idx] <- "Outside"
  alignment[df$tier == "Undergraduate"] <- "Inside"
  df$discipline_alignment <- alignment
  df
}

# Run randomization for each event workbook ---------------------------------------
event_files <- list.files(events_dir, pattern = "\\.xlsx$", full.names = TRUE)
event_files <- event_files[!grepl("~\\$", basename(event_files))]
event_files <- sort(event_files)

if (length(event_files) == 0) {
  stop("No event workbooks (.xlsx) found in ", events_dir)
}

run_timestamp <- format(Sys.time(), tz = "UTC", usetz = TRUE)

for (event_path in event_files) {
  message("Randomizing assignments for ", basename(event_path))
  event_name <- tools::file_path_sans_ext(basename(event_path))
  roster <- read_excel(event_path, sheet = 1, guess_max = 5000)
  participants <- clean_participants(roster)

  if (nrow(participants) == 0) {
    warning("No eligible participants found for ", basename(event_path), "; skipping.")
    next
  }

  if (any(is.na(participants$tier))) {
    unmapped_positions <- unique(participants$position_raw[is.na(participants$tier)])
    stop("Unmapped position categories for ", basename(event_path), ": ", paste(unmapped_positions, collapse = "; "))
  }

  if (any(is.na(participants$primary_discipline))) {
    problematic <- participants %>% filter(is.na(primary_discipline)) %>% pull(discipline_raw) %>% unique()
    stop("Unmapped disciplines for ", basename(event_path), ": ", paste(problematic, collapse = "; "))
  }

  participants <- participants |>
    arrange(tier, participant_name) |>
    group_by(tier) |>
    group_modify(~assign_treatment(.x)) |>
    ungroup()

  participants <- assign_alignment_global(participants, target_share = 0.30)

  paper_draws <- map2_dfr(participants$primary_discipline, participants$discipline_alignment,
                          ~draw_paper(.x, .y, papers))

  assignments <- bind_cols(participants, paper_draws) |>
    mutate(
      event = event_name,
      out_of_discipline = if_else(discipline_alignment == "Outside", "Yes", "No"),
      randomization_seed = seed,
      randomization_timestamp_utc = run_timestamp
    ) |>
    select(
      event,
      response_id,
      participant_name,
      participant_email = email_clean,
      tier,
      tier_2,
      primary_discipline,
      treatment_arm,
      discipline_alignment,
      out_of_discipline,
      paper_title,
      paper_discipline,
      Journal,
      paper_url,
      randomization_seed,
      randomization_timestamp_utc
    )

  sheet_base <- paste0("Assignments_", seed)
  wb <- loadWorkbook(event_path)
  sheet_name <- sheet_base
  if (sheet_name %in% names(wb)) {
    suffix <- format(Sys.time(), "%H%M%S")
    sheet_name <- substr(paste0(sheet_base, "_", suffix), 1, 31)
    while (sheet_name %in% names(wb)) {
      suffix <- sprintf("%s%02d", format(Sys.time(), "%H%M%S"), sample.int(99, 1))
      sheet_name <- substr(paste0(sheet_base, "_", suffix), 1, 31)
    }
  }

  addWorksheet(wb, sheet_name)
  writeDataTable(wb, sheet = sheet_name, x = assignments)
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(assignments), widths = "auto")
  saveWorkbook(wb, event_path, overwrite = TRUE)

  message(sprintf(
    "  -> Added sheet '%s' with %d participants (%d AI, %d Human).",
    sheet_name,
    nrow(assignments),
    sum(assignments$treatment_arm == "AI-Assisted"),
    sum(assignments$treatment_arm == "Human-Only")
  ))
}
