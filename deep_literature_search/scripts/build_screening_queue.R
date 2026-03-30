#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

resolve_project_dir <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  if (file.exists(file.path(cwd, "plant_body_mass_scaling_comprehensive.Rmd"))) {
    return(cwd)
  }

  candidate <- file.path(cwd, "plant_body_mass_scaling_project")
  if (file.exists(file.path(candidate, "plant_body_mass_scaling_comprehensive.Rmd"))) {
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }

  stop("Could not resolve project root. Run from workspace root or plant_body_mass_scaling_project.")
}

priority_from_rank <- function(rank_value) {
  case_when(
    rank_value <= 100 ~ "high",
    rank_value <= 300 ~ "medium",
    TRUE ~ "lower"
  )
}

project_dir <- resolve_project_dir()
input_file <- file.path(
  project_dir,
  "deep_literature_search",
  "output",
  "openalex_deep_ranked_candidates.csv"
)
screening_dir <- file.path(project_dir, "deep_literature_search", "screening")
dir.create(screening_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Ranked candidate file not found: ", input_file)
}

ranked <- read_csv(input_file, show_col_types = FALSE)

screening_queue <- ranked |>
  mutate(
    screening_rank = row_number(),
    screening_priority = priority_from_rank(screening_rank),
    screening_status = "not_started",
    title_screen_decision = "undecided",
    abstract_screen_decision = "undecided",
    full_text_priority = if_else(screening_rank <= 150, "priority_full_text", "defer_until_screened"),
    include_for_catalog = "undecided",
    exclusion_reason = NA_character_,
    exclusion_detail = NA_character_,
    reviewer_notes = NA_character_,
    candidate_relationship_family = NA_character_,
    candidate_size_metric = NA_character_,
    candidate_response_trait = NA_character_,
    empirical_fit_evidence = "undecided",
    exponent_reported = "unknown",
    extraction_ready = "no",
    reported_taxon_name = NA_character_,
    accepted_taxon_name = NA_character_,
    accepted_status = "unreviewed",
    backbone_source = NA_character_,
    backbone_version_date = NA_character_,
    backbone_id = NA_character_,
    match_method = NA_character_,
    match_confidence = NA_character_,
    reconciliation_timestamp_utc = NA_character_,
    source_stream = "openalex_deep_search_v0_2_0"
  ) |>
  select(
    screening_rank,
    screening_priority,
    screening_status,
    title_screen_decision,
    abstract_screen_decision,
    full_text_priority,
    include_for_catalog,
    exclusion_reason,
    exclusion_detail,
    reviewer_notes,
    candidate_relationship_family,
    candidate_size_metric,
    candidate_response_trait,
    empirical_fit_evidence,
    exponent_reported,
    extraction_ready,
    reported_taxon_name,
    accepted_taxon_name,
    accepted_status,
    backbone_source,
    backbone_version_date,
    backbone_id,
    match_method,
    match_confidence,
    reconciliation_timestamp_utc,
    source_stream,
    everything()
  )

write_csv(
  screening_queue,
  file.path(screening_dir, "deep_search_screening_queue.csv")
)

summary_tbl <- screening_queue |>
  summarise(
    total_records = n(),
    high_priority_n = sum(screening_priority == "high"),
    medium_priority_n = sum(screening_priority == "medium"),
    lower_priority_n = sum(screening_priority == "lower"),
    priority_full_text_n = sum(full_text_priority == "priority_full_text")
  )

write_csv(
  summary_tbl,
  file.path(screening_dir, "deep_search_screening_queue_summary.csv")
)

cat("screening_queue_written\n")
cat("records=", nrow(screening_queue), "\n", sep = "")
