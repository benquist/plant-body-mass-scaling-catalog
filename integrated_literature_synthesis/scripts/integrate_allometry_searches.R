#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

STRICT_INFERENCE_GATE <- TRUE

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

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

require_columns <- function(df, required, df_name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(df_name, " is missing required columns: ", paste(missing, collapse = ", "))
  }
}

extract_numeric_exponents <- function(text) {
  if (is.na(text) || !nzchar(text)) return(numeric())

  txt <- str_to_lower(text)
  txt <- str_replace_all(txt, ",", ".")

  fraction_hits <- str_extract_all(txt, "\\b(?:3/4|2/3|1/4|-3/2)\\b")[[1]]
  fraction_map <- c("3/4" = 0.75, "2/3" = 0.6666667, "1/4" = 0.25, "-3/2" = -1.5)
  frac_vals <- unname(fraction_map[fraction_hits])

  contextual <- str_match_all(
    txt,
    "(?:exponent|slope|alpha|beta)\\s*(?:=|~|:|of)?\\s*(-?[0-9]+(?:\\.[0-9]+)?)"
  )[[1]]
  contextual_vals <- suppressWarnings(as.numeric(contextual[, 2]))

  power_form <- str_match_all(txt, "\\^\\s*(-?[0-9]+(?:\\.[0-9]+)?)")[[1]]
  power_vals <- suppressWarnings(as.numeric(power_form[, 2]))

  vals <- c(frac_vals, contextual_vals, power_vals)
  vals <- vals[is.finite(vals)]
  vals <- vals[vals >= -3 & vals <= 3]
  unique(round(vals, 6))
}

classify_study_type <- function(text) {
  if (is.na(text) || !nzchar(text)) return("unspecified")

  txt <- str_to_lower(text)

  if (str_detect(txt, "review|meta-analysis|metaanalysis|synthesis|systematic")) {
    return("review_synthesis")
  }
  if (str_detect(txt, "experiment|experimental|manipulat|treatment|greenhouse|common garden")) {
    return("experimental")
  }
  if (str_detect(txt, "field|plot|inventory|survey|transect|destructive sampling|chronosequence")) {
    return("field_observational")
  }
  if (str_detect(txt, "model|modelling|modeling|simulation|theory|theoretical")) {
    return("theoretical_or_modeling")
  }
  if (str_detect(txt, "method|protocol|algorithm|regression framework")) {
    return("methods")
  }

  "unspecified"
}

relationship_lookup <- tribble(
  ~pattern_group, ~relationship_family, ~evidence_signal,
  "dbh_biomass", "DBH-biomass", NA_character_,
  "height_diameter", "Height-diameter", NA_character_,
  "leaf_mass_area", "Leaf mass-area (LMA/SLA)", NA_character_,
  "root_shoot", "Root-shoot allocation", NA_character_,
  "pipe_model", "Pipe-model sapwood-leaf", NA_character_,
  "hydraulic_allometry", "Hydraulic/conduit allometry", NA_character_,
  "self_thinning", "Self-thinning density-biomass", NA_character_,
  "metabolic_scaling", "Metabolic scaling", NA_character_,
  "corner_rules", "Corner rules twig-leaf", NA_character_,
  "power_law", "General power-law allometry", NA_character_,
  "general_allometry", "General allometry", NA_character_,
  "methods_signal", NA_character_, "Methods/statistical signal",
  "empirical_signal", NA_character_, "Empirical signal"
)

safe_share <- function(x) {
  total <- sum(x, na.rm = TRUE)
  if (isTRUE(total <= 0)) return(rep(NA_real_, length(x)))
  x / total
}

summarise_stage <- function(stage_name, papers_stage, paper_relationships) {
  study_type_summary <- papers_stage |>
    count(study_type, sort = TRUE, name = "paper_n") |>
    mutate(analysis_stage = stage_name, share = safe_share(paper_n))

  relationship_summary <- paper_relationships |>
    filter(!is.na(relationship_family)) |>
    semi_join(papers_stage |> select(record_key), by = "record_key") |>
    distinct(record_key, relationship_family) |>
    count(relationship_family, sort = TRUE, name = "paper_n") |>
    mutate(analysis_stage = stage_name, share = safe_share(paper_n))

  n_rel <- nrow(relationship_summary)
  if (n_rel >= 2) {
    n_side <- min(10, floor(n_rel / 2))
    most <- relationship_summary |>
      slice_max(order_by = paper_n, n = n_side, with_ties = FALSE) |>
      mutate(rank_class = "most_studied")
    least <- relationship_summary |>
      anti_join(most |> select(relationship_family), by = "relationship_family") |>
      slice_min(order_by = paper_n, n = n_side, with_ties = FALSE) |>
      mutate(rank_class = "least_studied")
    most_least <- bind_rows(most, least)
  } else {
    most_least <- tibble(
      relationship_family = character(),
      paper_n = integer(),
      analysis_stage = character(),
      share = double(),
      rank_class = character()
    )
  }

  exponent_by_relationship <- paper_relationships |>
    filter(!is.na(relationship_family)) |>
    semi_join(papers_stage |> select(record_key), by = "record_key") |>
    distinct(record_key, relationship_family) |>
    left_join(
      papers_stage |> select(record_key, exponent_values),
      by = "record_key"
    ) |>
    unnest(exponent_values, keep_empty = FALSE) |>
    rename(exponent_value = exponent_values) |>
    group_by(relationship_family) |>
    summarise(
      exponent_values = list(exponent_value[is.finite(exponent_value)]),
      n_exponent_values = sum(is.finite(exponent_value), na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      exponent_mean = map_dbl(exponent_values, ~ if (length(.x) == 0) NA_real_ else mean(.x)),
      exponent_median = map_dbl(exponent_values, ~ if (length(.x) == 0) NA_real_ else median(.x)),
      exponent_sd = map_dbl(exponent_values, ~ if (length(.x) <= 1) NA_real_ else sd(.x)),
      exponent_min = map_dbl(exponent_values, ~ if (length(.x) == 0) NA_real_ else min(.x)),
      exponent_max = map_dbl(exponent_values, ~ if (length(.x) == 0) NA_real_ else max(.x))
    ) |>
    select(-exponent_values) |>
    filter(n_exponent_values > 0) |>
    mutate(analysis_stage = stage_name) |>
    arrange(desc(n_exponent_values), relationship_family)

  evidence_signal_summary <- paper_relationships |>
    filter(!is.na(evidence_signal)) |>
    semi_join(papers_stage |> select(record_key), by = "record_key") |>
    distinct(record_key, evidence_signal) |>
    count(evidence_signal, sort = TRUE, name = "paper_n") |>
    mutate(analysis_stage = stage_name, share = safe_share(paper_n))

  list(
    study_type_summary = study_type_summary,
    relationship_summary = relationship_summary,
    most_least = most_least,
    exponent_by_relationship = exponent_by_relationship,
    evidence_signal_summary = evidence_signal_summary
  )
}

project_dir <- resolve_project_dir()
deep_dir <- file.path(project_dir, "deep_literature_search")
out_dir <- file.path(project_dir, "integrated_literature_synthesis", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ranked_path <- file.path(deep_dir, "output", "openalex_deep_ranked_candidates.csv")
pattern_path <- file.path(deep_dir, "output", "openalex_pattern_hits_long.csv")
query_log_path <- file.path(deep_dir, "output", "search_query_log.csv")
screening_path <- file.path(deep_dir, "screening", "deep_search_screening_queue.csv")

if (!file.exists(ranked_path) || !file.exists(pattern_path)) {
  stop("Missing deep-search outputs. Run deep search pipeline first.")
}

ranked <- read_csv(ranked_path, show_col_types = FALSE)
pattern_hits <- read_csv(pattern_path, show_col_types = FALSE)
query_log <- if (file.exists(query_log_path)) read_csv(query_log_path, show_col_types = FALSE) else tibble()
screening <- if (file.exists(screening_path)) read_csv(screening_path, show_col_types = FALSE) else tibble()

require_columns(
  ranked,
  c("record_key", "abstract_text", "language", "publication_year", "cited_by_count"),
  "ranked"
)
require_columns(pattern_hits, c("record_key", "pattern_group"), "pattern_hits")
if (nrow(screening) > 0) {
  require_columns(screening, c("record_key", "include_for_catalog", "screening_status"), "screening")
}

papers <- ranked |>
  distinct(record_key, .keep_all = TRUE) |>
  mutate(
    study_type = map_chr(abstract_text, classify_study_type),
    exponent_values = map(abstract_text, extract_numeric_exponents),
    exponent_count = map_int(exponent_values, length),
    exponent_mean = map_dbl(exponent_values, ~ if (length(.x) == 0) NA_real_ else mean(.x)),
    exponent_median = map_dbl(exponent_values, ~ if (length(.x) == 0) NA_real_ else median(.x)),
    exponent_min = map_dbl(exponent_values, ~ if (length(.x) == 0) NA_real_ else min(.x)),
    exponent_max = map_dbl(exponent_values, ~ if (length(.x) == 0) NA_real_ else max(.x))
  )

if (nrow(screening) > 0) {
  papers <- papers |>
    left_join(
      screening |>
        select(record_key, include_for_catalog, screening_status, extraction_ready) |>
        distinct(record_key, .keep_all = TRUE),
      by = "record_key"
    )
} else {
  papers <- papers |>
    mutate(
      include_for_catalog = NA_character_,
      screening_status = NA_character_,
      extraction_ready = NA_character_
    )
}

papers_candidate <- papers
papers_screened <- papers |>
  filter(str_to_lower(include_for_catalog %||% "") %in% c("yes", "include", "included")) |>
  filter(str_to_lower(extraction_ready %||% "") %in% c("yes", "true", "ready"))

if (isTRUE(STRICT_INFERENCE_GATE) && nrow(papers_screened) == 0) {
  stop(
    "Inference gate failed: no records satisfy include_for_catalog in {yes/include/included} ",
    "and extraction_ready in {yes/true/ready}. Complete screening/extraction before ",
    "running comparative quantitative synthesis."
  )
}

paper_relationships <- pattern_hits |>
  select(record_key, pattern_group) |>
  distinct() |>
  left_join(relationship_lookup, by = "pattern_group")

candidate_summary <- summarise_stage(
  stage_name = "candidate_exploratory",
  papers_stage = papers_candidate,
  paper_relationships = paper_relationships
)

screened_summary <- summarise_stage(
  stage_name = "screened_inference_ready",
  papers_stage = papers_screened,
  paper_relationships = paper_relationships
)

study_type_summary <- bind_rows(candidate_summary$study_type_summary, screened_summary$study_type_summary)
relationship_summary <- bind_rows(candidate_summary$relationship_summary, screened_summary$relationship_summary)
most_least <- bind_rows(candidate_summary$most_least, screened_summary$most_least)
exponent_by_relationship <- bind_rows(candidate_summary$exponent_by_relationship, screened_summary$exponent_by_relationship)
evidence_signal_summary <- bind_rows(candidate_summary$evidence_signal_summary, screened_summary$evidence_signal_summary)

language_summary <- papers |>
  count(language, sort = TRUE, name = "paper_n") |>
  mutate(analysis_stage = "candidate_exploratory", share = safe_share(paper_n))

year_summary <- papers |>
  filter(!is.na(publication_year)) |>
  count(publication_year, sort = TRUE, name = "paper_n") |>
  mutate(analysis_stage = "candidate_exploratory")

query_coverage_summary <- if (nrow(query_log) > 0) {
  query_log |>
    select(query_id, query_text, records_fetched, terminal_reason, pages_attempted, pages_failed)
} else {
  tibble()
}

screening_status_summary <- if (nrow(screening) > 0) {
  screening |>
    count(screening_status, sort = TRUE, name = "paper_n")
} else {
  tibble()
}

papers_export <- papers |>
  mutate(
    exponent_values = map_chr(exponent_values, ~ if (length(.x) == 0) "" else paste(.x, collapse = "; "))
  )

write_csv(papers_export, file.path(out_dir, "integrated_paper_table.csv"))
write_csv(study_type_summary, file.path(out_dir, "study_type_summary.csv"))
write_csv(relationship_summary, file.path(out_dir, "relationship_frequency_summary.csv"))
write_csv(most_least, file.path(out_dir, "most_least_studied_allometry.csv"))
write_csv(exponent_by_relationship, file.path(out_dir, "exponent_by_relationship_summary.csv"))
write_csv(evidence_signal_summary, file.path(out_dir, "evidence_signal_summary.csv"))
write_csv(language_summary, file.path(out_dir, "language_summary.csv"))
write_csv(year_summary, file.path(out_dir, "year_summary.csv"))
write_csv(query_coverage_summary, file.path(out_dir, "query_coverage_summary.csv"))
write_csv(screening_status_summary, file.path(out_dir, "screening_status_summary.csv"))

stage_counts <- tibble(
  analysis_stage = c("candidate_exploratory", "screened_inference_ready"),
  paper_n = c(nrow(papers_candidate), nrow(papers_screened))
)
write_csv(stage_counts, file.path(out_dir, "analysis_stage_counts.csv"))

cat("integrated_summary_written\n")
cat("candidate_papers=", nrow(papers_candidate), "\n", sep = "")
cat("screened_papers=", nrow(papers_screened), "\n", sep = "")
cat("biological_relationships=", n_distinct(relationship_summary$relationship_family[relationship_summary$analysis_stage == "candidate_exploratory"]), "\n", sep = "")
