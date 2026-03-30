#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidyr)
  library(readr)
})

SCRIPT_VERSION <- "0.2.0"
API_BASE <- "https://api.openalex.org/works"
API_FILTERS <- "type:article,has_abstract:true"
PER_PAGE <- 100
MAX_PAGES <- 8
REQUEST_PAUSE_SEC <- 0.2
USER_AGENT <- "plant-body-mass-scaling-deep-search/0.2"
TARGET_LANGUAGES <- c("en", "es", "fr", "de")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

as_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x[[1]])
}

collapse_chr <- function(x, n_max = 5) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  paste(head(x, n_max), collapse = "; ")
}

invert_abstract <- function(inv_idx) {
  if (is.null(inv_idx) || length(inv_idx) == 0) return(NA_character_)
  tokens <- names(inv_idx)
  if (is.null(tokens) || length(tokens) == 0) return(NA_character_)

  positions <- unlist(inv_idx, use.names = FALSE)
  words <- rep(tokens, lengths(inv_idx))

  ord <- order(positions)
  paste(words[ord], collapse = " ")
}

safe_get <- function(url) {
  req <- request(url) |>
    req_user_agent(USER_AGENT)

  tryCatch(
    {
      resp <- req_perform(req)
      list(ok = TRUE, response = resp, error_message = NA_character_)
    },
    error = function(e) {
      list(ok = FALSE, response = NULL, error_message = conditionMessage(e))
    }
  )
}

extract_authors <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) return(NA_character_)
  names <- map_chr(authorships, function(a) {
    a$author$display_name %||% NA_character_
  })
  collapse_chr(names, n_max = 8)
}

extract_journal <- function(primary_location) {
  if (is.null(primary_location)) return(NA_character_)
  source <- primary_location$source
  if (is.null(source)) return(NA_character_)
  source$display_name %||% NA_character_
}

resolve_project_dir <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  if (file.exists(file.path(cwd, "plant_body_mass_scaling_comprehensive.Rmd"))) {
    return(cwd)
  }

  candidate <- file.path(cwd, "plant_body_mass_scaling_project")
  if (file.exists(file.path(candidate, "plant_body_mass_scaling_comprehensive.Rmd"))) {
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }

  stop(
    "Could not resolve project root. Run from workspace root (VSCode) or plant_body_mass_scaling_project."
  )
}

query_families <- tribble(
  ~query_id, ~query_text,
  "Q01", "plant allometry biomass diameter height scaling exponent",
  "Q02", "tree allometry dbh biomass equation log-log slope",
  "Q03", "leaf mass area allometry plant scaling relationship",
  "Q04", "root shoot allometry plant biomass partitioning",
  "Q05", "pipe model sapwood leaf area allometry",
  "Q06", "hydraulic allometry conduit diameter tapering plant",
  "Q07", "self-thinning -3/2 rule plant stand biomass density",
  "Q08", "metabolic scaling theory plants quarter-power",
  "Q09", "crown allometry stem diameter tree architecture",
  "Q10", "reproductive allocation allometry seed mass plant size",
  "Q11", "elastic similarity geometric similarity plant height diameter",
  "Q12", "corner rules leaf twig allometry",
  "Q13", "planta alometria biomasa diametro altura exponente de escala",
  "Q14", "plante allometrie biomasse diametre hauteur exposant",
  "Q15", "pflanze allometrie biomasse durchmesser hoehe skalierung"
)

pattern_dictionary <- tribble(
  ~pattern_group, ~regex,
  "general_allometry", "\\ballometr(y|ic|ies)\\b",
  "power_law", "\\bpower\\s*law|\\bscal(ing|ed)\\s*exponent|\\bquarter[- ]power\\b",
  "dbh_biomass", "\\b(dbh|diameter at breast height|stem diameter)\\b.*\\bbiomass\\b|\\bbiomass\\b.*\\b(dbh|diameter)\\b",
  "height_diameter", "\\bheight\\b.*\\bdiameter\\b|\\bdiameter\\b.*\\bheight\\b",
  "leaf_mass_area", "\\bleaf\\s*mass\\s*per\\s*area\\b|\\bLMA\\b|\\bSLA\\b",
  "root_shoot", "\\broot[: ]?shoot\\b|\\broot\\b.*\\bshoot\\b|\\bshoot\\b.*\\broot\\b",
  "pipe_model", "\\bpipe model\\b|\\bsapwood\\b.*\\bleaf area\\b",
  "hydraulic_allometry", "\\bhydraulic\\b.*\\ballometr|\\bconduit\\b.*\\bdiameter\\b|\\bvessel\\b.*\\btaper",
  "self_thinning", "\\bself[- ]thinning\\b|\\b-?3/2\\s*rule\\b",
  "metabolic_scaling", "\\bmetabolic scaling\\b|\\bMST\\b|\\bWest[, ]+Brown[, ]+Enquist\\b",
  "corner_rules", "\\bCorner'?s? rules?\\b|\\btwig\\b.*\\bleaf\\b",
  "methods_signal", "\\bstandardized major axis\\b|\\bSMA\\b|\\bRMA\\b|\\blog[- ]log\\b",
  "empirical_signal", "\\bfield study\\b|\\bexperiment\\b|\\bsample size\\b|\\bn\\s*=\\s*[0-9]+"
)

high_specificity_groups <- c(
  "general_allometry",
  "dbh_biomass",
  "height_diameter",
  "leaf_mass_area",
  "root_shoot",
  "pipe_model",
  "hydraulic_allometry",
  "self_thinning",
  "corner_rules"
)

project_dir <- resolve_project_dir()
out_dir <- file.path(project_dir, "deep_literature_search", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Starting deep abstract search via OpenAlex...")

fetch_openalex <- function(query_id, query_text, per_page = PER_PAGE, max_pages = MAX_PAGES) {
  cursor <- "*"
  page <- 1
  out <- list()
  diagnostics <- list()
  terminal_reason <- "max_pages_reached"

  while (page <= max_pages) {
    request_url <- paste0(
      API_BASE,
      "?search=", URLencode(query_text, reserved = TRUE),
      "&filter=", URLencode(API_FILTERS, reserved = TRUE),
      "&per-page=", per_page,
      "&cursor=", URLencode(cursor, reserved = TRUE)
    )

    t_start <- Sys.time()
    resp_obj <- safe_get(request_url)
    elapsed_sec <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

    if (!isTRUE(resp_obj$ok)) {
      diagnostics[[length(diagnostics) + 1]] <- tibble(
        query_id = query_id,
        query_text = query_text,
        page_index = page,
        request_url = request_url,
        request_status = "request_error",
        http_status = NA_integer_,
        records_returned = 0L,
        has_next_cursor = NA,
        elapsed_sec = elapsed_sec,
        error_message = resp_obj$error_message %||% "Unknown request error"
      )
      terminal_reason <- "request_error"
      break
    }

    resp <- resp_obj$response
    status_code <- resp_status(resp)
    if (status_code >= 400) {
      diagnostics[[length(diagnostics) + 1]] <- tibble(
        query_id = query_id,
        query_text = query_text,
        page_index = page,
        request_url = request_url,
        request_status = "http_error",
        http_status = status_code,
        records_returned = 0L,
        has_next_cursor = NA,
        elapsed_sec = elapsed_sec,
        error_message = paste0("HTTP status ", status_code)
      )
      terminal_reason <- "http_error"
      break
    }

    txt <- resp_body_string(resp)
    parsed <- tryCatch(
      fromJSON(txt, simplifyVector = FALSE),
      error = function(e) e
    )

    if (inherits(parsed, "error")) {
      diagnostics[[length(diagnostics) + 1]] <- tibble(
        query_id = query_id,
        query_text = query_text,
        page_index = page,
        request_url = request_url,
        request_status = "parse_error",
        http_status = status_code,
        records_returned = 0L,
        has_next_cursor = NA,
        elapsed_sec = elapsed_sec,
        error_message = conditionMessage(parsed)
      )
      terminal_reason <- "parse_error"
      break
    }

    results <- parsed$results %||% list()
    next_cursor <- parsed$meta$next_cursor %||% NA_character_

    diagnostics[[length(diagnostics) + 1]] <- tibble(
      query_id = query_id,
      query_text = query_text,
      page_index = page,
      request_url = request_url,
      request_status = "ok",
      http_status = status_code,
      records_returned = as.integer(length(results)),
      has_next_cursor = !is.na(next_cursor) && nzchar(next_cursor),
      elapsed_sec = elapsed_sec,
      error_message = NA_character_
    )

    if (length(results) == 0) {
      terminal_reason <- "empty_page"
      break
    }

    out[[length(out) + 1]] <- results

    if (is.na(next_cursor) || !nzchar(next_cursor)) {
      terminal_reason <- "no_next_cursor"
      break
    }

    cursor <- next_cursor
    page <- page + 1
    Sys.sleep(REQUEST_PAUSE_SEC)
  }

  diag_tbl <- if (length(diagnostics) == 0) {
    tibble(
      query_id = query_id,
      query_text = query_text,
      page_index = integer(),
      request_url = character(),
      request_status = character(),
      http_status = integer(),
      records_returned = integer(),
      has_next_cursor = logical(),
      elapsed_sec = double(),
      error_message = character()
    )
  } else {
    bind_rows(diagnostics)
  }

  summary_tbl <- diag_tbl |>
    summarise(
      query_id = first(query_id),
      query_text = first(query_text),
      pages_attempted = n(),
      pages_ok = sum(request_status == "ok", na.rm = TRUE),
      pages_failed = sum(request_status != "ok", na.rm = TRUE),
      records_fetched = sum(records_returned, na.rm = TRUE),
      failure_messages = collapse_chr(unique(error_message[!is.na(error_message)]), n_max = 10)
    ) |>
    mutate(terminal_reason = terminal_reason)

  if (length(out) == 0) {
    return(list(records = tibble(), diagnostics = diag_tbl, summary = summary_tbl))
  }

  works <- flatten(out)

  records <- tibble(
    query_id = query_id,
    query_text = query_text,
    openalex_id = map_chr(works, ~ as_chr(.x$id)),
    doi = map_chr(works, ~ as_chr(.x$doi)),
    title = map_chr(works, ~ as_chr(.x$display_name)),
    language = map_chr(works, ~ as_chr(.x$language)),
    publication_year = map_int(works, ~ as.integer(.x$publication_year %||% NA_integer_)),
    cited_by_count = map_int(works, ~ as.integer(.x$cited_by_count %||% 0L)),
    journal = map_chr(works, ~ extract_journal(.x$primary_location)),
    authors = map_chr(works, ~ extract_authors(.x$authorships)),
    abstract_text = map_chr(works, ~ invert_abstract(.x$abstract_inverted_index)),
    concepts = map_chr(works, ~ collapse_chr(map_chr(.x$concepts %||% list(), ~ as_chr(.x$display_name)), n_max = 10))
  )

  list(records = records, diagnostics = diag_tbl, summary = summary_tbl)
}

fetch_results <- pmap(query_families, fetch_openalex)
all_hits <- map_dfr(fetch_results, "records")
retrieval_diagnostics <- map_dfr(fetch_results, "diagnostics")
query_run_summary <- map_dfr(fetch_results, "summary")

if (nrow(all_hits) == 0) {
  stop("No records returned from OpenAlex. Try reducing query strictness or increasing max_pages.")
}

all_hits <- all_hits |>
  filter(language %in% TARGET_LANGUAGES) |>
  mutate(
    record_key = coalesce(doi, openalex_id, paste0(title, "_", publication_year)),
    text_for_mining = str_to_lower(paste(title, abstract_text, sep = " \n "))
  ) |>
  filter(!is.na(text_for_mining), nchar(text_for_mining) > 50)

pattern_hits <- crossing(
  all_hits |> select(record_key, query_id, query_text, openalex_id, doi, title, language, publication_year, cited_by_count, journal, authors, abstract_text, concepts, text_for_mining),
  pattern_dictionary
) |>
  mutate(is_hit = str_detect(text_for_mining, regex(regex, ignore_case = TRUE))) |>
  filter(is_hit) |>
  select(-is_hit)

ranked_raw <- pattern_hits |>
  group_by(record_key, openalex_id, doi, title, language, publication_year, cited_by_count, journal, authors, abstract_text, concepts) |>
  summarise(
    n_pattern_groups = n_distinct(pattern_group),
    n_high_specificity_groups = n_distinct(pattern_group[pattern_group %in% high_specificity_groups]),
    has_high_specificity = n_high_specificity_groups >= 1,
    hit_groups = paste(sort(unique(pattern_group)), collapse = "; "),
    supporting_queries = paste(sort(unique(query_id)), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(
    precision_gate_pass = has_high_specificity & n_pattern_groups >= 2,
    recency_score = pmax(0, publication_year - 1990),
    citation_score = log1p(cited_by_count),
    deep_relevance_score = n_pattern_groups * 3 + recency_score * 0.03 + citation_score
  ) |>
  arrange(desc(deep_relevance_score), desc(n_pattern_groups), desc(cited_by_count), desc(publication_year))

ranked <- ranked_raw |>
  filter(precision_gate_pass)

precision_rejections <- ranked_raw |>
  filter(!precision_gate_pass)

run_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)

search_log <- query_families |>
  left_join(query_run_summary, by = c("query_id", "query_text")) |>
  mutate(
    run_utc = run_utc,
    script_version = SCRIPT_VERSION,
    api_base = API_BASE,
    api_filters = API_FILTERS,
    per_page = PER_PAGE,
    max_pages = MAX_PAGES,
    request_pause_sec = REQUEST_PAUSE_SEC,
    user_agent = USER_AGENT,
    target_languages = paste(TARGET_LANGUAGES, collapse = ","),
    precision_rule = "keep if n_high_specificity_groups>=1 AND n_pattern_groups>=2"
  )

run_parameters <- tibble(
  run_utc = run_utc,
  script_version = SCRIPT_VERSION,
  project_dir = project_dir,
  api_base = API_BASE,
  api_filters = API_FILTERS,
  per_page = PER_PAGE,
  max_pages = MAX_PAGES,
  request_pause_sec = REQUEST_PAUSE_SEC,
  target_languages = paste(TARGET_LANGUAGES, collapse = ","),
  n_query_families = nrow(query_families),
  n_pattern_groups = nrow(pattern_dictionary),
  precision_rule = "keep if n_high_specificity_groups>=1 AND n_pattern_groups>=2",
  raw_candidate_n = nrow(all_hits),
  pattern_hit_rows_n = nrow(pattern_hits),
  ranked_pre_gate_n = nrow(ranked_raw),
  ranked_post_gate_n = nrow(ranked)
)

write_csv(all_hits |> select(-text_for_mining), file.path(out_dir, "openalex_abstract_candidates_raw.csv"))
write_csv(pattern_hits, file.path(out_dir, "openalex_pattern_hits_long.csv"))
write_csv(ranked_raw, file.path(out_dir, "openalex_deep_ranked_candidates_pre_gate.csv"))
write_csv(ranked, file.path(out_dir, "openalex_deep_ranked_candidates.csv"))
write_csv(precision_rejections, file.path(out_dir, "openalex_precision_gate_rejections.csv"))
write_csv(retrieval_diagnostics, file.path(out_dir, "openalex_retrieval_diagnostics.csv"))
write_csv(search_log, file.path(out_dir, "search_query_log.csv"))
write_csv(run_parameters, file.path(out_dir, "run_parameters.csv"))

message("Deep search complete.")
message("Raw candidates: ", nrow(all_hits))
message("Pattern hits: ", nrow(pattern_hits))
message("Ranked unique papers (pre-gate): ", nrow(ranked_raw))
message("Ranked unique papers (post-gate): ", nrow(ranked))
message("Queries with retrieval failures: ", sum(search_log$pages_failed > 0, na.rm = TRUE))
message("Outputs written to: ", out_dir)
