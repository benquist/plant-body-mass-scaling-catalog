# Deep Literature Search (Abstract Mining)

This directory contains a second, independent literature-search pipeline that augments the original catalog workflow.

The original approach in `plant_body_mass_scaling_comprehensive.Rmd` is unchanged.

## Purpose

Use online abstract mining to detect plant allometry papers that may be missed by standard curated search strings. This deeper pass focuses on empirical allometric pattern signals in abstracts.

Current language scope in the search pipeline: English, Spanish, French, and German.

## What This Pipeline Does

1. Queries OpenAlex with multiple allometry-focused query families.
2. Collects article metadata and abstract text (when available) for `en`, `es`, `fr`, and `de` records.
3. Mines abstracts using a broad allometric pattern dictionary, including:
   - DBH-biomass equations
   - height-diameter scaling
   - root-shoot allometry
   - LMA/SLA and leaf-size scaling
   - pipe model and hydraulic allometry
   - self-thinning and metabolic scaling
   - Corner's rules and allometric methods terms (SMA/RMA/log-log)
4. Records retrieval diagnostics by query/page, including API failures.
5. Applies a precision gate before final ranking:
   - require at least one high-specificity allometry anchor
   - require at least two total detected pattern groups
6. Ranks candidate papers by multi-pattern support, recency, and citation count.
7. Writes reproducible CSV outputs for review and integration.

## Run

Canonical (from workspace root, `VSCode`):

```bash
Rscript plant_body_mass_scaling_project/deep_literature_search/scripts/deep_allometry_abstract_search.R
```

Alternative (from inside `plant_body_mass_scaling_project`):

```bash
Rscript deep_literature_search/scripts/deep_allometry_abstract_search.R
```

## Outputs

Generated in `deep_literature_search/output/`:

- `openalex_abstract_candidates_raw.csv`
- `openalex_pattern_hits_long.csv`
- `openalex_deep_ranked_candidates_pre_gate.csv`
- `openalex_deep_ranked_candidates.csv`
- `openalex_precision_gate_rejections.csv`
- `openalex_retrieval_diagnostics.csv`
- `search_query_log.csv`
- `run_parameters.csv`

Generated in `deep_literature_search/screening/` after queue creation:

- `deep_search_screening_queue.csv`
- `deep_search_screening_queue_summary.csv`

Generated in `deep_literature_search/` after summary render:

- `deep_search_results_summary.Rmd`
- `deep_search_results_summary.html`

## Build Screening Queue

After running the abstract search, create a structured screening table with manual-review and taxonomy-handoff fields:

Canonical (from workspace root):

```bash
Rscript plant_body_mass_scaling_project/deep_literature_search/scripts/build_screening_queue.R
```

Alternative (from inside `plant_body_mass_scaling_project`):

```bash
Rscript deep_literature_search/scripts/build_screening_queue.R
```

## Render Summary Report

Canonical (from workspace root):

```bash
Rscript -e "rmarkdown::render('plant_body_mass_scaling_project/deep_literature_search/deep_search_results_summary.Rmd', quiet = TRUE)"
```

Alternative (from inside `plant_body_mass_scaling_project`):

```bash
Rscript -e "rmarkdown::render('deep_literature_search/deep_search_results_summary.Rmd', quiet = TRUE)"
```

The summary report includes quantitative analyses such as:

- number of papers found by language (raw and post-gate)
- citation statistics (mean, median, quantiles)
- publication year and decade distributions
- query performance and capped-query diagnostics

## Taxonomy Handoff Requirements

Before adding deep-search candidates into the main catalog, attach taxonomy reconciliation fields for each accepted paper:

- reported_taxon_name
- accepted_taxon_name
- accepted_status (accepted/synonym/unresolved)
- backbone_source and backbone_version_date (for example POWO or WFO)
- backbone_id
- match_method and match_confidence
- reconciliation_timestamp_utc

## Integration Guidance

- Treat `openalex_deep_ranked_candidates.csv` as a high-priority screening list.
- Review top-ranked papers manually to confirm empirical fit quality and relevance.
- Add validated relationships/citations into the main catalog workflow.
- Keep this pipeline as an augmentation layer; do not replace the original curated approach.
