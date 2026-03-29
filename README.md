# Plant Body-Mass Scaling Relationships: Comprehensive Working Catalog

GitHub repository description: Evidence-oriented catalog of plant body-mass scaling relationships across physiology, ecology, and evolution, with assumptions, confidence notes, and reproducible R Markdown output.

## Intention

This project is a living, evidence-oriented synthesis of plant allometric scaling relationships that link plant size (mass, diameter, height, or related proxies) to physiology, ecology, and evolution traits.

The goal is to provide a scientifically useful, analysis-ready starting point for biodiversity experts and ecologists, rather than a static one-off table.

## Core Research Question

What trait and life-history relationships have been measured to scale allometrically with plant size, and what are the assumptions and seminal sources behind each relationship?

## What Is Included

- A comprehensive table of plant size-scaling relationships with:
  - domain (physiology/ecology/evolution/cross-domain)
  - relationship definition
  - typical scaling form
  - first discovery year (commonly cited seminal source year)
  - key citations
  - plain-language explanation
  - core assumptions
- A second table with analysis-prioritization metadata:
  - evidence confidence
  - typical exponent range
  - typical data scope
  - analysis-use notes

## Current Files

- `plant_body_mass_scaling_comprehensive.Rmd`
  - Main source document with both tables.
- `plant_body_mass_scaling_comprehensive.html`
  - Rendered output for review/sharing.

## Workflow

1. Curate relationships from foundational and synthesis literature.
2. Standardize each entry into the table schema.
3. Add assumptions that must hold for interpretation.
4. Add confidence and exponent-range metadata for downstream analysis use.
5. Render to HTML for easy inspection and discussion.
6. Iterate by adding clade/biome/method-specific updates.

## Updated Search Protocol (Near-Exhaustive Target)

To support near-exhaustive coverage, run and log searches across:

- Web of Science
- Scopus
- BIOSIS
- CAB Abstracts
- AGRICOLA
- JSTOR
- Google Scholar (first 300 relevance-ranked records per query)
- Crossref/OpenAlex + forward/backward citation chaining

Minimum query families are included in the Rmd and cover:

- General plant allometry
- Physiology scaling
- Ecology and demography scaling
- Evolutionary trait-size scaling
- Statistical methods used for exponent estimation

For each search run, log:

- database/source
- query string
- run date (UTC)
- records returned
- records retained after deduplication/screening

## Inclusion/Exclusion and Taxonomy Quality Controls

- Include empirical plant studies with fitted scaling relationships or recoverable exponents.
- Exclude non-plant-only, purely theoretical-without-calibration, or metadata-insufficient records.
- Reconcile taxonomy with an explicit plant backbone version/date (for example POWO/WFO).
- Preserve verbatim taxon names and reconciled accepted names; never force unresolved names.

## Reproducibility

From the workspace root:

```bash
PATH="/opt/homebrew/bin:$PATH" Rscript -e 'rmarkdown::render("plant_body_mass_scaling_project/plant_body_mass_scaling_comprehensive.Rmd", output_file="plant_body_mass_scaling_comprehensive.html", output_dir="plant_body_mass_scaling_project", quiet=FALSE)'
```

## Scientific Use Guidance

- Treat this as a high-coverage working catalog, not a final exhaustive endpoint.
- Expect exponent variation by clade, biome, ontogeny, and model fitting method.
- Explicitly report assumptions and uncertainty when applying relationships in inference or projection.

## Next Steps

- Add a formal references section with DOI links/BibTeX keys.
- Add clade-specific and biome-specific rows.
- Add confidence intervals and sample-size metadata per relationship where available.
