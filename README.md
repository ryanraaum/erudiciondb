
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erudiciondb

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/raaum/erudiciondb/workflows/R-CMD-check/badge.svg)](https://github.com/raaum/erudiciondb/actions)
<!-- badges: end -->

**erudiciondb** provides a database schema and toolkit for storing,
querying, and managing scholarly data (bibliography information and
associated scholars) with a built-in revision tracking system and
immutable audit trails.

## Key Features

- **Dual Backend Support**: Works with both SQLite and DuckDB
- **Revision Tracking**: Immutable audit trail - updates create new
  revisions rather than modifying in place
- **Smart Person Matching**: Hierarchical matching algorithm using
  ORCID, names, and fuzzy matching
- **Bibliography Generation**: Export to CSL JSON format for pandoc,
  LaTeX, and Markdown
- **CSL-Compliant Schema**: Full Citation Style Language data item
  support (~70 fields)
- **Flexible Validation**: Extensible validator and augmentor system

## Installation

You can install the development version of erudiciondb from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("raaum/erudiciondb")
```

## Quick Start

### 1. Create a Database

``` r
library(erudiciondb)

# SQLite in-memory database
db <- ErudicionDB$new(list(drv = RSQLite::SQLite(), dbdir = ":memory:"))

# Or DuckDB
# db <- ErudicionDB$new(list(drv = duckdb::duckdb(), dbdir = ":memory:"))

# Create tables
edb_create_tables(db$con)
```

### 2. Add Focal Persons

Add scholars you want to track in your database:

``` r
# Create a focal person
person <- list(
  primary_given_names = "Marie",
  surnames = "Curie",
  orcid = "0000-0001-2345-6789"
)

person_id <- db$insert_new_object("person", person)
```

The augmentor automatically generates:

- ASCII name variants (for matching)
- Display referents (short, long, sorting)
- Default flags

### 3. Add Bibliography Items

Import items with their creators:

``` r
# Item data from an online repository (e.g., via repo2cp package)
item_data <- list(
  item = list(
    type = "article-journal",
    title = "Recherches sur les substances radioactives",
    container_title = "Annales de chimie et de physique",
    volume = "8",
    page_first = "1",
    issued = as.Date("1903-01-01"),
    doi = "10.1000/example.doi",
    pmid = "12345678"
  ),
  author = list(
    list(family = "Curie", given = "Marie"),
    list(family = "Curie", given = "Pierre")
  )
)

# Insert item (automatically generates citation key, matches persons)
item_id <- db$insert_new_object("item", item_data)
```

The system automatically:

- Generates unique citation keys (e.g., `Curie1903Recherches`)
- Matches creators to focal persons (via `.match_person()`)
- Creates issues for ambiguous matches

### 4. Find Items

Multiple search strategies with automatic fallback:

``` r
# By DOI
found <- db$find("item", list(doi = "10.1000/example.doi"))

# By title (fuzzy matching)
found <- db$find("item", list(title = "Recherches sur les substances"))

# By year + volume + page
found <- db$find("item", list(
  issued = as.Date("1903-01-01"),
  volume = "8",
  page_first = "1"
))
```

### 5. Generate Bibliographies

Export to CSL JSON for use with pandoc:

``` r
# Get all items for a person
biblio_items <- .person_id_to_biblio_items(db$con, person_id)

# Convert to CSL list
csl_list <- .biblio_items_to_csl_list(biblio_items)

# Export to JSON
csl_json <- .csl_list_to_json(csl_list)

# Write to file
writeLines(csl_json, "bibliography.json")
```

Use in Markdown/LaTeX:

``` markdown
---
bibliography: bibliography.json
---

As described by @Curie1903Recherches...
```

## Core Concepts

### Revision Tracking

ErudicionDB uses an immutable audit trail system:

- **Stage**: `0` = active, `-1` = inactive
- **Revision**: Integer incremented with each update
- **Immutable History**: Old revisions preserved with `stage = -1`

``` r
# Retrieve active revision (default)
person <- db$retrieve("person", person_id, stage = 0, revision = "max")

# Retrieve all revisions
all_revisions <- db$retrieve("person", person_id, revision = "all", as_list = FALSE)

# Retrieve specific revision
old_version <- db$retrieve("person", person_id, revision = 2)
```

Updates create new revisions atomically:

1.  Destage old revision (`stage = -1`)
2.  Insert new revision (`stage = 0`)
3.  Both wrapped in database transaction

### Person Matching Algorithm

When importing items, creators are matched to focal persons using four
strategies:

1.  **ORCID exact match** (if available)
2.  **Full name exact match** (including ASCII variants)
3.  **Initials + surname match**
4.  **Approximate name match** (Jaro-Winkler distance \< 0.05)

``` r
# Match creator data to focal person
creator_data <- list(
  given = "Marie",
  family = "Curie",
  orcid = "0000-0001-2345-6789"
)

matched <- db$match_person(creator_data)
# Returns: person_id, similarity, found_by
```

### Database Schema

Nine core tables with revision tracking:

- **`items`**: Bibliography/citation records (CSL-compliant)
- **`persons`**: Focal scholars you're tracking
- **`personlists`**: Links persons to items by role (author, editor,
  etc.)
- **`item_persons`**: Creators/contributors from imported items
- **`person_identifiers`**: External IDs (ORCID, etc.) for focal
  persons
- **`person_roles`**: Career history/affiliations for focal persons
- **`affiliation_references`**: Affiliations for item creators
- **`item_person_identifiers`**: IDs for non-focal item creators
- **`issues`**: Data quality problem tracking

All tables include: `revision`, `stage`, `created` timestamp.

## Advanced Usage

### Custom Validators and Augmentors

``` r
# Add custom validation
db$add_validator("item", function(item) {
  if (is.na(item$title)) stop("Title required")
})

# Add custom augmentation
db$add_augmentor("item", function(item) {
  item$custom_field <- toupper(item$title)
  item
})
```

### Direct Database Access

``` r
# Use dplyr for custom queries
library(dplyr)

items_tbl <- db$tbl("items")
recent_items <- items_tbl %>%
  filter(stage == 0, issued >= "2020-01-01") %>%
  collect()

# Read-only connection access
con <- db$con
```

### Working with Both Backends

``` r
# Test against both backends
for (dbtype in c("sqlite", "duckdb")) {
  if (dbtype == "sqlite") {
    db <- ErudicionDB$new(list(drv = RSQLite::SQLite(), dbdir = ":memory:"))
  } else {
    db <- ErudicionDB$new(list(drv = duckdb::duckdb(), dbdir = ":memory:"))
  }

  edb_create_tables(db$con)
  # ... your code ...
  db$disconnect()
}
```

## Documentation

- Internal API documentation: All 54 private functions are documented
  with `?function_name`
- Package documentation: `?erudiciondb`
- See `CLAUDE.md` for architecture details and development guidelines

## Development

``` r
# Load package for development
devtools::load_all()

# Run tests (2344 tests across both backends)
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-interface.R")

# Check package
devtools::check()

# Update documentation
devtools::document()
```

## Citation

``` r
citation("erudiciondb")
```

## License

GPL (\>= 3)

## Author

Ryan Raaum (ryan.raaum@lehman.cuny.edu) ORCID:
[0000-0003-4723-5475](https://orcid.org/0000-0003-4723-5475)

------------------------------------------------------------------------

*Note: This package is in early development. The API may change.*
