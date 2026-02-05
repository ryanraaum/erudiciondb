# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ErudicionDB is an R package providing a database schema and toolkit for storing, querying, and managing scholarly data (bibliography information and associated scholars). It implements a revision-tracking system with immutable audit trails and supports both SQLite and DuckDB backends.

## Development Commands

### Load and Test
```r
# Load package for development
devtools::load_all()

# Run full test suite (1193 tests)
devtools::test()

# Run tests for specific file
devtools::test_active_file()  # when file is open
testthat::test_file("tests/testthat/test-interface.R")
```

### Build and Check
```r
# Document package (update man/ files from roxygen comments)
devtools::document()

# Check package
devtools::check()

# Install locally
devtools::install()
```

## Architecture

### Database Schema

Nine core tables, all with `revision` (integer), `stage` (0=active, -1=inactive), and `created` (timestamp):

- **`items`**: Bibliography/citation records (CSL-compliant, ~70 fields)
- **`persons`**: Focal scholars (name components, ORCID, ASCII variants)
- **`personlists`**: Links persons to items with roles (author, editor, etc.)
- **`item_persons`**: Creators/contributors from imported items
- **`person_identifiers`**: External IDs for focal persons
- **`person_roles`**: Career history/affiliations for focal persons
- **`affiliation_references`**: Affiliations for item creators
- **`item_person_identifiers`**: IDs for non-focal item creators
- **`issues`**: Data quality problem tracking

### Code Organization

```
R/
├── interface.R          # Main public API (R6 ErudicionDB class) + private functions
├── create_tables.R      # Database schema (DDL)
├── validators.R         # Validation functions for objects
├── augmentors.R         # Data enrichment/transformation
├── helper.R             # Utility functions
└── erudiciondb-package.R # Package metadata
```

### Key Design Patterns

**1. Revision Tracking**
- Each object creates a new revision on update (never modifies in place)
- Only one revision per object has `stage=0` (active) at any time
- Old revisions have `stage=-1` (inactive) for audit trail
- All multi-step operations wrapped in database transactions

**CRITICAL**: When updating, `.destage_one()` MUST be called BEFORE inserting the new revision to prevent race conditions where two active revisions exist simultaneously (see R/interface.R:86-99).

**2. Validator-Augmentor Pattern**

`.new_object()` applies in order:
1. Validator function (raises errors if data invalid)
2. Augmentor function (enriches/transforms data - e.g., auto-generates citation keys, ASCII variants)

Custom validators/augmentors can be registered via `ErudicionDB$add_validator()` and `ErudicionDB$add_augmentor()`.

**3. Connection Pooling**

Uses `pool::dbPool()` for thread-safe connection management:
- `pool::poolWithTransaction()` for complex multi-step operations
- All database interactions go through the pool
- Supports both SQLite and DuckDB via DBI abstraction

**4. Smart Searching**

`.find()` uses hierarchical search strategies:

For items:
1. By identifier (DOI, PMID, PMCID)
2. By year/volume/first_page
3. By title (Jaro-Winkler similarity)
4. By associated person
5. Fallback to inner join on all fields

For persons:
1. ORCID exact match
2. Full name exact match (including ASCII variants)
3. Initials + surname match
4. Approximate name match (Jaro-Winkler distance < 0.05)

### Public API (ErudicionDB R6 Class)

```r
db <- ErudicionDB$new(list(drv=RSQLite::SQLite(), dbdir="path/to/db"))

# Core operations
db$new_object(object_type, object, stage=0)
db$insert_object(object)
db$insert_new_object(object_type, object, stage=0)  # Special handling for items with creators
db$retrieve(object_type, object_id, stage=NULL, revision=NULL, by=NULL, as_list=TRUE)
db$find(object_type, object_data, stage=0, revision=NULL)
db$match_person(person_data)

# Database access
db$tbl(tbl_name)  # Get dplyr table for custom queries
db$con            # Read-only pool access

# Extensibility
db$add_validator(for_what, validate_function)
db$add_augmentor(for_what, augment_function)
```

### Private Functions (Internal API)

Prefixed with `.` - used internally but not exported:

**Object Lifecycle:**
- `.new_object()`, `.insert_one()`, `.insert_new_object()`, `.revise_object()`, `.update_object()`

**Retrieval:**
- `.retrieve()`, `.find()`, `.make_revision_filter()`

**Staging/History:**
- `.next_revision()`, `.destage_one()`

**Person Matching:**
- `.match_person()`, `.find_person_identifier()`

**Item Lookup:**
- `.find_item_by_identifier()`, `.find_item_by_title()`, `.find_item_by_year_volume_page()`, `.find_item_by_person()`

**Bibliography Generation:**
- `.person_id_to_biblio_items()`, `.item_ids_to_biblio_items()`, `.items_to_biblio_items()`, `.biblio_items_to_csl_list()`, `.csl_list_to_json()`

## Common Issues and Solutions

### `.next_revision()` Returns -Inf

**Problem**: `max()` on empty vector returns `-Inf`.

**Solution**: Always check `length(revisions) > 0` before calling `max()` (R/interface.R:42-54).

### Race Condition in Updates

**Problem**: If new revision inserted before old one destaged, two active revisions exist.

**Solution**: In `.update_object()`, call `.destage_one()` BEFORE `.insert_one()` (R/interface.R:90-92).

### Variable Shadowing in dplyr Pipes

**Problem**: Bare column names in dplyr can conflict with function parameters.

**Solution**: Use `.data$column_name` or `!!rlang::sym(column_name)` instead of bare names.

### Function Parameter Names

Functions use `revision` parameter, not `rev` (though R's partial matching may hide this in some cases).

## Testing Strategy

- 1193 tests across 5 test files
- Tests run against both SQLite and DuckDB backends
- Tests organized by component: augmentors, create_tables, helper, interface, validators
- Always run full test suite after fixing bugs: `devtools::test()`

## Dependencies

**Database**: DBI, RSQLite, duckdb, pool, dbplyr
**Data manipulation**: dplyr, purrr, tibble
**String processing**: stringr, stringi, stringdist, stopwords
**Validation**: assertthat
**Utilities**: glue, jsonlite, lubridate, uuid, rlang, withr, aidr
