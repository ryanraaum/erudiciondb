# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ErudicionDB is an R package providing a database schema and toolkit for storing, querying, and managing scholarly data (bibliography information and associated scholars). It implements a revision-tracking system with immutable audit trails and supports both SQLite and DuckDB backends.

## Development Commands

### Project Rules & Configuration

- **Environment**: Windows
- **R Executable Path**: `C:\\PROGRA~1\\R\\R-44~1.1\\bin\\Rscript.exe` (Use short path names to avoid spaces issues, or use forward slashes/escaped backslashes)
- **Test Command**: Always use `devtools::test()` to run tests.
- **Run Tests Procedure**: To run tests, execute the following command in the terminal:
  `"C:\\PROGRA~1\\R\\R-44~1.1\\bin\\Rscript.exe" -e "devtools::test()"`

#### Rules
- When asked to run tests, do NOT use just `R` or `devtools::test()`. Use the full path defined above.
- If tests fail, analyze the output and fix the code.

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

### Database Constraints

All 9 tables have the following database-level constraints for data integrity:

**Universal Constraints (All Tables)**
- `PRIMARY KEY (object_id, revision)` - Ensures uniqueness of each revision
- `NOT NULL` on `object_id`, `revision`, `stage`, `created` - Required fields
- `CHECK (stage IN (0, -1))` - Enforces valid stage values
- `CHECK (revision >= 1)` - Ensures positive revision numbers

**Table-Specific Constraints**
- **persons**: `CHECK` constraint requiring at least one name field (primary_given_names, other_given_names, or surnames) to be present
- **personlists**: `CHECK` constraint on `personlist_type` limiting to 24 valid CSL creator types (author, editor, translator, etc.)

**Important Design Notes**
- **No UNIQUE constraint on object_id**: This would prevent multiple revisions of the same object. The revision tracking system allows multiple rows with the same object_id but different revision numbers.
- **No UNIQUE constraint on citation_key**: This would prevent updating items while keeping the same citation key. Application-level validation prevents duplicate active citation keys.
- **No foreign key constraints**: Child tables (personlists, item_persons, etc.) store parent object_ids without database-level FK constraints. The revision tracking design makes FK enforcement complex, as child records reference object_ids without revision numbers. Referential integrity is enforced at the application level.
- **DuckDB limitation**: DuckDB does not support partial indexes (`WHERE` clauses on indexes). For SQLite, consider adding partial unique indexes manually: `CREATE UNIQUE INDEX idx_{table}_active_revision ON {table}(object_id) WHERE stage = 0` to prevent two active revisions of the same object at the database level.

### Code Organization

```
R/
├── interface.R          # Main public API (R6 ErudicionDB class) + private functions
├── create_tables.R      # Database schema (DDL)
├── validators.R         # Validation functions for objects
├── augmentors.R         # Data enrichment/transformation
├── helper.R             # Utility functions
└── erudiciondb-package.R # Package metadata

inst/
└── sql/
    └── common/          # SQL schema files (SQLite/DuckDB compatible)
        ├── persons.sql
        ├── person_roles.sql
        ├── person_identifiers.sql
        ├── items.sql
        ├── personlists.sql
        ├── item_persons.sql
        ├── item_person_identifiers.sql
        ├── affiliation_references.sql
        └── issues.sql
```

### Database Schema Files

SQL table definitions are stored in `inst/sql/common/`:
- **One file per table** (9 files total) for clarity and maintainability
- **Pure SQL DDL** (no R code) - can be used by database tools
- **Identical for SQLite and DuckDB** (standard SQL syntax)
- **Loaded at runtime** by `R/create_tables.R` functions via `system.file()`
- **Easy to review** in git diffs and text editors with SQL syntax highlighting

This separation of concerns keeps SQL schemas visible and portable while R code handles orchestration.

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

**Solution**: Always check `length(revisions) > 0` before calling `max()` (R/interface.R:95-107). Code now includes check and throws error if object not found.

### Race Condition in Updates

**Problem**: If new revision inserted before old one destaged, two active revisions exist.

**Solution**: In `.update_object()`, call `.destage_one()` BEFORE `.insert_one()` (R/interface.R:191-206). Code correctly destages old revision first within a transaction.

### Variable Shadowing in dplyr Pipes

**Problem**: Bare column names in dplyr can conflict with function parameters.

**Solution**: Use `.data$column_name` or `!!rlang::sym(column_name)` instead of bare names.

### Function Parameter Names

Functions use `revision` parameter, not `rev` (though R's partial matching may hide this in some cases).

## Testing Strategy

- 2344 tests across 5 test files
- Tests run against both SQLite and DuckDB backends
- Tests organized by component: augmentors, create_tables, helper, interface, validators
- Always run full test suite after fixing bugs: `devtools::test()`

## Dependencies

**Database**: DBI, RSQLite, duckdb, pool, dbplyr
**Data manipulation**: dplyr, purrr, tibble
**String processing**: stringr, stringi, stringdist, stopwords
**Validation**: assertthat
**Utilities**: glue, jsonlite, lubridate, uuid, rlang, withr, aidr
