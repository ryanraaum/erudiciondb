
create_table_functions <- list()

#' Read SQL schema file
#'
#' Reads a SQL schema file from inst/sql/common/ directory.
#'
#' @param table_name Name of the table (e.g., "persons", "items")
#' @return Character string containing SQL CREATE TABLE statement
#' @keywords internal
.read_schema_sql <- function(table_name) {
  # inst/ directory is mapped to system.file() after package installation
  sql_file <- system.file(
    "sql", "common", paste0(table_name, ".sql"),
    package = "erudiciondb",
    mustWork = TRUE
  )

  # Read entire file as single string
  sql <- readLines(sql_file, warn = FALSE)
  sql <- paste(sql, collapse = "\n")

  return(sql)
}

#' Create persons table
#'
#' Creates the persons table for focal scholars using SQL schema from
#' inst/sql/common/persons.sql. See CLAUDE.md schema section for field
#' descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Core fields: person_id (UUID), name components (primary/other given names,
#' surnames, particles, prefix, suffix), ASCII variants for matching, computed
#' referents for display/sorting, pronouns, status. All tables include revision,
#' stage, and created timestamp.
#'
#' @keywords internal
.create_persons_table <- function(con) {
  sql <- .read_schema_sql("persons")
  con |> DBI::dbExecute(sql)
}
create_table_functions$persons <- .create_persons_table


#' Create person_roles table
#'
#' Creates the person_roles table for tracking career history and affiliations
#' of focal persons using SQL schema from inst/sql/common/person_roles.sql.
#' See CLAUDE.md schema section for field descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Tracks position/affiliation over time with organizational hierarchy
#' (superorganization → organization → suborganization → division → subdivision)
#' and optional date ranges (start/end year/month/day).
#'
#' @keywords internal
.create_person_roles_table <- function(con) {
  sql <- .read_schema_sql("person_roles")
  con |> DBI::dbExecute(sql)
}
create_table_functions$person_roles <- .create_person_roles_table


#' Create item_persons table
#'
#' Creates the item_persons table for creators/contributors from imported items
#' using SQL schema from inst/sql/common/item_persons.sql. CSL-compliant schema.
#' See CLAUDE.md schema section for field descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Stores creator data in CSL format with optional person_id link to focal persons.
#' Fields: literal (institutional authors), family/given, particles, suffix,
#' comma_suffix, static_ordering (non-standard CSL: family-first languages),
#' parse_names. See https://github.com/citation-style-language/schema/pull/322
#'
#' @keywords internal
.create_item_persons_table <- function(con) {
  sql <- .read_schema_sql("item_persons")
  con |> DBI::dbExecute(sql)
}
create_table_functions$item_persons <- .create_item_persons_table


#' Create personlists table
#'
#' Creates the personlists table linking items to their item_persons by role type
#' using SQL schema from inst/sql/common/personlists.sql. See CLAUDE.md schema
#' section for field descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Junction table with personlist_type (author, editor, translator, etc. from
#' CSL schema). Valid types defined in valid_personlist_types constant.
#'
#' @keywords internal
.create_personlists_table <- function(con) {
  sql <- .read_schema_sql("personlists")
  con |> DBI::dbExecute(sql)
}
create_table_functions$personlists <- .create_personlists_table


#' Create affiliation_references table
#'
#' Creates the affiliation_references table for affiliations of item creators
#' using SQL schema from inst/sql/common/affiliation_references.sql. See
#' CLAUDE.md schema section for field descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Stores institutional affiliations for item_persons (creators from imported
#' items). Each item_person can have multiple affiliations (ordered by position).
#'
#' @keywords internal
.create_affiliation_references_table <- function(con) {
  sql <- .read_schema_sql("affiliation_references")
  con |> DBI::dbExecute(sql)
}
create_table_functions$affiliation_references <- .create_affiliation_references_table


#' Create items table
#'
#' Creates the items table for bibliography/citation records using SQL schema
#' from inst/sql/common/items.sql. CSL-compliant with ~70 fields. See CLAUDE.md
#' schema section for field descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Full CSL data item schema with snake_case field names. Includes identifiers
#' (DOI, PMID, PMCID, ISBN, etc.), dates, bibliographic metadata, and citation_key
#' for LaTeX/Markdown. Note: CSL `categories` field not included (unclear purpose,
#' no documentation found).
#'
#' @keywords internal
.create_items_table <- function(con) {
  sql <- .read_schema_sql("items")
  con |> DBI::dbExecute(sql)
}
create_table_functions$items <- .create_items_table


#' Create issues table
#'
#' Creates the issues table for data quality problem tracking using SQL schema
#' from inst/sql/common/issues.sql. See CLAUDE.md schema section for field
#' descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Tracks data quality problems (e.g., ambiguous person matches, missing data).
#' Fields: status (open/closed), object_type/object_id (what the issue relates to),
#' description.
#'
#' @keywords internal
.create_issues_table <- function(con) {
  sql <- .read_schema_sql("issues")
  con |> DBI::dbExecute(sql)
}
create_table_functions$issues <- .create_issues_table


#' Create person_identifiers table
#'
#' Creates the person_identifiers table for external IDs of focal persons
#' (ORCID, etc.) using SQL schema from inst/sql/common/person_identifiers.sql.
#' See CLAUDE.md schema section for field descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Links focal persons to external identifier systems. Fields: person_id,
#' id_type (e.g., "orcid"), id_value, id_value_uppercase (for case-insensitive
#' matching).
#'
#' @keywords internal
.create_person_identifiers_table <- function(con) {
  sql <- .read_schema_sql("person_identifiers")
  con |> DBI::dbExecute(sql)
}
create_table_functions$person_identifiers <- .create_person_identifiers_table

#' Create item_person_identifiers table
#'
#' Creates the item_person_identifiers table for external IDs of item creators
#' (non-focal persons) using SQL schema from inst/sql/common/item_person_identifiers.sql.
#' See CLAUDE.md schema section for field descriptions.
#'
#' @param con Database connection
#'
#' @return Number of rows affected (invisible)
#'
#' @details
#' Links item_persons to external identifier systems (e.g., ORCID from imported
#' bibliography data). Fields: item_person_id, id_type, id_value.
#'
#' @keywords internal
.create_item_person_identifiers_table <- function(con) {
  sql <- .read_schema_sql("item_person_identifiers")
  con |> DBI::dbExecute(sql)
}
create_table_functions$item_person_identifiers <- .create_item_person_identifiers_table


## Exported

#' Create erudiciondb tables
#'
#' @param con A DBI database connection
#' @param tables Which tables to create ("all" is default)
#'
#' @returns NULL
#' @export
#'
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' edb_create_tables(con)
#' DBI::dbDisconnect(con)
edb_create_tables <- function(con, tables="all") {
  if (tables == "all") {
    for (table in names(create_table_functions)) {
      create_table_functions[[table]](con)
    }
  } else {
    for (table in tables) {
      if (table %in% names(create_table_functions)) {
        create_table_functions[[table]](con)
      } else {
        warning(glue::glue("Unknown table '{table}' - passed over without creating"))
      }
    }
  }
}
