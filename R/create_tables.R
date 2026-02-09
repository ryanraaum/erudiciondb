
create_table_functions <- list()

#' Create persons table
#'
#' Creates the persons table for focal scholars. See CLAUDE.md schema section
#' for field descriptions.
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
  con |> DBI::dbExecute("CREATE TABLE persons (
                      person_id UUID NOT NULL,
                      primary_person_id UUID,
                      also_known_as BOOLEAN,
                      primary_given_names VARCHAR,
                      other_given_names VARCHAR,
                      ascii_given_names VARCHAR,
                      surnames VARCHAR,
                      ascii_surnames VARCHAR,
                      dropping_particle VARCHAR,
                      non_dropping_particle VARCHAR,
                      prefix VARCHAR,
                      suffix VARCHAR,
                      comma_suffix BOOLEAN,
                      short_referent VARCHAR,
                      long_referent VARCHAR,
                      sorting_referent VARCHAR,
                      name_in_origin_script VARCHAR,
                      subjective_pronoun VARCHAR,
                      objective_pronoun VARCHAR,
                      possessive_pronoun VARCHAR,
                      status VARCHAR,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (person_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1),
                      CHECK (
                        (primary_given_names IS NOT NULL AND primary_given_names != '') OR
                        (other_given_names IS NOT NULL AND other_given_names != '') OR
                        (surnames IS NOT NULL AND surnames != '')
                      )
  )")
}
create_table_functions$persons <- .create_persons_table


#' Create person_roles table
#'
#' Creates the person_roles table for tracking career history and affiliations
#' of focal persons. See CLAUDE.md schema section for field descriptions.
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
  con |> DBI::dbExecute("CREATE TABLE person_roles (
                      person_role_id UUID NOT NULL,
                      person_id UUID,
                      position VARCHAR,
                      superorganization VARCHAR,
                      organization VARCHAR,
                      suborganization VARCHAR,
                      division VARCHAR,
                      subdivision VARCHAR,
                      address VARCHAR,
                      city VARCHAR,
                      region VARCHAR,
                      country VARCHAR,
                      start_year INTEGER,
                      end_year INTEGER,
                      start_month INTEGER,
                      end_month INTEGER,
                      start_day INTEGER,
                      end_day INTEGER,
                      order_priority INTEGER,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (person_role_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1)
  )")

}
create_table_functions$person_roles <- .create_person_roles_table


#' Create item_persons table
#'
#' Creates the item_persons table for creators/contributors from imported items.
#' CSL-compliant schema. See CLAUDE.md schema section for field descriptions.
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
  con |> DBI::dbExecute("CREATE TABLE item_persons (
                      item_person_id UUID NOT NULL,
                      personlist_id UUID,
                      position INTEGER,
                      person_id UUID,
                      literal VARCHAR,
                      family VARCHAR,
                      given VARCHAR,
                      dropping_particle VARCHAR,
                      non_dropping_particle VARCHAR,
                      suffix VARCHAR,
                      comma_suffix BOOLEAN,
                      static_ordering BOOLEAN,
                      parse_names BOOLEAN,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (item_person_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1)
  )")
}
create_table_functions$item_persons <- .create_item_persons_table


#' Create personlists table
#'
#' Creates the personlists table linking items to their item_persons by role type.
#' See CLAUDE.md schema section for field descriptions.
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
  con |> DBI::dbExecute("CREATE TABLE personlists (
                      personlist_id UUID NOT NULL,
                      item_id UUID,
                      personlist_type VARCHAR,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (personlist_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1),
                      CHECK (personlist_type IN (
                        'author', 'editor', 'translator', 'director', 'performer', 'chair',
                        'organizer', 'collection-editor', 'compiler', 'composer', 'container-author',
                        'contributor', 'curator', 'editorial-director', 'executive-producer',
                        'guest', 'host', 'interviewer', 'illustrator', 'narrator', 'original-author',
                        'producer', 'recipient', 'reviewed-author', 'script-writer', 'series-creator'
                      ))
  )")
}
create_table_functions$personlists <- .create_personlists_table


#' Create affiliation_references table
#'
#' Creates the affiliation_references table for affiliations of item creators.
#' See CLAUDE.md schema section for field descriptions.
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
  con |> DBI::dbExecute("CREATE TABLE affiliation_references (
                      affiliation_reference_id UUID NOT NULL,
                      item_person_id UUID,
                      position INTEGER,
                      affiliation VARCHAR,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (affiliation_reference_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1)
  )")
}
create_table_functions$affiliation_references <- .create_affiliation_references_table


#' Create items table
#'
#' Creates the items table for bibliography/citation records. CSL-compliant with
#' ~70 fields. See CLAUDE.md schema section for field descriptions.
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
  # NOTE: there is a `categories` array entry in the CSL schema
  #       - no idea what it is for
  #       - can't find any documentation
  #       - not included here for now
  con |> DBI::dbExecute("CREATE TABLE items (
                      item_id UUID NOT NULL,
                      type VARCHAR,
                      citation_key VARCHAR,
                      language VARCHAR,
                      accessed DATE,
                      available_date DATE,
                      event_date DATE,
                      issued DATE,
                      original_date DATE,
                      submitted DATE,
                      abstract VARCHAR,
                      annote VARCHAR,
                      archive VARCHAR,
                      archive_collection VARCHAR,
                      archive_location VARCHAR,
                      archive_place VARCHAR,
                      authority VARCHAR,
                      call_number VARCHAR,
                      chapter_number VARCHAR,
                      citation_number VARCHAR,
                      citation_label VARCHAR,
                      collection_number VARCHAR,
                      collection_title VARCHAR,
                      container_title VARCHAR,
                      container_title_short VARCHAR,
                      dimensions VARCHAR,
                      division VARCHAR,
                      doi VARCHAR,
                      edition VARCHAR,
                      event_title VARCHAR,
                      event_place VARCHAR,
                      first_reference_note_number VARCHAR,
                      genre VARCHAR,
                      isbn VARCHAR,
                      issn VARCHAR,
                      issue VARCHAR,
                      jurisdiction VARCHAR,
                      keyword VARCHAR,
                      locator VARCHAR,
                      medium VARCHAR,
                      note VARCHAR,
                      number VARCHAR,
                      number_of_pages VARCHAR,
                      number_of_volumes VARCHAR,
                      original_publisher VARCHAR,
                      original_publisher_place VARCHAR,
                      original_title VARCHAR,
                      page VARCHAR,
                      page_first VARCHAR,
                      part VARCHAR,
                      part_title VARCHAR,
                      pmcid VARCHAR,
                      pmid VARCHAR,
                      printing VARCHAR,
                      publisher VARCHAR,
                      publisher_place VARCHAR,
                      refers VARCHAR,
                      reviewed_genre VARCHAR,
                      reviewed_title VARCHAR,
                      scale VARCHAR,
                      section VARCHAR,
                      source VARCHAR,
                      status VARCHAR,
                      supplement VARCHAR,
                      title VARCHAR,
                      title_short VARCHAR,
                      url VARCHAR,
                      version VARCHAR,
                      volume VARCHAR,
                      volume_title VARCHAR,
                      volume_title_short VARCHAR,
                      year_suffix VARCHAR,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (item_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1)
  )")
}
create_table_functions$items <- .create_items_table


#' Create issues table
#'
#' Creates the issues table for data quality problem tracking. See CLAUDE.md
#' schema section for field descriptions.
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
  con |> DBI::dbExecute("CREATE TABLE issues (
                      issue_id UUID NOT NULL,
                      status VARCHAR,
                      object_type VARCHAR,
                      object_id UUID,
                      description VARCHAR,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (issue_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1)
  )")
}
create_table_functions$issues <- .create_issues_table


#' Create person_identifiers table
#'
#' Creates the person_identifiers table for external IDs of focal persons
#' (ORCID, etc.). See CLAUDE.md schema section for field descriptions.
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
  con |> DBI::dbExecute("CREATE TABLE person_identifiers (
                      person_identifier_id UUID NOT NULL,
                      person_id UUID,
                      id_type VARCHAR,
                      id_value VARCHAR,
                      id_value_uppercase VARCHAR,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (person_identifier_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1)
  )")
}
create_table_functions$person_identifiers <- .create_person_identifiers_table

#' Create item_person_identifiers table
#'
#' Creates the item_person_identifiers table for external IDs of item creators
#' (non-focal persons). See CLAUDE.md schema section for field descriptions.
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
  con |> DBI::dbExecute("CREATE TABLE item_person_identifiers (
                      item_person_identifier_id UUID NOT NULL,
                      item_person_id UUID,
                      id_type VARCHAR,
                      id_value VARCHAR,
                      revision INTEGER NOT NULL,
                      stage INTEGER NOT NULL,
                      created TIMESTAMP DEFAULT current_timestamp NOT NULL,
                      PRIMARY KEY (item_person_identifier_id, revision),
                      CHECK (stage IN (0, -1)),
                      CHECK (revision >= 1)
  )")
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
