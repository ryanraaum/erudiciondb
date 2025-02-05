
create_table_functions <- list()

.create_persons_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE persons (
                      person_id UUID,
                      primary_person_id UUID,
                      also_known_as BOOLEAN,
                      primary_given_names VARCHAR,
                      other_given_names VARCHAR,
                      surnames VARCHAR,
                      surname_particle VARCHAR,
                      drop_particle BOOLEAN,
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
                      orcid VARCHAR,
                      scopus VARCHAR,
                      researcherid VARCHAR,
                      googlescholar VARCHAR,
                      revision INTEGER,
                      stage INTEGER,
                      updated TIMESTAMPTZ
  )")
}
create_table_functions$persons <- .create_persons_table


# division - department
# subdivision - group
.create_person_roles_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE person_roles (
                      person_role_id UUID,
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
                      revision INTEGER,
                      stage INTEGER,
                      updated TIMESTAMPTZ
  )")
}
create_table_functions$person_roles <- .create_person_roles_table


# CSL institution names new features information
# - https://github.com/citation-style-language/schema/pull/322
#
# `static_ordering` is a non-standard(?) feature to indicate if the family name should always be given first
.create_item_persons_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE item_persons (
                      item_person_id UUID,
                      personlist_id UUID,
                      position INTEGER,
                      person_id UUID,
                      orcid VARCHAR,
                      literal VARCHAR,
                      family VARCHAR,
                      given VARCHAR,
                      particle VARCHAR,
                      drop_particle BOOLEAN,
                      suffix VARCHAR,
                      comma_suffix BOOLEAN,
                      static_ordering BOOLEAN,
                      parse_names BOOLEAN,
                      revision INTEGER,
                      stage INTEGER,
                      updated TIMESTAMPTZ
  )")
}
create_table_functions$item_persons <- .create_item_persons_table


# valid list types are from the CSL data item schema
.create_personlists_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE personlists (
                      personlist_id UUID,
                      item_id UUID,
                      personlist_type VARCHAR,
                      revision INTEGER,
                      stage INTEGER,
                      updated TIMESTAMPTZ
  )")
}
create_table_functions$personlists <- .create_personlists_table


.create_affiliation_references_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE affiliation_references (
                      affiliation_reference_id UUID,
                      item_person_id UUID,
                      position INTEGER,
                      affiliation VARCHAR,
                      revision INTEGER,
                      stage INTEGER,
                      updated TIMESTAMPTZ
  )")
}
create_table_functions$affiliation_references <- .create_affiliation_references_table


.create_items_table <- function(con) {
  # NOTE: there is a `categories` array entry in the CSL schema
  #       - no idea what it is for
  #       - can't find any documentation
  #       - not included here for now
  con |> DBI::dbExecute("CREATE TABLE items (
                      item_id UUID,
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
                      DOI VARCHAR,
                      edition VARCHAR,
                      event_title VARCHAR,
                      event_place VARCHAR,
                      first_reference_note_number VARCHAR,
                      genre VARCHAR,
                      ISBN VARCHAR,
                      ISSN VARCHAR,
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
                      PMCID VARCHAR,
                      PMID VARCHAR,
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
                      URL VARCHAR,
                      version VARCHAR,
                      volume VARCHAR,
                      volume_title VARCHAR,
                      volume_title_short VARCHAR,
                      year_suffix VARCHAR,
                      revision INTEGER,
                      stage INTEGER,
                      updated TIMESTAMPTZ
  )")
}
create_table_functions$items <- .create_items_table


.create_issues_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE issues (
                      issue_id UUID,
                      status VARCHAR,
                      object_type VARCHAR,
                      object_id UUID,
                      description VARCHAR,
                      revision INTEGER,
                      stage INTEGER,
                      updated TIMESTAMPTZ
  )")
}
create_table_functions$issues <- .create_issues_table


.create_person_identifiers_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE person_identifiers (
                      person_identifier_id UUID,
                      person_id UUID,
                      id_type VARCHAR,
                      id_value VARCHAR,
                      revision INTEGER,
                      stage INTEGER,
                      updated TIMESTAMPTZ
  )")
}
create_table_functions$person_identifiers <- .create_person_identifiers_table


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
