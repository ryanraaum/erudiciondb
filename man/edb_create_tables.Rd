% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_tables.R
\name{edb_create_tables}
\alias{edb_create_tables}
\title{Create erudiciondb tables}
\usage{
edb_create_tables(con, tables = "all")
}
\arguments{
\item{con}{A DBI database connection}

\item{tables}{Which tables to create ("all" is default)}
}
\description{
Create erudiciondb tables
}
\examples{
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
edb_create_tables(con)
DBI::dbDisconnect(con)
}
