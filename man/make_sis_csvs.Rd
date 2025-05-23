% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_sis_csvs.R
\name{make_sis_csvs}
\alias{make_sis_csvs}
\title{Generate all SIS connect csv files}
\usage{
make_sis_csvs(
  unique_id,
  wcvp_ipni_id,
  first_name,
  second_name,
  email,
  institution,
  gbif_ref = NULL,
  powo_ref = FALSE,
  native_ranges = NULL,
  occs = NULL,
  family,
  genus,
  species,
  taxonomicAuthority,
  kingdom = "plantae"
)
}
\arguments{
\item{unique_id}{(character) Unique identifier - default is the GBIF usage key}

\item{first_name}{(character) First name of assessor}

\item{second_name}{(character) Second name of assessor}

\item{email}{(character) Email of assessor}

\item{institution}{(character) Name of institution or affiliation}

\item{gbif_ref}{(data frame) A GBIF download citation according to IUCN format.}

\item{powo_ref}{(data frame) A citation for use of POWO according to IUCN format.}

\item{native_ranges}{(data frame) Native ranges derived from [`get_native_range()`]}

\item{family}{(character) Field containing the family}

\item{genus}{(character) Field containing the genus}

\item{species}{(character) Field containing the specific epithet}

\item{taxonomicAuthority}{(character) Field containing the taxonomic authority}

\item{kingdom}{(character) Default is 'plantae', but can also be 'fungi'}
}
\value{
Returns an SIS compliant zip file
}
\description{
Generate all SIS connect csv files
}
