% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_occs.R
\name{clean_occs}
\alias{clean_occs}
\title{Clean GBIF occurrences based on user-selected flags and/or specific gbifIDs}
\usage{
clean_occs(
  checked_occs,
  remove_flags = c("flag_no_coords", "flag_cc_capitals", "flag_cc_centroids",
    "flag_cc_institutions", "flag_cc_equal", "flag_cc_gbif", "flag_cc_zeros",
    "flag_cc_urb", "flag_high_uncertainty", "flag_outside_native"),
  remove_gbifids = NULL,
  gbifid_reason = NULL,
  keep_gbifids = NULL
)
}
\arguments{
\item{checked_occs}{Result from check_occs() function}

\item{remove_flags}{Vector of flag names to use for cleaning.
Set to NULL to ignore all automatic flags. Flag names must be: "flag_no_coords" Missing coordinates;
"flag_cc_capitals" Near country capitals; "flag_cc_centroids" Near country centroids;
"flag_cc_institutions" Near biodiversity institutions; "flag_cc_equal" Equal coordinates; "flag_cc_gbif"
GBIF headquarters; "flag_high_uncertainty" High coordinate uncertainty; "flag_outside_native" Outside native range}

\item{remove_gbifids}{Vector of gbifIDs to remove from the dataset, or a named list where
names are reasons and values are vectors of gbifIDs sharing that reason.
Example: list("identification uncertain" = c("1234", "5678"),
             "habitat mismatch" = c("9012"))}

\item{gbifid_reason}{Character string providing the default reason for manual gbifID removal
when remove_gbifids is a simple vector.}

\item{keep_gbifids}{Vector of gbifIDs to keep in the clean dataset even if they are flagged
as problematic by automatic flags. These records will override any flags.
Example: c("5101884416", "1252668978")}
}
\value{
A list containing:
  - clean_occs: Cleaned occurrences based on selected flags and/or gbifIDs
  - problem_occs: Occurrences that were filtered out with their flags/reasons
}
\description{
Clean GBIF occurrences based on user-selected flags and/or specific gbifIDs
}
