library(tidyverse)
library(jsonlite)

#' Transform SIS export tables and occurrence data into per-species JSON files
#'
#' @param sis_files Named list of data frames: allfields, assessments, countries,
#'                  plantspecific, taxonomy
#' @param occs      Data frame of occurrence records with columns:
#'                  internal_taxon_id, sci_name, recordedby, recordno,
#'                  day, month, event_year, dec_lat, dec_long, locality,
#'                  coorduncert, dist_comm, basisofrec, min_elev, catalog_no,
#'                  source, yrcompiled, citation, compiler, data_sens,
#'                  sens_comm, presence, origin, seasonal, spatialref,
#'                  latitude, longitude
#' @param out_dir   Directory to write JSON files to (default: "output")
#' @param combined  If TRUE, writes a single assessments.json containing all
#'                  species as an array. If FALSE (default), writes one JSON
#'                  file per species named by species_slug.
#' @export
#'
#' @return Invisibly returns a named list of all species JSON objects
make_json <- function(sis_files, occs, out_dir = "output", combined = FALSE) {

  allfields    <- sis_files$allfields
  assessments  <- sis_files$assessments
  countries    <- sis_files$countries
  plantspecific <- sis_files$plantspecific
  taxonomy     <- sis_files$taxonomy

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # All tables share internal_taxon_id as the key
  taxon_ids <- unique(taxonomy$internal_taxon_id)
  message("Building JSON for ", length(taxon_ids), " taxa...")

  results <- purrr::map(taxon_ids, function(id) {

    # ── Taxonomy ──────────────────────────────────────────────────────────────
    tax <- taxonomy %>% filter(internal_taxon_id == id) %>% slice(1)

    species_name <- paste(tax$genus, tax$species)
    species_slug <- gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(species_name)))

    # ── Allfields (EOO, AOO, elevation) ───────────────────────────────────────
    af <- allfields %>% filter(internal_taxon_id == id) %>% slice(1)

    # ── Assessments (narratives, category, trend) ─────────────────────────────
    ass <- assessments %>% filter(internal_taxon_id == id) %>% slice(1)

    # ── Countries (one-to-many → list of {name, origin}) ─────────────────────
    ctry <- countries %>%
      filter(internal_taxon_id == id) %>%
      transmute(
        name   = countryoccurrence.countryoccurrencesubfield.countryoccurrencename,
        origin = CountryOccurrence.CountryOccurrenceSubfield.origin
      ) %>%
      # drop rows where both fields are blank
      filter(!is.na(name) | !is.na(origin))

    countries_list <- if (nrow(ctry) > 0) {
      purrr::map(seq_len(nrow(ctry)), function(i) {
        list(name = ctry$name[i], origin = ctry$origin[i])
      })
    } else {
      list()
    }

    # ── Plant growth form (one-to-many, collapse to single string) ────────────
    pgf <- plantspecific %>%
      filter(internal_taxon_id == id) %>%
      pull(PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName) %>%
      na.omit() %>%
      paste(collapse = ", ")

    plant_growth_form <- if (nchar(pgf) > 0) pgf else NA_character_

    # ── Occurrences ───────────────────────────────────────────────────────────
    occ_rows <- occs %>% filter(internal_taxon_id == id)

    occs_list <- if (nrow(occ_rows) > 0) {
      purrr::map(seq_len(nrow(occ_rows)), function(i) {
        row <- occ_rows[i, ]

        # Prefer dec_lat/dec_long; fall back to latitude/longitude
        lat <- if (!is.na(row$dec_lat)) row$dec_lat else row$latitude
        lng <- if (!is.na(row$dec_long)) row$dec_long else row$longitude

        out <- list(
          lat        = na_to_null(as.numeric(lat)),
          lng        = na_to_null(as.numeric(lng)),
          year       = na_to_null(as.integer(row$event_year)),
          basis      = na_to_null(row$basisofrec),
          locality   = na_to_null(row$locality),
          source     = na_to_null(row$source),
          recorded_by       = na_to_null(row$recordedby),
          record_number     = na_to_null(row$recordno),
          catalog_number    = na_to_null(row$catalog_no),
          coord_uncertainty = na_to_null(as.numeric(row$coorduncert)),
          presence          = na_to_null(row$presence),
          origin            = na_to_null(row$origin),
          seasonal          = na_to_null(row$seasonal),
          data_sensitive    = na_to_null(row$data_sens)
        )

        # Include date parts only when at least year is present
        if (!is.na(row$event_year)) {
          if (!is.na(row$month)) out$month <- as.integer(row$month)
          if (!is.na(row$day))   out$day   <- as.integer(row$day)
        }

        # Include dist_comm and sens_comm only when non-empty
        if (!is.null(na_to_null(row$dist_comm)))
          out$dist_comm <- row$dist_comm
        if (!is.null(na_to_null(row$sens_comm)))
          out$sens_comm <- row$sens_comm

        out
      })
    } else {
      list()
    }

    # ── Assemble JSON structure ───────────────────────────────────────────────
    species_json <- list(
      species_name = species_name,
      species_slug = species_slug,
      status       = "draft",
      data = list(

        # Taxonomy
        kingdom             = na_to_null(tax$kingdom),
        phylum              = na_to_null(tax$phylum),
        class               = na_to_null(tax$classname),
        order               = na_to_null(tax$ordername),
        family              = na_to_null(tax$family),
        genus               = na_to_null(tax$genus),
        species             = na_to_null(tax$species),
        taxonomic_authority = na_to_null(tax$taxonomicAuthority),

        # Assessment metadata
        red_list_category   = na_to_null(ass$RedListCriteria.manualCategory),
        assessment_date     = na_to_null(ass$RedListAssessmentDate.value),
        red_list_rationale  = na_to_null(ass$RedListRationale.value),

        # Range & geography
        range               = na_to_null(ass$RangeDocumentation.narrative),
        eoo                 = na_to_null(as.numeric(af$EOO.range)),
        aoo                 = na_to_null(as.numeric(af$AOO.range)),
        system              = na_to_null(ass$System.value),
        biogeographic_realm = na_to_null(ass$BiogeographicRealm.realm),
        countries           = countries_list,

        # Occurrences
        occurrences         = occs_list,

        # Population
        population          = na_to_null(ass$PopulationDocumentation.narrative),
        population_trend    = na_to_null(ass$PopulationTrend.value),

        # Habitat & ecology
        habitat             = na_to_null(ass$HabitatDocumentation.narrative),
        lower_elevation     = na_to_null(as.numeric(af$ElevationLower.limit)),
        upper_elevation     = na_to_null(as.numeric(af$ElevationUpper.limit)),
        plant_growth_form   = na_to_null(plant_growth_form),

        # Threats
        threats             = na_to_null(ass$ThreatsDocumentation.value)
      )
    )

    # Write individual JSON file (only when not in combined mode)
    if (!combined) {
      out_path <- file.path(out_dir, paste0(species_slug, ".json"))
      write_json(species_json, path = out_path, pretty = TRUE, auto_unbox = TRUE,
                 null = "null")
      message("  Written: ", out_path)
    }

    species_json
  })
  results <- setNames(results, taxon_ids)

  # Write combined file if requested
  if (combined) {
    out_path <- file.path(out_dir, "assessments.json")
    write_json(unname(results), path = out_path, pretty = TRUE,
               auto_unbox = TRUE, null = "null")
    message("Done. Combined file written to '", out_path, "' (",
            length(results), " taxa, ",
            round(file.size(out_path) / 1024, 1), " KB)")
  } else {
    message("Done. ", length(results), " JSON files written to '", out_dir, "/'")
  }

  invisible(results)
}

# ── Helper ────────────────────────────────────────────────────────────────────
# Convert NA/empty string to NULL so jsonlite writes JSON null rather than NA
na_to_null <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.na(x) || (is.character(x) && nchar(trimws(x)) == 0)) return(NULL)
  x
}
