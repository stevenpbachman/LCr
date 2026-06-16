# Transform SIS export tables and occurrence data into per-species JSON files

Transform SIS export tables and occurrence data into per-species JSON
files

## Usage

``` r
make_json(sis_files, occs, out_dir = "output", combined = FALSE)
```

## Arguments

- sis_files:

  Named list of data frames: allfields, assessments, countries,
  plantspecific, taxonomy

- occs:

  Data frame of occurrence records with columns: internal_taxon_id,
  sci_name, recordedby, recordno, day, month, event_year, dec_lat,
  dec_long, locality, coorduncert, dist_comm, basisofrec, min_elev,
  catalog_no, source, yrcompiled, citation, compiler, data_sens,
  sens_comm, presence, origin, seasonal, spatialref, latitude, longitude

- out_dir:

  Directory to write JSON files to (default: "output")

- combined:

  If TRUE, writes a single assessments.json containing all species as an
  array. If FALSE (default), writes one JSON file per species named by
  species_slug.

## Value

Invisibly returns a named list of all species JSON objects
