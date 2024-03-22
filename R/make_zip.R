
#' Generate the SIS connect zip file
#'
#' @param csv_list (list) List of SIS csv files you want to zip
#' @param zip_filename (character) Defaults to "SIS_files.zip"
#'
#' @return Returns an SIS compliant zip file
#' @export

# make_zip <- function(csv_list = list(allfields = allfields,
#                                     assessments = assessments,
#                                     plantspecific = plantspecific,
#                                     countries = countries,
#                                     habitats = habitats,
#                                     taxonomy = taxonomy,
#                                     references = references,
#                                     credits = credits),
#                     zip_filename = "singlezip.zip") {

make_zip <- function(csv_list = list(sis_files),
                     zip_filename = "SIS_files.zip") {

  # Create a temporary directory
  temp_dir <- here::here()

  # temporarily save the csvs to the temp dir
  for (name in names(csv_list)) {
    write.csv(csv_list[[name]], file.path(temp_dir, paste0(name, ".csv")), row.names = FALSE)
  }

  # zip them
  zip(zip_filename, files = list.files(temp_dir, pattern = ".*\\.csv"))

  # Remove individual CSV files after zipping
  file.remove(list.files(temp_dir, pattern = ".*\\.csv"))

}
