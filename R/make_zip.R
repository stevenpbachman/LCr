
#' Generate the SIS connect zip file
#'
#' @param csv_list (list) List of SIS csv files you want to zip
#' @param zip_filename (character) Defaults to "SIS_files.zip"
#'
#' @return Returns an SIS compliant zip file
#' @export

make_zip <- function(csv_list = list(sis_files),
                     zip_filename = "SIS_files.zip")
{
  temp_dir <- tempdir()
  written_files <- character()

  for (name in names(csv_list)) {
    file_path <- file.path(temp_dir, paste0(name, ".csv"))
    write.csv(csv_list[[name]], file_path, row.names = FALSE)
    written_files <- c(written_files, file_path)
  }

  # Save the zip in the current project directory, but only include the CSVs from temp_dir
  zip_path <- file.path(here::here(), zip_filename)
  #zip(zip_path, files = written_files, flags = "-j")  # -j strips directory paths inside the zip
  #file.remove(written_files)

  # Create zip silently using system2
  system2("zip",
          args = c("-j", shQuote(zip_path), shQuote(written_files)),
          stdout = NULL, stderr = NULL)

  file.remove(written_files)
  zip_path

  # little message to signal success and shop path to zip
  cli::cli_alert_success(paste0("ZIP file saved here: ", zip_path))

}
