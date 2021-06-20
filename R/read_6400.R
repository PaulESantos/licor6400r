#' Reads data from LI-6400
#'
#' Read data from the plain text or txt files and organizes it into
#' a tibble to add this data into R workflow.
#'
#' @param file The name of the file
#'
#' @return Returns a tibble from raw Licor 6400 files. Current
#' support for Licor 6400  wit soil CO2 flux chamber configuration.
#'
#' @examples \dontrun{
#' read_6400("/path/filename")
#' }
#' @export

read_6400 <- function(file) {

  # Check file exists
  checkmate::assert_file_exists(file)

  # Determine if x is raw or excel
  if (stringr::str_detect(file, ".xlsx$")) {
    print("licor6400 does not currently read xlsx files. Use the plain text files.")

  }
  else if (stringr::str_detect(file, ".xls$")) {
    print("licor6400 does not currently read xls files. Use the plain text files.")

  }
  else{
    licor_tbl <- read6400(file)
  }

  #Return data
  return(licor_tbl)

}
