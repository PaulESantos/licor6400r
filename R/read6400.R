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
#' @keywords Internal
read6400 <- function(file) {
  remarks_exp <- "\\d{2}:\\d{2}:\\d{2} \\d{1,}[A-Za-z]|Remark"
  checkmate::assert_file_exists(file)
  ff <- readr::read_lines(file, skip_empty_rows = TRUE)
  ff1 <- dplyr::tibble(value = ff) %>%
    dplyr::mutate(id = dplyr::row_number())

  colnames_row <- ff1 %>%
    dplyr::filter(stringr::str_detect(value,"HHMMSS"))

  colnames <-
    gsub('"', "", colnames_row$value %>%
           stringr::str_split("\t") %>%
           unlist() )

  data_rows <- function(x){
    rows <- x %>%
      stringr::str_split("\t") %>%
      unlist()

    length(rows[nchar(rows)>0])

  }
  const_data <- ff1 %>%
    dplyr::filter(stringr::str_detect(value,"Const=|Target|out|Coolers"))


  ff2 <- ff1 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(keeping = data_rows(value)) %>%
    dplyr::mutate(remark = dplyr::case_when(
      stringr::str_detect(value,
                          remarks_exp) == TRUE ~ value,
      TRUE ~ NA_character_
    ))
  ff3 <- ff2  %>%
    dplyr::filter(!id %in% c(colnames_row$id,
                             const_data$id)) %>%
    dplyr::ungroup() %>%
    tidyr::fill(remark, .direction = "down") %>%
    dplyr::filter(keeping == length(colnames)) %>%
    tidyr::separate(value, colnames, sep = "\t") %>%
    dplyr::mutate_all(~gsub('"', "", .)) %>%
    dplyr::select(remark, dplyr::everything(),-c(id, keeping)) %>%
    dplyr::mutate(remark = stringr::str_extract(remark, "\\d{1,}[A-Za-z]"))

  if(length(colnames)>30){
    return(ff3 %>%
             dplyr::mutate_at(dplyr::vars(-c(1, colnames[c(2, 30)])),
                              ~as.numeric(.)))
  }
  else{
    return(
      ff3 %>%
        dplyr::mutate_at(dplyr::vars(-c(1, colnames[2])),
                         ~as.numeric(.))
    )
  }

}
