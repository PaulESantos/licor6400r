library(here)
library(janitor)
library(tidyverse)
source("R/utils.R")
# list internal data --------------------------------------------
files <- list.files("./inst/extdata", full.names = TRUE)
files
#read6400 <- function(file) {

file = files[1]

  raw_file<- readr::read_lines(file,
                          skip_empty_rows = TRUE)
  #raw_file
  # model cleaning
  model <- get_model(raw_file)
  model

  ff <- clean_licor(raw_file)
  ff1 <- ff[-c(1, 2, 4, 5)]
  ff1
  r_with_data <- grep("^[0-9]{1,}", ff1)
  data_inf <- ff1[r_with_data]
  data_inf
  meta_data <- ff1[-r_with_data]
  meta_data |>
    str_replace_all('\"', "")
  # get column names
  colnames_raw <- grep("hhmmss|HHMMSS", ff)
  colnames_raw
  col_names <- ff[colnames_raw] |>
               stringr::str_split("\t") |>
               unlist() |>
               stringr::str_replace_all('\"', "")
  col_names
  # delete in/ot or units rows
  metadata <- grep("^<", raw_file)

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
