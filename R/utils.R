#' Get the model of licor machime
#'
#' @param x licor file
#'
#' @keywords Internal
get_model <- function(x) {
  licor_6400 <- grep("OPEN|$STARTOFDATA$", x)
  licor_6800 <- grep("\\[Header\\]|\\[Data\\]", x)
  if(length(licor_6400) > 0){
    return("Li-6400")
  }
  else if(length(licor_6800) > 0 ){
    return("Li-6800")
  }
}

#' Count character in a string
#' @param x licor file
#' @keywords Internal

rows_with_data <- function(x){
  rows <- x %>%
    stringr::str_split("\t") %>%
    unlist()

  length(rows[nchar(rows)>0])

}

#' Remove empty rows
#' @param x licor file
#' @keywords Internal
remove_emptyrows <- function(x) {
  e_rows <- grep('^\"\t\t|^\t\t', x)
  if(length(e_rows) > 0){
    file <- x[-e_rows]
  }
  else{
    file <- x
  }
  return(file)
}
#' Clean Licor files
#'
#' @param x licor file
#' @keywords Internal
#'
clean_licor <- function(x) {
  model <- get_model(x)
  if(model == "Li-6400"){
    in_out <- grep("^in\t", x)
    if(length(in_out) > 0 ){
      file <- x[-in_out]
    }
    else{
      file <- x
    }
    #delete empty rows
    #grep('^\\\"', ff)
    }
  else if(model == "Li-6800"){
    col_n <- grep("hhmmss|HHMMSS", x)
    x1 <-  gsub("\\t$","", x)
    file <- x1[-(col_n + 1)]
  }
  clean_data <- remove_emptyrows(file)
  meta_data <- grep("^<", clean_data)
  return(clean_data[-meta_data])
}

# Functions for handling remarks
#'
#' @param .s A string to be checked for remarks
#'
#' Detect remarks in LI-6800 raw data
#'
#' @name is_remark
#' @title is_remark
#'
#' @export

is_remark <- function(.s) {

  checkmate::assert_string(.s)
  stringr::str_detect(.s, '^\"\t[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}\\t.*|Remark')

}


#' Gets remarks attached to data, if any.
#'
#' @param x A licor object
#'
#' @return Returns the remarks present in the original file, if any, or null
#' if none exist. Remarks are listed with the row they come immediately before.
#'
#' @keywords Internal

get_remarks <- function(x) {
  y <- 0
  if (length(attributes(x)$remarks) >= 1) {
    for (i in row(attributes(x)$remarks)) {
      y[i] <- min(which(x$hhmmss_Sys > attributes(x)$remarks[[i, 1]]))
    }
    attributes(x)$remarks[3] <- y
    for (i in row(attributes(x)$remarks)) {
      y[i] <- max(which(x$hhmmss_Sys < attributes(x)$remarks[[i, 1]]))
    }
    attributes(x)$remarks[4] <- y
    attributes(attributes(x)$remarks)$names[3] <- "Before row"
    attributes(attributes(x)$remarks)$names[4] <- "After row"
    return(attributes(x)$remarks)
  } else {
    NULL
  }
}
