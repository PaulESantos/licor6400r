
.onAttach <- function(lib, pkg) {
  packageStartupMessage("This is licor6400 ",
                        utils::packageDescription("licor6400",
                                                  fields = "Version"
                        ),
                        appendLF = TRUE
  )
}


# -------------------------------------------------------------------------

show_progress <- function() {
  isTRUE(getOption("licor6400.show_progress")) && # user disables progress bar
    interactive() # Not actively knitting a document
}



.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_licor6400 <- list(
    ppendemic.show_progress = TRUE
  )
  to_set <- !(names(opt_licor6400) %in% names(opt))
  if (any(to_set)) options(opt_licor6400[to_set])
  invisible()
}
