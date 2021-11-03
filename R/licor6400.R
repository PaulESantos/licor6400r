
.onAttach <- function(lib, pkg) {
  packageStartupMessage("This is licor6400r ",
                        utils::packageDescription("licor6400r",
                                                  fields = "Version"
                        ),
                        appendLF = TRUE
  )
}


# -------------------------------------------------------------------------

show_progress <- function() {
  isTRUE(getOption("licor6400r.show_progress")) && # user disables progress bar
    interactive() # Not actively knitting a document
}



.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_licor6400r <- list(
    ppendemic.show_progress = TRUE
  )
  to_set <- !(names(opt_licor6400r) %in% names(opt))
  if (any(to_set)) options(opt_licor6400r[to_set])
  invisible()
}
