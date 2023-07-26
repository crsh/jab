.onLoad <- function(libname, pkgname) { # nocov start
  op <- options()
  op_jab <- list(
    jab.ratio = "01"
    , jab.use.p = TRUE
  )

  toset <- !(names(op_jab) %in% names(op))
  if (any(toset)) options(op_jab[toset])

  invisible()
} # nocov end
