.jab01 <- function(g, se, w) {
  assertthat::is.number(g)
  assertthat::is.number(se)
  assertthat::is.number(w)

  exp(-0.5 * w) / sqrt(2 * pi) / g / se
}

.jab01.z <- function(..., z) {
  assertthat::is.number(z)

  .jab01(..., w = z^2)
}

.jab01.p <- function(..., p) {
  assertthat::is.number(p)

  .jab01(..., w = qchisq(1 -  p, df = 1))
}


.ratio <- function(x, ratio = getOption("jab.ratio")) {
  assertthat::is.number(x)
  assertthat::is.string(ratio)

  switch(
      ratio
      , "01" = x
      , "10" = 1 / x
      , stop("`ratio` must be either '01' or '10'.")
  )
}

require_broom_mixed <- function() {
  
  if(!isTRUE(require("broom.mixed"))) {
    stop("Processing objects of class '", class(x), "' requires the package `broom.mixed`.")
  } else {
    invisible(TRUE)
  }
}
