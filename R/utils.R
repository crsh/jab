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
