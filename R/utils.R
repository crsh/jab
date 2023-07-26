.jab01_w <- function(w, g, se) {
  assertthat::assert_that(is.numeric(w))
  assertthat::assert_that(is.numeric(g))
  assertthat::assert_that(is.numeric(se))

  exp(-0.5 * w) / sqrt(2 * pi) / g / se
}

.jab01 <- function(p = NULL, z = NULL, w = NULL, ...) {
  if (!is.null(p)) {
    assertthat::assert_that(is.numeric(p))
    w <- stats::qchisq(1 -  p, df = 1)
  } else if (!is.null(z)) {
    assertthat::assert_that(is.numeric(z))
    w <- z^2
  } else if (!is.null(w)) {
    assertthat::assert_that(is.numeric(w))
  } else {
    stop("Either `p`, `z`, or `w` must be specified.")
  }

  .jab01_w(w = w, ...)
}

.ratio <- function(x, ratio = getOption("jab.ratio")) {
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(assertthat::is.string(ratio))

  switch(
      ratio
      , "01" = x
      , "10" = 1 / x
      , stop("`ratio` must be either '01' or '10'.")
  )
}

.tidy_jab <- function(x, method, prior, ..., names = NULL, ratio = getOption("jab.ratio")) {
  assertthat::assert_that(is.function(prior))

  x_tidy <- broom::tidy(x)

  if (!"std.error" %in% names(x_tidy)) {
    if ("estimate" %in% names(x_tidy)) {
      warning("Standard error not included in analysis output. Trying to recover from Wald test statistic.")
      x_tidy$std.error <- x_tidy$estimate / x_tidy$statistic
    } else {
      stop("Standard error not included in analysis output.")
    }
  }

  if (method == "p" && "adj.p.value" %in% names(x_tidy)) {
    method <- "adj.p"
  }

  jab01 <- switch(
    method
    , "w" = .jab01(
      w = x_tidy$statistic
      , g = prior(x_tidy$estimate, ...)
      , se = x_tidy$std.error
    )
    , "z" = .jab01(
      z = x_tidy$statistic
      , g = prior(x_tidy$estimate, ...)
      , se = x_tidy$std.error
    )
    , "p" = .jab01(
      p = x_tidy$p.value
      , g = prior(x_tidy$estimate, ...)
      , se = x_tidy$std.error
    )
    , "adj.p" = .jab01(
      p = x_tidy$adj.p.value
      , g = prior(x_tidy$estimate, ...)
      , se = x_tidy$std.error
    )
    , stop("`method` must be either 'w', 'z', or 'p'.")
  )

  if (!is.null(names)) {
    names(jab01) <- x_tidy[[names]]
  }

  .ratio(jab01, ratio)
}

require_broom_mixed <- function() {
  
  if(!isTRUE(requireNamespace("broom.mixed"))) {
    stop("Processing objects of class '", class(x), "' requires the package `broom.mixed`.")
  } else {
    invisible(TRUE)
  }
}
