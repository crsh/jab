.jab01_w_a_n <- function(w, a, n) {
  assertthat::assert_that(is.numeric(w))
  assertthat::assert_that(is.numeric(a))
  assertthat::assert_that(is.numeric(n))

  exp(-0.5 * w) * sqrt(n) * a
}

.jab01_w <- function(w, g, se) {
  assertthat::assert_that(is.numeric(w))
  assertthat::assert_that(is.numeric(g))
  assertthat::assert_that(is.numeric(se))

  exp(-0.5 * w) / sqrt(2 * pi) / g / se
}

.jab01 <- function(p = NULL, z = NULL, w = NULL, ...) {
  if (!is.null(p)) {
    assertthat::assert_that(is.numeric(p))
    w <- stats::qchisq(1 - p, df = 1)
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

.tidy_jab <- function(x, method, prior, ..., curvature = NULL, names = NULL, ratio = getOption("jab.ratio")) {
  assertthat::assert_that(is.function(prior))
  if(!is.null(curvature)) assertthat::assert_that(is.function(curvature))

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

  g <- curvature_g(
    , mle = x_tidy$estimate
    , prior = prior
    , se = x_tidy$std.error
    , curvature = curvature
    , ...
  )

  jab_args <- list(
    g = g
    , se = x_tidy$std.error
  )

  jab_method_arg <- switch(
    method
    , "w"     = list(w = x_tidy$statistic)
    , "z"     = list(z = x_tidy$statistic)
    , "p"     = list(p = x_tidy$p.value)
    , "adj.p" = list(p = x_tidy$adj.p.value)
    , stop("`method` must be either 'w', 'z', or 'p'.")
  )

  jab01 <- do.call(
    what = .jab01_w_a_n
    , args = c(jab_method_arg, jab_args)
  )

  if (!is.null(names)) {
    names(jab01) <- x_tidy[[names]]
  }

  .ratio(jab01, ratio)
}

curvature_g <- function(mle, prior, se = NULL, curvature = NULL, ...) {
  assertthat::assert_that(is.numeric(mle))
  assertthat::assert_that(is.function(prior))
  
  if(!is.null(curvature)) {
    assertthat::assert_that(is.function(curvature) || is.character(curvature))
    assertthat::assert_that(is.numeric(se))
  }

  g <- prior(x = mle, ...)

  if(!is.null(curvature)) {
    if(is.character(curvature) && curvature == "approx" && "Rdistance" %in% rownames(installed.packages())) {
      curvature <- function(x, ...) {
        Rdistance::secondDeriv(x = x, FUN = prior, ...) |>
          as.numeric()
      }
    }
  } else {
    curvature <- \(x, ...) 0
  }

  if(!is.null(se)) g <- g + se^2/2 * curvature(x = mle, ...)
  g
}

require_broom_mixed <- function() {
  
  if(!isTRUE(requireNamespace("broom.mixed"))) {
    stop("Processing objects of class '", class(x), "' requires the package `broom.mixed`.")
  } else {
    invisible(TRUE)
  }
}
