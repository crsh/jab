#' Jeffrey's approximate Bayes factor
#'
#' Generic function to compute Jeffrey's approximate Bayes factors for
#' results from frequentist Wald tests.
#'
#' @param x Frequentist analysis output
#' @param prior Function. Function to compute prior density at MLE.
#' @param test Character. Wald statistics to use for calculations.
#' @param ... Additional arguements to pass to `prior`.
#' @param ratio Character. Either `"01"` or `"10"` to define the
#'    direction of the evidence ratio.
#'
#' @return A numeric (vector of) Jeffrey's approximate Bayes factors.
#' @export
#'
#' @examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#'
#' lm.D9 <- lm(weight ~ group)
#' jab(lm.D9, prior = dcauchy, location = 0, scale = sqrt(2)/4)

jab <- function(x, ...) UseMethod("jab", x)

#' @rdname jab
#' @method jab default
#' @export

jab.default <- function(x, prior, ..., ratio = getOption("jab.ratio")) {
  x_class <- class(x)

  message("No method defined for objects of class `", x_class, "`. Attempting
  to use on `broom` to provide automagic result. This will be invalid if the
  test is not a Wald test (or closely approximates one), the results may be
  wildly inaccurate.")

  .tidy_jab(
    x
    , method = if (getOption("jab.use.p")) "p" else "w"
    , prior = prior
    , ...
    , names = "term"
    , ratio = ratio
  )
}

#' @rdname jab
#' @method jab htest
#' @export

jab.htest <- function(x, prior, ..., ratio = getOption("jab.ratio")) {
  .tidy_jab(
    x
    , method = if (getOption("jab.use.p")) "p" else "z"
    , prior = prior
    , ...
    , names = "term"
    , ratio = ratio
  )
}


#' @rdname jab
#' @method jab lm
#' @export

jab.lm <- function(x, prior, ..., ratio = getOption("jab.ratio")) {
  .tidy_jab(
    x
    , method = if (getOption("jab.use.p")) "p" else "z"
    , prior = prior
    , ...
    , names = "term"
    , ratio = ratio
  )
}


#' @rdname jab
#' @method jab glht
#' @export

jab.glht <- function(x, prior, ..., ratio = getOption("jab.ratio")) {
  .tidy_jab(
    x
    , method = if (getOption("jab.use.p")) "p" else "z"
    , prior = prior
    , ...
    , names = "contrast"
    , ratio = ratio
  )
}


#' @rdname jab
#' @method jab cocor.dep.groups.overlap
#' @export

jab.cocor.dep.groups.overlap <- function(
  x
  , test
  , prior
  , ...
  , ratio = getOption("jab.ratio")
) {
  assertthat::assert_that(is.function(prior))

  fisher_z_tests <- c(
    "dunn1969"
    , "steiger1980"
    , "meng1992"
    , "hittner2003"
  )

  res <- methods::slot(x, test)
  if (test %in% fisher_z_tests) {
    estimate <- atanh(x@r.jk) - atanh(x@r.jh)
  } else {
    estimate <- x@r.jk - x@r.jh
  }

  std_error <- estimate / res$statistic

  if (getOption("jab.use.p")) {
    jab01 <- .jab01(
      , p = res$p.value
      , g = prior(estimate, ...)
      , se = std_error
    )
  } else {
    jab01 <- .jab01(
      , z = res$statistic
      , g = prior(estimate, ...)
      , se = std_error
    )
  }
  

  .ratio(jab01, ratio)
}


#' @rdname jab
#' @method jab merMod
#' @export

jab.merMod <- function(x, prior, ..., ratio = getOption("jab.ratio")) {
  require_broom_mixed()
  assertthat::assert_that(is.function(prior))

  x_tidy <- broom.mixed::tidy(x) |>
    subset(effect == "fixed")

  if (getOption("jab.use.p")) {
    jab01 <- .jab01(
      , p = x_tidy$p.value
      , g = prior(x_tidy$estimate, ...)
      , se = x_tidy$std_error
    )
  } else {
    jab01 <- .jab01(
      , z = x_tidy$statistic
      , g = prior(x_tidy$estimate, ...)
      , se = x_tidy$std_error
    )
  }

  names(jab01) <- x_tidy$term

  .ratio(jab01, ratio)
}


#' @rdname jab
#' @method jab emmGrid
#' @export

jab.emmGrid <- function(x, prior, ..., ratio = getOption("jab.ratio")) {
  .tidy_jab(
    x
    , method = if (getOption("jab.use.p")) "p" else "z"
    , prior = prior
    , ...
    , names = NULL
    , ratio = ratio
  )
}


#' Jeffrey's approximate Bayes factor
#'
#' Piecewise approximation of Jeffrey's approximate Bayes factors for
#' results from frequentist Wald tests form p-values.
#'
#' @param x Numeric. *p*-value from frequentist Wald test.
#' @param n Numeric. Sample size.
# #' @param prior Function. Function to compute prior density at MLE.
# #' @param ... Additional arguements to pass to `prior`.
#' @param ratio Character. Either `"01"` or `"10"` to define the
#'    direction of the evidence ratio.
#' @details Bayes factors are calculated using the precise piecewise
#'    approximation method suggested by
#'    \insertCite{Wagenmakers2022;textual}{jab},
#' 
#'    \deqn{
#'    \text{JAB}_{01} = \begin{cases}
#'    3p \sqrt{n} \text{} & \text{if }p \leq .10 \\
#'    \frac{4}{3}p^{2/3} \sqrt{n} \text{} & \text{if }0.1 < p \leq .50 \\
#'    3p \sqrt{n} \text{} & \text{if }p > .50
#'    \end{cases}
#'    }
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @return A numeric (vector of) Jeffrey's approximate Bayes factors.
#' @export
#'
#' @examples
#' jab_p(x = 0.0495, n = 136)

jab_p <- function(x, n, ratio = getOption("jab.ratio")) {
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(n))

  # Piecewise approximation assuming A = 1
  p <- (x > 0 & x <= 0.1) * 3 * x +
    (x > 0.1 & x <= 0.5) * 4/3 * x^(2/3) +
    (x > 0.5 & x < 1) * x^(1/4)

  .ratio(p * sqrt(n), ratio)
}
