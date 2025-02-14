
#' Calculate effective sample size for JAB
#'
#' For one- and two-sample t-tests this function determines the effective
#' N to be used in the JAB calculation.
#'
#' @param n1 Numeric. One or more sample sizes of the first sample.
#' @param n2 Numeric. One or more sample sizes of the second sample. If
#'    `NULL`, a one-sample t-test is assumed and the function returns `n1`.
#' @details For the two-sample t-test, the effective sample size $N$ is
#'    derived from the formula of the standard error and thus is
#'    $(n_1 \cdot n_2) / (n_1 + n_2)$.
#' @return One or more effective sample sizes to be used in the calculation
#'    of JAB.
#' @examples
#' sleep_ttest <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
#' n <- table(sleep$group)
#' jab_p(sleep_ttest$p.value, n = ess(n1 = n[1], n[2]))
#' @export

ess <- function(n1, n2 = NULL) {
  if(is.null(n2)) n2 <- 0
  os <- n2 == 0
  n1 * (n2 + os) / ((n1 + n2) - (n1 + n2 - 1) * os)
}

#' Calculate required effective sample size
#'
#' This function calculates the effective sample size required to achieve a
#' Bayes factor with a defined probability according to the $3p\sqrt{n}-rule
#' *assuming the null hypothesis is true*.
#'
#' @param x Numeric. Bayes factor in favor of the null hypothesis.
#' @param p Numeric. Probability of obtaining a Bayes factor of `x` or larger.
#' @details The function uses different formulas based on the value of `x` to calculate the effective sample size.
#' @return The effective sample size for the given `x` and `p`.
#' @examples
#' ess_3pn(3, 0.5)
#' ess_3pn(10, 0.7)
#' @export

ess_3pn <- function(x, p) {
  assertthat::assert_that(all(p > 0 & p < 1))
  assertthat::assert_that(all(x > 0))

  (p >= 0 & p <= 0.1) * ((x / (3 * p))^2) +
    (p > 0.1 & p <= 0.5) * ((3 * x)^2 / (16 * p^(4/3))) +
    (p > 0.5 & p <= 1) * (x^2 / sqrt(p))
}
