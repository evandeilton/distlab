#' Probability of an interval
#'
#' Computes \eqn{P(a < X \le b)} for a `dl_dist` object, with a choice of
#' open or closed ends. For continuous distributions the ends do not change
#' the result. For discrete distributions they decide which support points
#' are counted: `P(2 <= X < 5)` sums the mass at 2, 3 and 4.
#'
#' The difference of two CDF values is taken on the tail where it is
#' accurate: when \eqn{F(a) > 1/2} the upper tails \eqn{S(a) - S(b)} are
#' used, computed with `lower.tail = FALSE`, so upper-tail probabilities are
#' never formed as `1 - p`. Both terms are combined on the log scale, so
#' probabilities below the smallest double (about \eqn{10^{-308}}) are still
#' available with `log = TRUE`.
#'
#' Accuracy is still bounded by the subtraction of two CDF values: for a very
#' narrow interval where both values are of moderate size (near the median),
#' the relative error grows as the interval shrinks, roughly
#' \eqn{10^{-16} / P(a < X \le b)}, so an interval of probability
#' \eqn{10^{-7}} keeps about nine significant digits.
#'
#' @param dist A `dl_dist` object from [dl_dist()].
#' @param lower,upper Single numbers with `lower <= upper`; `-Inf` and `Inf`
#'   give one-sided probabilities.
#' @param lower_closed,upper_closed Logical; whether each end belongs to the
#'   interval. The default is the half-open interval \eqn{(a, b]}, the one
#'   given by \eqn{F(b) - F(a)}.
#' @param log Logical; if `TRUE`, the natural log of the probability.
#'
#' @returns A single number: the probability, or its log.
#'
#' @seealso [dl_interval()], [dl_cdf()]
#' @family engine
#' @export
#' @examples
#' g <- dl_dist("gamma", shape = 2, rate = 1)
#' dl_prob(g, 1.2, 3.4)
#' dl_prob(g, lower = 3)                  # P(X > 3), upper tail
#' b <- dl_dist("binomial", size = 10, prob = 0.5)
#' dl_prob(b, 2, 5)                       # P(2 < X <= 5)
#' dl_prob(b, 2, 5, lower_closed = TRUE, upper_closed = FALSE)  # P(2 <= X < 5)
#' # Far tails stay accurate on the log scale.
#' dl_prob(dl_dist("normal"), lower = 40, log = TRUE)
dl_prob <- function(dist, lower = -Inf, upper = Inf, lower_closed = FALSE,
                    upper_closed = TRUE, log = FALSE) {
  if (!inherits(dist, "dl_dist")) {
    stop("`dist` must be a dl_dist object; see dl_dist().", call. = FALSE)
  }
  for (arg in c("lower", "upper")) {
    v <- get(arg)
    if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
      stop(sprintf("`%s` must be a single number (not NA).", arg), call. = FALSE)
    }
  }
  for (arg in c("lower_closed", "upper_closed", "log")) {
    v <- get(arg)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) {
      stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
    }
  }
  if (lower > upper) {
    stop("`lower` must not be greater than `upper`.", call. = FALSE)
  }
  ends <- prob_ends(dist, lower, upper, lower_closed, upper_closed)
  lp <- if (is.null(ends)) -Inf else log_cdf_diff(dist, ends[1], ends[2])
  if (log) lp else exp(lp)
}

# CDF arguments (lo, hi) such that P(event) = F(hi) - F(lo); NULL for an
# empty event. Discrete supports are whole numbers, so open and closed ends
# become integer cut points: P(L <= X <= U) = F(U) - F(L - 1).
prob_ends <- function(dist, lower, upper, lower_closed, upper_closed) {
  if (!identical(dist$entry$type, "discrete")) {
    if (lower == upper) return(NULL)
    return(c(lower, upper))
  }
  first <- if (lower_closed) ceiling(lower) else floor(lower) + 1
  last <- if (upper_closed) floor(upper) else ceiling(upper) - 1
  if (last < first) return(NULL)
  c(first - 1, last)
}

# log(F(hi) - F(lo)) for lo <= hi, from the tail where F(lo) is far from 1.
log_cdf_diff <- function(dist, lo, hi) {
  if (prob_upper_side(dist, lo, hi)) {
    la <- dl_cdf(dist, lo, lower.tail = FALSE, log.p = TRUE)
    lb <- dl_cdf(dist, hi, lower.tail = FALSE, log.p = TRUE)
  } else {
    la <- dl_cdf(dist, hi, log.p = TRUE)
    lb <- dl_cdf(dist, lo, log.p = TRUE)
  }
  log_diff_exp(la, lb)
}

# TRUE when F(hi) - F(lo) is better taken as S(lo) - S(hi): F(lo) is above
# one half, or hi is infinite (S(lo) alone, never 1 - F(lo)).
prob_upper_side <- function(dist, lo, hi) {
  if (lo == -Inf) return(hi == Inf)
  hi == Inf || dl_cdf(dist, lo) > 0.5
}

# log(exp(a) - exp(b)) for b <= a; rounding that puts b above a gives -Inf.
log_diff_exp <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA_real_)
  if (a == -Inf || b >= a) return(-Inf)
  if (b == -Inf) return(a)
  a + log1mexp(b - a)
}

# log(1 - exp(x)) for x < 0, accurate on both sides of -log(2).
log1mexp <- function(x) {
  if (x > -log(2)) log(-expm1(x)) else log1p(-exp(x))
}

#' Central interval with a given probability
#'
#' Finds the interval that leaves probability `(1 - prob) / 2` in each tail.
#' The upper end uses the upper-tail quantile (`lower.tail = FALSE`), so it
#' keeps precision when `prob` is close to 1. For discrete distributions the
#' ends are quantiles in the R sense (smallest `x` with \eqn{F(x) \ge p}), so
#' the closed interval `[lower, upper]` holds at least `prob`; its exact
#' probability is returned as `coverage`.
#'
#' @param dist A `dl_dist` object from [dl_dist()].
#' @param prob Probability of the interval, a single number in (0, 1).
#'
#' @returns A named numeric vector with `lower`, `upper` and `coverage`, the
#'   probability of the closed interval `[lower, upper]`.
#'
#' @seealso [dl_prob()], [dl_quantile()]
#' @family engine
#' @export
#' @examples
#' dl_interval(dl_dist("normal"), 0.95)
#' dl_interval(dl_dist("poisson", lambda = 4), 0.9)
dl_interval <- function(dist, prob) {
  if (!inherits(dist, "dl_dist")) {
    stop("`dist` must be a dl_dist object; see dl_dist().", call. = FALSE)
  }
  if (!is.numeric(prob) || length(prob) != 1L || is.na(prob) ||
      !(prob > 0 && prob < 1)) {
    stop("`prob` must be a single number in (0, 1).", call. = FALSE)
  }
  tail <- (1 - prob) / 2
  lo <- dl_quantile(dist, tail)
  hi <- dl_quantile(dist, tail, lower.tail = FALSE)
  coverage <- if (identical(dist$entry$type, "discrete")) {
    dl_prob(dist, lo, hi, lower_closed = TRUE, upper_closed = TRUE)
  } else {
    prob
  }
  c(lower = lo, upper = hi, coverage = coverage)
}
