# Closed-form moments keyed by registry id. Each function takes the parameters
# by name and returns mean, variance, skewness and excess kurtosis.
.dl_moment_formulas <- list(
  normal = function(mean, sd) {
    c(mean = mean, variance = sd^2, skewness = 0, excess_kurtosis = 0)
  },
  gamma = function(shape, rate) {
    c(mean = shape / rate, variance = shape / rate^2,
      skewness = 2 / sqrt(shape), excess_kurtosis = 6 / shape)
  },
  poisson = function(lambda) {
    c(mean = lambda, variance = lambda,
      skewness = 1 / sqrt(lambda), excess_kurtosis = 1 / lambda)
  }
)

#' Moments of a distribution
#'
#' Returns the mean, variance, standard deviation, skewness and excess
#' kurtosis. Closed forms are used when the registry has them; otherwise the
#' moments are computed numerically, by adaptive integration for continuous
#' distributions or by summing the mass function over the support for
#' discrete ones.
#'
#' Numeric moments of discrete distributions sum the mass function up to the
#' quantile with upper-tail probability \eqn{10^{-15}}; very heavy tails or
#' supports wider than \eqn{10^7} values give `NA`.
#'
#' @param dist A `dl_dist` object.
#' @param method `"auto"` (closed form when available, numeric otherwise),
#'   `"exact"` or `"numeric"`.
#'
#' @returns A named numeric vector with elements `mean`, `variance`, `sd`,
#'   `skewness` and `excess_kurtosis`. A moment that cannot be computed is
#'   `NA`.
#'
#' @family engine
#' @export
#' @examples
#' g <- dl_dist("gamma", shape = 3, rate = 2)
#' dl_moments(g)
#' dl_moments(g, method = "numeric")
dl_moments <- function(dist, method = c("auto", "exact", "numeric")) {
  method <- match.arg(method)
  formula <- .dl_moment_formulas[[dist$id]]
  if (method == "exact" && is.null(formula)) {
    stop("No closed-form moments for ", dist$id, "; use method = \"numeric\".",
         call. = FALSE)
  }
  m <- if (method != "numeric" && !is.null(formula)) {
    do.call(formula, dist$params)
  } else {
    numeric_moments(dist)
  }
  out <- c(m["mean"], m["variance"], sd = sqrt(unname(m["variance"])),
           m["skewness"], m["excess_kurtosis"])
  # Degenerate ratios (0/0) give NaN; report every non-computable moment as NA.
  out[is.nan(out)] <- NA_real_
  out
}

# Mean, variance, skewness and excess kurtosis by integration or summation.
numeric_moments <- function(dist) {
  if (identical(dist$entry$type, "discrete")) {
    central <- discrete_central_moments(dist)
  } else {
    central <- continuous_central_moments(dist)
  }
  v <- central[2]
  c(mean = central[1], variance = v,
    skewness = central[3] / v^1.5, excess_kurtosis = central[4] / v^2 - 3)
}

# Integrate (x - c)^k f(x) over the support. Breakpoints at extreme and central
# quantiles make sure narrow peaks are sampled.
continuous_central_moments <- function(dist) {
  s <- dist$entry$support
  qs <- dl_quantile(dist, c(1e-10, 0.25, 0.5, 0.75))
  qs <- c(qs, dl_quantile(dist, 1e-10, lower.tail = FALSE))
  breaks <- unique(c(s$lower, qs[is.finite(qs)], s$upper))
  # Absolute tolerance scaled by the IQR^k, so tiny moments keep accuracy and
  # moments that are exactly zero (symmetric skewness) still converge.
  scale <- qs[4] - qs[2]
  integral <- function(g, k) {
    piece <- function(a, b) {
      stats::integrate(function(x) g(x) * dl_density(dist, x), a, b,
                       rel.tol = 1e-10, abs.tol = 1e-12 * scale^k,
                       subdivisions = 1000L)$value
    }
    tryCatch(sum(mapply(piece, utils::head(breaks, -1), breaks[-1])),
             error = function(e) NA_real_)
  }
  # A mass far from 1 means the integration missed part of the density.
  total <- integral(function(x) 1, 0)
  if (is.na(total) || abs(total - 1) > 1e-6) return(rep(NA_real_, 4))
  # Integrate around the median so a large location does not swamp a small
  # spread (cancellation in (x - mu)^k when mu carries relative error).
  med <- qs[3]
  mu <- med + integral(function(x) x - med, 1)
  if (is.na(mu)) return(rep(NA_real_, 4))
  c(mu, vapply(2:4, function(k) integral(function(x) (x - mu)^k, k), numeric(1)))
}

# Sum over the support up to a far upper quantile (tail mass below 1e-15).
discrete_central_moments <- function(dist) {
  lo <- max(dist$entry$support$lower, dl_quantile(dist, 0))
  hi <- dl_quantile(dist, 1e-15, lower.tail = FALSE)
  if (!is.finite(lo) || !is.finite(hi) || hi - lo > 1e7) {
    return(rep(NA_real_, 4))
  }
  x <- seq(lo, hi)
  w <- dl_density(dist, x)
  mu <- sum(x * w)
  c(mu, vapply(2:4, function(k) sum((x - mu)^k * w), numeric(1)))
}
