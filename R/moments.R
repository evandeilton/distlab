# Closed-form moments keyed by registry id. Each function takes the parameters
# by name and returns mean, variance, skewness and excess kurtosis. A moment
# whose defining integral diverges to +Inf is Inf; one that has no value at all
# (the integral is undefined, or a lower moment already is) is NA.
.dl_moment_formulas <- list(
  bernoulli = function(prob) {
    q <- 1 - prob
    v <- prob * q
    c(mean = prob, variance = v, skewness = (q - prob) / sqrt(v),
      excess_kurtosis = (1 - 6 * v) / v)
  },
  binomial = function(size, prob) {
    q <- 1 - prob
    v <- size * prob * q
    c(mean = size * prob, variance = v, skewness = (q - prob) / sqrt(v),
      excess_kurtosis = (1 - 6 * prob * q) / v)
  },
  geometric = function(prob) {
    q <- 1 - prob
    c(mean = q / prob, variance = q / prob^2, skewness = (2 - prob) / sqrt(q),
      excess_kurtosis = 6 + prob^2 / q)
  },
  negbinomial = function(size, prob) {
    q <- 1 - prob
    c(mean = size * q / prob, variance = size * q / prob^2,
      skewness = (2 - prob) / sqrt(size * q),
      excess_kurtosis = 6 / size + prob^2 / (size * q))
  },
  poisson = function(lambda) {
    c(mean = lambda, variance = lambda,
      skewness = 1 / sqrt(lambda), excess_kurtosis = 1 / lambda)
  },
  hypergeometric = function(m, n, k) {
    N <- m + n
    # The skewness and kurtosis denominators vanish for N <= 3; such urns have
    # at most four outcomes, so their moments are summed exactly instead.
    if (N <= 3) {
      x <- seq(max(0, k - n), min(k, m))
      return(finite_sum_moments(x, stats::dhyper(x, m, n, k)))
    }
    v <- k * m * n * (N - k) / (N^2 * (N - 1))
    skew <- (N - 2 * m) * sqrt(N - 1) * (N - 2 * k) /
      (sqrt(k * m * n * (N - k)) * (N - 2))
    kurt <- ((N - 1) * N^2 * (N * (N + 1) - 6 * m * n - 6 * k * (N - k)) +
               6 * k * m * n * (N - k) * (5 * N - 6)) /
      (k * m * n * (N - k) * (N - 2) * (N - 3))
    c(mean = k * m / N, variance = v, skewness = skew, excess_kurtosis = kurt)
  },
  betabinomial = function(size, alpha, beta) {
    s <- alpha + beta
    ab <- alpha * beta
    v <- size * ab * (s + size) / (s^2 * (s + 1))
    skew <- (s + 2 * size) * (beta - alpha) / (s + 2) *
      sqrt((1 + s) / (size * ab * (size + s)))
    kurt <- s^2 * (1 + s) / (size * ab * (s + 2) * (s + 3) * (s + size)) *
      (s * (s - 1 + 6 * size) + 3 * ab * (size - 2) + 6 * size^2 -
         3 * ab * size * (6 - size) / s - 18 * ab * size^2 / s^2) - 3
    c(mean = size * alpha / s, variance = v, skewness = skew,
      excess_kurtosis = kurt)
  },
  discrete_uniform = function(min, max) {
    n2 <- (max - min + 1)^2
    c(mean = (min + max) / 2, variance = (n2 - 1) / 12, skewness = 0,
      excess_kurtosis = -6 * (n2 + 1) / (5 * (n2 - 1)))
  },
  uniform = function(min, max) {
    c(mean = (min + max) / 2, variance = (max - min)^2 / 12, skewness = 0,
      excess_kurtosis = -6 / 5)
  },
  beta = function(shape1, shape2) {
    a <- shape1
    b <- shape2
    s <- a + b
    c(mean = a / s, variance = a * b / (s^2 * (s + 1)),
      skewness = 2 * (b - a) * sqrt(s + 1) / ((s + 2) * sqrt(a * b)),
      excess_kurtosis = 6 * ((a - b)^2 * (s + 1) - a * b * (s + 2)) /
        (a * b * (s + 2) * (s + 3)))
  },
  exponential = function(rate) {
    c(mean = 1 / rate, variance = 1 / rate^2, skewness = 2, excess_kurtosis = 6)
  },
  gamma = function(shape, rate) {
    c(mean = shape / rate, variance = shape / rate^2,
      skewness = 2 / sqrt(shape), excess_kurtosis = 6 / shape)
  },
  chisq = function(df) {
    c(mean = df, variance = 2 * df, skewness = sqrt(8 / df),
      excess_kurtosis = 12 / df)
  },
  weibull = function(shape, scale) {
    # Raw moments E[X^r] = scale^r * Gamma(1 + r / shape), on the log scale
    # first so a small shape does not overflow.
    g <- exp(lgamma(1 + (1:4) / shape))
    v <- g[2] - g[1]^2
    c(mean = scale * g[1], variance = scale^2 * v,
      skewness = (g[3] - 3 * g[1] * g[2] + 2 * g[1]^3) / v^1.5,
      excess_kurtosis = (g[4] - 4 * g[1] * g[3] + 6 * g[1]^2 * g[2] -
                           3 * g[1]^4) / v^2 - 3)
  },
  lognormal = function(meanlog, sdlog) {
    s2 <- sdlog^2
    # expm1 keeps exp(s2) - 1 accurate when sdlog is small.
    e1 <- expm1(s2)
    c(mean = exp(meanlog + s2 / 2), variance = e1 * exp(2 * meanlog + s2),
      skewness = (e1 + 3) * sqrt(e1),
      excess_kurtosis = exp(4 * s2) + 2 * exp(3 * s2) + 3 * exp(2 * s2) - 6)
  },
  f = function(df1, df2) {
    d1 <- df1
    d2 <- df2
    # The r-th moment of this positive variable is finite only for d2 > 2r and
    # +Inf otherwise; a ratio built on an infinite lower moment is NA.
    mean <- if (d2 > 2) d2 / (d2 - 2) else Inf
    var <- if (d2 > 4) {
      2 * d2^2 * (d1 + d2 - 2) / (d1 * (d2 - 2)^2 * (d2 - 4))
    } else if (d2 > 2) Inf else NA_real_
    skew <- if (d2 > 6) {
      (2 * d1 + d2 - 2) * sqrt(8 * (d2 - 4)) /
        ((d2 - 6) * sqrt(d1 * (d1 + d2 - 2)))
    } else if (d2 > 4) Inf else NA_real_
    kurt <- if (d2 > 8) {
      12 * (d1 * (5 * d2 - 22) * (d1 + d2 - 2) + (d2 - 4) * (d2 - 2)^2) /
        (d1 * (d2 - 6) * (d2 - 8) * (d1 + d2 - 2))
    } else if (d2 > 4) Inf else NA_real_
    c(mean = mean, variance = var, skewness = skew, excess_kurtosis = kurt)
  },
  normal = function(mean, sd) {
    c(mean = mean, variance = sd^2, skewness = 0, excess_kurtosis = 0)
  },
  student_t = function(df) {
    # Both tails are heavy: an odd moment of order r has no value for
    # df <= r, an even one is +Inf for r - 2 < df <= r (with the mean defined).
    mean <- if (df > 1) 0 else NA_real_
    var <- if (df > 2) df / (df - 2) else if (df > 1) Inf else NA_real_
    skew <- if (df > 3) 0 else NA_real_
    kurt <- if (df > 4) 6 / (df - 4) else if (df > 2) Inf else NA_real_
    c(mean = mean, variance = var, skewness = skew, excess_kurtosis = kurt)
  },
  cauchy = function(location, scale) {
    # No moment exists: E|X| is infinite in both tails.
    c(mean = NA_real_, variance = NA_real_, skewness = NA_real_,
      excess_kurtosis = NA_real_)
  },
  laplace = function(mu, sigma) {
    c(mean = mu, variance = 2 * sigma^2, skewness = 0, excess_kurtosis = 3)
  },
  logistic = function(location, scale) {
    c(mean = location, variance = scale^2 * pi^2 / 3, skewness = 0,
      excess_kurtosis = 6 / 5)
  }
)

# Mean, variance, skewness and excess kurtosis of a finite discrete law.
finite_sum_moments <- function(x, w) {
  mu <- sum(x * w)
  cm <- vapply(2:4, function(k) sum((x - mu)^k * w), numeric(1))
  c(mean = mu, variance = cm[1], skewness = cm[2] / cm[1]^1.5,
    excess_kurtosis = cm[3] / cm[1]^2 - 3)
}

#' Moments of a distribution
#'
#' Returns the mean, variance, standard deviation, skewness and excess
#' kurtosis. Closed forms are used when the registry has them; otherwise the
#' moments are computed numerically: for continuous distributions by adaptive
#' integration of \eqn{g(Q(u))} over \eqn{u \in (0, 1)}, where \eqn{Q} is the
#' quantile function, and for discrete ones by summing the mass function over
#' the support.
#'
#' Closed forms tell apart two ways a moment can fail to be finite:
#' * `Inf`: the moment is infinite, its integral diverges to \eqn{+\infty}.
#'   Examples: the variance of Student's t with \eqn{1 < \nu \le 2} and the
#'   mean of F with \eqn{d_2 \le 2}.
#' * `NA`: the moment does not exist (the integral has no value, as for the
#'   mean of the Cauchy or of Student's t with \eqn{\nu \le 1}), or it is a
#'   ratio built on a lower moment that is already infinite or undefined.
#'
#' A moment the closed form marks as `Inf` or `NA` is never integrated
#' numerically, also with `method = "numeric"`. Skewness and kurtosis of a
#' distribution with zero variance (a point mass) are `NA`.
#'
#' Numeric moments of discrete distributions sum the mass function up to the
#' quantile with upper-tail probability \eqn{10^{-15}}; very heavy tails or
#' supports wider than \eqn{10^7} values give `NA`. Numeric moments that
#' cannot be computed are also `NA`.
#'
#' @param dist A `dl_dist` object.
#' @param method `"auto"` (closed form when available, numeric otherwise),
#'   `"exact"` or `"numeric"`.
#'
#' @returns A named numeric vector with elements `mean`, `variance`, `sd`,
#'   `skewness` and `excess_kurtosis`, where `Inf` marks an infinite moment
#'   and `NA` one that does not exist or cannot be computed.
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
  exact <- if (!is.null(formula)) do.call(formula, dist$params)
  m <- if (method != "numeric" && !is.null(exact)) {
    exact
  } else {
    num <- numeric_moments(dist)
    # Moments the closed form marks as infinite or nonexistent are kept as is.
    if (!is.null(exact)) {
      gone <- !is.finite(exact)
      num[gone] <- exact[gone]
    }
    num
  }
  out <- c(m["mean"], m["variance"], sd = sqrt(unname(m["variance"])),
           m["skewness"], m["excess_kurtosis"])
  # A point mass has no shape: standardized moments would be 0/0 or x/0.
  if (isTRUE(out[["variance"]] == 0)) {
    out[c("skewness", "excess_kurtosis")] <- NA_real_
  }
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

# Central moments as integrals over probability: E[g(X)] is the integral of
# g(Q(u)) for u in (0, 1). The lower half uses Q(u) and the upper half the
# upper-tail quantile, so both tails keep full precision. On a bounded support
# the integrand stays bounded even where the density is unbounded (Beta with
# shapes below 1). A moment whose integral fails is NA.
continuous_central_moments <- function(dist) {
  lower_q <- function(u) dl_quantile(dist, u)
  upper_q <- function(v) dl_quantile(dist, v, lower.tail = FALSE)
  # Absolute tolerance scaled by the IQR^k, so tiny moments keep accuracy and
  # moments that are exactly zero (symmetric skewness) still converge.
  scale <- upper_q(0.25) - lower_q(0.25)
  integral <- function(g, k) {
    piece <- function(q) {
      stats::integrate(function(u) g(q(u)), 0, 0.5, rel.tol = 1e-10,
                       abs.tol = 1e-13 * scale^k, subdivisions = 1000L)$value
    }
    tryCatch(piece(lower_q) + piece(upper_q), error = function(e) NA_real_)
  }
  # Integrate around the median so a large location does not swamp a small
  # spread (cancellation in (x - mu)^k when mu carries relative error).
  med <- lower_q(0.5)
  mu <- med + integral(function(x) x - med, 1)
  if (!is.finite(mu)) return(rep(NA_real_, 4))
  c(mu, vapply(2:4, function(k) integral(function(x) (x - mu)^k, k), numeric(1)))
}

# Sum over the support up to a far upper quantile (tail mass below 1e-15).
discrete_central_moments <- function(dist) {
  s <- dl_support(dist)
  lo <- max(s$lower, dl_quantile(dist, 0))
  hi <- min(s$upper, dl_quantile(dist, 1e-15, lower.tail = FALSE))
  if (!is.finite(lo) || !is.finite(hi) || hi - lo > 1e7) {
    return(rep(NA_real_, 4))
  }
  x <- seq(lo, hi)
  w <- dl_density(dist, x)
  mu <- sum(x * w)
  c(mu, vapply(2:4, function(k) sum((x - mu)^k * w), numeric(1)))
}
