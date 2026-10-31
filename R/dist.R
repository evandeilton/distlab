#' Create a distribution object
#'
#' Builds a `dl_dist` object from a registry id and parameter values.
#' Parameters not given take their registry defaults. Every value is checked
#' against its domain.
#'
#' @param id Distribution id, one of [dl_ids()].
#' @param ... Named parameter values, using the R parameterization stored in
#'   the registry (for example `shape` and `rate` for `"gamma"`).
#'
#' @returns An object of class `dl_dist`: a list with `id`, `entry` (the
#'   registry entry) and `params` (a named list of parameter values).
#'
#' @seealso [dl_density()], [dl_moments()]
#' @family engine
#' @export
#' @examples
#' g <- dl_dist("gamma", shape = 3, rate = 2)
#' g
#' dl_density(g, 1)
dl_dist <- function(id, ...) {
  entry <- dl_entry(id)
  values <- list(...)
  par_names <- vapply(entry$parameters, `[[`, character(1), "name")
  if (length(values) && (is.null(names(values)) || any(!nzchar(names(values))))) {
    stop("All parameters must be named.", call. = FALSE)
  }
  unknown <- setdiff(names(values), par_names)
  if (length(unknown)) {
    stop("Unknown parameter(s) for ", id, ": ", paste(unknown, collapse = ", "),
         ". Expected: ", paste(par_names, collapse = ", "), ".", call. = FALSE)
  }
  params <- stats::setNames(lapply(entry$parameters, `[[`, "default"), par_names)
  params[names(values)] <- values
  for (par in entry$parameters) {
    if (!in_domain(params[[par$name]], par)) {
      stop(sprintf("Parameter '%s' = %s is outside its domain for %s.",
                   par$name, format(params[[par$name]]), id), call. = FALSE)
    }
  }
  # Constraints that tie parameters together, such as min < max.
  for (cond in entry$constraints) {
    if (!isTRUE(eval_bound(cond, params))) {
      stop(sprintf("Parameters of %s must satisfy %s.", id, cond), call. = FALSE)
    }
  }
  structure(list(id = id, entry = entry, params = params), class = "dl_dist")
}

# Numeric support of a distribution object. Registry bounds may be numbers or
# expressions of the parameters (e.g. "min(k, m)"), resolved here.
dl_support <- function(dist) {
  s <- dist$entry$support
  list(lower = eval_bound(s$lower, dist$params),
       upper = eval_bound(s$upper, dist$params),
       lower_open = isTRUE(s$lower_open), upper_open = isTRUE(s$upper_open))
}

#' @export
print.dl_dist <- function(x, ...) {
  vals <- vapply(x$params, function(v) format(v, digits = 6), character(1))
  cat(sprintf("<dl_dist> %s(%s)\n", x$entry$names[["en-US"]],
              paste(names(vals), vals, sep = " = ", collapse = ", ")))
  invisible(x)
}

#' Density, distribution, quantile and random generation
#'
#' Evaluate a `dl_dist` object with the R functions listed in its registry
#' entry. For discrete distributions `dl_density()` returns the probability
#' mass function.
#'
#' @param dist A `dl_dist` object from [dl_dist()].
#' @param x,q Numeric vector of values.
#' @param p Numeric vector of probabilities.
#' @param n Number of draws.
#' @param log,log.p Logical; if `TRUE`, densities and probabilities are on the
#'   log scale.
#' @param lower.tail Logical; if `TRUE` probabilities are \eqn{P(X \le x)},
#'   otherwise \eqn{P(X > x)}.
#'
#' @returns A numeric vector.
#'
#' @family engine
#' @export
#' @examples
#' nrm <- dl_dist("normal", mean = 0, sd = 1)
#' dl_density(nrm, 0)
#' dl_cdf(nrm, 1.96)
#' dl_quantile(nrm, 0.975)
#' set.seed(1)
#' dl_sample(nrm, 3)
dl_density <- function(dist, x, log = FALSE) {
  call_dist_fun(dist, "d", list(x, log = log))
}

#' @rdname dl_density
#' @export
dl_cdf <- function(dist, q, lower.tail = TRUE, log.p = FALSE) {
  call_dist_fun(dist, "p", list(q, lower.tail = lower.tail, log.p = log.p))
}

#' @rdname dl_density
#' @export
dl_quantile <- function(dist, p, lower.tail = TRUE, log.p = FALSE) {
  call_dist_fun(dist, "q", list(p, lower.tail = lower.tail, log.p = log.p))
}

#' @rdname dl_density
#' @export
dl_sample <- function(dist, n) {
  call_dist_fun(dist, "r", list(n))
}

# Call the registry function of the given kind with the object's parameters.
call_dist_fun <- function(dist, kind, args) {
  if (!inherits(dist, "dl_dist")) {
    stop("`dist` must be a dl_dist object; see dl_dist().", call. = FALSE)
  }
  f <- resolve_fun(dist$entry$functions[[kind]])
  do.call(f, c(args[1], dist$params, args[-1]))
}

#' Grid of density and CDF values for plotting
#'
#' Computes the density (or mass) and CDF over a range that covers the
#' central `1 - 2 * tail` probability of the distribution. For heavy-tailed
#' continuous distributions the range is clipped to the median plus or minus
#' 15 interquartile ranges, so the body of the density stays visible. Continuous
#' distributions use an even grid; discrete ones use every integer in range.
#'
#' @param dist A `dl_dist` object.
#' @param n Number of grid points for continuous distributions (at least 2).
#' @param tail Probability left out in each tail when choosing the range, in
#'   (0, 0.5). Discrete ranges wider than 5000 values are refused.
#'
#' @returns A data frame with columns `x`, `density` and `cdf`.
#'
#' @family engine
#' @export
#' @examples
#' head(dl_curve(dl_dist("poisson", lambda = 2)))
dl_curve <- function(dist, n = 401L, tail = 1e-4) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 2) {
    stop("`n` must be a single number >= 2.", call. = FALSE)
  }
  if (!is.numeric(tail) || length(tail) != 1L || !(tail > 0 && tail < 0.5)) {
    stop("`tail` must be a single number in (0, 0.5).", call. = FALSE)
  }
  # Upper tail via lower.tail = FALSE keeps precision for small tail values.
  lo <- dl_quantile(dist, tail)
  hi <- dl_quantile(dist, tail, lower.tail = FALSE)
  discrete <- identical(dist$entry$type, "discrete")
  # A discrete range may collapse to one point; a continuous one may not.
  if (!is.finite(lo) || !is.finite(hi) || hi < lo || (!discrete && hi == lo)) {
    stop("Cannot find a finite plotting range for this distribution.",
         call. = FALSE)
  }
  # Very wide discrete ranges would produce plots too large to draw.
  if (discrete && hi - lo > 5000) {
    stop("The discrete range spans more than 5000 values; increase `tail`.",
         call. = FALSE)
  }
  # Heavy tails (Cauchy, t or F with small df) push the extreme quantiles so
  # far out that the body collapses to a point; clip to median +/- 15 IQR.
  if (!discrete) {
    qq <- dl_quantile(dist, c(0.25, 0.5, 0.75))
    iqr <- qq[3] - qq[1]
    if (is.finite(iqr) && iqr > 0 && hi - lo > 30 * iqr) {
      lo <- max(lo, qq[2] - 15 * iqr)
      hi <- min(hi, qq[2] + 15 * iqr)
    }
  }
  x <- if (discrete) {
    seq(floor(lo), ceiling(hi))
  } else {
    seq(lo, hi, length.out = n)
  }
  dens <- dl_density(dist, x)
  # An extreme quantile may round onto a support end where the density is
  # infinite (Beta with a shape below 1); such points cannot be drawn.
  keep <- is.finite(dens)
  data.frame(x = x[keep], density = dens[keep], cdf = dl_cdf(dist, x[keep]))
}
