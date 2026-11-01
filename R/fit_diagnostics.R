#' Diagnostics for fitted distributions
#'
#' Produces plot data and descriptive goodness-of-fit statistics without
#' expanding frequency tables. Q-Q envelopes are pointwise order-statistic
#' intervals conditional on the fitted parameters: they do not account for
#' parameter estimation. Grouped data retain interval-based diagnostics;
#' individual-observation Q-Q/P-P and continuous GOF are unavailable.
#'
#' @param fit A result from [dl_fit()].
#' @param id Fitted family; defaults to the first fitted family in the ranking.
#' @param points Maximum number of Q-Q points and continuous curve grid size.
#' @param level Pointwise Q-Q envelope confidence level in `(0, 1)`.
#'
#' @returns A list with `id`, `discrete`, `density`, `cdf`, `qq`, `pp`,
#'   `gof`, `bins` and `notes`. Density rows have `kind` equal to `fitted`
#'   or `empirical`. GOF p-values are descriptive simple-null references,
#'   explicitly uncalibrated when parameters are estimated. Chi-square bins
#'   include unobserved tails and are merged until expected counts reach 5.
#' @export
#' @examples
#' fit <- dl_fit(c(-1, 0, 1, 2), "normal")
#' dl_fit_diagnostics(fit)$gof
dl_fit_diagnostics <- function(fit, id = NULL, points = 200L, level = .95) {
  if (!inherits(fit, "dl_fit") || !inherits(fit$data, "dl_fit_data")) {
    stop("`fit` must be a dl_fit result.", call. = FALSE)
  }
  if (!is.numeric(points) || length(points) != 1L || !is.finite(points) ||
      points < 2 || points > 10000 || points != floor(points)) {
    stop("`points` must be a whole number between 2 and 10000.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      level <= 0 || level >= 1) stop("`level` must be in (0, 1).", call. = FALSE)
  if (is.null(id)) {
    available <- fit$ranking$id[fit$ranking$id %in% names(fit$fits)]
    id <- if (length(available)) available[1] else names(fit$fits)[1]
  }
  if (!is_string(id) || is.null(fit$fits[[id]])) {
    stop("Choose a successfully fitted distribution.", call. = FALSE)
  }
  model <- fit$fits[[id]]
  data <- fit$data
  dist <- model$dist
  grouped <- data$type == "grouped"
  discrete <- identical(dist$entry$type, "discrete")
  notes <- c("fit.note_gof_descriptive")
  density <- fit_diagnostic_density(data, dist, points)
  notes <- c(notes, attr(density, "notes"))
  if (data$n > .Machine$integer.max) notes <- c(notes, "fit.note_asymptotic")
  keep <- data$counts > 0
  x <- data$x[keep]
  counts <- data$counts[keep]
  cdf_x <- if (grouped) data$upper[keep] else x
  cdf <- data.frame(x = cdf_x, empirical = cumsum(counts) / data$n,
                    fitted = dl_cdf(dist, cdf_x))
  qq <- data.frame(theoretical = numeric(), observed = numeric(),
                   lower = numeric(), upper = numeric(), p = numeric())
  pp <- data.frame(theoretical = numeric(), observed = numeric())
  if (grouped) {
    notes <- c(notes, "fit.note_grouped_diagnostics")
  } else {
    # Select ranks, then locate them in cumulative frequencies. This is
    # bounded by points even if counts represent billions of observations.
    ranks <- unique(round(seq(1, data$n, length.out = min(points, data$n))))
    p <- (ranks - .5) / data$n
    observed <- x[findInterval(ranks - 1, cumsum(counts)) + 1L]
    a <- (1 - level) / 2
    qq <- data.frame(theoretical = dl_quantile(dist, p), observed = observed,
                     lower = dl_quantile(dist, stats::qbeta(a, ranks, data$n + 1 - ranks)),
                     upper = dl_quantile(dist, stats::qbeta(1 - a, ranks, data$n + 1 - ranks)),
                     p = p)
    pp <- data.frame(theoretical = dl_cdf(dist, observed), observed = p)
    notes <- c(notes, "fit.note_conditional_envelope")
    if (any(counts > 1) && !discrete) notes <- c(notes, "fit.note_ties")
  }
  bins <- data.frame(lower = numeric(), upper = numeric(), observed = numeric(),
                     expected = numeric())
  if (discrete || grouped) {
    bins <- fit_chisq_bins(data, dist)
    statistic <- if (any(bins$expected == 0 & bins$observed > 0)) Inf else
      sum((bins$observed[bins$expected > 0] - bins$expected[bins$expected > 0])^2 /
            bins$expected[bins$expected > 0])
    df <- sum(bins$expected > 0) - 1 - model$k
    enough <- df > 0 && all(bins$expected[bins$expected > 0] >= 5)
    pvalue <- if (enough) stats::pchisq(statistic, df, lower.tail = FALSE) else NA_real_
    gof <- data.frame(test = "Chi-square", statistic = statistic,
                      df = if (df > 0) df else NA_real_, p.value = pvalue,
                      label = if (model$k > 0) "descriptive (parameters estimated; merged expected bins)" else
                        "descriptive (fixed parameters; merged expected bins)")
    if (!enough) notes <- c(notes, "fit.note_insufficient_bins")
  } else gof <- fit_continuous_gof(data, dist, estimated = model$k > 0)
  list(id = id, discrete = discrete, density = density, cdf = cdf, qq = qq,
       pp = pp, gof = gof, bins = bins, notes = unique(notes))
}

fit_diagnostic_density <- function(data, dist, points) {
  discrete <- identical(dist$entry$type, "discrete")
  empty <- data.frame(x = numeric(), empirical = numeric(), fitted = numeric(),
                       lower = numeric(), upper = numeric(), kind = character())
  curve <- tryCatch(dl_curve(dist, n = points, tail = .001), error = function(e) NULL)
  fitted <- if (!is.null(curve)) data.frame(x = curve$x, empirical = NA_real_,
                                           fitted = curve$density, lower = NA_real_,
                                           upper = NA_real_, kind = "fitted") else empty
  if (data$type == "grouped") {
    keep <- is.finite(data$x)
    if (!any(keep)) {
      attr(empty, "notes") <- c("fit.note_grouped_mass", "fit.note_open_bins")
      return(empty)
    }
    empirical <- data.frame(x = data$x[keep], empirical = data$counts[keep] / data$n,
                            fitted = NA_real_, lower = data$lower[keep],
                            upper = data$upper[keep], kind = "empirical")
    fitted <- empirical
    fitted$kind <- "fitted"
    fitted$empirical <- NA_real_
    fitted$fitted <- vapply(which(keep), function(i) {
      dl_prob(dist, data$lower[i], data$upper[i])
    }, numeric(1))
    out <- rbind(fitted, empirical)
    attr(out, "notes") <- c("fit.note_grouped_mass",
                           if (any(!keep)) "fit.note_open_bins")
    return(out)
  } else if (discrete) {
    empirical <- data.frame(x = data$x, empirical = data$counts / data$n,
                            fitted = NA_real_, lower = data$x - .5,
                            upper = data$x + .5, kind = "empirical")
  } else {
    x <- data$x
    r <- range(x)
    qs <- fit_weighted_quantile(x, data$counts, c(.25, .75))
    clipped <- !is.null(curve) && diff(qs) > 0 && diff(r) > 30 * diff(qs)
    if (clipped) r <- range(curve$x)
    if (r[1] == r[2]) r <- r + c(-.5, .5) * max(1, abs(r[1]) * .01)
    if (!is.finite(diff(r))) return(fitted)
    # Fixed bounded bin count keeps plotting independent of frequency size.
    breaks <- seq(r[1], r[2], length.out = min(61L, max(3L, ceiling(sqrt(data$n)) + 1L)))
    within <- x >= r[1] & x <= r[2]
    index <- pmax(1L, pmin(length(breaks) - 1L, findInterval(x[within], breaks)))
    counts <- numeric(length(breaks) - 1L)
    sums <- rowsum(data$counts[within], index, reorder = FALSE)
    counts[as.integer(rownames(sums))] <- as.numeric(sums)
    lower <- breaks[-length(breaks)]
    upper <- breaks[-1]
    empirical <- data.frame(x = lower / 2 + upper / 2,
                            empirical = counts / data$n / (upper - lower),
                            fitted = NA_real_, lower = lower, upper = upper,
                            kind = "empirical")
  }
  out <- rbind(fitted, empirical)
  if (exists("clipped", inherits = FALSE) && clipped) attr(out, "notes") <- "fit.note_clipped"
  out
}

# Compressed versions of the EDF statistics, algebraically identical to
# evaluating the sorted, expanded sample. Log tails avoid log(1 - F).
fit_continuous_gof <- function(data, dist, estimated = TRUE) {
  n <- data$n
  w <- data$counts
  u <- dl_cdf(dist, data$x)
  cumulative <- cumsum(w)
  previous <- cumulative - w
  dplus <- max(cumulative / n - u)
  dminus <- max(u - previous / n)
  ks <- max(dplus, dminus)
  mean_rank <- (previous + w / 2) / n
  cvm <- 1 / (12 * n) + sum(w * ((u - mean_rank)^2 + ((w / n)^2 - 1 / n^2) / 12))
  lf <- dl_cdf(dist, data$x, log.p = TRUE)
  ls <- dl_cdf(dist, data$x, lower.tail = FALSE, log.p = TRUE)
  ad <- -n - sum((w / n) * ((2 * previous + w) * lf +
                            (2 * (n - cumulative) + w) * ls))
  ad <- max(0, ad)
  reference_n <- if (n > .Machine$integer.max) Inf else n
  p_ad <- if (is.infinite(ad)) 0 else goftest::pAD(ad, n = reference_n, lower.tail = FALSE)
  p_cvm <- if (is.infinite(cvm)) 0 else goftest::pCvM(cvm, n = reference_n, lower.tail = FALSE)
  data.frame(test = c("KS", "AD", "CvM"), statistic = c(ks, ad, cvm),
             df = NA_real_, p.value = c(fit_ks_reference(sqrt(n) * ks), p_ad, p_cvm),
             label = if (estimated) "descriptive (parameters estimated; simple-null reference)" else
               "descriptive (fixed parameters; simple-null reference)")
}

# Limiting two-sided Kolmogorov distribution, evaluated on the numerically
# stable side of its theta-function representation. No fitted-null calibration.
fit_ks_reference <- function(z) {
  if (z <= 0) return(1)
  if (!is.finite(z)) return(0)
  if (z < 1) {
    k <- seq_len(100L)
    p <- 1 - sqrt(2 * pi) / z * sum(exp(-(2 * k - 1)^2 * pi^2 / (8 * z^2)))
  } else {
    k <- seq_len(100L)
    p <- 2 * sum((-1)^(k - 1) * exp(-2 * k^2 * z^2))
  }
  max(0, min(1, p))
}

# Complete partitions of the model support, including empty exterior/gap
# bins. Never renormalize away probability outside the observed classes.
fit_chisq_bins <- function(data, dist) {
  if (data$type == "grouped") {
    breaks <- sort(unique(c(-Inf, data$lower, data$upper, Inf)))
    observed <- numeric(length(breaks) - 1L)
    observed[match(data$lower, breaks)] <- data$counts
  } else {
    m <- min(100L, max(2L, floor(data$n / 5)))
    internal <- dl_quantile(dist, seq_len(m - 1L) / m)
    breaks <- sort(unique(c(-Inf, internal[is.finite(internal)], Inf)))
    observed <- numeric(length(breaks) - 1L)
    index <- findInterval(data$x, breaks, left.open = TRUE)
    sums <- rowsum(data$counts, index, reorder = FALSE)
    observed[as.integer(rownames(sums))] <- as.numeric(sums)
  }
  lower <- breaks[-length(breaks)]
  upper <- breaks[-1]
  expected <- data$n * vapply(seq_along(lower), function(i) {
    dl_prob(dist, lower[i], upper[i])
  }, numeric(1))
  # Pool adjacent bins from left to right and combine the final short tail.
  out <- data.frame(lower = numeric(), upper = numeric(), observed = numeric(),
                     expected = numeric())
  first <- 1L
  obs <- expct <- 0
  for (i in seq_along(expected)) {
    obs <- obs + observed[i]
    expct <- expct + expected[i]
    if (expct >= 5 || i == length(expected)) {
      out <- rbind(out, data.frame(lower = lower[first], upper = upper[i],
                                  observed = obs, expected = expct))
      first <- i + 1L
      obs <- expct <- 0
    }
  }
  last <- nrow(out)
  if (last > 1L && out$expected[last] < 5) {
    out$upper[last - 1L] <- out$upper[last]
    out$observed[last - 1L] <- sum(out$observed[c(last - 1L, last)])
    out$expected[last - 1L] <- sum(out$expected[c(last - 1L, last)])
    out <- out[-last, , drop = FALSE]
  }
  out
}
