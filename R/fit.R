# Likelihood fitting without Shiny. Frequency weights are never expanded;
# grouped observations contribute interval probabilities, including tail bins.

#' Fit registry distributions by maximum likelihood
#'
#' Fits the R parameterization of each selected family. Integer design
#' parameters must be supplied through `fixed`: `size` for Binomial and
#' Beta-binomial, and `m`, `n`, `k` for Hypergeometric. Student t estimates
#' only `df` (its location and scale remain zero and one).
#'
#' Raw observations and frequency tables use densities or probability masses.
#' These likelihoods are ranked separately. Grouped data use probabilities
#' of the supplied `(lower, upper]` intervals, without conditioning on the
#' observed range, and can compare continuous and discrete candidates.
#' Counts of zero contribute zero, even when the interval probability is zero.
#'
#' Uniform bounds are sample extrema for ungrouped data. For grouped data
#' both bounds must be fixed because endpoints need not be identified within
#' a class. Nonregular estimates and boundary estimates have no Wald intervals.
#'
#' Ungrouped Normal, Lognormal, Exponential, Poisson, Bernoulli, Binomial and
#' Geometric fits use their closed-form estimates and exact observed
#' information; Gamma, Chi-square and Beta likelihoods are evaluated from
#' sufficient statistics. Other families are maximized numerically, with
#' locations measured from the center of the data in units of its spread and
#' scales on the log scale, so estimates, standard errors and the
#' identification checks do not depend on the units or location of the data;
#' their observed information is a numerical Hessian in those coordinates.
#'
#' Standard errors come from the observed information, by the delta method
#' for the natural parameters. Wald intervals are built where each parameter
#' is unconstrained (log scale for positive parameters, logit scale for
#' probabilities) and transformed back, so they stay inside the parameter
#' space. They are asymptotic approximations, not bootstrap intervals.
#'
#' Every free parameter must be identified with stable local information.
#' Flat likelihoods, singular observed information and numerically saturated
#' limiting fits are reported as failures rather than ranked as finite
#' interior maxima; so are Cauchy samples in which half or more of the
#' observations coincide (any sample of two) and Beta-binomial samples whose
#' variance does not exceed the Binomial variance.
#'
#' @param x Numeric observations or a [dl_fit_data()] object.
#' @param ids Character vector of registry ids to fit.
#' @param fixed Named list of named parameter lists, indexed by distribution id.
#' @param level Confidence level in `(0, 1)`.
#' @param control Named control list passed to [stats::nlminb()]. Defaults use
#'   500 iterations, 1000 evaluations and relative tolerance `1e-10`.
#'
#' @returns A `dl_fit` object with `data`, successful `fits`, a `ranking` data
#'   frame, named `failures` and reproducible R `code`. Individual fits include
#'   the `dl_dist`, `estimates` (including standard errors and Wald limits),
#'   `vcov` (natural parameters), `logLik`, `k`, `AIC`, `BIC`, optimizer
#'   `convergence`, a `boundary` flag and `warnings`. Failed candidates remain
#'   visible in the ranking.
#'
#' @family engine
#' @export
#' @examples
#' dl_fit(c(1.2, 2.5, 0.8, 3.1, 1.9), c("normal", "gamma"))
#' dl_fit(c(0, 2, 1, 3, 2), "binomial", fixed = list(binomial = list(size = 5)))
dl_fit <- function(x, ids = c("normal", "gamma"), fixed = list(),
                   level = 0.95, control = list()) {
  fit_models(x, ids, fixed, level, control)
}

# dl_fit() with an optional `progress(id, i, total)` callback, called before
# each candidate is fitted; the app uses it to report progress.
fit_models <- function(x, ids = c("normal", "gamma"), fixed = list(),
                       level = 0.95, control = list(), progress = NULL) {
  data <- if (inherits(x, "dl_fit_data")) x else dl_fit_data(x)
  if (!is.character(ids) || !length(ids) || anyNA(ids) ||
      any(!ids %in% dl_ids()) || anyDuplicated(ids)) {
    stop("`ids` must contain distinct registry distribution ids.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    stop("`level` must be a single number in (0, 1).", call. = FALSE)
  }
  fit_check_named_list(fixed, "fixed")
  if (length(setdiff(names(fixed), ids))) {
    stop("`fixed` contains a distribution not selected in `ids`.", call. = FALSE)
  }
  fit_check_named_list(control, "control")
  settings <- utils::modifyList(list(iter.max = 500, eval.max = 1000,
                                     rel.tol = 1e-10), control)
  # The data literal is the long part of every script; write it once.
  input <- fit_data_code(data)
  fits <- failures <- list()
  for (i in seq_along(ids)) {
    id <- ids[i]
    if (is.function(progress)) progress(id, i, length(ids))
    result <- tryCatch(fit_one(data, id, fixed[[id]] %||% list(), level, settings),
                       error = function(e) conditionMessage(e))
    if (is.character(result)) failures[[id]] <- result else {
      own <- if (length(fixed[[id]])) stats::setNames(list(fixed[[id]]), id) else list()
      result$code <- fit_code(input, id, own, level, control)
      fits[[id]] <- result
    }
  }
  ranking <- fit_ranking(ids, data$type, fits, failures)
  code <- fit_code(input, ids, fixed, level, control)
  structure(list(data = data, fits = fits, ranking = ranking,
                 failures = failures, code = code), class = "dl_fit")
}

#' @export
print.dl_fit <- function(x, ...) {
  cat(sprintf("<dl_fit> n = %s; %d fitted, %d failed\n", format(x$data$n),
              length(x$fits), length(x$failures)))
  print(x$ranking, row.names = FALSE)
  invisible(x)
}

fit_check_named_list <- function(x, name) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x))))) {
    stop("`", name, "` must be a named list.", call. = FALSE)
  }
}

# Stable log(P(lower < X <= upper)): use the upper tail above the median
# to avoid subtraction of CDFs that have both rounded to one.
fit_interval_logprob <- function(dist, lower, upper) {
  a <- dl_cdf(dist, lower, log.p = TRUE)
  b <- dl_cdf(dist, upper, log.p = TRUE)
  sa <- dl_cdf(dist, lower, lower.tail = FALSE, log.p = TRUE)
  sb <- dl_cdf(dist, upper, lower.tail = FALSE, log.p = TRUE)
  logdiff <- function(big, small) {
    ans <- big + log(-expm1(pmin(0, small - big)))
    ans[is.infinite(big) & big < 0] <- -Inf
    ans
  }
  out <- logdiff(b, a)
  high <- a > log(0.5)
  out[high] <- logdiff(sa[high], sb[high])
  out
}

fit_loglik <- function(data, dist) {
  keep <- data$counts > 0
  p <- if (identical(data$type, "grouped")) {
    fit_interval_logprob(dist, data$lower[keep], data$upper[keep])
  } else {
    dl_density(dist, data$x[keep], log = TRUE)
  }
  if (any(!is.finite(p))) return(-Inf)
  sum(data$counts[keep] * p)
}

# Weighted starts. For open tail bins the finite edge is only a starting
# proxy; the objective always uses the exact interval probabilities.
fit_start <- function(data, id) {
  x <- data$x
  if (identical(data$type, "grouped")) {
    x <- data$lower / 2 + data$upper / 2
    x[!is.finite(x)] <- ifelse(is.finite(data$lower[!is.finite(x)]),
                               data$lower[!is.finite(x)] + 1,
                               data$upper[!is.finite(x)] - 1)
    x[!is.finite(x)] <- 0
  }
  keep <- data$counts > 0
  x <- x[keep]
  w <- data$counts[keep] / data$n
  m <- sum(w * x)
  s <- sqrt(sum(w * (x - m)^2))
  if (!is.finite(s) || s <= 0) s <- max(1, abs(m) / 2)
  positive <- pmax(x, .Machine$double.eps)
  lm <- sum(w * log(positive))
  ls <- sqrt(sum(w * (log(positive) - lm)^2))
  if (!is.finite(ls) || ls <= 0) ls <- 1
  mm <- max(m, 0.01)
  shape <- min(1e4, max(0.01, (mm / s)^2))
  prob <- min(0.99, max(0.01, m))
  beta_m <- min(0.99, max(0.01, m))
  concentration <- max(0.1, beta_m * (1 - beta_m) / s^2 - 1)
  switch(id,
    normal = list(mean = m, sd = s),
    lognormal = list(meanlog = lm, sdlog = ls),
    gamma = list(shape = shape, rate = shape / mm),
    exponential = list(rate = 1 / mm),
    chisq = list(df = mm),
    weibull = list(shape = max(0.1, mm / s), scale = mm),
    beta = list(shape1 = beta_m * concentration,
                shape2 = (1 - beta_m) * concentration),
    poisson = list(lambda = mm),
    bernoulli = list(prob = prob),
    binomial = list(prob = prob),
    geometric = list(prob = 1 / (1 + mm)),
    negbinomial = {
      size <- if (s^2 > mm) mm^2 / (s^2 - mm) else 10
      list(size = max(size, 0.01), prob = size / (size + mm))
    },
    cauchy = list(location = m, scale = s / 2),
    logistic = list(location = m, scale = s * sqrt(3) / pi),
    laplace = list(mu = m, sigma = s / sqrt(2)),
    default_params(dl_entry(id)))
}

# Each free parameter has an identity, log or logit coordinate.
fit_transform <- function(spec, inverse = FALSE, x) {
  x <- unname(x)
  lo <- spec$lower
  hi <- spec$upper
  if (is.finite(lo) && is.finite(hi)) {
    if (inverse) lo + (hi - lo) * stats::plogis(x) else
      stats::qlogis(pmin(1 - 1e-8, pmax(1e-8, (x - lo) / (hi - lo))))
  } else if (is.finite(lo)) {
    if (inverse) lo + exp(x) else log(max(x - lo, .Machine$double.eps))
  } else if (is.finite(hi)) {
    if (inverse) hi - exp(x) else log(max(hi - x, .Machine$double.eps))
  } else x
}

# Location families whose location is found numerically (all grouped fits,
# raw Logistic and Cauchy) are fitted to data shifted by a central value.
# Their likelihood is exactly invariant, and difference steps then stay far
# above the rounding of a location such as 1e6 with a scale of 1e-3.
fit_one <- function(data, id, fixed, level, control) {
  grouped <- identical(data$type, "grouped")
  location <- c(normal = "mean", logistic = "location", cauchy = "location", laplace = "mu")[id]
  if (!grouped && id %in% c("normal", "laplace")) location <- NA
  limits <- if (grouped) c(data$lower, data$upper) else data$x
  center <- stats::median(limits[is.finite(limits)])
  if (is.na(location) || location %in% names(fixed) || !is.finite(center) || center == 0) {
    return(fit_one_core(data, id, fixed, level, control))
  }
  shifted <- data
  shifted$x <- data$x - center
  if (grouped) {
    shifted$lower <- data$lower - center
    shifted$upper <- data$upper - center
  }
  fit <- fit_one_core(shifted, id, fixed, level, control)
  params <- fit$dist$params
  params[[location]] <- params[[location]] + center
  fit$dist <- do.call(dl_dist, c(list(id), params))
  row <- fit$estimates$parameter == location
  fit$estimates[row, c("estimate", "lower", "upper")] <-
    fit$estimates[row, c("estimate", "lower", "upper")] + center
  logLik <- suppressWarnings(fit_loglik(data, fit$dist))
  if (is.finite(logLik)) {
    fit$logLik <- logLik
    fit$AIC <- 2 * fit$k - 2 * logLik
    fit$BIC <- log(data$n) * fit$k - 2 * logLik
  }
  fit
}

fit_one_core <- function(data, id, fixed, level, control) {
  entry <- dl_entry(id)
  fit_check_named_list(fixed, paste0("fixed$", id))
  specs <- stats::setNames(entry$parameters,
                           vapply(entry$parameters, `[[`, "", "name"))
  if (length(setdiff(names(fixed), names(specs)))) {
    stop("Unknown fixed parameter for ", id, ".", call. = FALSE)
  }
  for (nm in names(fixed)) {
    if (!in_domain(fixed[[nm]], specs[[nm]])) {
      stop("Fixed parameter `", nm, "` is outside its domain.", call. = FALSE)
    }
  }
  needed <- switch(id, binomial = "size", betabinomial = "size",
                   hypergeometric = c("m", "n", "k"), character())
  if (id %in% c("uniform", "discrete_uniform") && data$type == "grouped") {
    needed <- c("min", "max")
  }
  missing <- setdiff(needed, names(fixed))
  if (length(missing)) {
    stop("Fix ", paste(missing, collapse = ", "), " for ", id,
         "; integer designs or grouped endpoints are not estimated.", call. = FALSE)
  }
  free <- setdiff(names(specs), names(fixed))
  raw <- !identical(data$type, "grouped")
  x <- data$x[data$counts > 0]
  counts <- data$counts[data$counts > 0]
  if (raw && entry$type == "discrete" && any(x != round(x))) {
    stop("Discrete distributions require integer observations.", call. = FALSE)
  }
  if (raw && id %in% c("beta", "gamma", "lognormal", "weibull", "chisq", "f")) {
    if (any(x <= 0) || (id == "beta" && any(x >= 1))) {
      stop("Observations must be strictly inside this family's support; endpoint likelihoods are nonregular.",
           call. = FALSE)
    }
  }
  if (raw && length(free)) {
    degenerate <- length(unique(x)) == 1L
    collapse <- switch(id,
      normal = all(c("mean", "sd") %in% free) ||
        ("sd" %in% free && all(x == fixed$mean)),
      lognormal = all(c("meanlog", "sdlog") %in% free) ||
        ("sdlog" %in% free && all(log(x) == fixed$meanlog)),
      gamma = all(c("shape", "rate") %in% free),
      beta = all(c("shape1", "shape2") %in% free),
      weibull = all(c("shape", "scale") %in% free),
      cauchy = all(c("location", "scale") %in% free) ||
        ("scale" %in% free && all(x == fixed$location)),
      logistic = all(c("location", "scale") %in% free) ||
        ("scale" %in% free && all(x == fixed$location)),
      laplace = all(c("mu", "sigma") %in% free) ||
        ("sigma" %in% free && all(x == fixed$mu)),
      # Sample extrema would give min = max, outside the min < max domain.
      uniform = , discrete_uniform = all(c("min", "max") %in% free) ||
        ("max" %in% free && all(x == fixed$min)) ||
        ("min" %in% free && all(x == fixed$max)), FALSE)
    if (degenerate && collapse) {
      stop("Constant data have no finite MLE for these free parameters.", call. = FALSE)
    }
    # The Cauchy likelihood behaves like scale^(n - 2m) as the scale shrinks
    # around a value with m copies, so m >= n / 2 leaves no unique interior
    # maximum; this includes every sample of two.
    if (id == "cauchy" && "scale" %in% free) {
      tied <- if ("location" %in% free) max(counts) else sum(counts[x == fixed$location])
      if (tied >= data$n / 2) {
        stop("The Cauchy likelihood has no unique finite maximum when half or more of the observations coincide.",
             call. = FALSE)
      }
    }
    if (id == "negbinomial" && all(x == 0) && "size" %in% free) {
      stop("All-zero data do not identify Negative-binomial size.", call. = FALSE)
    }
    if (id == "negbinomial" && all(c("size", "prob") %in% free)) {
      # Existence criterion of Brons and Jensen (1988), section 3: the
      # empirical variance uses divisor n, not the unbiased divisor n - 1.
      mean <- sum(x * (counts / data$n))
      variance <- sum((counts / data$n) * (x - mean)^2)
      if (variance <= mean) {
        stop("No finite Negative-binomial size MLE: empirical variance is not greater than the mean; use the Poisson limit.",
             call. = FALSE)
      }
    }
    if (id == "betabinomial" && all(c("alpha", "beta") %in% free)) {
      # The overdispersion score at the Binomial limit (Tarone, 1979) is
      # positive only when the variance (divisor n) exceeds size p (1 - p);
      # otherwise the limit is reported, as for the Negative binomial.
      mean <- sum(x * (counts / data$n))
      variance <- sum((counts / data$n) * (x - mean)^2)
      p <- mean / fixed$size
      if (variance <= fixed$size * p * (1 - p)) {
        stop("No finite Beta-binomial MLE: empirical variance does not exceed the Binomial variance; use the Binomial limit.",
             call. = FALSE)
      }
    }
    if (id == "poisson" && all(x == 0)) {
      stop("The Poisson likelihood supremum is at lambda = 0, outside the registry domain.",
           call. = FALSE)
    }
    if (id == "exponential" && all(x == 0)) {
      stop("All-zero data have no finite Exponential rate MLE.", call. = FALSE)
    }
  }
  pars <- utils::modifyList(default_params(entry), fit_start(data, id))
  pars <- utils::modifyList(pars, fixed)
  if (id == "binomial" && !"prob" %in% names(fixed) && raw) {
    pars$prob <- sum(x * counts) / data$n / pars$size
  }
  warnings <- if (id == "student_t") {
    "Student t uses the registry's fixed location = 0 and scale = 1; its only parameter is df."
  } else character()
  boundary <- FALSE
  analytic <- FALSE
  if (raw && id %in% c("uniform", "discrete_uniform")) {
    if ("min" %in% free) pars$min <- min(x)
    if ("max" %in% free) pars$max <- max(x)
    analytic <- TRUE
    boundary <- length(free) > 0
    if (boundary) warnings <- c(warnings, "Support endpoints are nonregular; Wald uncertainty is unavailable and information criteria need caution.")
  }
  # Closed probability boundaries are genuine maxima; logit cannot reach them.
  if (raw && "prob" %in% free && id %in% c("bernoulli", "binomial", "geometric", "negbinomial")) {
    endpoint <- if (all(x == 0)) {
      if (id %in% c("geometric", "negbinomial")) 1 else 0
    } else if (id == "bernoulli" && all(x == 1)) 1 else
      if (id == "binomial" && all(x == pars$size)) 1 else NA_real_
    if (!is.na(endpoint)) {
      pars$prob <- endpoint
      analytic <- TRUE
      boundary <- TRUE
      warnings <- c(warnings, "Probability estimate is on the boundary; Wald uncertainty is unavailable.")
    }
  }
  # Laplace location has a kink at each observation. Weighted medians give
  # its exact maximum; a numerical Hessian there is not an information matrix.
  if (raw && id == "laplace") {
    if ("mu" %in% free) {
      ord <- order(x)
      pars$mu <- x[ord][which(cumsum(counts[ord]) >= data$n / 2)[1]]
    }
    if ("sigma" %in% free) pars$sigma <- sum(counts * abs(x - pars$mu)) / data$n
    analytic <- TRUE
    if ("mu" %in% free) warnings <- c(warnings, "Laplace location has a nonsmooth likelihood; Hessian-based Wald uncertainty is unavailable.")
  }
  exact <- NULL
  if (raw && length(free) && !analytic) {
    closed <- fit_closed_form(id, x, counts, data$n, free, pars)
    # Data outside the support can give out-of-domain formulas; the numerical
    # path then reports the infinite likelihood.
    if (!is.null(closed) &&
        all(vapply(free, function(nm) in_domain(closed$pars[[nm]], specs[[nm]]), logical(1)))) {
      pars <- closed$pars
      exact <- closed$vcov
      analytic <- TRUE
    }
  }
  make_dist <- function(p) do.call(dl_dist, c(list(id), p))
  theta <- vapply(free, function(nm) fit_transform(specs[[nm]], x = pars[[nm]]), 0)
  decode <- function(th) {
    out <- pars
    for (j in seq_along(free)) {
      out[[free[j]]] <- fit_transform(specs[[free[j]]], inverse = TRUE, x = th[j])
    }
    out
  }
  # Sufficient statistics make each evaluation O(1) for some families; the
  # reported logLik below always sums the registry density.
  fast <- if (raw) fit_sufficient_loglik(id, x, counts, data$n)
  loglik <- if (is.null(fast)) function(p) fit_loglik(data, make_dist(p)) else fast
  objective <- function(th) {
    ll <- suppressWarnings(tryCatch(loglik(decode(th)), error = function(e) -Inf))
    if (is.finite(ll)) -ll else 1e100
  }
  convergence <- 0L
  if (length(free) && !analytic) {
    # Optimize theta = origin + unit * z, with locations in units of the
    # starting spread, so starts and tolerances do not depend on the data scale.
    origin <- theta
    start_dist <- tryCatch(suppressWarnings(make_dist(pars)), error = function(e) NULL)
    unit <- fit_units(start_dist, free, specs, pars)
    unit[is.na(unit)] <- 1
    standardized <- function(z) objective(origin + unit * z)
    zero <- rep(0, length(free))
    starts <- list(zero, zero + 0.5, zero - 0.5, rep(c(-1, 1), length.out = length(free)))
    opt <- lapply(starts, function(start) {
      suppressWarnings(tryCatch(stats::nlminb(start, standardized, control = control),
                                error = function(e) NULL))
    })
    valid <- vapply(opt, function(o) !is.null(o) && is.finite(o$objective) &&
                      o$objective < 1e99, logical(1))
    if (!any(valid)) stop("No finite likelihood; check support and fixed parameters.", call. = FALSE)
    opt <- opt[valid]
    values <- vapply(opt, `[[`, 0, "objective")
    best_index <- which.min(values)
    # Prefer successful convergence only for numerically equivalent optima.
    # A materially better, nonconverged solution keeps its failure status.
    equivalent <- which(vapply(opt, `[[`, 0L, "convergence") == 0L &
                          values <= min(values) + 1e-8 * (1 + abs(min(values))))
    if (length(equivalent)) best_index <- equivalent[which.min(values[equivalent])]
    best <- opt[[best_index]]
    theta <- origin + unit * best$par
    pars <- decode(theta)
    convergence <- best$convergence
    if (convergence != 0L) warnings <- c(warnings, paste("Optimizer:", best$message))
    moved <- abs(theta - origin) > 20
    bounded <- vapply(specs[free], function(s) is.finite(s$lower) || is.finite(s$upper), logical(1))
    boundary <- any(moved & bounded)
    if (boundary) {
      stop("No stable finite interior MLE: optimization approaches a boundary or limiting family.", call. = FALSE)
    }
  }
  dist <- suppressWarnings(make_dist(pars))
  logLik <- suppressWarnings(fit_loglik(data, dist))
  if (!is.finite(logLik)) stop("No finite likelihood; check support and fixed parameters.", call. = FALSE)
  if (!raw && length(free)) {
    fit_grouped_check(data, dist, theta, free, specs, pars, decode, make_dist, objective)
  }
  # A numerical optimum without positive definite information is not a
  # finite interior maximum (flat ridges, limiting families).
  unit <- fit_units(dist, free, specs, pars)
  unit[is.na(unit)] <- 1
  uncertainty <- fit_uncertainty(theta, free, specs, pars, objective, level,
                                 regular = !boundary && convergence == 0L &&
                                   !(raw && id == "laplace" && "mu" %in% free),
                                 exact = exact, strict = raw && !analytic, unit = unit)
  warnings <- c(warnings, uncertainty$warnings)
  k <- length(free)
  list(id = id, dist = dist, estimates = uncertainty$estimates,
       vcov = uncertainty$vcov, logLik = logLik, k = k,
       AIC = 2 * k - 2 * logLik, BIC = log(data$n) * k - 2 * logLik,
       convergence = convergence, boundary = boundary, warnings = unique(warnings))
}

# Closed-form MLEs for ungrouped data, with the inverse of their exact
# observed information (natural parameters). NULL when none applies.
fit_closed_form <- function(id, x, w, n, free, pars) {
  average <- function(v) sum(w * v) / n
  variances <- list()
  if (id %in% c("normal", "lognormal")) {
    location <- if (id == "normal") "mean" else "meanlog"
    scale <- if (id == "normal") "sd" else "sdlog"
    y <- if (id == "normal") x else log(x)
    if (location %in% free) pars[[location]] <- average(y)
    if (scale %in% free) pars[[scale]] <- sqrt(average((y - pars[[location]])^2))
    s <- pars[[scale]]
    # Observed information n / s^2 and 2 n / s^2; the cross term vanishes.
    variances[[location]] <- s^2 / n
    variances[[scale]] <- s^2 / (2 * n)
  } else if (id == "exponential") {
    pars$rate <- n / sum(w * x)
    variances$rate <- pars$rate^2 / n
  } else if (id == "poisson") {
    pars$lambda <- average(x)
    variances$lambda <- pars$lambda / n
  } else if (id %in% c("bernoulli", "binomial")) {
    size <- if (id == "binomial") pars$size else 1
    pars$prob <- average(x) / size
    variances$prob <- pars$prob * (1 - pars$prob) / (n * size)
  } else if (id == "geometric") {
    # Failures before the first success: prob = 1 / (1 + mean).
    pars$prob <- 1 / (1 + average(x))
    variances$prob <- pars$prob^2 * (1 - pars$prob) / n
  } else return(NULL)
  v <- unlist(variances[free])
  list(pars = pars, vcov = diag(v, length(v), length(v), names = FALSE))
}

# Log-likelihoods written in sufficient statistics: sum(x), sum(log x) and,
# for the Beta, sum(log(1 - x)). NULL for other families.
fit_sufficient_loglik <- function(id, x, w, n) {
  if (!id %in% c("gamma", "chisq", "beta")) return(NULL)
  sx <- sum(w * x)
  slog <- sum(w * log(x))
  switch(id,
    gamma = function(p) {
      n * (p$shape * log(p$rate) - lgamma(p$shape)) + (p$shape - 1) * slog - p$rate * sx
    },
    chisq = function(p) {
      h <- p$df / 2
      -n * (h * log(2) + lgamma(h)) + (h - 1) * slog - sx / 2
    },
    beta = {
      slog1 <- sum(w * log1p(-x))
      function(p) (p$shape1 - 1) * slog + (p$shape2 - 1) * slog1 - n * lbeta(p$shape1, p$shape2)
    })
}

# CDF values at all finite class boundaries determine the whole observed
# partition, including zero-count gaps and tails. Check their local rank in
# dimensionless coordinates before trusting optimizer convergence. In
# particular, two exhaustive classes cannot identify two free parameters.
fit_grouped_check <- function(data, dist, theta, free, specs, pars, decode,
                              make_dist, objective) {
  fail <- function() stop(
    "Grouped data do not identify a stable finite maximum; refine classes or fix parameters.",
    call. = FALSE)
  cuts <- sort(unique(c(data$lower, data$upper)))
  cuts <- cuts[is.finite(cuts)]
  if (length(cuts) < length(free)) fail()
  scale <- fit_units(dist, free, specs, pars)
  if (anyNA(scale)) fail()
  upper <- dl_cdf(dist, cuts) > .5
  probabilities <- function(z) {
    d <- make_dist(decode(theta + scale * z))
    # Use survival probabilities in the upper tail, so a rounded F = 1
    # does not erase derivatives that are still numerically available.
    ifelse(upper, -dl_cdf(d, cuts, lower.tail = FALSE), dl_cdf(d, cuts))
  }
  eps <- 1e-4
  jac <- vapply(seq_along(free), function(j) {
    z <- rep(0, length(free))
    z[j] <- eps
    (probabilities(z) - probabilities(-z)) / (2 * eps)
  }, numeric(length(cuts)))
  if (any(!is.finite(jac))) fail()
  singular <- svd(jac, nu = 0, nv = 0)$d
  if (length(singular) < length(free) || max(singular) < 1e-9 ||
      min(singular) <= max(singular) * 1e-6) fail()
  # A CDF saturated at an optimizer's limiting point may still leave tiny
  # nonzero numerical derivatives. Check normalized observed information
  # as well; a flat likelihood cannot support a ranked parameter estimate.
  h <- tryCatch(numDeriv::hessian(function(z) {
    objective(theta + scale * z) / data$n
  }, rep(0, length(free))), error = function(e) NULL)
  if (is.null(h) || any(!is.finite(h))) fail()
  eigenvalues <- eigen(h, symmetric = TRUE, only.values = TRUE)$values
  if (max(eigenvalues) < 1e-9 || min(eigenvalues) <= max(eigenvalues) * 1e-7) fail()
  invisible(NULL)
}

# Units that make optimizer coordinates dimensionless. A location parameter
# is measured in interquartile ranges of `dist` (meanlog in sdlog); log and
# logit coordinates are already free of the data scale. NA when unavailable.
fit_units <- function(dist, free, specs, pars) {
  iqr <- if (is.null(dist)) NA_real_ else
    tryCatch(diff(dl_quantile(dist, c(.25, .75))), error = function(e) NA_real_)
  unit <- vapply(free, function(nm) {
    spec <- specs[[nm]]
    if (is.finite(spec$lower) || is.finite(spec$upper)) return(1)
    if (nm == "meanlog") pars$sdlog else iqr
  }, 0)
  unit[!is.finite(unit) | unit <= 0] <- NA_real_
  unname(unit)
}

# Derivative of a natural parameter with respect to its optimizer
# coordinate (fit_transform()), at the natural value `x`.
fit_transform_slope <- function(spec, x) {
  lo <- spec$lower
  hi <- spec$upper
  if (is.finite(lo) && is.finite(hi)) {
    u <- (x - lo) / (hi - lo)
    (hi - lo) * u * (1 - u)
  } else if (is.finite(lo)) x - lo else if (is.finite(hi)) x - hi else 1
}

# Standard errors from the observed information: `exact` is the inverse
# information of the natural parameters. Otherwise the Hessian is taken at
# theta + unit * z, so a location far from zero or a tiny or huge scale
# neither distorts the difference steps nor the singularity test, and is
# mapped back by the delta method. With `strict`, singular or indefinite
# information is a fitting failure instead of a note.
fit_uncertainty <- function(theta, free, specs, pars, objective, level, regular,
                            exact = NULL, strict = FALSE, unit = rep(1, length(free))) {
  nms <- names(pars)
  p <- unlist(pars, use.names = FALSE)
  cov <- matrix(NA_real_, length(p), length(p), dimnames = list(nms, nms))
  fixed <- !nms %in% free
  cov[fixed, ] <- cov[, fixed] <- 0
  se <- rep(NA_real_, length(p))
  se[fixed] <- 0
  warnings <- character()
  if (length(free) && regular) {
    result <- if (!is.null(exact)) exact else tryCatch({
      # In these units a first step of 0.01 is small for the curvature yet
      # large enough that rounding in the log-likelihood stays negligible.
      h <- numDeriv::hessian(function(z) objective(theta + unit * z), rep(0, length(free)),
                             method.args = list(eps = 1e-2, d = 1e-2))
      eigenvalues <- eigen(h, symmetric = TRUE, only.values = TRUE)$values
      if (any(!is.finite(h)) || min(eigenvalues) <= max(eigenvalues) * 1e-10) {
        stop("Singular or non-positive information matrix.")
      }
      # d natural / d z = slope of the coordinate times its unit.
      jac <- unit * vapply(free, function(nm) fit_transform_slope(specs[[nm]], pars[[nm]]), 0)
      solve(h) * outer(jac, jac)
    }, error = function(e) NULL)
    if (is.null(result) && strict) {
      stop("Singular or non-positive observed information: no stable finite interior MLE.",
           call. = FALSE)
    }
    if (is.null(result)) {
      warnings <- "Observed information is singular or unstable; Wald uncertainty is unavailable."
    } else {
      cov[free, free] <- result
      se[match(free, nms)] <- sqrt(pmax(0, diag(result)))
    }
  }
  z <- stats::qnorm((1 + level) / 2)
  limits <- vapply(seq_along(p), function(j) {
    fit_wald_limits(specs[[nms[j]]], p[j], se[j], z)
  }, numeric(2))
  estimates <- data.frame(parameter = nms, estimate = p, se = se,
                          lower = limits[1, ], upper = limits[2, ], fixed = fixed,
                          row.names = NULL)
  list(estimates = estimates, vcov = cov, warnings = warnings)
}

# Wald limits estimate +- z * se on the optimizer coordinate of the parameter
# (identity, log or logit, as in fit_transform()), transformed back. There
# the standard error is se divided by |d estimate / d coordinate|.
fit_wald_limits <- function(spec, estimate, se, z) {
  if (!is.finite(se) || !is.finite(estimate)) return(c(NA_real_, NA_real_))
  if (se == 0) return(c(estimate, estimate))
  lo <- spec$lower
  hi <- spec$upper
  if (is.finite(lo) && is.finite(hi)) {
    u <- (estimate - lo) / (hi - lo)
    if (!(u > 0 && u < 1)) return(c(NA_real_, NA_real_))
    width <- z * se / ((hi - lo) * u * (1 - u))
    lo + (hi - lo) * stats::plogis(stats::qlogis(u) + c(-width, width))
  } else if (is.finite(lo)) {
    d <- estimate - lo
    if (!(d > 0)) return(c(NA_real_, NA_real_))
    lo + d * exp(c(-1, 1) * z * se / d)
  } else if (is.finite(hi)) {
    d <- hi - estimate
    if (!(d > 0)) return(c(NA_real_, NA_real_))
    hi - d * exp(c(1, -1) * z * se / d)
  } else estimate + c(-1, 1) * z * se
}

fit_ranking <- function(ids, type, fits, failures) {
  rows <- lapply(ids, function(id) {
    f <- fits[[id]]
    dt <- dl_entry(id)$type
    group <- if (type == "grouped") "interval_probability" else
      if (dt == "discrete") "probability_mass" else "density"
    data.frame(id = id, type = dt, likelihood_group = group,
      logLik = if (is.null(f)) NA_real_ else f$logLik,
      k = if (is.null(f)) NA_integer_ else f$k,
      AIC = if (is.null(f)) NA_real_ else f$AIC,
      BIC = if (is.null(f)) NA_real_ else f$BIC,
      delta_AIC = NA_real_, weight = NA_real_,
      converged = !is.null(f) && f$convergence == 0L,
      message = if (is.null(f)) failures[[id]] else paste(f$warnings, collapse = " "),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  for (group in unique(out$likelihood_group)) {
    i <- which(out$likelihood_group == group & out$converged & is.finite(out$AIC))
    if (!length(i)) next
    delta <- out$AIC[i] - min(out$AIC[i])
    out$delta_AIC[i] <- delta
    weight <- exp(-delta / 2)
    out$weight[i] <- weight / sum(weight)
  }
  out <- out[order(out$likelihood_group, !out$converged, out$AIC, na.last = TRUE), ]
  rownames(out) <- NULL
  out
}

# R literal that reads back identically: deparse() with its default 15
# significant digits when they suffice (0.95 stays 0.95), otherwise the 17
# digits of dput()'s "digits17" option, as for most observed doubles.
fit_literal <- function(x) {
  text <- paste(deparse(x), collapse = "\n")
  if (!identical(eval(str2lang(text), baseenv()), x)) {
    text <- paste(deparse(x, control = c("keepNA", "keepInteger", "niceNames",
                                         "showAttributes", "digits17")),
                  collapse = "\n")
  }
  text
}

# Script line that rebuilds the data. Raw observations stay raw: ties are
# written as rep(values, counts) rather than as a frequency table.
fit_data_code <- function(data) {
  if (data$type == "grouped") {
    paste0("data <- distlab::dl_fit_data(lower = ", fit_literal(data$lower),
           ", upper = ", fit_literal(data$upper),
           ", frequency = ", fit_literal(data$counts), ")")
  } else if (data$type == "raw") {
    x <- if (all(data$counts == 1)) fit_literal(data$x) else
      paste0("rep(", fit_literal(data$x), ", ", fit_literal(data$counts), ")")
    paste0("data <- distlab::dl_fit_data(x = ", x, ")")
  } else {
    paste0("data <- distlab::dl_fit_data(x = ", fit_literal(data$x),
           ", frequency = ", fit_literal(data$counts), ")")
  }
}

fit_code <- function(input, ids, fixed, level, control) {
  paste(input, paste0("fit <- distlab::dl_fit(data, ids = ", fit_literal(ids),
         ", fixed = ", fit_literal(fixed), ", level = ", fit_literal(level),
         ", control = ", fit_literal(control), ")"), "fit$ranking", sep = "\n")
}
