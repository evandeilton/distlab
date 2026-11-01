# Simulation engine: seeded Monte Carlo checks of moments and probabilities,
# the law of large numbers, the central limit theorem and memorylessness.
# Every result carries R code that draws in the same order and reproduces its
# numbers exactly. No shiny here, so every piece can be tested on its own.

# Seed argument: NULL (use the current stream, reported as NA) or a single
# whole number in the integer range.
sim_seed <- function(seed) {
  if (is.null(seed)) return(NA_integer_)
  ok <- is.numeric(seed) && length(seed) == 1L && is.finite(seed) &&
    seed == round(seed) && abs(seed) <= .Machine$integer.max
  if (!ok) {
    stop("`seed` must be NULL or a single whole number between -2147483647 and 2147483647.",
         call. = FALSE)
  }
  as.integer(seed)
}

# Evaluate `code` right after set.seed(seed), then put the caller's random
# number stream back (or remove .Random.seed when there was none). With an NA
# seed the code runs on the current stream.
with_seed <- function(seed, code) {
  if (is.na(seed)) return(code)
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  })
  set.seed(seed)
  code
}

# Single whole number in [lower, upper], returned as a double so products of
# counts never overflow the integer range.
check_whole <- function(x, name, lower = 1, upper = 1e7) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x) && x == round(x) &&
    x >= lower && x <= upper
  if (!ok) {
    stop(sprintf("`%s` must be a single whole number from %s to %s.", name,
                 format(lower, big.mark = ",", scientific = FALSE),
                 format(upper, big.mark = ",", scientific = FALSE)), call. = FALSE)
  }
  as.numeric(x)
}

# Single finite number.
check_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop(sprintf("`%s` must be a single finite number.", name), call. = FALSE)
  }
  as.numeric(x)
}

check_dist <- function(dist) {
  if (!inherits(dist, "dl_dist")) {
    stop("`dist` must be a dl_dist object; see dl_dist().", call. = FALSE)
  }
}

# Error with a `key` the app translates through the dictionary entry
# "sim.error.<key>"; the message is the English text for the console.
sim_stop <- function(key, message) {
  stop(structure(class = c("distlab_sim_error", "error", "condition"),
                 list(message = message, call = NULL, key = key)))
}

# Kolmogorov-Smirnov distance sup|F_n - F| between a sample and a CDF, exact
# for a continuous CDF: the supremum is reached at a draw, on one side of the
# jump of F_n there. With ties F_n jumps by several steps at one point.
ks_dist <- function(v, cdf) {
  v <- sort(v)
  u <- cdf(v)
  i <- seq_along(v)
  max(i / length(v) - u, u - (i - 1) / length(v))
}

# The same helper as R source, for generated code.
.dl_ks_code <- paste(
  "ks_dist <- function(v, cdf) {",
  "  v <- sort(v)",
  "  u <- cdf(v)",
  "  i <- seq_along(v)",
  "  max(i / length(v) - u, u - (i - 1) / length(v))",
  "}", sep = "\n")

# Total variation distance between the empirical mass of whole-number draws
# and a mass function: half the absolute differences over the observed range,
# plus the mass the law puts outside it.
tv_dist <- function(v, pmf) {
  k <- seq(min(v), max(v))
  p <- pmf(k)
  p_hat <- tabulate(v - min(v) + 1, nbins = length(k)) / length(v)
  0.5 * (sum(abs(p_hat - p)) + max(0, 1 - sum(p)))
}

.dl_tv_code <- paste(
  "tv_dist <- function(v, pmf) {",
  "  k <- seq(min(v), max(v))",
  "  p <- pmf(k)",
  "  p_hat <- tabulate(v - min(v) + 1, nbins = length(k)) / length(v)",
  "  0.5 * (sum(abs(p_hat - p)) + max(0, 1 - sum(p)))",
  "}", sep = "\n")

# Call of the d, p, q or r function of `dist` as R source, with `first` (text)
# as first argument and exact parameter values; entries without a public R
# function go through the engine object `d` (see code_dist()).
code_fn <- function(dist, kind, first, extra = NULL) {
  cc <- code_call(dist, kind, exact = TRUE)
  if (is.null(cc)) {
    eng <- c(d = "distlab::dl_density", p = "distlab::dl_cdf", q = "distlab::dl_quantile",
             r = "distlab::dl_sample")[[kind]]
    return(paste0(eng, "(d, ", paste(c(first, extra), collapse = ", "), ")"))
  }
  paste0(cc$fun, "(", paste(c(first, cc$args, extra), collapse = ", "), ")")
}

# TRUE when code_fn() needs the engine object `d` for one of `kinds`.
code_needs_dist <- function(dist, kinds) {
  any(vapply(kinds, function(k) is.null(code_call(dist, k)), logical(1)))
}

# First lines of generated code: the seed, then `d` when needed.
code_head <- function(seed, dist = NULL) {
  c(if (!is.na(seed)) sprintf("set.seed(%d)", seed),
    if (!is.null(dist)) code_dist(dist, exact = TRUE))
}

# Numbers as an R vector literal, exact.
code_vec <- function(x) {
  v <- vapply(x, code_num, "", exact = TRUE)
  if (length(v) == 1L) v else paste0("c(", paste(v, collapse = ", "), ")")
}

#' Monte Carlo check of moments and probabilities
#'
#' Draws a seeded sample and compares its mean, variance, standard deviation,
#' skewness, excess kurtosis and the proportions below the three exact
#' quartiles with the exact values from [dl_moments()] and [dl_cdf()].
#'
#' Standard errors and Wald intervals \eqn{\hat\theta \pm z\,\mathrm{SE}} are
#' given where the asymptotics hold:
#' * mean: \eqn{s / \sqrt{n}}, when the exact variance is finite;
#' * variance: \eqn{\sqrt{(m_4 - m_2^2) / n}} with sample central moments
#'   \eqn{m_k}, when the exact fourth moment is finite (finite excess
#'   kurtosis);
#' * probabilities: \eqn{\sqrt{\hat p (1 - \hat p) / n}}.
#'
#' The standard deviation, skewness and kurtosis have no standard error here.
#' When the exact value is infinite or does not exist (`Inf` or `NA` from
#' [dl_moments()]) the estimate is still reported, without an interval. For
#' discrete distributions the exact probability at a quartile \eqn{q} is
#' \eqn{F(q) \ge 1/4}, not 1/4 itself. Sample skewness \eqn{m_3 / m_2^{3/2}}
#' needs at least three draws and excess kurtosis \eqn{m_4 / m_2^2 - 3} at
#' least four; a constant sample has neither.
#'
#' @param dist A `dl_dist` object from [dl_dist()].
#' @param n Number of draws, a whole number from 1 to \eqn{10^7}.
#' @param seed `NULL` to use the current random number stream, or a whole
#'   number passed to [set.seed()]. The caller's stream is restored afterwards.
#' @param level Confidence level of the intervals, in (0, 1).
#'
#' @returns A data frame with one row per quantity (`"mean"`, `"variance"`,
#'   `"sd"`, `"skewness"`, `"excess_kurtosis"`, `"cdf_q1"`, `"cdf_q2"`,
#'   `"cdf_q3"`) and the columns `quantity`, `q` (the quartile of a
#'   probability row), `exact`, `estimate`, `se`, `lower`, `upper` and
#'   `covered` (whether the interval holds the exact value; `NA` without an
#'   interval). Attributes `seed` (`NA` when `seed` is `NULL`) and `code` (R
#'   code that reproduces the table exactly when a seed is given).
#'
#' @seealso [dl_moments()], [dl_lln()]
#' @family simulation
#' @export
#' @examples
#' chk <- dl_mc_check(dl_dist("gamma", shape = 2, rate = 1), n = 2000, seed = 1)
#' chk[, c("quantity", "exact", "estimate", "covered")]
#' cat(attr(chk, "code"))
dl_mc_check <- function(dist, n = 10000, seed = NULL, level = 0.95) {
  res <- mc_check_run(dist, n, seed, level)
  out <- res$table
  attr(out, "seed") <- res$seed
  attr(out, "code") <- res$code
  out
}

# Monte Carlo check with the sample kept, for plots: list(table, sample,
# seed, code).
mc_check_run <- function(dist, n = 10000, seed = NULL, level = 0.95) {
  check_dist(dist)
  n <- check_whole(n, "n", 1, 1e7)
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      !(level > 0 && level < 1)) {
    stop("`level` must be a single number in (0, 1).", call. = FALSE)
  }
  seed <- sim_seed(seed)
  x <- with_seed(seed, dl_sample(dist, n))
  m <- dl_moments(dist)
  quartiles <- dl_quantile(dist, c(0.25, 0.5, 0.75))
  nn <- length(x)
  dev <- x - mean(x)
  m2 <- mean(dev^2)
  m3 <- mean(dev^3)
  m4 <- mean(dev^4)
  p_hat <- vapply(quartiles, function(q) mean(x <= q), numeric(1))
  # Sample skewness needs three draws and kurtosis four; 0/0 becomes NA.
  skew <- if (nn >= 3) m3 / m2^1.5 else NA
  kurt <- if (nn >= 4) m4 / m2^2 - 3 else NA
  estimate <- c(mean(x), stats::var(x), stats::sd(x), skew, kurt, p_hat)
  estimate[is.nan(estimate)] <- NA
  exact <- c(m, dl_cdf(dist, quartiles))
  se_mean <- is.finite(m[["variance"]])
  se_var <- is.finite(m[["excess_kurtosis"]])
  se <- c(if (se_mean) stats::sd(x) / sqrt(nn) else NA,
          if (se_var) sqrt(pmax(0, m4 - m2^2) / nn) else NA,
          NA, NA, NA, sqrt(p_hat * (1 - p_hat) / nn))
  z <- stats::qnorm(1 - (1 - level) / 2)
  lower <- estimate - z * se
  upper <- estimate + z * se
  table <- data.frame(
    quantity = c("mean", "variance", "sd", "skewness", "excess_kurtosis",
                 "cdf_q1", "cdf_q2", "cdf_q3"),
    q = c(NA, NA, NA, NA, NA, quartiles),
    exact = unname(exact), estimate, se, lower, upper,
    covered = unname(exact) >= lower & unname(exact) <= upper
  )
  code <- mc_check_code(dist, n, seed, level, nn >= 3, nn >= 4, se_mean, se_var)
  list(table = table, sample = x, seed = seed, code = code)
}

# R code of dl_mc_check(); the flags say which skewness, kurtosis and
# standard errors exist, as in mc_check_run().
mc_check_code <- function(dist, n, seed, level, has_skew, has_kurt, se_mean, se_var) {
  paste(c(
    code_head(seed, dist),
    sprintf("x <- %s", code_fn(dist, "r", code_num(n))),
    "n <- length(x)",
    "dev <- x - mean(x)",
    "m2 <- mean(dev^2)",
    "m3 <- mean(dev^3)",
    "m4 <- mean(dev^4)",
    sprintf("quartiles <- %s", code_fn(dist, "q", "c(0.25, 0.5, 0.75)")),
    "p_hat <- sapply(quartiles, function(q) mean(x <= q))",
    sprintf("estimate <- c(mean(x), var(x), sd(x), %s, %s, p_hat)",
            if (has_skew) "m3 / m2^1.5" else "NA", if (has_kurt) "m4 / m2^2 - 3" else "NA"),
    "estimate[is.nan(estimate)] <- NA",
    sprintf("exact <- unname(c(distlab::dl_moments(d), %s))", code_fn(dist, "p", "quartiles")),
    sprintf("se <- c(%s, %s, NA, NA, NA, sqrt(p_hat * (1 - p_hat) / n))",
            if (se_mean) "sd(x) / sqrt(n)" else "NA",
            if (se_var) "sqrt(pmax(0, m4 - m2^2) / n)" else "NA"),
    sprintf("z <- qnorm(1 - (1 - %s) / 2)", code_num(level, exact = TRUE)),
    "lower <- estimate - z * se",
    "upper <- estimate + z * se",
    "data.frame(",
    "  quantity = c(\"mean\", \"variance\", \"sd\", \"skewness\", \"excess_kurtosis\",",
    "               \"cdf_q1\", \"cdf_q2\", \"cdf_q3\"),",
    "  q = c(NA, NA, NA, NA, NA, quartiles),",
    "  exact, estimate, se, lower, upper,",
    "  covered = exact >= lower & exact <= upper",
    ")"
  ), collapse = "\n")
}

#' Law of large numbers: running means
#'
#' Draws `paths` independent sequences of `n` values and returns the running
#' mean \eqn{\bar X_k = (X_1 + \dots + X_k) / k} of each one. When the mean
#' exists the running means settle around it; for the Cauchy, or Student's t
#' with one degree of freedom, the mean does not exist and they never settle.
#'
#' @inheritParams dl_mc_check
#' @param n Length of each sequence, a whole number; `n * paths` may not
#'   exceed \eqn{10^7}.
#' @param paths Number of independent sequences, from 1 to 100.
#'
#' @returns A list with
#'   * `paths`: data frame with columns `path`, `k` and `mean` (the running
#'     mean after `k` draws);
#'   * `mean`, `sd`: exact mean and standard deviation (`Inf` when infinite,
#'     `NA` when they do not exist);
#'   * `seed`: the seed used, or `NA`;
#'   * `code`: R code that reproduces the running means exactly when a seed
#'     is given.
#'
#' @seealso [dl_clt()], [dl_mc_check()]
#' @family simulation
#' @export
#' @examples
#' lln <- dl_lln(dl_dist("exponential", rate = 2), n = 500, paths = 2, seed = 3)
#' tail(lln$paths, 3)
#' lln$mean
#' # No mean: the running means of the Cauchy keep jumping.
#' dl_lln(dl_dist("cauchy"), n = 500, seed = 3)$mean
dl_lln <- function(dist, n = 1000, paths = 1, seed = NULL) {
  check_dist(dist)
  n <- check_whole(n, "n", 1, 1e7)
  paths <- check_whole(paths, "paths", 1, 100)
  if (n * paths > 1e7) {
    stop("`n * paths` must not exceed 10,000,000 draws.", call. = FALSE)
  }
  seed <- sim_seed(seed)
  discrete <- identical(dist$entry$type, "discrete")
  x <- with_seed(seed, matrix(dl_sample(dist, n * paths), nrow = n))
  # Integer draws are summed as doubles, so long sequences cannot overflow.
  running <- if (discrete) {
    apply(x, 2, function(v) cumsum(as.numeric(v)) / seq_along(v))
  } else {
    apply(x, 2, function(v) cumsum(v) / seq_along(v))
  }
  m <- dl_moments(dist)
  draws <- if (paths == 1) "n" else sprintf("n * %s", code_num(paths))
  code <- paste(c(
    code_head(seed, if (code_needs_dist(dist, "r")) dist),
    sprintf("n <- %s", code_num(n)),
    sprintf("x <- matrix(%s, nrow = n)", code_fn(dist, "r", draws)),
    sprintf("running <- apply(x, 2, function(v) %s / seq_along(v))",
            if (discrete) "cumsum(as.numeric(v))" else "cumsum(v)")
  ), collapse = "\n")
  list(paths = data.frame(path = rep(seq_len(paths), each = n),
                          k = rep(seq_len(n), times = paths),
                          mean = as.vector(running)),
       mean = m[["mean"]], sd = m[["sd"]], seed = seed, code = code)
}

#' Central limit theorem: sample means of growing size
#'
#' For each sample size \eqn{m} in `sizes`, draws `reps` samples and takes
#' their means. When the exact variance is finite and positive the means are
#' standardized, \eqn{Z = (\bar X - \mu) / (\sigma / \sqrt{m})}, and compared
#' with the standard Normal by the Kolmogorov distance
#' \eqn{\sup_z |F_{reps}(z) - \Phi(z)|}. Without a finite variance (Cauchy,
#' Student's t with \eqn{\nu \le 2}) the raw means are returned: the mean of
#' \eqn{m} Cauchy draws is again Cauchy, so it does not narrow.
#'
#' Sizes are drawn in the order given, each as one block of `m * reps` values.
#'
#' @inheritParams dl_mc_check
#' @param sizes Distinct whole numbers from 1 to 10,000: the sample sizes.
#' @param reps Number of samples per size, from 2 to \eqn{10^6};
#'   `reps * sum(sizes)` may not exceed \eqn{2 \times 10^7}.
#'
#' @returns A list with
#'   * `means`: data frame with columns `size` and `value` (standardized or
#'     raw sample means);
#'   * `standardized`: `TRUE` when the means are standardized;
#'   * `distance`: data frame with columns `size` and `ks`, the Kolmogorov
#'     distance to the standard Normal (`NA` when not standardized);
#'   * `mean`, `sd`: exact mean and standard deviation;
#'   * `seed`, `code`: as in [dl_lln()].
#'
#' @seealso [dl_lln()], [dl_transform()]
#' @family simulation
#' @export
#' @examples
#' clt <- dl_clt(dl_dist("exponential"), sizes = c(1, 5, 30), reps = 500, seed = 4)
#' clt$distance
#' dl_clt(dl_dist("cauchy"), sizes = c(1, 30), reps = 200, seed = 4)$standardized
dl_clt <- function(dist, sizes = c(1, 2, 5, 10, 30, 100), reps = 2000, seed = NULL) {
  check_dist(dist)
  ok <- is.numeric(sizes) && length(sizes) >= 1L && length(sizes) <= 50L &&
    all(is.finite(sizes)) && all(sizes == round(sizes)) && all(sizes >= 1) &&
    all(sizes <= 1e4) && !anyDuplicated(sizes)
  if (!ok) {
    stop("`sizes` must be 1 to 50 distinct whole numbers from 1 to 10,000.", call. = FALSE)
  }
  sizes <- as.numeric(sizes)
  reps <- check_whole(reps, "reps", 2, 1e6)
  if (reps * sum(sizes) > 2e7) {
    stop("`reps * sum(sizes)` must not exceed 20,000,000 draws.", call. = FALSE)
  }
  seed <- sim_seed(seed)
  m <- dl_moments(dist)
  mu <- m[["mean"]]
  sigma <- m[["sd"]]
  standardized <- is.finite(m[["variance"]]) && m[["variance"]] > 0
  means <- with_seed(seed, lapply(sizes, function(s) {
    colMeans(matrix(dl_sample(dist, s * reps), nrow = s))
  }))
  values <- if (standardized) {
    Map(function(v, s) (v - mu) / (sigma / sqrt(s)), means, sizes)
  } else {
    means
  }
  ks <- if (standardized) vapply(values, ks_dist, numeric(1), cdf = stats::pnorm) else
    rep(NA_real_, length(sizes))
  code <- c(
    code_head(seed, if (code_needs_dist(dist, "r")) dist),
    sprintf("sizes <- %s", code_vec(sizes)),
    sprintf("reps <- %s", code_num(reps)),
    sprintf("means <- lapply(sizes, function(m) colMeans(matrix(%s, nrow = m)))",
            code_fn(dist, "r", "m * reps"))
  )
  if (standardized) {
    code <- c(code,
              sprintf("mu <- %s", code_num(mu, exact = TRUE)),
              sprintf("sigma <- %s", code_num(sigma, exact = TRUE)),
              "z <- Map(function(v, m) (v - mu) / (sigma / sqrt(m)), means, sizes)",
              .dl_ks_code,
              "sapply(z, ks_dist, cdf = pnorm)")
  }
  list(means = data.frame(size = rep(sizes, each = reps), value = unlist(values)),
       standardized = standardized,
       distance = data.frame(size = sizes, ks = ks),
       mean = mu, sd = sigma, seed = seed, code = paste(code, collapse = "\n"))
}

#' Memorylessness: the residual after a threshold
#'
#' Compares the law of the residual \eqn{X - s} given \eqn{X > s} with the
#' law of \eqn{X}. For continuous distributions the exact conditional
#' survival is \eqn{P(X > s + t \mid X > s) = S(s + t) / S(s)}; for discrete
#' ones the residual is \eqn{X - s} given \eqn{X \ge s}, with
#' \eqn{P(X \ge s + t \mid X \ge s)}, so the Geometric (failures counted from
#' 0) satisfies \eqn{P(X \ge s + t \mid X \ge s) = P(X \ge t)}. Only the
#' Exponential and the Geometric are memoryless, with their special cases
#' Gamma(1, \eqn{\beta}), Weibull with shape 1, Chi-square with 2 degrees of
#' freedom and Negative binomial with size 1: the two curves coincide for every
#' \eqn{s}. Survival ratios are taken on the log scale with
#' `lower.tail = FALSE`, so they keep their precision far in the tail.
#'
#' The exact curves are drawn for \eqn{t} from 0 to the 95% point of both
#' \eqn{X} and the residual, but at most 15 interquartile ranges of \eqn{X}
#' above its median, so heavy tails stay readable.
#'
#' @inheritParams dl_mc_check
#' @param s Threshold, a finite number \eqn{\ge 0} (a whole number for
#'   discrete distributions).
#'
#' @returns A list with
#'   * `residual`: the simulated residuals;
#'   * `kept`: how many of the `n` draws passed the threshold (at least 30,
#'     otherwise an error asks for a smaller `s` or more draws);
#'   * `exact`: data frame with columns `t`, `survival` (\eqn{S(t)}) and
#'     `residual` (the conditional survival of the residual);
#'   * `memoryless`: `TRUE` for the Exponential, the Geometric and the
#'     parameter values that make another law one of them;
#'   * `s`, `seed`, `code` (R code that reproduces the residuals, both
#'     survival curves and their largest gap exactly when a seed is given).
#'
#' @details The distribution must have support \eqn{[0, \infty)}.
#'
#' @seealso [dl_transform()]
#' @family simulation
#' @export
#' @examples
#' ex <- dl_memoryless(dl_dist("exponential", rate = 1), s = 1, n = 2000, seed = 5)
#' head(ex$exact)
#' wb <- dl_memoryless(dl_dist("weibull", shape = 2, scale = 1), s = 1, n = 2000, seed = 5)
#' wb$memoryless
dl_memoryless <- function(dist, s, n = 10000, seed = NULL) {
  check_dist(dist)
  sup <- dl_support(dist)
  if (!(sup$lower == 0 && sup$upper == Inf)) {
    sim_stop("memoryless_support",
             "Memorylessness is defined here for distributions with support [0, Inf).")
  }
  s <- check_number(s, "s")
  discrete <- identical(dist$entry$type, "discrete")
  if (s < 0 || (discrete && s != round(s))) {
    stop(if (discrete) "`s` must be a whole number >= 0." else "`s` must be >= 0.",
         call. = FALSE)
  }
  n <- check_whole(n, "n", 1, 1e7)
  seed <- sim_seed(seed)
  x <- with_seed(seed, dl_sample(dist, n))
  keep <- if (discrete) x >= s else x > s
  if (sum(keep) < 30) {
    sim_stop("memoryless_few", sprintf(
      "Only %d of the %s draws passed s = %s; choose a smaller s or more draws (at least 30 must pass).",
      sum(keep), format(n, scientific = FALSE), format(s)))
  }
  residual <- x[keep] - s
  # Log survival: P(X > v) for continuous laws, P(X >= v) for discrete ones.
  log_surv <- function(v) {
    dl_cdf(dist, if (discrete) v - 1 else v, lower.tail = FALSE, log.p = TRUE)
  }
  # Grid up to the 95% point of both X and the residual, capped at the median
  # of X plus 15 interquartile ranges so heavy tails stay readable.
  hi <- c(dl_quantile(dist, 0.05, lower.tail = FALSE),
          dl_quantile(dist, log(0.05) + log_surv(s), lower.tail = FALSE, log.p = TRUE) - s)
  qq <- dl_quantile(dist, c(0.25, 0.5, 0.75))
  t_max <- min(max(hi[is.finite(hi)], -Inf), qq[2] + 15 * (qq[3] - qq[1]))
  # Degenerate quantiles fall back to the largest residual.
  if (!is.finite(t_max) || t_max <= 0) t_max <- max(residual)
  if (discrete) {
    t_max <- min(max(ceiling(t_max), 1), 5000)
    t <- 0:t_max
    t_code <- sprintf("0:%s", code_num(t_max))
  } else {
    t <- seq(0, t_max, length.out = 201)
    t_code <- sprintf("seq(0, %s, length.out = 201)", code_num(t_max, exact = TRUE))
  }
  shift <- if (discrete) " - 1" else ""
  survival <- dl_cdf(dist, if (discrete) t - 1 else t, lower.tail = FALSE)
  conditional <- exp(log_surv(s + t) - log_surv(s))
  tail_args <- c("lower.tail = FALSE", "log.p = TRUE")
  code <- paste(c(
    code_head(seed, if (code_needs_dist(dist, c("r", "p"))) dist),
    sprintf("x <- %s", code_fn(dist, "r", code_num(n))),
    sprintf("s <- %s", code_num(s, exact = TRUE)),
    sprintf("residual <- x[x %s s] - s", if (discrete) ">=" else ">"),
    sprintf("t <- %s", t_code),
    sprintf("survival <- %s", code_fn(dist, "p", paste0("t", shift), "lower.tail = FALSE")),
    sprintf("conditional <- exp(%s -", code_fn(dist, "p", paste0("s + t", shift), tail_args)),
    sprintf("  %s)", code_fn(dist, "p", paste0("s", shift), tail_args)),
    "max(abs(survival - conditional))"
  ), collapse = "\n")
  list(residual = residual, kept = length(residual),
       exact = data.frame(t = t, survival = survival, residual = conditional),
       memoryless = is_memoryless(dist),
       s = s, seed = seed, code = code)
}

# TRUE for the memoryless laws: the Exponential, the Geometric, and the
# parameter values that turn another entry into one of them.
is_memoryless <- function(dist) {
  p <- dist$params
  switch(dist$id,
         exponential = TRUE, geometric = TRUE,
         gamma = p$shape == 1, weibull = p$shape == 1, chisq = p$df == 2,
         negbinomial = p$size == 1,
         FALSE)
}

# Tidy histogram data: integer bins centered on whole numbers for discrete
# samples, bars of width `step` centered on the lattice the draws sit on
# (origin at the smallest draw), or the bins of `breaks` (an automatic grid on
# `range` when NULL) for continuous ones. Shares are taken over `total`
# draws, so bins inside a clipped range still estimate the density of the
# whole law; the density of a lattice bar is its share over `step`. Columns:
# left, right, mid, count, proportion and density.
sim_bins <- function(x, discrete = FALSE, breaks = NULL, range = NULL, total = length(x),
                     step = NULL) {
  x <- x[is.finite(x)]
  empty <- data.frame(left = numeric(0), right = numeric(0), mid = numeric(0),
                      count = integer(0), proportion = numeric(0), density = numeric(0))
  if (!length(x) || total <= 0) return(empty)
  if (!is.null(step) && !discrete) {
    x0 <- min(x)
    r <- if (is.null(range)) base::range(x) else range
    # Lattice indices of the range ends and of each draw inside it.
    j_lo <- ceiling((r[1] - x0) / step - 1e-9)
    j_hi <- floor((r[2] - x0) / step + 1e-9)
    if (j_hi < j_lo) return(empty)
    j <- round((x - x0) / step)
    j <- j[j >= j_lo & j <= j_hi]
    counts <- tabulate(j - j_lo + 1, nbins = j_hi - j_lo + 1)
    mid <- x0 + seq(j_lo, j_hi) * step
    return(data.frame(left = mid - step / 2, right = mid + step / 2, mid = mid, count = counts,
                      proportion = counts / total, density = counts / (total * step)))
  }
  if (discrete) {
    lo <- ceiling(if (is.null(range)) min(x) else range[1])
    hi <- floor(if (is.null(range)) max(x) else range[2])
    if (hi < lo) return(empty)
    k <- seq(lo, hi)
    v <- x[x >= lo & x <= hi]
    counts <- tabulate(round(v - lo) + 1, nbins = length(k))
    return(data.frame(left = k - 0.5, right = k + 0.5, mid = k, count = counts,
                      proportion = counts / total, density = counts / total))
  }
  if (is.null(breaks)) breaks <- sim_breaks(x, range)
  nb <- length(breaks) - 1L
  v <- x[x >= breaks[1] & x <= breaks[nb + 1L]]
  idx <- findInterval(v, breaks, rightmost.closed = TRUE, all.inside = TRUE)
  counts <- tabulate(idx, nbins = nb)
  left <- breaks[-(nb + 1L)]
  right <- breaks[-1L]
  data.frame(left = left, right = right, mid = (left + right) / 2, count = counts,
             proportion = counts / total, density = counts / (total * (right - left)))
}

# Spacing of the lattice the finite draws sit on: the smallest gap between
# distinct values when every gap is a whole multiple of it (up to rounding),
# else NA (no lattice, or a single value).
lattice_step <- function(x, tol = 1e-6) {
  u <- sort(unique(x[is.finite(x)]))
  if (length(u) < 2L) return(NA_real_)
  d <- diff(u)
  h <- min(d)
  r <- d / h
  if (h > 0 && all(abs(r - round(r)) <= tol * pmax(1, r))) h else NA_real_
}

# Lattice step for a histogram of `x` on `range`, or NA for continuous bins.
# "law": draws of a discrete law or a whole-number transform (unit step when
# drawn against a mass function); "auto": a lattice found in the data, used
# only when it is coarser than the continuous bins; "none": continuous bins.
# A lattice that would need more than `max_bins` bars gives NA. `width` is the
# continuous bin width to compare with (the automatic one when NULL).
bin_step <- function(x, range, mode = c("auto", "law", "none"), unit = FALSE,
                     max_bins = 2000, width = NULL) {
  mode <- match.arg(mode)
  if (mode == "none") return(NA_real_)
  x <- x[is.finite(x)]
  whole <- length(x) > 0 && all(x == round(x))
  h <- if (mode == "law" && unit && whole) 1 else lattice_step(x)
  if (is.na(h) && mode == "law" && whole) h <- 1
  if (is.na(h) || (range[2] - range[1]) / h + 1 > max_bins) return(NA_real_)
  if (mode == "auto" && h < (width %||% diff(sim_breaks(x, range))[1])) return(NA_real_)
  h
}

# Even breaks on `range` (the sample range when NULL), with a bin width from
# the Freedman-Diaconis rule on the draws inside it, between 10 and 80 bins.
sim_breaks <- function(x, range = NULL) {
  x <- x[is.finite(x)]
  r <- if (is.null(range)) base::range(x) else range
  lo <- r[1]
  hi <- r[2]
  # A constant sample gets a unit-wide window around its value.
  if (!(hi > lo)) {
    lo <- lo - 0.5
    hi <- hi + 0.5
  }
  v <- x[x >= lo & x <= hi]
  h <- if (length(v) > 1L) 2 * stats::IQR(v) / length(v)^(1 / 3) else NA_real_
  nb <- if (is.finite(h) && h > 0) ceiling((hi - lo) / h) else 30
  seq(lo, hi, length.out = min(max(nb, 10), 80) + 1)
}

# TRUE when a law has heavy tails for plotting: excess kurtosis infinite,
# undefined or above that of the Exponential (6).
heavy_tailed <- function(dist) {
  k <- tryCatch(dl_moments(dist)[["excess_kurtosis"]], error = function(err) NA_real_)
  !is.finite(k) || k > 6
}

# Plotting range for draws `x` shown against `dist`. Discrete laws cover every
# draw and the central mass (only the draws when the law is very wide). Heavy-tailed continuous laws are cut at their
# 0.5% and 99.5% quantiles, and at most 15 interquartile ranges from the
# median so the body stays visible (finite support ends are used as they
# are); other continuous laws cover every draw and the usual curve range.
sim_range <- function(dist, x) {
  x <- x[is.finite(x)]
  s <- dl_support(dist)
  if (identical(dist$entry$type, "discrete")) {
    # A law spread over more than 5000 values has no curve range; the draws
    # then set the range.
    r <- tryCatch(curve_range(dist), error = function(err) range(x))
    lo <- min(r[1], x)
    hi <- max(r[2], x)
    return(c(floor(lo), ceiling(hi)) + 0)
  }
  if (heavy_tailed(dist)) {
    qq <- dl_quantile(dist, c(0.25, 0.5, 0.75))
    iqr <- qq[3] - qq[1]
    lo <- if (is.finite(s$lower)) s$lower else
      max(dl_quantile(dist, 0.005), qq[2] - 15 * iqr)
    hi <- if (is.finite(s$upper)) s$upper else
      min(dl_quantile(dist, 0.005, lower.tail = FALSE), qq[2] + 15 * iqr)
    return(c(lo, hi))
  }
  r <- curve_range(dist)
  lo <- max(min(r[1], x), s$lower)
  hi <- min(max(r[2], x), s$upper)
  # A finite support end within 5% of the width is taken as the end itself.
  w <- hi - lo
  if (is.finite(s$lower) && lo - s$lower < 0.05 * w) lo <- s$lower
  if (is.finite(s$upper) && s$upper - hi < 0.05 * w) hi <- s$upper
  c(lo, hi) + 0
}

# Plotting range of draws with no reference law: their 0.5% and 99.5%
# sample quantiles when the spread is far wider than the interquartile
# range (heavy tails), else the whole sample.
sample_range <- function(x) {
  x <- x[is.finite(x)]
  r <- range(x)
  iqr <- stats::IQR(x)
  if (iqr > 0 && r[2] - r[1] > 30 * iqr) {
    return(unname(stats::quantile(x, c(0.005, 0.995))))
  }
  r
}
