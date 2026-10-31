# Parameter values on both sides of every condition in the registry formulas.
latex_grid <- list(
  bernoulli = expand.grid(prob = c(0, 0.3, 0.5, 0.8, 1)),
  binomial = rbind(expand.grid(size = 5, prob = c(0, 0.25, 1 / 3, 0.5, 0.7, 1)),
                   data.frame(size = 4, prob = 0.5)),
  geometric = expand.grid(prob = c(0.2, 0.5, 0.75, 1 - 2^(-1 / 2), 1)),
  negbinomial = data.frame(size = c(0.5, 1, 3, 2.5, 3, 4),
                           prob = c(0.3, 0.5, 0.5, 0.4, 1, 0.25)),
  poisson = expand.grid(lambda = c(0.5, 2, 3.7)),
  hypergeometric = data.frame(m = c(10, 3, 1, 1, 2, 5, 4, 2),
                              n = c(15, 2, 1, 2, 1, 3, 4, 2),
                              k = c(8, 5, 1, 1, 2, 6, 4, 2)),
  betabinomial = data.frame(size = c(5, 10), alpha = c(2, 0.5), beta = c(3, 0.5)),
  discrete_uniform = data.frame(min = c(0, 1, -3), max = c(5, 7, 2)),
  uniform = data.frame(min = c(0, -2), max = c(1, 3)),
  beta = expand.grid(shape1 = c(0.5, 1, 2), shape2 = c(0.5, 1, 3)),
  exponential = expand.grid(rate = 1.5),
  gamma = expand.grid(shape = c(0.5, 1, 2), rate = 1.3),
  chisq = expand.grid(df = c(0.5, 1, 2, 3)),
  weibull = expand.grid(shape = c(0.5, 1, 3), scale = 2),
  lognormal = data.frame(meanlog = c(0, 1), sdlog = c(1, 0.5)),
  f = expand.grid(df1 = c(1, 2, 3), df2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
  normal = data.frame(mean = 0, sd = 1),
  student_t = expand.grid(df = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5)),
  cauchy = data.frame(location = 0, scale = 1),
  laplace = data.frame(mu = 0, sigma = 1),
  logistic = data.frame(location = 0, scale = 1)
)

.moment_keys <- c("mean", "variance", "skewness", "excess_kurtosis")

# All LaTeX strings of an entry: pdf, cdf, every expr and every condition.
latex_strings <- function(e) {
  out <- c(e$latex$pdf, e$latex$cdf)
  for (key in setdiff(names(e$latex), c("pdf", "cdf"))) {
    for (pc in formula_pieces(e$latex[[key]])) {
      out <- c(out, pc$expr, pc$condition)
    }
  }
  out
}

test_that("the grid covers every registry entry", {
  expect_setequal(names(latex_grid), dl_ids())
})

test_that("LaTeX strings are balanced and carry no math delimiters", {
  for (e in dl_registry()) {
    for (s in latex_strings(e)) {
      info <- paste(e$id, s)
      # Drop escaped braces and line breaks before counting.
      t <- gsub("\\\\\\\\", " ", s)
      t <- gsub("\\\\[{}]", " ", t)
      ch <- strsplit(t, "")[[1]]
      depth <- cumsum(ch == "{") - cumsum(ch == "}")
      expect_true(all(depth >= 0) && utils::tail(depth, 1) == 0, info = info)
      n_left <- lengths(regmatches(s, gregexpr("\\\\left(?![a-z])", s, perl = TRUE)))
      n_right <- lengths(regmatches(s, gregexpr("\\\\right(?![a-z])", s, perl = TRUE)))
      expect_identical(n_left, n_right, info = info)
      expect_false(grepl("$", s, fixed = TRUE), info = info)
      expect_false(grepl("\\\\[()]|\\\\\\[|\\\\\\]", s, perl = TRUE), info = info)
    }
  }
})

test_that("conditions are math only, with no words", {
  for (e in dl_registry()) {
    for (key in setdiff(names(e$latex), c("pdf", "cdf"))) {
      for (pc in formula_pieces(e$latex[[key]])) {
        if (is.null(pc$condition)) next
        bare <- gsub("\\\\[A-Za-z]+", " ", pc$condition)
        expect_false(grepl("[A-Za-z]{2,}", bare), info = paste(e$id, key, pc$condition))
      }
    }
  }
})

test_that("piecewise parameter conditions pick exactly one piece", {
  for (e in dl_registry()) {
    grid <- latex_grid[[e$id]]
    for (key in setdiff(names(e$latex), c("pdf", "cdf"))) {
      pieces <- formula_pieces(e$latex[[key]])
      if (length(pieces) < 2L) next
      conds <- lapply(pieces, function(pc) latex_condition_r(pc$condition, e))
      # Conditions on u, t, z or j are not functions of the parameters alone.
      if (any(vapply(conds, is.null, logical(1)))) next
      for (i in seq_len(nrow(grid))) {
        params <- as.list(grid[i, , drop = FALSE])
        hits <- vapply(conds, latex_condition_eval, logical(1), params = params)
        expect_identical(sum(hits), 1L,
                         info = paste(e$id, key, format(unlist(params))))
      }
    }
  }
})

test_that("existence statuses agree with dl_moments() on both sides of each condition", {
  # A finite closed form, an infinite moment (Inf) and a nonexistent one (NA).
  expected_kind <- function(pc) {
    if (!is.null(pc$status)) pc$status else "finite"
  }
  engine_kind <- function(v) {
    if (is.na(v)) "undefined" else if (is.infinite(v)) "infinite" else "finite"
  }
  for (e in dl_registry()) {
    grid <- latex_grid[[e$id]]
    for (key in .moment_keys) {
      pieces <- formula_pieces(e$latex[[key]])
      conds <- lapply(pieces, function(pc) {
        if (is.null(pc$condition)) "TRUE" else latex_condition_r(pc$condition, e)
      })
      expect_false(any(vapply(conds, is.null, logical(1))), info = paste(e$id, key))
      used <- logical(length(pieces))
      for (i in seq_len(nrow(grid))) {
        params <- as.list(grid[i, , drop = FALSE])
        hits <- vapply(conds, latex_condition_eval, logical(1), params = params)
        info <- paste(e$id, key, format(unlist(params)))
        expect_identical(sum(hits), 1L, info = info)
        d <- do.call(dl_dist, c(list(e$id), params))
        got <- dl_moments(d, method = "exact")[[key]]
        expect_identical(engine_kind(got), expected_kind(pieces[[which(hits)[1]]]),
                         info = info)
        used <- used | hits
      }
      # Every piece is reached by some grid point.
      expect_true(all(used), info = paste(e$id, key))
    }
  }
})

test_that("closed forms agree with the engine for selected entries", {
  ex <- function(id, key) formula_pieces(dl_registry()[[id]]$latex[[key]])
  # Gamma: moments, mgf and entropy (rate parameterization).
  a <- 2.5; b <- 1.3
  expect_identical(ex("gamma", "mean")[[1]]$expr, "\\frac{\\alpha}{\\beta}")
  m <- dl_moments(dl_dist("gamma", shape = a, rate = b))
  expect_equal(unname(m[c("mean", "variance", "skewness", "excess_kurtosis")]),
               c(a / b, a / b^2, 2 / sqrt(a), 6 / a))
  t <- 0.6
  # Log scale keeps the integrand finite far in the tail.
  mgf <- stats::integrate(function(x) exp(t * x + stats::dgamma(x, a, b, log = TRUE)),
                          0, Inf)$value
  expect_equal(mgf, (1 - t / b)^(-a), tolerance = 1e-7)
  ent <- -stats::integrate(function(x) {
    stats::dgamma(x, a, b, log = TRUE) * stats::dgamma(x, a, b)
  }, 0, Inf)$value
  expect_equal(ent, a - log(b) + lgamma(a) + (1 - a) * digamma(a), tolerance = 1e-7)
  # Student's t: variance, kurtosis and entropy.
  nu <- 6
  m <- dl_moments(dl_dist("student_t", df = nu))
  expect_equal(unname(m[c("variance", "excess_kurtosis")]), c(nu / (nu - 2), 6 / (nu - 4)))
  ent <- -stats::integrate(function(x) stats::dt(x, nu, log = TRUE) * stats::dt(x, nu),
                           -Inf, Inf)$value
  expect_equal(ent, (nu + 1) / 2 * (digamma((nu + 1) / 2) - digamma(nu / 2)) +
                 log(sqrt(nu) * beta(nu / 2, 1 / 2)), tolerance = 1e-7)
  # F: variance against the engine.
  d1 <- 3; d2 <- 10
  expect_equal(dl_moments(dl_dist("f", df1 = d1, df2 = d2))[["variance"]],
               2 * d2^2 * (d1 + d2 - 2) / (d1 * (d2 - 2)^2 * (d2 - 4)))
  # Closed-form quantiles and medians.
  u <- c(0.05, 0.3, 0.6, 0.95)
  expect_equal(stats::qweibull(u, 1.7, 2), 2 * (-log(1 - u))^(1 / 1.7))
  expect_equal(extraDistr::qlaplace(u, 0.4, 1.3),
               0.4 - 1.3 * sign(u - 0.5) * log(1 - 2 * abs(u - 0.5)))
  expect_equal(stats::qgeom(0.5, 0.3), ceiling(-log(2) / log(0.7)) - 1)
  # Negative binomial pgf and mode, summed over the support.
  r <- 3.5; p <- 0.4; x <- 0:2000; w <- stats::dnbinom(x, r, p); z <- 0.7
  expect_equal(sum(w * z^x), (p / (1 - (1 - p) * z))^r)
  expect_equal(x[which.max(w)], max(0, floor((r - 1) * (1 - p) / p)))
})

test_that("the condition translator handles sets, integers and chains", {
  e <- dl_registry()$binomial
  r <- latex_condition_r("(n + 1) p \\in \\{1, 2, \\ldots, n\\}", e)
  expect_true(latex_condition_eval(r, list(size = 5, prob = 0.5)))
  expect_false(latex_condition_eval(r, list(size = 5, prob = 0.3)))
  expect_false(latex_condition_eval(r, list(size = 5, prob = 1)))
  r <- latex_condition_r("0 < p < 1", e)
  expect_true(latex_condition_eval(r, list(size = 5, prob = 0.2)))
  expect_false(latex_condition_eval(r, list(size = 5, prob = 1)))
  expect_null(latex_condition_r("t < -\\log(1 - p)", e))
})
