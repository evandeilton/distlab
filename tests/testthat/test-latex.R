# Parameter values on both sides of every parameter condition in the
# registry formulas.
latex_grid <- list(
  bernoulli = expand.grid(prob = c(0, 0.3, 0.5, 0.8, 1)),
  binomial = rbind(expand.grid(size = 5, prob = c(0, 0.25, 1 / 3, 0.5, 0.7, 1)),
                   data.frame(size = 4, prob = 0.5)),
  geometric = expand.grid(prob = c(0.2, 0.5, 0.75, 1 - 2^(-1 / 2), 1)),
  negbinomial = data.frame(size = c(0.5, 1, 3, 2.5, 3, 4),
                           prob = c(0.3, 0.5, 0.5, 0.4, 1, 0.25)),
  poisson = expand.grid(lambda = c(0.5, 2, 3.7)),
  hypergeometric = data.frame(m = c(10, 3, 1, 1, 2, 5, 4, 2, 6),
                              n = c(15, 2, 1, 2, 1, 3, 4, 2, 2),
                              k = c(8, 5, 1, 1, 2, 6, 4, 2, 4)),
  betabinomial = data.frame(size = c(5, 10), alpha = c(2, 0.5), beta = c(3, 0.5)),
  discrete_uniform = data.frame(min = c(0, 1, -3), max = c(5, 7, 2)),
  uniform = data.frame(min = c(0, -2, 0.5, -3, -2), max = c(1, 3, 2, -1, 0)),
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
.formula_keys <- function(e) setdiff(names(e$latex), c("pdf", "cdf"))

test_that("the grid and the transcriptions cover every registry entry and key", {
  reg <- dl_registry()
  expect_setequal(names(latex_grid), names(reg))
  expect_setequal(names(latex_ref), names(reg))
  for (e in reg) {
    expect_setequal(names(latex_ref[[e$id]]), .formula_keys(e))
  }
})

test_that("every condition translates to a predicate", {
  for (e in dl_registry()) {
    for (key in .formula_keys(e)) {
      conds <- piece_conditions(e, key)
      expect_false(any(vapply(conds, is.null, logical(1))), info = paste(e$id, key))
    }
  }
})

test_that("each grid point picks exactly one piece, and every piece is reached", {
  for (e in dl_registry()) {
    for (key in .formula_keys(e)) {
      conds <- piece_conditions(e, key)
      used <- logical(length(conds))
      for (values in key_grid(latex_grid[[e$id]], key)) {
        i <- which_piece(conds, values)
        expect_false(is.na(i), info = paste(e$id, key, format(unlist(values))))
        if (!is.na(i)) used[i] <- TRUE
      }
      expect_true(all(used), info = paste(e$id, key, "pieces", toString(which(!used))))
    }
  }
})

test_that("existence statuses agree with dl_moments() on both sides of each condition", {
  kind <- function(v) if (is.na(v)) "undefined" else if (is.infinite(v)) "infinite" else "finite"
  for (e in dl_registry()) {
    for (key in .moment_keys) {
      pieces <- formula_pieces(e$latex[[key]])
      conds <- piece_conditions(e, key)
      for (values in key_grid(latex_grid[[e$id]], key)) {
        i <- which_piece(conds, values)
        got <- dl_moments(do.call(dl_dist, c(list(e$id), values)), method = "exact")[[key]]
        expect_identical(kind(got), pieces[[i]]$status %||% "finite",
                         info = paste(e$id, key, format(unlist(values))))
      }
    }
  }
})

test_that("statuses follow the Inf/NA convention", {
  for (e in dl_registry()) {
    pieces <- formula_pieces(e$latex$mgf)
    # E[exp(tX)] has a positive integrand, so it is never undefined.
    st <- unlist(lapply(pieces, `[[`, "status"))
    expect_false("undefined" %in% st, info = e$id)
  }
})

# Variable values for the numeric checks, kept moderate so series and
# integrals stay accurate.
.check_vars <- list(
  u = c(0, 0.13, 0.5, 0.62, 0.9, 1),
  t = c(-1, -0.4, 0, 0.3, 0.8, 1.2),
  z = c(-1.5, -0.5, 0, 0.5, 1, 1.5),
  j = c(-1.5, -1, -0.5, 0, 0.5, 1, 2, 3)
)

# Finite range of a discrete support that holds all but a negligible mass.
discrete_points <- function(d) {
  s <- dl_support(d)
  lo <- max(s$lower, dl_quantile(d, 0))
  hi <- min(s$upper, dl_quantile(d, 1e-300, lower.tail = FALSE))
  x <- seq(lo, hi)
  w <- dl_density(d, x)
  list(x = x[w > 0], w = w[w > 0])
}

# E[g(X)] for a continuous law as an integral over probabilities, in two
# halves so both tails keep precision; NA when the integral fails.
quantile_expectation <- function(d, g) {
  piece <- function(q) {
    stats::integrate(function(u) g(q(u)), 0, 0.5, rel.tol = 1e-10,
                     subdivisions = 2000L)$value
  }
  tryCatch(piece(function(u) dl_quantile(d, u)) +
             piece(function(u) dl_quantile(d, u, lower.tail = FALSE)),
           error = function(e) NA_real_)
}

# Numeric value of a key at a distribution and variable value.
latex_truth <- function(key, d, v) {
  disc <- identical(d$entry$type, "discrete")
  pts <- if (disc) discrete_points(d)
  expect_g <- function(g) {
    if (disc) sum(pts$w * g(pts$x)) else quantile_expectation(d, g)
  }
  switch(key,
    # Discrete: the generalized inverse inf{x : F(x) >= u} for u > 0, the
    # lower support end at u = 0 (as in R) and +Inf at u = 1 on an
    # unbounded support.
    quantile = if (!disc || v == 0) dl_quantile(d, v) else
      if (v == 1 && is.infinite(dl_support(d)$upper)) Inf else
        pts$x[which(cumsum(pts$w) >= v - 1e-12)[1]],
    mean = , variance = , skewness = , excess_kurtosis =
      dl_moments(d, method = "numeric")[[key]],
    raw_moment = expect_g(function(x) x^v),
    mgf = if (disc) expect_g(function(x) exp(v * x)) else {
      s <- dl_support(d)
      tryCatch(stats::integrate(function(x) exp(v * x + dl_density(d, x, log = TRUE)),
                                s$lower, s$upper, rel.tol = 1e-10)$value,
               error = function(e) NA_real_)
    },
    cf = complex(real = expect_g(function(x) cos(v * x)),
                 imaginary = expect_g(function(x) sin(v * x))),
    pgf = expect_g(function(x) v^x),
    entropy = if (disc) -sum(pts$w * log(pts$w)) else
      -quantile_expectation(d, function(x) dl_density(d, x, log = TRUE)),
    median = if (disc) {
      Fx <- cumsum(pts$w)
      Sx <- rev(cumsum(rev(pts$w)))
      pts$x[Fx >= 0.5 - 1e-12 & Sx >= 0.5 - 1e-12]
    } else dl_quantile(d, 0.5),
    mode = if (disc) pts$x[pts$w >= max(pts$w) * (1 - 1e-12)] else NULL
  )
}

# Check a continuous mode against the density on a fine quantile grid.
check_continuous_mode <- function(d, r, info) {
  s <- dl_support(d)
  x <- dl_quantile(d, seq(1e-6, 1 - 1e-6, length.out = 2001))
  f <- dl_density(d, x)
  m <- r$value
  if (r$kind == "interval") {
    expect_lt(diff(range(f)), 1e-8 * max(f), label = info)
  } else if (r$kind == "set") {
    # Both ends carry the supremum: the density rises toward each of them.
    for (end in m) {
      near <- if (end == s$lower) end + c(1e-8, 1e-4) else end - c(1e-8, 1e-4)
      expect_gt(dl_density(d, near[1]), dl_density(d, near[2]), label = info)
    }
  } else if (m %in% c(s$lower, s$upper)) {
    # The density is largest next to the end, possibly unbounded there.
    near <- if (m == s$lower) m + 1e-9 else m - 1e-9
    expect_gte(dl_density(d, near), max(f[abs(x - m) > 2e-9]) * (1 - 1e-6), label = info)
  } else {
    opt <- stats::optimize(function(x) dl_density(d, x), range(x), maximum = TRUE,
                           tol = 1e-12)$maximum
    expect_equal(opt, m, tolerance = 1e-5, label = info)
  }
}

test_that("transcriptions pick the YAML piece and match sums, integrals and stats", {
  for (e in dl_registry()) {
    for (key in .formula_keys(e)) {
      conds <- piece_conditions(e, key)
      var <- .latex_vars[key]
      grid <- latex_grid[[e$id]]
      vals <- if (is.na(var)) list(NULL) else .check_vars[[var]]
      for (i in seq_len(nrow(grid))) {
        p <- as.list(grid[i, , drop = FALSE])
        d <- do.call(dl_dist, c(list(e$id), p))
        for (v in vals) {
          values <- if (is.na(var)) p else c(p, stats::setNames(list(v), var))
          info <- paste(e$id, key, format(unlist(values)), collapse = " ")
          r <- latex_ref[[e$id]][[key]](p, v)
          expect_identical(r$piece, which_piece(conds, values), info = info)
          if (is.null(r$value)) next
          if (key == "mode" && !identical(e$type, "discrete")) {
            check_continuous_mode(d, r, info)
            next
          }
          truth <- latex_truth(key, d, v)
          if (key %in% c("median", "mode")) {
            want <- if (r$kind == "interval") range(truth) else truth
            expect_equal(sort(r$value), sort(want), tolerance = 1e-8, info = info)
            next
          }
          val <- r$value
          if (key %in% .moment_keys) {
            # Statuses are covered by the dl_moments() test above.
            if (!is.finite(val)) next
          } else if (!all(is.finite(c(Re(val), Im(val)))) ||
                     !all(is.finite(c(Re(truth), Im(truth))))) {
            # Exact sums can confirm an infinite value; integrals cannot.
            if (identical(e$type, "discrete") && isTRUE(val == Inf)) {
              expect_identical(Re(truth), Inf, info = info)
            }
            next
          }
          tol <- if (key %in% .moment_keys) 1e-5 else 1e-6
          expect_equal(Re(val), Re(truth), tolerance = tol, info = info)
          expect_equal(Im(val), Im(truth), tolerance = tol, info = info)
        }
      }
    }
  }
})

test_that("the registry formulas match the reviewed snapshot", {
  # A changed formula fails here until its transcription is revisited.
  latex <- lapply(dl_registry(), `[[`, "latex")
  expect_snapshot(cat(yaml::as.yaml(latex)))
})

test_that("LaTeX strings are balanced, delimiter free and math only in conditions", {
  for (e in dl_registry()) {
    expect_identical(distlab:::validate_latex(e$latex, e$type), character(), info = e$id)
  }
})

test_that("the condition translator handles sets, integers, chains and variables", {
  e <- dl_registry()$binomial
  r <- latex_condition_r("(n + 1) p \\in \\{1, 2, \\ldots, n\\}", e)
  expect_true(latex_condition_eval(r, list(size = 5, prob = 0.5)))
  expect_false(latex_condition_eval(r, list(size = 5, prob = 0.3)))
  expect_false(latex_condition_eval(r, list(size = 5, prob = 1)))
  r <- latex_condition_r("0 < p < 1", e)
  expect_true(latex_condition_eval(r, list(size = 5, prob = 0.2)))
  expect_false(latex_condition_eval(r, list(size = 5, prob = 1)))
  expect_null(latex_condition_r("t < -\\log(1 - p)", e))
  r <- latex_condition_r("t < -\\log(1 - p)", e, "t")
  expect_true(latex_condition_eval(r, list(size = 5, prob = 0.5, t = 0.5)))
  expect_false(latex_condition_eval(r, list(size = 5, prob = 0.5, t = 0.7)))
  r <- latex_condition_r("|z| < \\frac{1}{1 - p}", e, "z")
  expect_true(latex_condition_eval(r, list(size = 5, prob = 0.5, z = -1.5)))
  expect_false(latex_condition_eval(r, list(size = 5, prob = 0.5, z = -2.5)))
  r <- latex_condition_r("j \\in \\{-2, -4, -6, \\ldots\\}", e, "j")
  expect_true(latex_condition_eval(r, list(j = -4)))
  expect_false(latex_condition_eval(r, list(j = -3)))
  expect_false(latex_condition_eval(r, list(j = 0)))
})
