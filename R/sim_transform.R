# Sums, means, extremes and transformations of simulated draws, with the
# exact catalog law of the result when one is known.

.dl_transform_ops <- c("sum", "mean", "min", "max", "affine", "square", "exp", "log")

# Operations that combine k independent copies of X.
.dl_transform_multi <- c("sum", "mean", "min", "max")

#' Sums, extremes and transformations of a random variable
#'
#' Simulates \eqn{Y = g(X)} or a statistic of \eqn{k} independent copies of
#' \eqn{X}, and names the exact law of \eqn{Y} when it is a catalog
#' distribution:
#' * sum of \eqn{k}: Bernoulli(p) to Binomial(k, p); Binomial(n, p) to
#'   Binomial(kn, p); Poisson(\eqn{\lambda}) to Poisson(\eqn{k\lambda});
#'   Geometric(p) to Negative binomial(k, p); Negative binomial(r, p) to
#'   Negative binomial(kr, p); Normal(\eqn{\mu}, \eqn{\sigma}) to
#'   Normal(\eqn{k\mu}, \eqn{\sigma\sqrt{k}}); Exponential(\eqn{\lambda}) to
#'   Gamma(k, \eqn{\lambda}); Gamma(\eqn{\alpha}, \eqn{\beta}) to
#'   Gamma(\eqn{k\alpha}, \eqn{\beta}); Chi-square(\eqn{\nu}) to
#'   Chi-square(\eqn{k\nu}); Cauchy(\eqn{x_0}, \eqn{\gamma}) to
#'   Cauchy(\eqn{kx_0}, \eqn{k\gamma});
#' * mean of \eqn{k}: Normal to Normal(\eqn{\mu}, \eqn{\sigma/\sqrt{k}});
#'   Cauchy to the same Cauchy; Gamma(\eqn{\alpha}, \eqn{\beta}) to
#'   Gamma(\eqn{k\alpha}, \eqn{k\beta}); Exponential(\eqn{\lambda}) to
#'   Gamma(k, \eqn{k\lambda});
#' * minimum of \eqn{k}: Exponential(\eqn{\lambda}) to
#'   Exponential(\eqn{k\lambda}); Weibull(shape, scale) to Weibull(shape,
#'   \eqn{\mathrm{scale}\,k^{-1/\mathrm{shape}}}); Geometric(p) to
#'   Geometric(\eqn{1 - (1 - p)^k});
#' * maximum of \eqn{k}: Uniform(0, 1) to Beta(k, 1);
#' * \eqn{a + bX} with \eqn{b \ne 0}: Normal, Uniform, Cauchy, Laplace and
#'   Logistic stay in their family; with \eqn{a = 0} and \eqn{b > 0} also the
#'   Exponential, Gamma, Weibull and Lognormal;
#' * \eqn{X^2}: Normal(0, 1) to Chi-square(1); Normal(0, \eqn{\sigma}) to
#'   Gamma(1/2, rate \eqn{1 / (2\sigma^2)}); Student's t(\eqn{\nu}) to
#'   F(1, \eqn{\nu});
#' * \eqn{e^X}: Normal to Lognormal; \eqn{\log X}: Lognormal to Normal.
#'
#' For sums and means without an exact law, a Normal approximation with the
#' same mean and variance is given when the variance is finite and
#' \eqn{k \ge 2}. Results that are not finite (\eqn{e^X} of a heavy tail, or
#' \eqn{\log 0}) are dropped and counted.
#'
#' @inheritParams dl_mc_check
#' @param op One of `"sum"`, `"mean"`, `"min"`, `"max"` (of `k` independent
#'   copies), `"affine"` (\eqn{a + bX}), `"square"`, `"exp"` or `"log"`.
#'   `"log"` needs a support inside \eqn{(0, \infty)}.
#' @param k Number of copies for `"sum"`, `"mean"`, `"min"` and `"max"`, from
#'   1 to 1000; `n * k` may not exceed \eqn{10^7}.
#' @param n Number of simulated values of \eqn{Y}.
#' @param a,b Intercept and slope of `"affine"`; `b` must not be 0.
#'
#' @returns A list with
#'   * `sample`: the finite simulated values of \eqn{Y};
#'   * `dropped`: how many non-finite values were removed;
#'   * `target`: a `dl_dist` with the exact law of \eqn{Y}, or `NULL`;
#'   * `approx`: the Normal approximation (a `dl_dist`) for sums and means
#'     without an exact law, or `NULL`;
#'   * `op`, `k`, `a`, `b`, `seed`, and `code` (R code that reproduces the
#'     sample, its mean and standard deviation, and the moments of the
#'     reference law, exactly when a seed is given).
#'
#' @seealso [dl_clt()], [dl_relation_demo()]
#' @family simulation
#' @export
#' @examples
#' s <- dl_transform(dl_dist("exponential", rate = 1), "sum", k = 3, n = 2000, seed = 6)
#' s$target
#' mean(s$sample)
#' m <- dl_transform(dl_dist("cauchy"), "mean", k = 50, n = 2000, seed = 6)
#' m$target   # the mean of Cauchy draws is the same Cauchy
#' dl_transform(dl_dist("uniform"), "max", k = 4, n = 1000, seed = 6)$target
dl_transform <- function(dist, op, k = 2, n = 5000, seed = NULL, a = 0, b = 1) {
  check_dist(dist)
  if (missing(op) || !is_string(op) || !op %in% .dl_transform_ops) {
    stop("`op` must be one of: ", paste(.dl_transform_ops, collapse = ", "), ".",
         call. = FALSE)
  }
  k <- check_whole(k, "k", 1, 1000)
  n <- check_whole(n, "n", 1, 1e7)
  multi <- op %in% .dl_transform_multi
  if (multi && n * k > 1e7) {
    stop("`n * k` must not exceed 10,000,000 draws.", call. = FALSE)
  }
  a <- check_number(a, "a")
  b <- check_number(b, "b")
  if (op == "affine" && b == 0) {
    sim_stop("affine_b", "`b` must not be 0: a + 0 * X is a constant.")
  }
  if (op == "log" && !log_support_ok(dist)) {
    sim_stop("log_support", "log(X) needs a support inside (0, Inf).")
  }
  seed <- sim_seed(seed)
  y <- with_seed(seed, {
    if (multi) {
      x <- matrix(dl_sample(dist, n * k), ncol = k)
      switch(op, sum = rowSums(x), mean = rowMeans(x),
             min = apply(x, 1, min), max = apply(x, 1, max))
    } else {
      x <- dl_sample(dist, n)
      switch(op, affine = a + b * x, square = x^2, exp = exp(x), log = log(x))
    }
  })
  dropped <- sum(!is.finite(y))
  y <- y[is.finite(y)]
  target <- transform_target(dist, op, k, a, b)
  approx <- if (is.null(target) && op %in% c("sum", "mean") && k >= 2) {
    transform_approx(dist, op, k)
  }
  list(sample = y, dropped = dropped, target = target, approx = approx, op = op,
       k = k, a = a, b = b, seed = seed,
       code = transform_code(dist, op, k, n, a, b, seed, target %||% approx))
}

# TRUE when X > 0 with probability 1: a continuous support starting at 0 or
# above, or a discrete one starting at 1 or above.
log_support_ok <- function(dist) {
  lo <- dl_support(dist)$lower
  if (identical(dist$entry$type, "discrete")) lo > 0 else lo >= 0
}

# Exact catalog law of the result, or NULL. Parameter values that leave the
# domain (rounding at an extreme) also give NULL.
transform_target <- function(dist, op, k, a, b) {
  p <- dist$params
  id <- dist$id
  mk <- function(tid, ...) tryCatch(dl_dist(tid, ...), error = function(err) NULL)
  switch(op,
    sum = switch(id,
      bernoulli = mk("binomial", size = k, prob = p$prob),
      binomial = mk("binomial", size = k * p$size, prob = p$prob),
      poisson = mk("poisson", lambda = k * p$lambda),
      geometric = mk("negbinomial", size = k, prob = p$prob),
      negbinomial = mk("negbinomial", size = k * p$size, prob = p$prob),
      normal = mk("normal", mean = k * p$mean, sd = p$sd * sqrt(k)),
      exponential = mk("gamma", shape = k, rate = p$rate),
      gamma = mk("gamma", shape = k * p$shape, rate = p$rate),
      chisq = mk("chisq", df = k * p$df),
      cauchy = mk("cauchy", location = k * p$location, scale = k * p$scale),
      NULL),
    mean = switch(id,
      normal = mk("normal", mean = p$mean, sd = p$sd / sqrt(k)),
      cauchy = mk("cauchy", location = p$location, scale = p$scale),
      gamma = mk("gamma", shape = k * p$shape, rate = k * p$rate),
      exponential = mk("gamma", shape = k, rate = k * p$rate),
      NULL),
    min = switch(id,
      exponential = mk("exponential", rate = k * p$rate),
      weibull = mk("weibull", shape = p$shape, scale = p$scale * k^(-1 / p$shape)),
      # 1 - (1 - p)^k without cancellation for small p.
      geometric = mk("geometric", prob = -expm1(k * log1p(-p$prob))),
      NULL),
    max = if (id == "uniform" && p$min == 0 && p$max == 1) mk("beta", shape1 = k, shape2 = 1),
    affine = affine_target(dist, a, b, mk),
    square = if (id == "normal" && p$mean == 0) {
      if (p$sd == 1) mk("chisq", df = 1) else mk("gamma", shape = 0.5, rate = 1 / (2 * p$sd^2))
    } else if (id == "student_t") {
      mk("f", df1 = 1, df2 = p$df)
    },
    exp = if (id == "normal") mk("lognormal", meanlog = p$mean, sdlog = p$sd),
    log = if (id == "lognormal") mk("normal", mean = p$meanlog, sd = p$sdlog)
  )
}

# Location-scale families stay in their family under a + bX; scale families
# only for a = 0 and b > 0.
affine_target <- function(dist, a, b, mk) {
  p <- dist$params
  ab <- abs(b)
  scale_only <- a == 0 && b > 0
  switch(dist$id,
    normal = mk("normal", mean = a + b * p$mean, sd = ab * p$sd),
    uniform = {
      e <- sort(c(a + b * p$min, a + b * p$max))
      mk("uniform", min = e[1], max = e[2])
    },
    cauchy = mk("cauchy", location = a + b * p$location, scale = ab * p$scale),
    laplace = mk("laplace", mu = a + b * p$mu, sigma = ab * p$sigma),
    logistic = mk("logistic", location = a + b * p$location, scale = ab * p$scale),
    exponential = if (scale_only) mk("exponential", rate = p$rate / b),
    gamma = if (scale_only) mk("gamma", shape = p$shape, rate = p$rate / b),
    weibull = if (scale_only) mk("weibull", shape = p$shape, scale = b * p$scale),
    lognormal = if (scale_only) mk("lognormal", meanlog = p$meanlog + log(b), sdlog = p$sdlog),
    NULL)
}

# Normal law with the mean and variance of the sum or mean of k copies, when
# the variance is finite and positive.
transform_approx <- function(dist, op, k) {
  m <- dl_moments(dist)
  if (!is.finite(m[["variance"]]) || m[["variance"]] <= 0) return(NULL)
  out <- if (op == "sum") {
    list(mean = k * m[["mean"]], sd = sqrt(k) * m[["sd"]])
  } else {
    list(mean = m[["mean"]], sd = m[["sd"]] / sqrt(k))
  }
  tryCatch(do.call(dl_dist, c(list("normal"), out)), error = function(err) NULL)
}

# R code of dl_transform(); `ref` is the exact or approximate law, whose
# moments close the code.
transform_code <- function(dist, op, k, n, a, b, seed, ref) {
  draw <- if (op %in% .dl_transform_multi) {
    c(sprintf("x <- matrix(%s, ncol = %s)",
              code_fn(dist, "r", sprintf("%s * %s", code_num(n), code_num(k))), code_num(k)),
      sprintf("y <- %s", switch(op, sum = "rowSums(x)", mean = "rowMeans(x)",
                                min = "apply(x, 1, min)", max = "apply(x, 1, max)")))
  } else {
    c(sprintf("x <- %s", code_fn(dist, "r", code_num(n))),
      sprintf("y <- %s", switch(op,
        affine = sprintf("%s + %s * x", code_num(a, exact = TRUE), code_num(b, exact = TRUE)),
        square = "x^2", exp = "exp(x)", log = "log(x)")))
  }
  paste(c(
    code_head(seed, if (code_needs_dist(dist, "r")) dist),
    draw,
    "dropped <- sum(!is.finite(y))",
    "y <- y[is.finite(y)]",
    "c(mean = mean(y), sd = sd(y))",
    if (!is.null(ref)) {
      sprintf("distlab::dl_moments(%s)[c(\"mean\", \"sd\")]",
              sub("^d <- ", "", code_dist(ref, exact = TRUE)))
    }
  ), collapse = "\n")
}
