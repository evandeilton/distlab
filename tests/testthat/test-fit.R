test_that("Normal MLE and information agree with analytic sampling results", {
  x <- c(-2.5, -1.1, 0.2, 0.7, 1.5, 2.8, 4.1, 5.2)
  fit <- dl_fit(x, "normal")$fits$normal
  sigma <- sqrt(mean((x - mean(x))^2))
  expect_equal(fit$dist$params$mean, mean(x), tolerance = 1e-5)
  expect_equal(fit$dist$params$sd, sigma, tolerance = 1e-5)
  expect_equal(fit$estimates$se, c(sigma / sqrt(length(x)),
                                sigma / sqrt(2 * length(x))), tolerance = 1e-4)
  expect_equal(fit$logLik, sum(dnorm(x, mean(x), sigma, log = TRUE)), tolerance = 1e-7)
  expect_equal(fit$k, 2L)
  expect_equal(fit$convergence, 0L)
  expect_false(fit$boundary)
})

test_that("Poisson and weighted Binomial recover their sufficient statistics", {
  x <- c(0, 1, 2, 3, 5)
  w <- c(3, 4, 8, 2, 1)
  data <- dl_fit_data(x, frequency = w)
  fit <- dl_fit(data, c("poisson", "binomial"),
                fixed = list(binomial = list(size = 8)))
  m <- sum(x * w) / sum(w)
  expect_equal(fit$fits$poisson$dist$params$lambda, m, tolerance = 1e-5)
  expect_equal(fit$fits$binomial$dist$params$prob, m / 8, tolerance = 1e-5)
  expect_equal(fit$fits$binomial$k, 1L)
  expect_true(fit$fits$binomial$estimates$fixed[1])
  expect_equal(fit$fits$binomial$estimates$se[1], 0)
  expanded <- dl_fit(rep(x, w), c("poisson", "binomial"),
                     fixed = list(binomial = list(size = 8)))
  expect_equal(fit$ranking, expanded$ranking, tolerance = 1e-7)
})

test_that("incompatible observation measures are ranked in separate groups", {
  fit <- dl_fit(c(0, 1, 1, 2, 3, 4), c("normal", "logistic", "poisson"))
  r <- fit$ranking
  expect_setequal(r$likelihood_group, c("density", "probability_mass"))
  expect_equal(r$weight[r$id == "poisson"], 1)
  expect_equal(sum(r$weight[r$likelihood_group == "density"]), 1)
  expect_equal(min(r$delta_AIC[r$likelihood_group == "density"]), 0)
})

test_that("grouped likelihoods use intervals and include unobserved tails", {
  data <- dl_fit_data(lower = c(-Inf, -1, 0, 1), upper = c(-1, 0, 1, Inf),
                      frequency = c(2, 6, 6, 2))
  fit <- dl_fit(data, c("normal", "logistic", "poisson"))
  expect_equal(length(unique(fit$ranking$likelihood_group)), 1L)
  f <- fit$fits$normal
  expect_equal(f$dist$params$mean, 0, tolerance = 1e-4)
  expected_sd <- 1 / qnorm(0.875)
  expect_equal(f$dist$params$sd, expected_sd, tolerance = 1e-4)
  expected <- sum(data$counts * log(c(0.125, 0.375, 0.375, 0.125)))
  expect_equal(f$logLik, expected, tolerance = 1e-7)
  # Missing classes are zero observations, not truncation/conditioning.
  incomplete <- dl_fit_data(lower = c(-1, 0), upper = c(0, 1), frequency = c(5, 5))
  fixed <- list(normal = list(mean = 0, sd = 1))
  f <- dl_fit(incomplete, "normal", fixed = fixed)$fits$normal
  expect_equal(f$logLik, 10 * log(pnorm(1) - pnorm(0)))
  expect_equal(f$k, 0L)
})

test_that("zero grouped cells and far upper tail differences remain finite", {
  data <- dl_fit_data(lower = c(-Inf, 30, 31), upper = c(0, 31, 32),
                      frequency = c(0, 2, 1))
  fit <- dl_fit(data, "normal", fixed = list(normal = list(mean = 0, sd = 1)))
  f <- fit$fits$normal
  expect_true(is.finite(f$logLik))
  # R's upper-tail CDF retains these probabilities; lower-tail subtraction
  # has rounded to zero at these values.
  expected <- 2 * log(pnorm(30, lower.tail = FALSE) - pnorm(31, lower.tail = FALSE)) +
    log(pnorm(31, lower.tail = FALSE) - pnorm(32, lower.tail = FALSE))
  expect_equal(f$logLik, expected, tolerance = 1e-10)
  positive <- dl_fit_data(lower = c(-2, 0), upper = c(-1, Inf), frequency = c(0, 8))
  expect_length(dl_fit(positive, "exponential", fixed = list(exponential = list(rate = 1)))$failures, 0)
  expect_match(dl_fit(positive, "exponential")$failures$exponential, "Grouped data")
})

test_that("grouped partitions must identify a stable finite maximum", {
  one <- dl_fit_data(lower = -1, upper = 1, frequency = 100)
  result <- dl_fit(one, "normal")
  expect_match(result$failures$normal, "Grouped data")
  expect_false(result$ranking$converged)
  expect_true(is.na(result$ranking$weight))
  all <- dl_fit_data(lower = -Inf, upper = Inf, frequency = 100)
  expect_match(dl_fit(all, "normal")$failures$normal, "Grouped data")
  halves <- dl_fit_data(lower = c(-Inf, 0), upper = c(0, Inf), frequency = c(30, 70))
  flat <- dl_fit(halves, c("normal", "logistic", "cauchy", "laplace", "student_t"))
  expect_length(flat$fits, 0)
  expect_true(all(!flat$ranking$converged & is.na(flat$ranking$weight)))
  # With known scale the same partition identifies the Normal location.
  identified <- dl_fit(halves, "normal", fixed = list(normal = list(sd = 1)))$fits$normal
  expect_equal(identified$dist$params$mean, -qnorm(.3), tolerance = 1e-5)
  # Two disjoint occupied classes can have a regular finite two-parameter
  # maximum; counting occupied classes alone must not reject this fit.
  separated <- dl_fit_data(lower = c(-2, 1), upper = c(-1, 2), frequency = c(50, 50))
  f <- dl_fit(separated, "normal")$fits$normal
  expect_false(is.null(f))
  expect_equal(f$dist$params$mean, 0, tolerance = 1e-4)
  expect_true(all(is.finite(f$estimates$se)))
  # A finite single interval also identifies a one-parameter Exponential.
  exp_data <- dl_fit_data(lower = 1, upper = 2, frequency = 100)
  e <- dl_fit(exp_data, "exponential")$fits$exponential
  expect_equal(e$dist$params$rate, log(2), tolerance = 1e-5)
})

test_that("boundary and nonexistent MLEs are reported honestly", {
  zero <- dl_fit(rep(0, 10), c("poisson", "exponential", "normal", "bernoulli", "geometric"))
  expect_setequal(names(zero$failures), c("poisson", "exponential", "normal"))
  expect_match(zero$failures$poisson, "lambda = 0")
  expect_equal(zero$fits$bernoulli$dist$params$prob, 0)
  expect_equal(zero$fits$geometric$dist$params$prob, 1)
  expect_true(zero$fits$bernoulli$boundary)
  expect_true(all(is.na(zero$fits$bernoulli$estimates$se)))
  endpoint <- dl_fit(c(0, 0.5, 1), c("beta", "lognormal", "gamma"))
  expect_length(endpoint$fits, 0)
  expect_true(all(!endpoint$ranking$converged))
  expect_match(dl_fit(c(1, 1, 1, 2), "cauchy")$failures$cauchy, "no unique finite maximum")
  expect_match(dl_fit(c(0, 1, 1, 2), "negbinomial")$failures$negbinomial, "Poisson limit")
  expect_length(dl_fit(rep(2, 10), "normal", fixed = list(normal = list(sd = 1)))$fits, 1)
})

test_that("integer designs and all-fixed families have explicit contracts", {
  expect_match(dl_fit(0:5, "binomial")$failures$binomial, "Fix size")
  expect_match(dl_fit(0:5, "betabinomial")$failures$betabinomial, "Fix size")
  f <- dl_fit(c(1, 2, 3, 4), "hypergeometric",
              fixed = list(hypergeometric = list(m = 10, n = 15, k = 6)))$fits$hypergeometric
  expect_equal(f$k, 0L)
  expect_equal(f$logLik, sum(dhyper(c(1, 2, 3, 4), 10, 15, 6, log = TRUE)))
  expect_equal(f$estimates$se, rep(0, 3))
  expect_match(dl_fit(c(0, 0.5, 1), "poisson")$failures$poisson, "integer")
})

test_that("uniform and Laplace nonregular estimates have appropriate uncertainty", {
  x <- c(1, 2, 2, 3, 7)
  fit <- dl_fit(x, c("uniform", "discrete_uniform", "laplace"))
  expect_equal(fit$fits$uniform$dist$params, list(min = 1, max = 7))
  expect_true(fit$fits$uniform$boundary)
  expect_true(all(is.na(fit$fits$uniform$estimates$se)))
  expect_equal(fit$fits$laplace$dist$params$mu, 2)
  expect_equal(fit$fits$laplace$dist$params$sigma, mean(abs(x - 2)))
  expect_true(all(is.na(fit$fits$laplace$estimates$se)))
  expect_match(paste(fit$fits$laplace$warnings), "nonsmooth")
  data <- dl_fit_data(lower = 0:1, upper = 1:2, frequency = c(3, 4))
  expect_match(dl_fit(data, "uniform")$failures$uniform, "Fix min, max")
  expect_length(dl_fit(rep(2, 5), "uniform")$fits, 0)
})

test_that("fitting is deterministic and does not consume randomness", {
  set.seed(126)
  x <- rgamma(40, 2.7, 1.2)
  rng <- .Random.seed
  fit <- dl_fit(x, c("gamma", "weibull"))
  expect_identical(.Random.seed, rng)
  expect_equal(fit, dl_fit(x, c("gamma", "weibull")))
  expect_type(fit$code, "character")
  # The generated script rebuilds the same raw data and ranking. Some
  # platforms do not round 17-digit literals correctly, hence the tolerance.
  env <- new.env()
  eval(parse(text = fit$code), envir = env)
  expect_identical(env$data$type, "raw")
  expect_equal(env$data$x, fit$data$x, tolerance = 1e-14)
  expect_equal(env$fit$ranking, fit$ranking, tolerance = 1e-10)
})

test_that("Gamma MLE agrees with an independent optimizer", {
  skip_if_not_installed("MASS")
  x <- c(0.3, 0.8, 1.2, 1.5, 1.7, 2.1, 2.5, 3.8, 5.1, 7.2)
  reference <- suppressWarnings(MASS::fitdistr(x, "gamma"))
  fit <- dl_fit(x, "gamma")$fits$gamma
  expect_equal(unlist(fit$dist$params), reference$estimate, tolerance = 2e-4)
  expect_equal(fit$logLik, as.numeric(logLik(reference)), tolerance = 1e-6)
})

test_that("each catalog family fits without reducing its generating likelihood", {
  set.seed(137)
  for (id in dl_ids()) {
    generating <- dl_dist(id)
    x <- dl_sample(generating, 100)
    design <- switch(id,
      binomial = list(size = generating$params$size),
      betabinomial = list(size = generating$params$size),
      hypergeometric = generating$params, list())
    fixed <- if (length(design)) stats::setNames(list(design), id) else list()
    result <- dl_fit(x, id, fixed = fixed)
    expect_equal(length(result$failures), 0, info = id)
    fit <- result$fits[[id]]
    if (is.null(fit)) next
    expect_equal(fit$convergence, 0L, info = id)
    reference <- sum(dl_density(generating, x, log = TRUE))
    expect_gte(fit$logLik + 1e-5, reference, label = id)
    expect_true(all(is.finite(fit$estimates$estimate)), info = id)
  }
})

test_that("argument validation and individual failures preserve other candidates", {
  expect_error(dl_fit(1:5, "unknown"), "registry")
  expect_error(dl_fit(1:5, c("normal", "normal")), "distinct")
  expect_error(dl_fit(1:5, level = 1), "level")
  expect_error(dl_fit(1:5, fixed = list(list(sd = 1))), "named list")
  fit <- dl_fit(1:5, c("normal", "gamma"), fixed = list(gamma = list(rate = -1)))
  expect_length(fit$fits, 1)
  expect_match(fit$failures$gamma, "outside its domain")
  expect_true(all(is.na(fit$ranking$AIC[fit$ranking$id == "gamma"])))
})

test_that("closed-form estimates have zero score and exact observed information", {
  set.seed(31)
  cases <- list(
    normal = list(x = rnorm(40, 3, 2)),
    lognormal = list(x = rlnorm(40, .5, .7)),
    exponential = list(x = rexp(40, 1.5)),
    poisson = list(x = rpois(40, 3.2)),
    bernoulli = list(x = rbinom(40, 1, .3)),
    binomial = list(x = rbinom(40, 12, .35), fixed = list(size = 12)),
    geometric = list(x = rgeom(40, .3)))
  for (id in names(cases)) {
    x <- cases[[id]]$x
    fixed <- if (length(cases[[id]]$fixed)) stats::setNames(list(cases[[id]]$fixed), id) else list()
    f <- dl_fit(x, id, fixed = fixed)$fits[[id]]
    free <- f$estimates$parameter[!f$estimates$fixed]
    nll <- function(v) {
      p <- f$dist$params
      p[free] <- as.list(v)
      -sum(dl_density(do.call(dl_dist, c(list(id), p)), x, log = TRUE))
    }
    v <- unlist(f$dist$params[free])
    expect_lt(max(abs(numDeriv::grad(nll, v))), 1e-6, label = id)
    expect_equal(unname(f$vcov[free, free, drop = FALSE]), solve(numDeriv::hessian(nll, v)),
                 tolerance = 1e-6, info = id)
    expect_equal(f$estimates$se[!f$estimates$fixed], sqrt(diag(solve(numDeriv::hessian(nll, v)))),
                 tolerance = 1e-6, info = id)
  }
  # Fixing one Normal parameter keeps the closed form of the other.
  x <- cases$normal$x
  sd_only <- dl_fit(x, "normal", fixed = list(normal = list(mean = 3)))$fits$normal
  expect_equal(sd_only$dist$params$sd, sqrt(mean((x - 3)^2)))
  expect_equal(sd_only$estimates$se[2], sd_only$dist$params$sd / sqrt(2 * length(x)))
})

test_that("closed forms reproduce MASS::fitdistr", {
  skip_if_not_installed("MASS")
  set.seed(8)
  x <- rgamma(50, 3, 2)
  for (id in c("normal", "lognormal", "exponential")) {
    ref <- MASS::fitdistr(x, id)
    f <- dl_fit(x, id)$fits[[id]]
    expect_equal(unname(unlist(f$dist$params)), unname(ref$estimate), tolerance = 1e-10, info = id)
    expect_equal(f$estimates$se, unname(ref$sd), tolerance = 1e-10, info = id)
    expect_equal(f$logLik, ref$loglik, tolerance = 1e-10, info = id)
  }
  counts <- rpois(50, 2.4)
  for (id in c("poisson", "geometric")) {
    ref <- MASS::fitdistr(counts, id)
    f <- dl_fit(counts, id)$fits[[id]]
    expect_equal(unname(unlist(f$dist$params)), unname(ref$estimate), tolerance = 1e-10, info = id)
    expect_equal(f$estimates$se, unname(ref$sd), tolerance = 1e-10, info = id)
  }
  u <- rbeta(60, 2, 5)
  ref <- suppressWarnings(MASS::fitdistr(u, "beta", start = list(shape1 = 1, shape2 = 1)))
  f <- dl_fit(u, "beta")$fits$beta
  expect_equal(unname(unlist(f$dist$params)), unname(ref$estimate), tolerance = 1e-4)
  expect_equal(f$logLik, ref$loglik, tolerance = 1e-8)
})

test_that("sufficient-statistic likelihoods equal the density sums", {
  x <- c(.12, .3, .45, .7, .81)
  w <- c(2, 1, 3, 1, 4)
  n <- sum(w)
  gamma <- distlab:::fit_sufficient_loglik("gamma", x, w, n)
  expect_equal(gamma(list(shape = 2.3, rate = 1.7)), sum(w * dgamma(x, 2.3, 1.7, log = TRUE)),
               tolerance = 1e-12)
  chisq <- distlab:::fit_sufficient_loglik("chisq", x, w, n)
  expect_equal(chisq(list(df = 3.4)), sum(w * dchisq(x, 3.4, log = TRUE)), tolerance = 1e-12)
  beta <- distlab:::fit_sufficient_loglik("beta", x, w, n)
  expect_equal(beta(list(shape1 = 2.1, shape2 = 3.3)), sum(w * dbeta(x, 2.1, 3.3, log = TRUE)),
               tolerance = 1e-12)
  expect_null(distlab:::fit_sufficient_loglik("weibull", x, w, n))
  # The Chi-square fit agrees with a one-dimensional optimizer.
  set.seed(12)
  y <- rchisq(60, 3.3)
  ref <- optimize(function(d) -sum(dchisq(y, d, log = TRUE)), c(.1, 50), tol = 1e-10)
  expect_equal(dl_fit(y, "chisq")$fits$chisq$dist$params$df, ref$minimum, tolerance = 1e-6)
})

test_that("degenerate and limiting likelihoods are failures, not ranked fits", {
  half <- dl_fit(c(0, 0, 0, 1, 2, 5), c("cauchy", "normal"))
  expect_match(half$failures$cauchy, "half or more")
  expect_true(is.na(half$ranking$weight[half$ranking$id == "cauchy"]))
  expect_equal(half$ranking$weight[half$ranking$id == "normal"], 1)
  expect_match(dl_fit(c(1, 3), "cauchy")$failures$cauchy, "half or more")
  expect_match(dl_fit(c(1, 3, 3, 8), "cauchy", fixed = list(cauchy = list(location = 3)))$failures$cauchy,
               "half or more")
  expect_length(dl_fit(c(1, 3, 3.5), "cauchy")$fits, 1)
  size <- list(betabinomial = list(size = 10))
  expect_match(dl_fit(rep(3, 10), "betabinomial", fixed = size)$failures$betabinomial,
               "Binomial limit")
  expect_match(dl_fit(c(rep(3, 5), rep(4, 5)), "betabinomial", fixed = size)$failures$betabinomial,
               "Binomial limit")
  over <- dl_fit(c(0, 1, 2, 5, 8, 9, 10, 3), "betabinomial", fixed = size)
  expect_length(over$failures, 0)
  expect_true(all(is.finite(over$fits$betabinomial$estimates$se)))
  # A numerical optimum with singular information is a failure; analytic
  # fits keep the estimate and only lose their standard errors.
  specs <- stats::setNames(dl_entry("cauchy")$parameters, c("location", "scale"))
  pars <- list(location = 0, scale = 1)
  flat <- function(th) th[1]^2
  expect_error(distlab:::fit_uncertainty(c(0, 0), names(pars), specs, pars, flat, .95,
                                         regular = TRUE, strict = TRUE), "Singular")
  soft <- distlab:::fit_uncertainty(c(0, 0), names(pars), specs, pars, flat, .95, regular = TRUE)
  expect_match(soft$warnings, "singular")
  expect_true(all(is.na(soft$estimates$se)))
  # Constant data on a Uniform report constant data, not a domain violation.
  expect_match(dl_fit(rep(2, 5), "uniform")$failures$uniform, "Constant data")
  expect_match(dl_fit(rep(0, 5), "uniform", fixed = list(uniform = list(min = 0)))$failures$uniform,
               "Constant data")
})

test_that("Wald intervals are built on unconstrained scales and stay in the domain", {
  z <- qnorm(.975)
  b <- dl_fit(c(1, rep(0, 19)), "bernoulli")$fits$bernoulli$estimates
  se <- sqrt(.05 * .95 / 20)
  expect_equal(b$se, se)
  expect_equal(c(b$lower, b$upper), plogis(qlogis(.05) + c(-1, 1) * z * se / (.05 * .95)))
  expect_true(b$lower > 0 && b$upper < 1)
  g <- dl_fit(c(0, 0, 1), "geometric")$fits$geometric$estimates
  expect_true(g$lower > 0 && g$upper < 1)
  e <- dl_fit(c(.5, 1, 2), "exponential")$fits$exponential$estimates
  expect_equal(c(e$lower, e$upper), e$estimate * exp(c(-1, 1) * z / sqrt(3)))
  w <- dl_fit(c(.02, .5, 3), "weibull")$fits$weibull$estimates
  expect_lt(w$estimate[2] - z * w$se[2], 0)
  expect_true(all(w$lower > 0))
  expect_true(all(w$lower < w$estimate & w$estimate < w$upper))
  # Unbounded parameters keep the symmetric interval; the level is honored.
  n <- dl_fit(c(-1, .5, 2, 3.5), "normal", level = .9)$fits$normal$estimates
  expect_equal(n$upper[1] - n$estimate[1], qnorm(.95) * n$se[1])
  expect_equal(n$estimate[1] - n$lower[1], qnorm(.95) * n$se[1])
  expect_equal(n$lower[2], n$estimate[2] * exp(-qnorm(.95) / sqrt(8)))
})

test_that("generated literals read back exactly and keep raw data raw", {
  x <- c(0.1, 1 / 3, pi * 1e10, -2.5e-300, Inf, NA)
  expect_equal(eval(parse(text = distlab:::fit_literal(x))), x, tolerance = 1e-15)
  expect_match(distlab:::fit_literal(1 / 3), "0.33333333333333331", fixed = TRUE)
  expect_identical(distlab:::fit_literal(.95), "0.95")
  expect_identical(distlab:::fit_literal(list(normal = list(sd = 1))), "list(normal = list(sd = 1))")
  d <- dl_fit_data(c(1.5, 2, 2, 3))
  code <- distlab:::fit_data_code(d)
  expect_match(code, "rep(", fixed = TRUE)
  env <- new.env()
  eval(parse(text = code), envir = env)
  expect_identical(env$data$type, "raw")
  expect_equal(env$data$counts, d$counts)
  table <- dl_fit_data(c(0, 1, 2), frequency = c(4, 0, 6))
  env <- new.env()
  eval(parse(text = distlab:::fit_data_code(table)), envir = env)
  expect_identical(env$data$type, "frequency")
  expect_equal(env$data$x, table$x)
})

test_that("fitting reports each candidate to a progress callback", {
  seen <- character()
  fit <- distlab:::fit_models(1:5, c("normal", "poisson"), progress = function(id, i, total) {
    seen <<- c(seen, sprintf("%s %d/%d", id, i, total))
  })
  expect_identical(seen, c("normal 1/2", "poisson 2/2"))
  expect_equal(fit$ranking, dl_fit(1:5, c("normal", "poisson"))$ranking)
})

test_that("Wald limits use the derivative of the optimizer coordinate", {
  z <- qnorm(.95)
  specs <- list(list(lower = 0, upper = 1), list(lower = 0, upper = Inf),
                list(lower = -Inf, upper = 5), list(lower = -Inf, upper = Inf))
  estimates <- c(.2, 2.5, 4.7, 2.5)
  se <- .07
  for (j in seq_along(specs)) {
    spec <- specs[[j]]
    est <- estimates[j]
    slope <- numDeriv::grad(function(v) distlab:::fit_transform(spec, x = v), est)
    theta <- distlab:::fit_transform(spec, x = est)
    expected <- sort(vapply(theta + c(-1, 1) * z * se * abs(slope), function(t) {
      distlab:::fit_transform(spec, inverse = TRUE, x = t)
    }, 0))
    expect_equal(distlab:::fit_wald_limits(spec, est, se, z), expected, tolerance = 1e-8)
  }
  expect_equal(distlab:::fit_wald_limits(specs[[2]], 2, 0, z), c(2, 2))
  expect_true(all(is.na(distlab:::fit_wald_limits(specs[[2]], 2, NA_real_, z))))
  expect_true(all(is.na(distlab:::fit_wald_limits(specs[[1]], 1, .1, z))))
})

test_that("location-scale estimates and standard errors are equivariant", {
  skip_if_not_installed("MASS")
  set.seed(21)
  samples <- list(logistic = rlogis(80), cauchy = rcauchy(80))
  cases <- list(c(1e-6, 0), c(1e-3, 0), c(1e5, 0), c(1e6, 0), c(2, 1e3), c(2, 1e4), c(2, 1e6),
                c(1e-3, 1e6))
  for (id in names(samples)) {
    z <- samples[[id]]
    ref <- suppressWarnings(MASS::fitdistr(z, id))
    unit <- dl_fit(z, id)$fits[[id]]$estimates
    expect_equal(unit$estimate, unname(ref$estimate), tolerance = 1e-3, info = id)
    expect_equal(unit$se, unname(ref$sd), tolerance = 1e-3, info = id)
    for (case in cases) {
      s <- case[1]
      loc <- case[2]
      fit <- dl_fit(z * s + loc, id)
      label <- sprintf("%s scale %g location %g", id, s, loc)
      expect_length(fit$failures, 0)
      e <- fit$fits[[id]]$estimates
      # MASS on the unit sample, mapped by equivariance of the MLE.
      expect_equal((e$estimate - c(loc, 0)) / s, unname(ref$estimate), tolerance = 1e-3, info = label)
      expect_equal(e$se / s, unname(ref$sd), tolerance = 1e-3, info = label)
      expect_equal((e$estimate - c(loc, 0)) / s, unit$estimate, tolerance = 1e-5, info = label)
      expect_equal(e$se / s, unit$se, tolerance = 1e-5, info = label)
    }
  }
  # MASS directly on a shifted sample, where its optimizer is reliable.
  shifted <- samples$logistic * 2 + 1e4
  direct <- suppressWarnings(MASS::fitdistr(shifted, "logistic"))
  e <- dl_fit(shifted, "logistic")$fits$logistic$estimates
  expect_equal(e$estimate, unname(direct$estimate), tolerance = 1e-3)
  expect_equal(e$se, unname(direct$sd), tolerance = 1e-3)
})

test_that("grouped standard errors do not depend on the class location", {
  set.seed(3)
  breaks <- c(-Inf, seq(-6, 6, 2), Inf)
  counts <- as.vector(table(cut(rnorm(500, 0, 3), breaks)))
  near <- dl_fit(dl_fit_data(lower = head(breaks, -1), upper = breaks[-1], frequency = counts),
                 "normal")$fits$normal$estimates
  far <- dl_fit(dl_fit_data(lower = head(breaks, -1) + 1e4, upper = breaks[-1] + 1e4,
                            frequency = counts), "normal")$fits$normal$estimates
  expect_equal(far$estimate - c(1e4, 0), near$estimate, tolerance = 1e-6)
  expect_equal(far$se, near$se, tolerance = 1e-6)
  # Information of the interval likelihood, differentiated around the estimate.
  nll <- function(d) -sum(counts * log(diff(pnorm(breaks + 1e4, far$estimate[1] + d[1],
                                                    far$estimate[2] + d[2]))))
  expect_equal(far$se, sqrt(diag(solve(numDeriv::hessian(nll, c(0, 0))))), tolerance = 1e-5)
})

test_that("coordinate slopes agree with numerical derivatives", {
  specs <- list(list(lower = 0, upper = 1), list(lower = 0, upper = Inf),
                list(lower = -Inf, upper = 5), list(lower = -Inf, upper = Inf))
  for (spec in specs) {
    for (x in c(.2, .7)) {
      x <- if (is.finite(spec$upper) && spec$upper == 5) 5 - x else x
      t <- distlab:::fit_transform(spec, x = x)
      slope <- numDeriv::grad(function(v) distlab:::fit_transform(spec, inverse = TRUE, x = v), t)
      expect_equal(distlab:::fit_transform_slope(spec, x), slope, tolerance = 1e-8)
    }
  }
  # Location units come from the fitted spread; bounded coordinates keep unit 1.
  specs <- stats::setNames(dl_entry("logistic")$parameters, c("location", "scale"))
  d <- dl_dist("logistic", location = 1e6, scale = 3)
  expect_equal(distlab:::fit_units(d, c("location", "scale"), specs, d$params),
               c(diff(qlogis(c(.25, .75), 1e6, 3)), 1))
  expect_true(is.na(distlab:::fit_units(NULL, "location", specs, d$params)))
})
