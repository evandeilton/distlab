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
  expect_match(dl_fit(c(1, 1, 1, 2), "cauchy")$failures$cauchy, "unbounded")
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
  # Execute the generated script, using namespace functions supplied by the
  # package loaded by testthat; no screenshots or implementation mocks.
  env <- new.env()
  eval(parse(text = fit$code), envir = env)
  expect_equal(env$fit$ranking, fit$ranking)
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
