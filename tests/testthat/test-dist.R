test_that("dl_dist fills defaults and checks domains", {
  g <- dl_dist("gamma")
  expect_s3_class(g, "dl_dist")
  expect_identical(g$params$shape, 2L)
  expect_error(dl_dist("gamma", shape = 0), "outside its domain")
  expect_error(dl_dist("gamma", scale = 2), "Unknown parameter")
  expect_error(dl_dist("nope"), "Unknown distribution id")
  expect_error(dl_dist("gamma", 2), "must be named")
})

test_that("d/p/q/r agree with base R", {
  g <- dl_dist("gamma", shape = 2.5, rate = 0.7)
  x <- c(0.1, 1, 5, 20)
  expect_equal(dl_density(g, x), dgamma(x, 2.5, 0.7))
  expect_equal(dl_cdf(g, x, lower.tail = FALSE), pgamma(x, 2.5, 0.7, lower.tail = FALSE))
  expect_equal(dl_quantile(g, c(0.01, 0.5, 0.99)), qgamma(c(0.01, 0.5, 0.99), 2.5, 0.7))
  set.seed(42); a <- dl_sample(g, 5)
  set.seed(42); b <- rgamma(5, 2.5, 0.7)
  expect_equal(a, b)
})

test_that("quantile inverts the CDF", {
  for (id in dl_ids(type = "continuous")) {
    d <- dl_dist(id)
    u <- c(1e-6, 0.1, 0.5, 0.9, 1 - 1e-6)
    expect_equal(dl_cdf(d, dl_quantile(d, u)), u, tolerance = 1e-8, info = id)
  }
  # Discrete convention: smallest x with F(x) >= u.
  pz <- dl_dist("poisson", lambda = 2.3)
  u <- c(0.05, 0.5, 0.95)
  q <- dl_quantile(pz, u)
  expect_true(all(dl_cdf(pz, q) >= u))
  expect_true(all(dl_cdf(pz, q - 1) < u))
})

test_that("densities integrate and masses sum to one", {
  for (id in dl_ids(type = "continuous")) {
    d <- dl_dist(id)
    s <- d$entry$support
    total <- integrate(function(x) dl_density(d, x), s$lower, s$upper,
                       rel.tol = 1e-10)$value
    expect_equal(total, 1, tolerance = 1e-8, info = id)
  }
  pz <- dl_dist("poisson", lambda = 7)
  expect_equal(sum(dl_density(pz, 0:200)), 1, tolerance = 1e-12)
})

test_that("log scale is used directly, not log of underflowed values", {
  nrm <- dl_dist("normal")
  expect_equal(dl_density(nrm, 40, log = TRUE), -0.5 * log(2 * pi) - 800)
  expect_equal(dl_cdf(nrm, 40, lower.tail = FALSE, log.p = TRUE),
               pnorm(40, lower.tail = FALSE, log.p = TRUE))
  expect_true(is.finite(dl_cdf(nrm, 40, lower.tail = FALSE, log.p = TRUE)))
})

test_that("dl_curve covers the central mass and handles small discrete ranges", {
  cv <- dl_curve(dl_dist("normal", mean = 5, sd = 2))
  expect_named(cv, c("x", "density", "cdf"))
  expect_equal(range(cv$cdf), c(1e-4, 1 - 1e-4), tolerance = 1e-8)
  cp <- dl_curve(dl_dist("poisson", lambda = 0.1))
  expect_true(all(cp$x == round(cp$x)))
  expect_equal(cp$x[1], 0)
  expect_true(all(diff(cp$cdf) >= 0))
})

test_that("dl_curve validates its arguments and limits discrete ranges", {
  nrm <- dl_dist("normal")
  expect_error(dl_curve(nrm, tail = 0), "`tail`")
  expect_error(dl_curve(nrm, tail = 0.5), "`tail`")
  expect_error(dl_curve(nrm, n = 1), "`n`")
  expect_error(dl_curve(dl_dist("poisson", lambda = 1e9)), "5000")
  # A tiny rate collapses the discrete range to the single point 0.
  one <- dl_curve(dl_dist("poisson", lambda = 1e-10))
  expect_identical(nrow(one), 1L)
})
