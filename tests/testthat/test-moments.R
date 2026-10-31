test_that("closed-form moments match numeric moments", {
  cases <- list(
    dl_dist("normal", mean = -2, sd = 3),
    dl_dist("gamma", shape = 0.5, rate = 2),
    dl_dist("gamma", shape = 7, rate = 0.3),
    dl_dist("poisson", lambda = 0.2),
    dl_dist("poisson", lambda = 25)
  )
  for (d in cases) {
    exact <- dl_moments(d, "exact")
    num <- dl_moments(d, "numeric")
    expect_equal(num, exact, tolerance = 1e-6, info = format(d$params))
  }
})

test_that("moments agree with Monte Carlo within sampling error", {
  set.seed(2026)
  g <- dl_dist("gamma", shape = 3, rate = 2)
  x <- dl_sample(g, 2e5)
  m <- dl_moments(g)
  expect_lt(abs(mean(x) - m[["mean"]]), 4 * m[["sd"]] / sqrt(length(x)))
  expect_equal(var(x), m[["variance"]], tolerance = 0.02)
})

test_that("moment vector has a fixed shape", {
  m <- dl_moments(dl_dist("normal"))
  expect_named(m, c("mean", "variance", "sd", "skewness", "excess_kurtosis"))
  expect_equal(m[["sd"]], 1)
})

test_that("numeric moments stay accurate for narrow, wide and skewed shapes", {
  cases <- list(
    dl_dist("normal", mean = 3, sd = 1e-6),
    dl_dist("normal", mean = 0, sd = 1e4),
    dl_dist("gamma", shape = 0.1, rate = 1),
    dl_dist("poisson", lambda = 1e-3)
  )
  for (d in cases) {
    expect_equal(dl_moments(d, "numeric"), dl_moments(d, "exact"),
                 tolerance = 1e-6, info = format(d$params))
  }
})

test_that("non-computable numeric moments are NA, never NaN", {
  m <- dl_moments(dl_dist("poisson", lambda = 1e9), "numeric")
  expect_true(all(is.na(m)))
  expect_false(any(is.nan(m)))
})
