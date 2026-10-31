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

# Parameter sets per entry where every moment is finite, including shapes near
# the slider ends (Beta shapes < 1, skewed Weibull, tiny probabilities).
finite_cases <- list(
  bernoulli = list(list(prob = 0.3), list(prob = 0.01), list(prob = 0.99)),
  binomial = list(list(size = 10, prob = 0.3), list(size = 1, prob = 0.5),
                  list(size = 100, prob = 0.02)),
  geometric = list(list(prob = 0.3), list(prob = 0.02), list(prob = 0.95)),
  negbinomial = list(list(size = 3, prob = 0.4), list(size = 0.7, prob = 0.2),
                     list(size = 40, prob = 0.9)),
  poisson = list(list(lambda = 3), list(lambda = 0.05), list(lambda = 30)),
  hypergeometric = list(list(m = 10, n = 15, k = 8), list(m = 1, n = 1, k = 1),
                        list(m = 2, n = 1, k = 2), list(m = 3, n = 40, k = 20),
                        list(m = 50, n = 2, k = 49)),
  betabinomial = list(list(size = 10, alpha = 2, beta = 3),
                      list(size = 1, alpha = 0.4, beta = 2),
                      list(size = 60, alpha = 0.2, beta = 0.3),
                      list(size = 25, alpha = 20, beta = 1)),
  discrete_uniform = list(list(min = 1, max = 6), list(min = -20, max = -19),
                          list(min = -5, max = 30)),
  uniform = list(list(min = 0, max = 1), list(min = -7, max = 2.5)),
  beta = list(list(shape1 = 2, shape2 = 5), list(shape1 = 0.5, shape2 = 0.5),
              list(shape1 = 0.1, shape2 = 0.1), list(shape1 = 20, shape2 = 0.1)),
  exponential = list(list(rate = 1), list(rate = 0.1), list(rate = 10)),
  gamma = list(list(shape = 0.5, rate = 2), list(shape = 7, rate = 0.3)),
  chisq = list(list(df = 4), list(df = 0.5), list(df = 50)),
  weibull = list(list(shape = 1.5, scale = 1), list(shape = 0.5, scale = 2),
                 list(shape = 10, scale = 0.1)),
  lognormal = list(list(meanlog = 0, sdlog = 0.5), list(meanlog = -3, sdlog = 0.1),
                   list(meanlog = 2, sdlog = 1)),
  f = list(list(df1 = 5, df2 = 10), list(df1 = 0.5, df2 = 12), list(df1 = 50, df2 = 30)),
  normal = list(list(mean = -2, sd = 3), list(mean = 10, sd = 0.1)),
  student_t = list(list(df = 5), list(df = 12), list(df = 50)),
  laplace = list(list(mu = 0, sigma = 1), list(mu = -10, sigma = 0.1)),
  logistic = list(list(location = 0, scale = 1), list(location = 10, scale = 10))
)

test_that("every entry has closed-form moments", {
  expect_setequal(names(distlab:::.dl_moment_formulas), dl_ids())
  expect_setequal(c(names(finite_cases), "cauchy"), dl_ids())
})

test_that("closed-form moments match numeric moments for every entry", {
  for (id in names(finite_cases)) {
    for (pars in finite_cases[[id]]) {
      d <- do.call(dl_dist, c(list(id), pars))
      exact <- dl_moments(d, "exact")
      expect_true(all(is.finite(exact)), info = paste(id, format(pars)))
      # Compare each moment on its own scale; zeros are compared absolutely.
      num <- dl_moments(d, "numeric")
      for (k in names(exact)) {
        expect_equal(num[[k]], exact[[k]], tolerance = 1e-6,
                     info = paste(id, k, format(pars)))
      }
    }
  }
})

test_that("infinite moments are Inf and nonexistent ones are NA", {
  na <- NA_real_
  check <- function(d, expected) {
    m <- dl_moments(d)
    expect_identical(unname(m[c("mean", "variance", "skewness", "excess_kurtosis")]),
                     expected, info = format(d$params))
    # The numeric method must not replace them by an integral.
    expect_identical(is.na(dl_moments(d, "numeric")), is.na(m))
  }
  check(dl_dist("cauchy"), c(na, na, na, na))
  check(dl_dist("student_t", df = 1), c(na, na, na, na))
  check(dl_dist("student_t", df = 0.5), c(na, na, na, na))
  check(dl_dist("student_t", df = 1.5), c(0, Inf, na, na))
  check(dl_dist("student_t", df = 2), c(0, Inf, na, na))
  check(dl_dist("student_t", df = 3), c(0, 3, na, Inf))
  check(dl_dist("student_t", df = 4), c(0, 2, 0, Inf))
  check(dl_dist("student_t", df = 4.5), c(0, 4.5 / 2.5, 0, 12))
  check(dl_dist("f", df1 = 3, df2 = 2), c(Inf, na, na, na))
  check(dl_dist("f", df1 = 3, df2 = 3), c(3, Inf, na, na))
  # F(3, 5): variance 2 * 25 * 6 / (3 * 9 * 1); F(3, 8): variance 2 * 64 * 9 /
  # (3 * 36 * 4) and skewness 12 * sqrt(32) / (2 * sqrt(27)).
  check(dl_dist("f", df1 = 3, df2 = 5), c(5 / 3, 300 / 27, Inf, Inf))
  m <- dl_moments(dl_dist("f", df1 = 3, df2 = 8))
  expect_equal(unname(m[c("mean", "variance", "skewness")]),
               c(4 / 3, 1152 / 432, 12 * sqrt(32) / (2 * sqrt(27))))
  expect_identical(m[["excess_kurtosis"]], Inf)
  expect_true(is.na(dl_moments(dl_dist("cauchy"))[["sd"]]))
  expect_identical(dl_moments(dl_dist("student_t", df = 1.5))[["sd"]], Inf)
})

test_that("finite heavy-tail moments match numeric integration near the limits", {
  for (d in list(dl_dist("student_t", df = 2.5), dl_dist("f", df1 = 4, df2 = 4.5),
                 dl_dist("f", df1 = 4, df2 = 6.5))) {
    exact <- dl_moments(d)
    num <- dl_moments(d, "numeric")
    ok <- is.finite(exact) & exact != 0
    expect_equal(num[ok], exact[ok], tolerance = 1e-6, info = format(d$params))
  }
})

test_that("a point mass has zero variance and no shape moments", {
  for (d in list(dl_dist("bernoulli", prob = 1), dl_dist("binomial", prob = 0),
                 dl_dist("geometric", prob = 1), dl_dist("negbinomial", prob = 1),
                 dl_dist("hypergeometric", m = 3, n = 4, k = 7))) {
    m <- dl_moments(d)
    expect_identical(m[["variance"]], 0, info = d$id)
    expect_true(all(is.na(m[c("skewness", "excess_kurtosis")])), info = d$id)
  }
})

test_that("moments agree with Monte Carlo for several families", {
  set.seed(20261031)
  cases <- list(dl_dist("betabinomial", size = 20, alpha = 0.6, beta = 1.4),
                dl_dist("hypergeometric", m = 12, n = 30, k = 15),
                dl_dist("weibull", shape = 0.8, scale = 2),
                dl_dist("laplace", mu = 1, sigma = 2),
                dl_dist("negbinomial", size = 2.5, prob = 0.3))
  n <- 2e5
  for (d in cases) {
    x <- dl_sample(d, n)
    m <- dl_moments(d)
    expect_lt(abs(mean(x) - m[["mean"]]), 4 * m[["sd"]] / sqrt(n))
    expect_equal(var(x), m[["variance"]], tolerance = 0.03, info = d$id)
    sk <- mean((x - mean(x))^3) / sd(x)^3
    expect_equal(sk, m[["skewness"]], tolerance = 0.1, info = d$id)
  }
})
