# Densities and CDFs against 'distributions3' for the families it implements.
test_that("d/p agree with distributions3", {
  skip_if_not_installed("distributions3")
  d3 <- asNamespace("distributions3")
  xd <- 0:30
  xc <- c(-3, -0.5, 0.05, 0.4, 0.9, 1.7, 4, 12)
  cases <- list(
    list(dl_dist("bernoulli", prob = 0.3), d3$Bernoulli(p = 0.3), xd),
    list(dl_dist("binomial", size = 12, prob = 0.4), d3$Binomial(size = 12, p = 0.4), xd),
    list(dl_dist("geometric", prob = 0.2), d3$Geometric(p = 0.2), xd),
    list(dl_dist("negbinomial", size = 3, prob = 0.45),
         d3$NegativeBinomial(size = 3, p = 0.45), xd),
    list(dl_dist("poisson", lambda = 4.2), d3$Poisson(lambda = 4.2), xd),
    list(dl_dist("hypergeometric", m = 9, n = 14, k = 7),
         d3$HyperGeometric(m = 9, n = 14, k = 7), xd),
    list(dl_dist("uniform", min = -1, max = 2), d3$Uniform(a = -1, b = 2), xc),
    list(dl_dist("beta", shape1 = 0.7, shape2 = 2), d3$Beta(alpha = 0.7, beta = 2),
         c(0.01, 0.2, 0.5, 0.9)),
    list(dl_dist("exponential", rate = 1.5), d3$Exponential(rate = 1.5), xc[xc > 0]),
    list(dl_dist("gamma", shape = 2.5, rate = 0.8), d3$Gamma(shape = 2.5, rate = 0.8),
         xc[xc > 0]),
    list(dl_dist("chisq", df = 3), d3$ChiSquare(df = 3), xc[xc > 0]),
    list(dl_dist("weibull", shape = 1.3, scale = 2), d3$Weibull(shape = 1.3, scale = 2),
         xc[xc > 0]),
    list(dl_dist("lognormal", meanlog = 0.2, sdlog = 0.6),
         d3$LogNormal(log_mu = 0.2, log_sigma = 0.6), xc[xc > 0]),
    list(dl_dist("f", df1 = 4, df2 = 7), d3$FisherF(df1 = 4, df2 = 7), xc[xc > 0]),
    list(dl_dist("normal", mean = 1, sd = 2), d3$Normal(mu = 1, sigma = 2), xc),
    list(dl_dist("student_t", df = 3.5), d3$StudentsT(df = 3.5), xc),
    list(dl_dist("cauchy", location = 1, scale = 0.5),
         d3$Cauchy(location = 1, scale = 0.5), xc),
    list(dl_dist("logistic", location = -1, scale = 2),
         d3$Logistic(location = -1, scale = 2), xc)
  )
  covered <- character()
  for (cs in cases) {
    d <- cs[[1]]; ref <- cs[[2]]; x <- cs[[3]]
    expect_equal(dl_density(d, x), d3$pdf(ref, x), tolerance = 1e-12, info = d$id)
    expect_equal(dl_cdf(d, x), d3$cdf(ref, x), tolerance = 1e-12, info = d$id)
    covered <- c(covered, d$id)
  }
  # The remaining entries have no distributions3 counterpart; see below.
  expect_setequal(setdiff(dl_ids(), covered),
                  c("betabinomial", "discrete_uniform", "laplace"))
})

# Families outside distributions3, checked against their defining formulas.
test_that("Beta-binomial, discrete uniform and Laplace match their formulas", {
  n <- 11; a <- 0.8; b <- 2.3
  x <- 0:n
  pmf <- choose(n, x) * beta(x + a, n - x + b) / beta(a, b)
  bb <- dl_dist("betabinomial", size = n, alpha = a, beta = b)
  expect_equal(dl_density(bb, x), pmf, tolerance = 1e-12)
  expect_equal(dl_cdf(bb, x), cumsum(pmf), tolerance = 1e-12)

  du <- dl_dist("discrete_uniform", min = -2, max = 7)
  expect_equal(dl_density(du, -2:7), rep(0.1, 10))
  expect_equal(dl_cdf(du, c(-3, -2, 0.5, 7)), c(0, 0.1, 0.3, 1))

  lp <- dl_dist("laplace", mu = 1, sigma = 2)
  xl <- c(-4, 0, 1, 2.5, 9)
  expect_equal(dl_density(lp, xl), exp(-abs(xl - 1) / 2) / 4, tolerance = 1e-12)
  expect_equal(dl_cdf(lp, xl),
               ifelse(xl < 1, 0.5 * exp((xl - 1) / 2), 1 - 0.5 * exp(-(xl - 1) / 2)),
               tolerance = 1e-12)
})
