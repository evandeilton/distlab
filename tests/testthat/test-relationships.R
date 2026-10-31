# Each registry relationship "from > to" is verified numerically here:
# special cases by equal d/p on a grid, sums by exact convolution or
# simulation, limits by a distance that shrinks below a stated tolerance, and
# transformations by a Kolmogorov-Smirnov distance with a fixed seed.

# Equal densities and CDFs of two distribution objects on a grid.
expect_same_law <- function(a, b, x) {
  expect_equal(dl_density(a, x), dl_density(b, x), tolerance = 1e-12)
  expect_equal(dl_cdf(a, x), dl_cdf(b, x), tolerance = 1e-12)
}

# KS distance of a sample against a CDF stays under the 0.1% critical value.
expect_ks <- function(x, cdf) {
  stat <- suppressWarnings(stats::ks.test(x, cdf)$statistic)
  expect_lt(unname(stat), 1.95 / sqrt(length(x)))
}

n_sim <- 2e4
xc <- c(0.01, 0.3, 1, 2.5, 7, 15)

relation_checks <- list(
  # Special cases.
  "binomial>bernoulli" = function() {
    for (p in c(0, 0.2, 0.9)) {
      expect_same_law(dl_dist("binomial", size = 1, prob = p),
                      dl_dist("bernoulli", prob = p), -1:2)
    }
  },
  "negbinomial>geometric" = function() {
    expect_same_law(dl_dist("negbinomial", size = 1, prob = 0.35),
                    dl_dist("geometric", prob = 0.35), 0:40)
  },
  "betabinomial>discrete_uniform" = function() {
    expect_same_law(dl_dist("betabinomial", size = 9, alpha = 1, beta = 1),
                    dl_dist("discrete_uniform", min = 0, max = 9), 0:9)
  },
  "beta>uniform" = function() {
    expect_same_law(dl_dist("beta", shape1 = 1, shape2 = 1), dl_dist("uniform"),
                    seq(0.01, 0.99, by = 0.07))
  },
  "gamma>exponential" = function() {
    expect_same_law(dl_dist("gamma", shape = 1, rate = 2.5),
                    dl_dist("exponential", rate = 2.5), xc)
  },
  "gamma>chisq" = function() {
    for (k in c(1, 3, 7.5)) {
      expect_same_law(dl_dist("gamma", shape = k / 2, rate = 0.5),
                      dl_dist("chisq", df = k), xc)
    }
  },
  "weibull>exponential" = function() {
    expect_same_law(dl_dist("weibull", shape = 1, scale = 4),
                    dl_dist("exponential", rate = 1 / 4), xc)
  },
  "student_t>cauchy" = function() {
    expect_same_law(dl_dist("student_t", df = 1), dl_dist("cauchy"), c(-50, -2, 0, 0.7, 9))
  },

  # Sums: exact convolution of mass functions, or simulation.
  "bernoulli>binomial" = function() {
    p <- 0.37
    pmf <- 1
    for (i in 1:12) pmf <- stats::convolve(pmf, rev(c(1 - p, p)), type = "open")
    expect_equal(pmf, dl_density(dl_dist("binomial", size = 12, prob = p), 0:12),
                 tolerance = 1e-12)
  },
  "geometric>negbinomial" = function() {
    p <- 0.4
    g <- dl_density(dl_dist("geometric", prob = p), 0:200)
    pmf <- g
    for (i in 2:4) pmf <- stats::convolve(pmf, rev(g), type = "open")[1:201]
    expect_equal(pmf[1:60], dl_density(dl_dist("negbinomial", size = 4, prob = p), 0:59),
                 tolerance = 1e-10)
  },
  "exponential>gamma" = function() {
    set.seed(11)
    x <- rowSums(matrix(dl_sample(dl_dist("exponential", rate = 1.7), 5 * n_sim), ncol = 5))
    expect_ks(x, function(q) dl_cdf(dl_dist("gamma", shape = 5, rate = 1.7), q))
  },

  # Limits: the distance to the limit law falls below the stated tolerance.
  "binomial>poisson" = function() {
    # Total variation is bounded by lambda^2 / n (Le Cam).
    lambda <- 3
    tv <- function(n) {
      x <- 0:200
      0.5 * sum(abs(dbinom(x, n, lambda / n) - dpois(x, lambda)))
    }
    expect_lt(tv(1e4), lambda^2 / 1e4)
    expect_lt(tv(1e4), tv(1e2))
  },
  "binomial>normal" = function() {
    # Berry-Esseen: sup |F_n - Phi| <= 0.4748 * (p^2 + q^2) / sqrt(n p q).
    n <- 1e4; p <- 0.3
    x <- 0:n
    z <- (x - n * p) / sqrt(n * p * (1 - p))
    gap <- max(abs(pbinom(x, n, p) - pnorm(z)))
    expect_lt(gap, 0.4748 * (p^2 + (1 - p)^2) / sqrt(n * p * (1 - p)))
  },
  "poisson>normal" = function() {
    lambda <- 1e4
    x <- 9000:11000
    gap <- max(abs(ppois(x, lambda) - pnorm((x - lambda) / sqrt(lambda))))
    # Berry-Esseen for a sum of lambda Poisson(1) terms: rho = E|X - 1|^3 = 1.7358.
    expect_lt(gap, 0.4748 * 1.7358 / sqrt(lambda))
  },
  "hypergeometric>binomial" = function() {
    # The mass functions differ by O(k^2 / N) for an urn of size N.
    k <- 10; p <- 0.3; N <- 1e5
    h <- dl_density(dl_dist("hypergeometric", m = p * N, n = (1 - p) * N, k = k), 0:k)
    expect_lt(max(abs(h - dbinom(0:k, k, p))), k^2 / N)
  },
  "betabinomial>binomial" = function() {
    # With alpha + beta = c the mass functions differ by O(n^2 / c).
    n <- 15; p <- 0.25; c0 <- 1e6
    bb <- dl_density(dl_dist("betabinomial", size = n, alpha = p * c0,
                             beta = (1 - p) * c0), 0:n)
    expect_lt(max(abs(bb - dbinom(0:n, n, p))), n^2 / c0)
  },
  "student_t>normal" = function() {
    # Densities differ by O(1 / nu).
    x <- seq(-6, 6, by = 0.25)
    gap <- function(nu) max(abs(dt(x, nu) - dnorm(x)))
    expect_lt(gap(1e4), 1 / 1e4)
    expect_lt(gap(1e4), gap(10))
  },

  # Transformations, checked by simulation.
  "gamma>beta" = function() {
    set.seed(21)
    x <- dl_sample(dl_dist("gamma", shape = 2, rate = 3), n_sim)
    y <- dl_sample(dl_dist("gamma", shape = 0.7, rate = 3), n_sim)
    expect_ks(x / (x + y), function(q) dl_cdf(dl_dist("beta", shape1 = 2, shape2 = 0.7), q))
  },
  "chisq>f" = function() {
    set.seed(22)
    u <- dl_sample(dl_dist("chisq", df = 4), n_sim)
    v <- dl_sample(dl_dist("chisq", df = 9), n_sim)
    expect_ks((u / 4) / (v / 9), function(q) dl_cdf(dl_dist("f", df1 = 4, df2 = 9), q))
  },
  "exponential>laplace" = function() {
    set.seed(23)
    e <- dl_dist("exponential", rate = 0.5)
    x <- dl_sample(e, n_sim) - dl_sample(e, n_sim)
    expect_ks(x, function(q) dl_cdf(dl_dist("laplace", mu = 0, sigma = 2), q))
  },
  "uniform>exponential" = function() {
    set.seed(24)
    u <- dl_sample(dl_dist("uniform"), n_sim)
    expect_ks(-log(u) / 3, function(q) dl_cdf(dl_dist("exponential", rate = 3), q))
  },
  "uniform>logistic" = function() {
    set.seed(25)
    u <- dl_sample(dl_dist("uniform"), n_sim)
    expect_ks(log(u / (1 - u)), function(q) dl_cdf(dl_dist("logistic"), q))
  },
  "lognormal>normal" = function() {
    set.seed(26)
    x <- dl_sample(dl_dist("lognormal", meanlog = 1, sdlog = 0.4), n_sim)
    expect_ks(log(x), function(q) dl_cdf(dl_dist("normal", mean = 1, sd = 0.4), q))
  },
  "normal>normal" = function() {
    set.seed(27)
    x <- dl_sample(dl_dist("normal", mean = 5, sd = 3), n_sim)
    expect_ks((x - 5) / 3, function(q) dl_cdf(dl_dist("normal"), q))
  },
  "normal>lognormal" = function() {
    set.seed(28)
    x <- dl_sample(dl_dist("normal", mean = -1, sd = 0.8), n_sim)
    expect_ks(exp(x), function(q) dl_cdf(dl_dist("lognormal", meanlog = -1, sdlog = 0.8), q))
  },
  "normal>chisq" = function() {
    set.seed(29)
    z <- dl_sample(dl_dist("normal"), n_sim)
    expect_ks(z^2, function(q) dl_cdf(dl_dist("chisq", df = 1), q))
  },
  "normal>cauchy" = function() {
    set.seed(30)
    z <- dl_sample(dl_dist("normal"), 2 * n_sim)
    expect_ks(z[1:n_sim] / z[-(1:n_sim)], function(q) dl_cdf(dl_dist("cauchy"), q))
  },
  "student_t>f" = function() {
    set.seed(31)
    t <- dl_sample(dl_dist("student_t", df = 6), n_sim)
    expect_ks(t^2, function(q) dl_cdf(dl_dist("f", df1 = 1, df2 = 6), q))
  }
)

registry_relations <- function() {
  out <- character()
  for (e in dl_registry()) {
    for (rel in e$relationships) out <- c(out, paste0(e$id, ">", rel$to))
  }
  out
}

test_that("every registry relationship has a numeric check", {
  expect_setequal(registry_relations(), names(relation_checks))
})

for (key in names(relation_checks)) {
  test_that(paste("relationship holds:", key), relation_checks[[key]]())
}
