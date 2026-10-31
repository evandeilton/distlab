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

test_that("quantile inverts the CDF for every entry", {
  u <- c(1e-6, 0.1, 0.5, 0.9, 1 - 1e-6)
  for (id in dl_ids(type = "continuous")) {
    d <- dl_dist(id)
    expect_equal(dl_cdf(d, dl_quantile(d, u)), u, tolerance = 1e-8, info = id)
    # Upper-tail quantiles keep precision for tiny tail probabilities.
    expect_equal(dl_cdf(d, dl_quantile(d, 1e-12, lower.tail = FALSE),
                        lower.tail = FALSE), 1e-12, tolerance = 1e-6, info = id)
  }
  # Discrete convention: smallest x with F(x) >= u.
  for (id in dl_ids(type = "discrete")) {
    d <- dl_dist(id)
    q <- dl_quantile(d, u)
    expect_true(all(dl_cdf(d, q) >= u * (1 - 1e-12)), info = id)
    expect_true(all(dl_cdf(d, q - 1) < u), info = id)
  }
})

test_that("densities integrate and masses sum to one over the support", {
  for (id in dl_ids(type = "continuous")) {
    d <- dl_dist(id)
    s <- distlab:::dl_support(d)
    # Split at the median so peaked densities on long supports are sampled.
    med <- dl_quantile(d, 0.5)
    total <- sum(vapply(list(c(s$lower, med), c(med, s$upper)), function(ab) {
      integrate(function(x) dl_density(d, x), ab[1], ab[2], rel.tol = 1e-10)$value
    }, numeric(1)))
    expect_equal(total, 1, tolerance = 1e-8, info = id)
  }
  for (id in dl_ids(type = "discrete")) {
    d <- dl_dist(id)
    s <- distlab:::dl_support(d)
    hi <- min(s$upper, dl_quantile(d, 1e-16, lower.tail = FALSE))
    x <- seq(s$lower, hi)
    expect_equal(sum(dl_density(d, x)), 1, tolerance = 1e-12, info = id)
    # No mass just outside a finite support.
    expect_equal(dl_density(d, s$lower - 1), 0, info = id)
    if (is.finite(s$upper)) expect_equal(dl_density(d, s$upper + 1), 0, info = id)
  }
})

test_that("parameter-dependent supports follow the parameters", {
  b <- dl_dist("binomial", size = 7, prob = 0.4)
  expect_equal(dl_cdf(b, 7), 1)
  expect_equal(dl_quantile(b, 1), 7)
  h <- dl_dist("hypergeometric", m = 4, n = 3, k = 5)
  expect_equal(dl_density(h, 1), 0)
  expect_gt(dl_density(h, 2), 0)
  expect_equal(dl_quantile(h, c(0, 1)), c(2, 4))
  du <- dl_dist("discrete_uniform", min = -3, max = 2)
  expect_equal(dl_density(du, -3:2), rep(1 / 6, 6))
  expect_equal(dl_quantile(du, c(0, 1)), c(-3, 2))
  bb <- dl_dist("betabinomial", size = 12, alpha = 0.5, beta = 0.7)
  expect_equal(dl_quantile(bb, c(0, 1)), c(0, 12))
  un <- dl_dist("uniform", min = 2, max = 5)
  expect_equal(dl_quantile(un, c(0, 1)), c(2, 5))
})

test_that("qbbinom matches a direct search on the CDF", {
  for (pars in list(c(10, 2, 3), c(1, 0.5, 0.5), c(40, 0.2, 5), c(25, 20, 20))) {
    size <- pars[1]; a <- pars[2]; b <- pars[3]
    cdf <- extraDistr::pbbinom(0:size, size, a, b)
    u <- c(0, 1e-9, 0.01, 0.3, 0.5, 0.77, 0.999, 1)
    direct <- vapply(u, function(v) (0:size)[which(cdf >= v * (1 - 1e-13))[1]], 0)
    expect_equal(distlab:::qbbinom(u, size, a, b), direct, info = format(pars))
    # Upper tail and log scale agree with the lower-tail definition.
    expect_equal(distlab:::qbbinom(1 - u[3:7], size, a, b, lower.tail = FALSE),
                 direct[3:7], info = format(pars))
    expect_equal(distlab:::qbbinom(log(u[2:7]), size, a, b, log.p = TRUE),
                 direct[2:7], info = format(pars))
  }
  expect_true(is.nan(distlab:::qbbinom(1.5, 5, 1, 1)))
  expect_true(is.na(distlab:::qbbinom(NA, 5, 1, 1)))
  # A tiny upper tail is found from the summed upper tail, not 1 - p.
  q <- distlab:::qbbinom(1e-15, 30, 2, 2, lower.tail = FALSE)
  expect_lte(extraDistr::pbbinom(q, 30, 2, 2, lower.tail = FALSE), 1e-15)
  expect_gt(extraDistr::pbbinom(q - 1, 30, 2, 2, lower.tail = FALSE), 1e-15)
})

test_that("every entry works at the extremes of its sliders", {
  for (id in dl_ids()) {
    e <- dl_entry(id)
    pars <- e$parameters
    nm <- vapply(pars, `[[`, "", "name")
    combos <- list(stats::setNames(lapply(pars, function(p) p$slider$min), nm),
                   stats::setNames(lapply(pars, function(p) p$slider$max), nm))
    for (par in pars) {
      for (v in c(par$slider$min, par$slider$max)) {
        combos <- c(combos, list(stats::setNames(list(v), par$name)))
      }
    }
    for (cb in combos) {
      lab <- paste(id, paste(names(cb), unlist(cb), collapse = ", "))
      d <- tryCatch(do.call(dl_dist, c(list(id), cb)), error = function(err) NULL)
      # Combinations that break a constraint (e.g. min = max) are refused.
      if (is.null(d)) {
        expect_true(length(e$constraints) > 0, info = lab)
        next
      }
      cv <- dl_curve(d)
      expect_true(all(is.finite(cv$x) & is.finite(cv$cdf)), info = lab)
      expect_true(all(cv$density >= 0), info = lab)
      expect_true(all(diff(cv$cdf) >= -1e-12), info = lab)
      m <- dl_moments(d)
      expect_false(any(is.nan(m)), info = lab)
      expect_true(all(is.na(m) | m != -Inf), info = lab)
    }
  }
})

test_that("dl_curve handles bounded supports with unbounded densities", {
  for (sh in list(c(0.5, 0.5), c(0.1, 0.1), c(0.1, 20), c(20, 0.1))) {
    cv <- dl_curve(dl_dist("beta", shape1 = sh[1], shape2 = sh[2]))
    expect_true(all(cv$x > 0 & cv$x < 1), info = format(sh))
    expect_true(all(is.finite(cv$density)), info = format(sh))
  }
  cu <- dl_curve(dl_dist("uniform", min = -1, max = 3))
  expect_true(all(cu$x >= -1 & cu$x <= 3))
  expect_equal(cu$density, rep(0.25, nrow(cu)))
  # A U-shaped Beta gets the y-axis cap; the flat Uniform does not.
  expect_length(distlab:::density_ylim(dl_curve(dl_dist("beta", shape1 = 0.3,
                                                         shape2 = 0.3))), 2)
  expect_null(distlab:::density_ylim(cu))
})

test_that("integer parameters reject fractional values", {
  expect_error(dl_dist("binomial", size = 2.5), "outside its domain")
  expect_error(dl_dist("hypergeometric", k = 1.5), "outside its domain")
  expect_error(dl_dist("discrete_uniform", min = 0.5), "outside its domain")
  expect_s3_class(dl_dist("negbinomial", size = 2.5), "dl_dist")
})

test_that("tiny and huge parameter values stay finite", {
  expect_equal(dl_cdf(dl_dist("exponential", rate = 1e-8), 1e8), 1 - exp(-1))
  expect_equal(dl_quantile(dl_dist("normal", mean = 1e12, sd = 1e-3), 0.5), 1e12)
  expect_equal(dl_density(dl_dist("binomial", size = 1e6, prob = 0.5), 5e5),
               dbinom(5e5, 1e6, 0.5))
  cv <- dl_curve(dl_dist("poisson", lambda = 1e4))
  expect_true(all(is.finite(cv$density)))
  expect_equal(dl_cdf(dl_dist("bernoulli", prob = 0), 0), 1)
  expect_equal(dl_cdf(dl_dist("geometric", prob = 1), 0), 1)
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

test_that("Beta-binomial p and q round-trip exactly and stay in [0, 1]", {
  pars <- list(c(100, 20, 20), c(30, 5, 0.1), c(100, 0.1, 2), c(10, 20, 20),
               c(100, 0.1, 20), c(100, 2, 20))
  for (pr in pars) {
    d <- dl_dist("betabinomial", size = pr[1], alpha = pr[2], beta = pr[3])
    x <- 0:pr[1]
    p <- dl_cdf(d, x)
    expect_true(all(p >= 0 & p <= 1), info = toString(pr))
    # Where the CDF has saturated at 1 in double precision, only the upper
    # tail can tell values apart; check each tail where it is resolvable.
    lo <- p < 1 - 1e-10
    expect_identical(dl_quantile(d, p[lo]), as.numeric(x[lo]), info = toString(pr))
    s <- dl_cdf(d, x, lower.tail = FALSE)
    hi <- s > 1e-300 & s < 1
    expect_identical(dl_quantile(d, s[hi], lower.tail = FALSE),
                     as.numeric(x[hi]), info = toString(pr))
    expect_equal(s, c(rev(cumsum(rev(dl_density(d, x))))[-1], 0),
                 tolerance = 1e-12, info = toString(pr))
  }
})

test_that("heavy-tailed plotting ranges keep the body visible", {
  cv <- dl_curve(dl_dist("cauchy"))
  expect_lte(max(abs(cv$x)), 31)
  expect_gt(sum(abs(cv$x) < 2), 20)
  tv <- dl_curve(dl_dist("student_t", df = 0.5))
  expect_lt(diff(range(tv$x)), 100)
  # Light tails are not clipped.
  nv <- dl_curve(dl_dist("normal"))
  expect_equal(range(nv$cdf), c(1e-4, 1 - 1e-4), tolerance = 1e-8)
})
