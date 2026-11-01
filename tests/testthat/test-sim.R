# Simulation engine: seeds, Monte Carlo checks, LLN, CLT, transformations,
# memorylessness and histogram data.

strip_attrs <- function(x) {
  attr(x, "seed") <- NULL
  attr(x, "code") <- NULL
  x
}

test_that("a seed is used and the caller's random stream is restored", {
  set.seed(99)
  before <- .Random.seed
  a <- dl_mc_check(dl_dist("normal"), n = 50, seed = 5)
  expect_identical(.Random.seed, before)
  b <- dl_mc_check(dl_dist("normal"), n = 50, seed = 5)
  expect_identical(a, b)
  expect_identical(attr(a, "seed"), 5L)
  # Without a seed the current stream is used and advanced.
  set.seed(99)
  c1 <- dl_lln(dl_dist("normal"), n = 10)
  expect_false(identical(.Random.seed, before))
  expect_identical(c1$seed, NA_integer_)
  set.seed(99)
  expect_identical(dl_lln(dl_dist("normal"), n = 10)$paths, c1$paths)
  # No stream before the call: none after it either.
  without_random_seed({
    dl_lln(dl_dist("normal"), n = 10, seed = 1)
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  })
})

test_that("invalid seeds and counts are refused with clear messages", {
  d <- dl_dist("normal")
  for (bad in list(1.5, NA, Inf, "1", c(1, 2), 2^31)) {
    expect_error(dl_lln(d, n = 10, seed = bad), "`seed` must be NULL or a single whole number")
  }
  for (bad in list(0, 1.5, NA, Inf, -3, 1e8, "10")) {
    expect_error(dl_mc_check(d, n = bad), "`n` must be a single whole number")
  }
  expect_error(dl_mc_check(list(), n = 10), "dl_dist")
  expect_error(dl_mc_check(d, n = 10, level = 1), "`level`")
  expect_error(dl_lln(d, n = 1e6, paths = 20), "n \\* paths")
  expect_error(dl_lln(d, paths = 0), "`paths`")
  expect_error(dl_clt(d, sizes = c(1, 1)), "distinct")
  expect_error(dl_clt(d, sizes = c(0, 2)), "`sizes`")
  expect_error(dl_clt(d, reps = 1), "`reps`")
  expect_error(dl_clt(d, sizes = 1e4, reps = 1e4), "reps \\* sum")
  expect_error(dl_transform(d, "cube"), "`op` must be one of")
  expect_error(dl_transform(d), "`op` must be one of")
  expect_error(dl_transform(d, "sum", k = 0), "`k`")
  expect_error(dl_transform(d, "affine", a = NA), "`a`")
  expect_error(dl_memoryless(dl_dist("exponential"), s = -1), "`s` must be >= 0")
  expect_error(dl_memoryless(dl_dist("geometric"), s = 1.5), "whole number")
})

test_that("dl_mc_check compares estimates with exact values", {
  g <- dl_dist("gamma", shape = 2, rate = 1)
  res <- dl_mc_check(g, n = 4000, seed = 2026)
  expect_s3_class(res, "data.frame")
  expect_identical(res$quantity, c("mean", "variance", "sd", "skewness", "excess_kurtosis",
                                   "cdf_q1", "cdf_q2", "cdf_q3"))
  expect_named(res, c("quantity", "q", "exact", "estimate", "se", "lower", "upper", "covered"))
  expect_equal(res$exact[1:5], unname(dl_moments(g)))
  expect_equal(res$q[6:8], qgamma(c(0.25, 0.5, 0.75), 2, 1))
  expect_equal(res$exact[6:8], c(0.25, 0.5, 0.75))
  expect_true(all(is.na(res$se[3:5])))
  expect_true(all(is.finite(res$se[c(1, 2, 6:8)])))
  # The code rebuilds the same table, bit for bit.
  expect_identical(run_sim_code(attr(res, "code"))$.value, strip_attrs(res))
  # Discrete quartiles: the exact probability is F(q), at least 1/4.
  p <- dl_mc_check(dl_dist("poisson", lambda = 2), n = 2000, seed = 3)
  expect_equal(p$exact[6:8], ppois(qpois(c(0.25, 0.5, 0.75), 2), 2))
  expect_true(all(p$exact[6:8] >= c(0.25, 0.5, 0.75)))
  expect_identical(run_sim_code(attr(p, "code"))$.value, strip_attrs(p))
})

test_that("intervals of dl_mc_check cover the exact values at about the nominal rate", {
  skip_on_cran()
  for (id in c("normal", "poisson")) {
    d <- dl_dist(id)
    cov <- vapply(1:40, function(s) {
      r <- dl_mc_check(d, n = 1000, seed = s)
      r$covered[c(1, 2, 6:8)]
    }, logical(5))
    rate <- rowMeans(cov)
    expect_true(all(rate >= 0.8 & rate <= 1), info = paste(id, paste(rate, collapse = " ")))
  }
})

test_that("dl_mc_check keeps infinite and missing moments without intervals", {
  cy <- dl_mc_check(dl_dist("cauchy"), n = 500, seed = 1)
  expect_true(all(is.na(cy$exact[1:5])))
  expect_true(all(is.finite(cy$estimate[1:5])))
  expect_true(all(is.na(cy$se[1:5]) & is.na(cy$covered[1:5])))
  expect_true(all(cy$covered[6:8] %in% c(TRUE, FALSE)))
  # Finite mean, infinite variance: no standard error for either.
  t15 <- dl_mc_check(dl_dist("student_t", df = 1.5), n = 500, seed = 1)
  expect_identical(t15$exact[1:2], c(0, Inf))
  expect_true(all(is.na(t15$se[1:2])))
  # Finite variance, infinite fourth moment: the mean has an interval only.
  t3 <- dl_mc_check(dl_dist("student_t", df = 3), n = 500, seed = 1)
  expect_true(is.finite(t3$se[1]))
  expect_true(is.na(t3$se[2]))
  expect_identical(run_sim_code(attr(t3, "code"))$.value, strip_attrs(t3))
})

test_that("tiny samples give NA, not errors", {
  d <- dl_dist("normal")
  r1 <- dl_mc_check(d, n = 1, seed = 1)
  expect_true(all(is.na(r1$estimate[2:5])))
  expect_true(is.na(r1$se[1]))
  r2 <- dl_mc_check(d, n = 2, seed = 1)
  expect_true(is.finite(r2$estimate[3]))
  expect_true(all(is.na(r2$estimate[4:5])))
  r3 <- dl_mc_check(d, n = 3, seed = 1)
  expect_true(is.finite(r3$estimate[4]))
  expect_true(is.na(r3$estimate[5]))
  for (r in list(r1, r2, r3)) {
    expect_identical(run_sim_code(attr(r, "code"))$.value, strip_attrs(r))
  }
  # A constant sample (all zeros) has no skewness or kurtosis.
  b <- dl_mc_check(dl_dist("bernoulli", prob = 0.001), n = 20, seed = 1)
  expect_true(all(is.na(b$estimate[4:5])))
  # A tiny Hypergeometric support and a Beta with shapes below 1.
  expect_s3_class(dl_mc_check(dl_dist("hypergeometric", m = 1, n = 1, k = 1), n = 50, seed = 1),
                  "data.frame")
  bb <- dl_mc_check(dl_dist("beta", shape1 = 0.3, shape2 = 0.4), n = 200, seed = 1)
  expect_true(all(bb$estimate[1:2] > 0))
})

test_that("generated code reproduces every engine for every distribution", {
  skip_on_cran()
  for (id in dl_ids()) {
    d <- dl_dist(id)
    r <- dl_mc_check(d, n = 200, seed = 7)
    expect_identical(run_sim_code(attr(r, "code"))$.value, strip_attrs(r), info = id)
    l <- dl_lln(d, n = 40, paths = 2, seed = 3)
    expect_identical(as.vector(run_sim_code(l$code)$running), l$paths$mean, info = id)
    cl <- dl_clt(d, sizes = c(1, 4), reps = 50, seed = 4)
    env <- run_sim_code(cl$code)
    expect_identical(unlist(if (cl$standardized) env$z else env$means), cl$means$value, info = id)
    if (cl$standardized) expect_identical(unname(env$.value), cl$distance$ks, info = id)
  }
})

test_that("engine numbers are the doubles their code literals read back as", {
  # R's parser is not correctly rounded on every platform; engines use the
  # parsed literal so generated code reproduces them bit for bit everywhere.
  reads_back <- function(y) identical(eval(parse(text = distlab:::code_num(y, exact = TRUE))), y)
  for (x in c(0.15971914124998499, -0.15971914124998499, 0.92861519963480532, 1 / 3, 1e-300, 5e300)) {
    expect_true(reads_back(distlab:::code_value(x)), info = format(x, digits = 17))
  }
  expect_identical(distlab:::code_value(Inf), Inf)
  expect_identical(distlab:::code_value(NA_real_), NA_real_)
  d <- distlab:::check_dist(dl_dist("beta", shape1 = 2 / 3, shape2 = 5))
  expect_identical(d$params$shape1,
                   eval(parse(text = distlab:::code_num(2 / 3, exact = TRUE))))
  expect_identical(d$params$shape2, 5)
})

test_that("dl_lln running means settle for the Normal, not for the Cauchy", {
  res <- dl_lln(dl_dist("normal", mean = 3, sd = 2), n = 5000, paths = 3, seed = 10)
  expect_identical(nrow(res$paths), 15000L)
  expect_identical(unique(res$paths$path), 1:3)
  last <- res$paths$mean[res$paths$k == 5000]
  expect_true(all(abs(last - 3) < 4 * 2 / sqrt(5000)))
  expect_equal(res$mean, 3)
  expect_equal(res$sd, 2)
  x <- run_sim_code(res$code)$x
  expect_equal(res$paths$mean[1:3], cumsum(x[1:3, 1]) / 1:3)
  expect_identical(as.vector(run_sim_code(res$code)$running), res$paths$mean)
  cy <- dl_lln(dl_dist("cauchy"), n = 200, seed = 1)
  expect_true(is.na(cy$mean))
  expect_true(is.na(cy$sd))
  # Infinite variance with a mean: the mean is kept, the sd is Inf.
  t15 <- dl_lln(dl_dist("student_t", df = 1.5), n = 100, seed = 1)
  expect_identical(c(t15$mean, t15$sd), c(0, Inf))
  # One draw per path.
  one <- dl_lln(dl_dist("poisson"), n = 1, paths = 4, seed = 1)
  expect_identical(nrow(one$paths), 4L)
  expect_identical(as.vector(run_sim_code(one$code)$running), one$paths$mean)
})

test_that("dl_clt standardizes when the variance is finite", {
  e <- dl_clt(dl_dist("exponential"), sizes = c(1, 5, 50), reps = 2000, seed = 2)
  expect_true(e$standardized)
  expect_identical(nrow(e$means), 6000L)
  ks <- e$distance$ks
  expect_true(ks[1] > ks[3])
  expect_lt(ks[3], 1.95 / sqrt(2000) + 0.03)
  m5 <- e$means$value[e$means$size == 5]
  expect_equal(mean(m5), 0, tolerance = 0.1)
  expect_equal(sd(m5), 1, tolerance = 0.1)
  for (d in list(dl_dist("cauchy"), dl_dist("student_t", df = 2), dl_dist("student_t", df = 1.5))) {
    r <- dl_clt(d, sizes = c(1, 10), reps = 100, seed = 1)
    expect_false(r$standardized)
    expect_true(all(is.na(r$distance$ks)))
    expect_identical(unlist(run_sim_code(r$code)$means), r$means$value)
  }
  expect_true(dl_clt(dl_dist("student_t", df = 3), sizes = 2, reps = 10, seed = 1)$standardized)
  # Discrete laws give ties; the distance is still the supremum.
  b <- dl_clt(dl_dist("bernoulli", prob = 0.5), sizes = 1, reps = 1000, seed = 1)
  z <- b$means$value
  expect_equal(b$distance$ks, max(abs(c(mean(z <= -1) - pnorm(-1), mean(z < -1) - pnorm(-1),
                                         mean(z <= 1) - pnorm(1), mean(z < 1) - pnorm(1)))))
})

test_that("every exact transform result agrees with its target", {
  skip_on_cran()
  cases <- list(
    list("bernoulli", list(prob = 0.3), "sum", 4, "binomial"),
    list("binomial", list(size = 5, prob = 0.4), "sum", 3, "binomial"),
    list("poisson", list(lambda = 2), "sum", 3, "poisson"),
    list("geometric", list(prob = 0.3), "sum", 3, "negbinomial"),
    list("negbinomial", list(size = 2.5, prob = 0.4), "sum", 3, "negbinomial"),
    list("normal", list(mean = 1, sd = 2), "sum", 3, "normal"),
    list("exponential", list(rate = 2), "sum", 3, "gamma"),
    list("gamma", list(shape = 1.5, rate = 2), "sum", 3, "gamma"),
    list("chisq", list(df = 3), "sum", 3, "chisq"),
    list("cauchy", list(location = 1, scale = 2), "sum", 3, "cauchy"),
    list("normal", list(mean = 1, sd = 2), "mean", 4, "normal"),
    list("cauchy", list(location = 1, scale = 2), "mean", 40, "cauchy"),
    list("gamma", list(shape = 1.5, rate = 2), "mean", 4, "gamma"),
    list("exponential", list(rate = 2), "mean", 4, "gamma"),
    list("exponential", list(rate = 2), "min", 3, "exponential"),
    list("weibull", list(shape = 2, scale = 1.5), "min", 3, "weibull"),
    list("geometric", list(prob = 0.2), "min", 3, "geometric"),
    list("uniform", list(min = 0, max = 1), "max", 3, "beta"),
    list("normal", list(mean = 1, sd = 2), "affine", 1, "normal", -1, 3),
    list("uniform", list(min = 0, max = 2), "affine", 1, "uniform", 2, -3),
    list("cauchy", list(location = 1, scale = 2), "affine", 1, "cauchy", 2, -3),
    list("laplace", list(mu = 1, sigma = 2), "affine", 1, "laplace", 1, -0.5),
    list("logistic", list(location = 1, scale = 2), "affine", 1, "logistic", 1, 2),
    list("exponential", list(rate = 2), "affine", 1, "exponential", 0, 3),
    list("gamma", list(shape = 2, rate = 2), "affine", 1, "gamma", 0, 3),
    list("weibull", list(shape = 2, scale = 1.5), "affine", 1, "weibull", 0, 3),
    list("lognormal", list(meanlog = 0, sdlog = 0.5), "affine", 1, "lognormal", 0, 3),
    list("normal", list(mean = 0, sd = 1), "square", 1, "chisq"),
    list("normal", list(mean = 0, sd = 2), "square", 1, "gamma"),
    list("student_t", list(df = 4), "square", 1, "f"),
    list("normal", list(mean = 1, sd = 0.5), "exp", 1, "lognormal"),
    list("lognormal", list(meanlog = 1, sdlog = 0.5), "log", 1, "normal")
  )
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    d <- do.call(dl_dist, c(list(cs[[1]]), cs[[2]]))
    a <- if (length(cs) >= 6) cs[[6]] else 0
    b <- if (length(cs) >= 7) cs[[7]] else 1
    r <- dl_transform(d, cs[[3]], k = cs[[4]], n = 3000, seed = 100 + i, a = a, b = b)
    info <- paste(cs[[1]], cs[[3]])
    expect_identical(r$target$id, cs[[5]], info = info)
    expect_null(r$approx)
    expect_sample_law(r$sample, r$target, info = info)
    env <- run_sim_code(r$code)
    expect_identical(env$y, r$sample, info = info)
    expect_identical(env$dropped, r$dropped, info = info)
  }
  # The exact parameters of a few targets.
  w <- dl_transform(dl_dist("weibull", shape = 2, scale = 1.5), "min", k = 4, n = 10, seed = 1)
  expect_equal(w$target$params$scale, 1.5 * 4^(-1 / 2))
  g <- dl_transform(dl_dist("geometric", prob = 1e-10), "min", k = 3, n = 10, seed = 1)
  # 1 - (1 - p)^3 = 3p - 3p^2 + p^3, kept accurate for tiny p.
  expect_equal(g$target$params$prob, 3e-10 - 3e-20 + 1e-30, tolerance = 1e-14)
  u <- dl_transform(dl_dist("uniform", min = 1, max = 3), "affine", n = 10, seed = 1, a = 1, b = -2)
  expect_equal(unlist(u$target$params), c(min = -5, max = -1))
})

test_that("dl_transform approximates, drops and refuses where it should", {
  s <- dl_transform(dl_dist("uniform"), "sum", k = 12, n = 500, seed = 1)
  expect_null(s$target)
  expect_identical(s$approx$id, "normal")
  expect_equal(unlist(s$approx$params), c(mean = 6, sd = 1))
  m <- dl_transform(dl_dist("poisson", lambda = 4), "mean", k = 4, n = 500, seed = 1)
  expect_equal(unlist(m$approx$params), c(mean = 4, sd = 1))
  # No finite variance, or a single copy: no Normal approximation.
  expect_null(dl_transform(dl_dist("student_t", df = 1.5), "sum", k = 5, n = 50, seed = 1)$approx)
  expect_null(dl_transform(dl_dist("uniform"), "sum", k = 1, n = 50, seed = 1)$approx)
  expect_null(dl_transform(dl_dist("gamma"), "max", k = 3, n = 50, seed = 1)$target)
  # exp() of a heavy right tail overflows; those draws are dropped and counted.
  big <- dl_transform(dl_dist("lognormal", meanlog = 0, sdlog = 30), "exp", n = 1000, seed = 1)
  expect_gt(big$dropped, 0)
  expect_identical(length(big$sample) + big$dropped, 1000L)
  expect_true(all(is.finite(big$sample)))
  env <- run_sim_code(big$code)
  expect_identical(env$dropped, big$dropped)
  # log() needs X > 0 with probability 1.
  expect_error(dl_transform(dl_dist("poisson"), "log"), class = "distlab_sim_error")
  err <- tryCatch(dl_transform(dl_dist("normal"), "log"), error = function(e) e)
  expect_identical(err$key, "log_support")
  expect_type(dl_transform(dl_dist("discrete_uniform", min = 1, max = 6), "log", n = 10,
                           seed = 1), "list")
  err <- tryCatch(dl_transform(dl_dist("normal"), "affine", b = 0), error = function(e) e)
  expect_identical(err$key, "affine_b")
  expect_error(dl_transform(dl_dist("normal"), "sum", k = 1000, n = 1e5), "n \\* k")
})

test_that("only the Exponential and the Geometric are memoryless", {
  ex <- dl_memoryless(dl_dist("exponential", rate = 1.5), s = 0.8, n = 5000, seed = 3)
  expect_true(ex$memoryless)
  expect_equal(ex$exact$residual, ex$exact$survival, tolerance = 1e-12)
  expect_sample_law(ex$residual, dl_dist("exponential", rate = 1.5))
  expect_identical(ex$kept, sum(run_sim_code(ex$code)$x > 0.8))
  wb <- dl_memoryless(dl_dist("weibull", shape = 2, scale = 1), s = 1, n = 5000, seed = 3)
  expect_false(wb$memoryless)
  expect_gt(max(abs(wb$exact$residual - wb$exact$survival)), 0.1)
  ks <- suppressWarnings(ks.test(wb$residual, "pweibull", shape = 2, scale = 1))
  expect_lt(ks$p.value, 1e-6)
  ge <- dl_memoryless(dl_dist("geometric", prob = 0.3), s = 3, n = 5000, seed = 3)
  expect_true(ge$memoryless)
  expect_identical(ge$exact$t, 0:max(ge$exact$t))
  expect_equal(ge$exact$survival[1], 1)
  expect_equal(ge$exact$residual, ge$exact$survival, tolerance = 1e-12)
  expect_sample_law(ge$residual, dl_dist("geometric", prob = 0.3))
  for (r in list(ex, wb, ge)) {
    env <- run_sim_code(r$code)
    expect_identical(env$residual, r$residual)
    expect_identical(env$t, r$exact$t)
    expect_identical(env$survival, r$exact$survival)
    expect_identical(env$conditional, r$exact$residual)
  }
  # Far tails keep their precision on the log scale.
  far <- dl_memoryless(dl_dist("exponential"), s = 3, n = 5000, seed = 1)
  expect_equal(far$exact$residual, far$exact$survival, tolerance = 1e-12)
  expect_false(dl_memoryless(dl_dist("poisson", lambda = 4), s = 2, n = 500, seed = 1)$memoryless)
})

test_that("dl_memoryless refuses other supports and too few draws", {
  err <- tryCatch(dl_memoryless(dl_dist("normal"), s = 1), error = function(e) e)
  expect_s3_class(err, "distlab_sim_error")
  expect_identical(err$key, "memoryless_support")
  expect_error(dl_memoryless(dl_dist("uniform"), s = 0.2), "support")
  err <- tryCatch(dl_memoryless(dl_dist("exponential"), s = 20, n = 1000, seed = 1),
                  error = function(e) e)
  expect_identical(err$key, "memoryless_few")
  expect_match(conditionMessage(err), "at least 30")
})

test_that("histogram data has integer bins for discrete draws", {
  x <- c(0, 1, 1, 2, 2, 2, 5)
  b <- distlab:::sim_bins(x, discrete = TRUE)
  expect_equal(b$mid, 0:5)
  expect_identical(b$count, c(1L, 2L, 3L, 0L, 0L, 1L))
  expect_equal(sum(b$proportion), 1)
  expect_equal(b$right - b$left, rep(1, 6))
  # Shares over a larger total, and a clipped range.
  b2 <- distlab:::sim_bins(x, discrete = TRUE, range = c(1, 2), total = 14)
  expect_identical(b2$count, c(2L, 3L))
  expect_equal(b2$proportion, c(2, 3) / 14)
  set.seed(1)
  y <- rnorm(5000)
  h <- distlab:::sim_bins(y)
  expect_gte(nrow(h), 10L)
  expect_lte(nrow(h), 80L)
  expect_equal(sum(h$density * (h$right - h$left)), 1)
  h2 <- distlab:::sim_bins(y, breaks = seq(-1, 1, by = 0.5))
  expect_identical(sum(h2$count), sum(abs(y) <= 1))
  expect_equal(sum(h2$density * 0.5), mean(abs(y) <= 1))
  expect_identical(nrow(distlab:::sim_bins(numeric(0))), 0L)
  # A constant sample still gets a bin.
  expect_identical(sum(distlab:::sim_bins(rep(2, 5))$count), 5L)
})

test_that("plotting ranges cut heavy tails and keep support ends", {
  set.seed(1)
  cy <- dl_dist("cauchy")
  r <- distlab:::sim_range(cy, rcauchy(1000))
  expect_equal(r, c(-30, 30))
  r <- distlab:::sim_range(dl_dist("f", df1 = 5, df2 = 10), rf(1000, 5, 10))
  expect_identical(r[1], 0)
  expect_equal(r[2], qf(0.995, 5, 10))
  r <- distlab:::sim_range(dl_dist("uniform"), runif(1000))
  expect_identical(r, c(0, 1))
  r <- distlab:::sim_range(dl_dist("exponential"), rexp(1000))
  expect_identical(r[1], 0)
  expect_gte(r[2], qexp(1e-4, lower.tail = FALSE))
  r <- distlab:::sim_range(dl_dist("poisson", lambda = 3), c(0, 15))
  expect_identical(r, c(0, 15))
  expect_true(distlab:::heavy_tailed(dl_dist("student_t", df = 3)))
  expect_false(distlab:::heavy_tailed(dl_dist("normal")))
  x <- c(rnorm(1000), 1e6)
  expect_lt(diff(distlab:::sample_range(x)), 10)
})

test_that("special cases of the Exponential and Geometric are memoryless", {
  yes <- list(dl_dist("exponential", rate = 3), dl_dist("geometric", prob = 0.2),
              dl_dist("gamma", shape = 1, rate = 2), dl_dist("weibull", shape = 1, scale = 2),
              dl_dist("chisq", df = 2), dl_dist("negbinomial", size = 1, prob = 0.4))
  for (d in yes) {
    s <- if (d$entry$type == "discrete") 2 else 0.5
    r <- dl_memoryless(d, s = s, n = 2000, seed = 1)
    expect_true(r$memoryless, info = d$id)
    expect_equal(r$exact$residual, r$exact$survival, tolerance = 1e-10, info = d$id)
  }
  no <- list(dl_dist("gamma", shape = 1.0001, rate = 2), dl_dist("weibull", shape = 2),
             dl_dist("chisq", df = 3), dl_dist("negbinomial", size = 2, prob = 0.4),
             dl_dist("lognormal"))
  for (d in no) expect_false(distlab:::is_memoryless(d), info = d$id)
})

test_that("the survival grid stays readable for heavy tails and small scales", {
  for (d in list(dl_dist("f", df1 = 0.5, df2 = 0.5), dl_dist("weibull", shape = 0.2, scale = 1))) {
    qq <- dl_quantile(d, c(0.25, 0.5, 0.75))
    r <- dl_memoryless(d, s = qq[2], n = 5000, seed = 1)
    t_max <- max(r$exact$t)
    expect_lte(t_max, qq[2] + 15 * (qq[3] - qq[1]) + 1e-9)
    expect_gt(t_max, qq[2])
    # The curve drops well inside the grid instead of hugging the y axis.
    expect_lt(min(r$exact$survival), 0.25)
    expect_gt(mean(r$exact$survival < 0.9), 0.5)
    expect_identical(run_sim_code(r$code)$t, r$exact$t)
  }
  # No floor at t = 1: a fast Exponential gets a short grid.
  r <- dl_memoryless(dl_dist("exponential", rate = 100), s = 0.01, n = 2000, seed = 1)
  expect_lt(max(r$exact$t), 0.1)
  expect_equal(max(r$exact$t), qexp(0.05, 100, lower.tail = FALSE))
})

test_that("lattices are found in the draws and binned on their points", {
  expect_identical(distlab:::lattice_step(c(0, 3, 9, 3, 12)), 3)
  expect_equal(distlab:::lattice_step(c(1, 4, 5, 2) / 3), 1 / 3)
  expect_true(is.na(distlab:::lattice_step(c(0, 1, 2.5))))
  expect_true(is.na(distlab:::lattice_step(c(2, 2))))
  set.seed(1)
  expect_true(is.na(distlab:::lattice_step(rnorm(100))))
  b <- distlab:::sim_bins(c(0.5, 1, 1, 2.5), range = c(0, 3), step = 0.5, total = 4)
  # Lattice points of the range, from the origin at the smallest draw.
  expect_equal(b$mid, seq(0, 3, by = 0.5))
  expect_identical(b$count, c(0L, 1L, 2L, 0L, 0L, 1L, 0L))
  expect_equal(sum(b$density * 0.5), 1)
  # Too many lattice points, or a lattice finer than continuous bins: NA.
  expect_true(is.na(distlab:::bin_step((0:3000)^2, c(0, 9e6), "law")))
  y <- rpois(5000, 50) / 7
  expect_true(is.na(distlab:::bin_step(y, range(y), "auto")))
  expect_identical(distlab:::bin_step(c(0, 2, 4), c(0, 4), "law", unit = TRUE), 1)
  expect_true(is.na(distlab:::bin_step(c(0, 2, 4), c(0, 4), "none")))
})

test_that("a discrete law wider than 5000 values takes its range from the draws", {
  nb <- dl_dist("negbinomial", size = 1500, prob = 0.05)
  expect_error(distlab:::curve_range(nb), "5000")
  x <- dl_sample(nb, 1000)
  r <- distlab:::sim_range(nb, x)
  expect_identical(r, c(min(x), max(x)) + 0)
  cv <- distlab:::law_curve(nb, r)
  expect_lte(nrow(cv), 2001L)
  expect_true(all(cv$x == round(cv$x)))
})
