# Assemble a known model to compare diagnostics against independent reference
# calculations, independently of the optimizer's estimates.
known_diagnostic_fit <- function(data, dist, k = length(dist$params)) {
  id <- dist$id
  structure(list(data = data, fits = stats::setNames(list(list(id = id, dist = dist, k = k)), id),
                 ranking = data.frame(id = id)), class = "dl_fit")
}

test_that("compressed continuous GOF matches independent expanded calculations", {
  x <- c(-2, -.3, .5, 1, 2)
  w <- c(2, 3, 1, 4, 2)
  raw <- rep(x, w)
  dist <- dl_dist("normal", mean = .3, sd = 1.2)
  f <- known_diagnostic_fit(dl_fit_data(x, w), dist)
  diagnostic <- dl_fit_diagnostics(f)
  ks <- suppressWarnings(stats::ks.test(raw, "pnorm", mean = .3, sd = 1.2, exact = FALSE))
  ad <- goftest::ad.test(raw, null = "pnorm", mean = .3, sd = 1.2)
  cvm <- goftest::cvm.test(raw, null = "pnorm", mean = .3, sd = 1.2)
  expect_equal(diagnostic$gof$statistic, unname(c(ks$statistic, ad$statistic, cvm$statistic)),
               tolerance = 1e-12)
  expect_equal(diagnostic$gof$p.value[2:3], c(ad$p.value, cvm$p.value), tolerance = 1e-12)
  expect_true(all(grepl("descriptive", diagnostic$gof$label)))
  expect_true("fit.note_ties" %in% diagnostic$notes)
  expect_equal(diagnostic$cdf$empirical, cumsum(w) / sum(w))
  expect_equal(diagnostic$cdf$fitted, pnorm(x, .3, 1.2))
})

test_that("Q-Q envelope is a pointwise order-statistic interval without RNG", {
  raw <- c(-1, -.1, .2, .8, 1.1)
  dist <- dl_dist("normal")
  f <- known_diagnostic_fit(dl_fit_data(raw), dist)
  set.seed(91)
  before <- .Random.seed
  d <- dl_fit_diagnostics(f, level = .9)
  expect_identical(.Random.seed, before)
  expect_equal(d$qq$theoretical, qnorm((seq_along(raw) - .5) / length(raw)))
  expect_equal(d$qq$observed, raw)
  expect_equal(d$qq$lower, qnorm(qbeta(.05, 1:5, 5:1)))
  expect_equal(d$qq$upper, qnorm(qbeta(.95, 1:5, 5:1)))
  expect_equal(d$pp$theoretical, pnorm(raw))
  expect_equal(d$pp$observed, (1:5 - .5) / 5)
  expect_true("fit.note_conditional_envelope" %in% d$notes)
})

test_that("large compressed samples keep diagnostics bounded", {
  f <- known_diagnostic_fit(dl_fit_data(c(-1, 0, 1), c(1e9, 2e9, 1e9)), dl_dist("normal"))
  d <- dl_fit_diagnostics(f, points = 30)
  expect_lte(nrow(d$qq), 30)
  expect_lte(nrow(d$density), 100)
  expect_equal(nrow(d$cdf), 3)
  expect_true(all(is.finite(d$gof$statistic)))
  expect_true(all(d$gof$p.value >= 0 & d$gof$p.value <= 1))
})

test_that("merged discrete bins include unobserved tails and subtract fitted parameters", {
  dist <- dl_dist("poisson", lambda = 3)
  x <- 0:10
  w <- c(7, 15, 23, 22, 16, 10, 4, 2, 1, 0, 0)
  f <- known_diagnostic_fit(dl_fit_data(x, w), dist, k = 1)
  d <- dl_fit_diagnostics(f)
  b <- d$bins
  expect_true(all(b$expected >= 5))
  expect_equal(sum(b$observed), sum(w))
  expect_equal(sum(b$expected), sum(w), tolerance = 1e-12)
  expect_equal(b$lower[1], -Inf)
  expect_equal(tail(b$upper, 1), Inf)
  reference <- sum((b$observed - b$expected)^2 / b$expected)
  df <- nrow(b) - 1 - 1
  expect_equal(d$gof$statistic, reference)
  expect_equal(d$gof$df, df)
  expect_equal(d$gof$p.value, pchisq(reference, df, lower.tail = FALSE))
  expect_equal(d$density$empirical[d$density$kind == "empirical"], w[w > 0] / sum(w))
})

test_that("insufficient discrete degrees of freedom do not invent a p-value", {
  f <- known_diagnostic_fit(dl_fit_data(c(0, 1), c(2, 2)), dl_dist("bernoulli"), k = 1)
  d <- dl_fit_diagnostics(f)
  expect_true(is.na(d$gof$p.value))
  expect_true(is.na(d$gof$df))
  expect_true("fit.note_insufficient_bins" %in% d$notes)
})

test_that("default diagnostics skip failed families in earlier likelihood groups", {
  fit <- dl_fit(rep(0, 10), c("normal", "geometric"))
  expect_identical(fit$ranking$id[1], "normal")
  expect_identical(dl_fit_diagnostics(fit)$id, "geometric")
})

test_that("grouped diagnostics use class probabilities and retain gaps", {
  data <- dl_fit_data(lower = c(-2, 0, 2), upper = c(-1, 1, 3), frequency = c(20, 40, 20))
  dist <- dl_dist("normal", mean = .5, sd = 2)
  f <- known_diagnostic_fit(data, dist, k = 2)
  d <- dl_fit_diagnostics(f)
  expect_equal(nrow(d$qq), 0)
  expect_equal(nrow(d$pp), 0)
  expect_equal(d$cdf$x, data$upper)
  expect_equal(d$cdf$empirical, c(.25, .75, 1))
  expect_identical(d$gof$test, "Chi-square")
  expect_true("fit.note_grouped_diagnostics" %in% d$notes)
  expect_equal(sum(d$bins$expected), 80)
  expect_equal(sum(d$bins$observed), 80)
  expect_true(any(d$bins$observed == 0))
  expect_equal(d$bins$lower[1], -Inf)
  expect_equal(tail(d$bins$upper, 1), Inf)
  infinity <- dl_fit_data(lower = c(-Inf, 0), upper = c(0, Inf), frequency = c(20, 20))
  di <- dl_fit_diagnostics(known_diagnostic_fit(infinity, dist, 2))
  expect_true(all(is.finite(di$density$x)))
  expect_equal(nrow(di$qq), 0)
})

test_that("histogram integrates to one and diagnostics validate arguments", {
  f <- known_diagnostic_fit(dl_fit_data(c(-1, 0, 0, 2)), dl_dist("normal"))
  d <- dl_fit_diagnostics(f)
  b <- d$density[d$density$kind == "empirical", ]
  expect_equal(sum(b$empirical * (b$upper - b$lower)), 1)
  expect_error(dl_fit_diagnostics(list()), "dl_fit")
  expect_error(dl_fit_diagnostics(f, id = "poisson"), "successfully")
  expect_error(dl_fit_diagnostics(f, points = 1), "whole number")
  expect_error(dl_fit_diagnostics(f, level = 1), "level")
  expect_error(dl_fit_diagnostics(f, level = NA_real_), "level")
})

test_that("simple-null KS limiting reference is stable at both tails", {
  expect_equal(fit_ks_reference(0), 1)
  expect_equal(fit_ks_reference(.1), 1)
  expect_equal(fit_ks_reference(1), 2 * sum((-1)^(0:99) * exp(-2 * (1:100)^2)))
  expect_equal(fit_ks_reference(Inf), 0)
  expect_true(all(diff(vapply(seq(.1, 3, .1), fit_ks_reference, numeric(1))) <= 0))
})

test_that("grouped overlays compare probabilities of the same classes", {
  data <- dl_fit_data(lower = c(-.5, 1.5, 4.5), upper = c(1.5, 4.5, 10.5),
                      frequency = c(20, 60, 20))
  dist <- dl_dist("poisson", lambda = 3)
  d <- dl_fit_diagnostics(known_diagnostic_fit(data, dist, 1))
  observed <- d$density[d$density$kind == "empirical", ]
  fitted <- d$density[d$density$kind == "fitted", ]
  expect_equal(observed$empirical, c(.2, .6, .2))
  expect_equal(fitted$fitted, diff(ppois(c(-.5, 1.5, 4.5, 10.5), 3)))
  expect_true("fit.note_grouped_mass" %in% d$notes)
  open <- dl_fit_data(lower = c(-Inf, 0), upper = c(0, Inf), frequency = c(1, 1))
  di <- dl_fit_diagnostics(known_diagnostic_fit(open, dl_dist("normal")))
  expect_equal(nrow(di$density), 0)
  expect_true("fit.note_open_bins" %in% di$notes)
})

test_that("heavy-tail histograms disclose clipping and preserve tail denominator", {
  x <- c(-1e9, -2, -1, 0, 1, 2, 1e9)
  d <- dl_fit_diagnostics(known_diagnostic_fit(dl_fit_data(x), dl_dist("cauchy")))
  expect_true("fit.note_clipped" %in% d$notes)
  h <- d$density[d$density$kind == "empirical", ]
  expect_lt(max(h$upper), 1e9)
  expect_equal(sum(h$empirical * (h$upper - h$lower)), 5 / 7)
  expect_equal(tail(d$cdf$empirical, 1), 1)
})

test_that("fixed-model labels and fitted engine integration are consistent", {
  fixed <- known_diagnostic_fit(dl_fit_data(c(-1, 0, 1)), dl_dist("normal"), k = 0)
  expect_true(all(grepl("fixed parameters", dl_fit_diagnostics(fixed)$gof$label)))
  f <- dl_fit(c(-2, -1, .1, .5, .8, 1.5), c("normal", "cauchy"))
  d <- dl_fit_diagnostics(f)
  expect_identical(d$id, f$ranking$id[1])
  expect_equal(nrow(d$gof), 3)
  grouped <- dl_fit_data(lower = c(-Inf, -1, 0, 1), upper = c(-1, 0, 1, Inf),
                         frequency = c(8, 25, 23, 9))
  fit <- dl_fit(grouped, "normal")
  expect_identical(dl_fit_diagnostics(fit)$gof$test, "Chi-square")
})
