test_that("continuous intervals match CDF differences, whatever the ends", {
  g <- dl_dist("gamma", shape = 2, rate = 1)
  ref <- pgamma(3.4, 2, 1) - pgamma(1.2, 2, 1)
  expect_equal(dl_prob(g, 1.2, 3.4), ref, tolerance = 1e-14)
  expect_equal(dl_prob(g, 1.2, 3.4, lower_closed = TRUE, upper_closed = FALSE), ref,
               tolerance = 1e-14)
  expect_equal(dl_prob(g, upper = 2), pgamma(2, 2, 1))
  expect_identical(dl_prob(g, 2, 2), 0)
  expect_identical(dl_prob(g), 1)
  expect_identical(dl_prob(g, log = TRUE), 0)
})

test_that("upper tails use lower.tail = FALSE, never 1 - p", {
  g <- dl_dist("gamma", shape = 2, rate = 1)
  for (x in c(0.5, 5, 50, 500)) {
    expect_equal(dl_prob(g, lower = x), pgamma(x, 2, 1, lower.tail = FALSE),
                 tolerance = 1e-13, info = x)
  }
  # 1 - pgamma(50, 2) is exactly 0 in double precision; the tail is not.
  expect_identical(1 - pgamma(50, 2), 0)
  expect_gt(dl_prob(g, lower = 50), 0)
  n <- dl_dist("normal")
  expect_equal(dl_prob(n, 30, 31), pnorm(30, lower.tail = FALSE) - pnorm(31, lower.tail = FALSE),
               tolerance = 1e-12)
})

test_that("extreme tails stay finite on the log scale", {
  n <- dl_dist("normal")
  expect_identical(dl_prob(n, lower = 40), pnorm(40, lower.tail = FALSE))
  expect_equal(dl_prob(n, lower = 40, log = TRUE), pnorm(40, lower.tail = FALSE, log.p = TRUE))
  # Below the double range: the probability underflows, its log does not.
  expect_identical(dl_prob(n, upper = -40), 0)
  expect_equal(dl_prob(n, upper = -40, log = TRUE), pnorm(-40, log.p = TRUE))
  lp <- dl_prob(n, 38, 40, log = TRUE)
  expect_true(is.finite(lp))
  expect_lte(lp, pnorm(38, lower.tail = FALSE, log.p = TRUE))
  expect_equal(lp, pnorm(38, lower.tail = FALSE, log.p = TRUE), tolerance = 1e-12)
})

test_that("discrete endpoint rules count the right support points", {
  b <- dl_dist("binomial", size = 10, prob = 0.5)
  pm <- function(k) sum(dbinom(k, 10, 0.5))
  expect_equal(dl_prob(b, 2, 5), pm(3:5))
  expect_equal(dl_prob(b, 2, 5, lower_closed = TRUE), pm(2:5))
  expect_equal(dl_prob(b, 2, 5, upper_closed = FALSE), pm(3:4))
  expect_equal(dl_prob(b, 2, 5, lower_closed = TRUE, upper_closed = FALSE), pm(2:4))
  expect_equal(dl_prob(b, 3, 3, lower_closed = TRUE, upper_closed = TRUE), dbinom(3, 10, 0.5))
  expect_identical(dl_prob(b, 3, 3), 0)
  expect_identical(dl_prob(b, 3, 4, upper_closed = FALSE), 0)
  # Non-integer ends: 2.5 < X <= 5.5 is 3 <= X <= 5.
  expect_equal(dl_prob(b, 2.5, 5.5), pm(3:5))
  expect_equal(dl_prob(b, 2.5, 5.5, lower_closed = TRUE, upper_closed = FALSE), pm(3:5))
  expect_equal(dl_prob(b, lower = 7), pbinom(7, 10, 0.5, lower.tail = FALSE))
  p <- dl_dist("poisson", lambda = 3)
  expect_equal(dl_prob(p, lower = 60), ppois(60, 3, lower.tail = FALSE), tolerance = 1e-12)
  expect_gt(dl_prob(p, lower = 60), 0)
})

test_that("every entry gives total probability 1 and matches its CDF", {
  for (id in dl_ids()) {
    d <- dl_dist(id)
    q <- dl_quantile(d, c(0.2, 0.7))
    expect_equal(dl_prob(d), 1, info = id)
    expect_equal(dl_prob(d, q[1], q[2]), diff(dl_cdf(d, q)), tolerance = 1e-10, info = id)
  }
})

test_that("invalid input gives clear errors", {
  g <- dl_dist("gamma")
  expect_error(dl_prob(g, 3, 1), "must not be greater")
  expect_error(dl_prob(g, NA, 1), "single number")
  expect_error(dl_prob(g, c(1, 2), 3), "single number")
  expect_error(dl_prob(g, "a", 3), "single number")
  expect_error(dl_prob(g, 1, 2, lower_closed = NA), "TRUE or FALSE")
  expect_error(dl_prob("gamma", 1, 2), "dl_dist object")
  expect_error(dl_interval(g, 1), "\\(0, 1\\)")
  expect_error(dl_interval(g, NA), "\\(0, 1\\)")
  expect_error(dl_interval(list(), 0.5), "dl_dist object")
})

test_that("central intervals hold the probability, at least for discrete", {
  n <- dl_dist("normal")
  iv <- dl_interval(n, 0.95)
  expect_equal(unname(iv[c("lower", "upper")]), qnorm(c(0.025, 0.975)))
  expect_equal(iv[["coverage"]], 0.95)
  # The upper end keeps precision when prob is close to 1.
  prob <- 1 - 1e-12
  iv <- dl_interval(n, prob)
  expect_identical(iv[["upper"]], qnorm((1 - prob) / 2, lower.tail = FALSE))
  expect_equal(iv[["lower"]], -iv[["upper"]])
  for (id in dl_ids(type = "discrete")) {
    d <- dl_dist(id)
    iv <- dl_interval(d, 0.9)
    expect_gte(iv[["coverage"]], 0.9 - 1e-12)
    expect_equal(iv[["coverage"]], dl_prob(d, iv[["lower"]], iv[["upper"]], TRUE, TRUE))
  }
})

test_that("numbers typed with a decimal comma or point are parsed", {
  pn <- distlab:::parse_num
  expect_identical(pn("1,5", "pt-BR"), 1.5)
  expect_identical(pn("1.5", "pt-BR"), 1.5)
  expect_identical(pn(" -2,25 ", "pt-BR"), -2.25)
  expect_identical(pn("1.234,5", "pt-BR"), 1234.5)
  expect_identical(pn("1,234.5", "en-US"), 1234.5)
  expect_identical(pn("1e-3", "en-US"), 0.001)
  expect_identical(pn("−3", "en-US"), -3)
  expect_identical(pn("-Inf", "pt-BR"), -Inf)
  expect_identical(pn("∞", "pt-BR"), Inf)
  expect_true(is.na(pn("abc", "en-US")))
  expect_true(is.na(pn("1,2,3", "pt-BR")))
  expect_true(is.na(pn("", "pt-BR")))
  expect_true(is.na(pn(NULL, "pt-BR")))
  expect_identical(distlab:::parse_num_list("0,5; 1; 2", "pt-BR"), c(0.5, 1, 2))
  expect_identical(distlab:::parse_num_list("0.5, 1, 2", "en-US"), c(0.5, 1, 2))
  expect_identical(distlab:::parse_num_list("0.5 1;2", "en-US"), c(0.5, 1, 2))
})

test_that("probabilities are formatted for display in both languages", {
  fp <- distlab:::format_prob
  expect_identical(fp(log(0.4521), "pt-BR"), "0,4521")
  expect_identical(fp(log(0.4521), "en-US"), "0.4521")
  expect_identical(fp(-Inf, "en-US"), "0")
  expect_match(fp(-Inf, "en-US", empty = FALSE), "^< 10")
  # Tails beyond the double range show their size from the log.
  lp <- pnorm(40, lower.tail = FALSE, log.p = TRUE)
  expect_identical(fp(lp, "pt-BR"), "3,656 × 10⁻³⁵⁰")
  expect_identical(fp(log(3.1671e-5), "en-US"), "3.167 × 10⁻⁵")
  expect_identical(fp(log(1 - 1e-6), "en-US"), "0.999999")
  expect_identical(distlab:::format_pct(log(0.4521), "pt-BR"), "45,21%")
  expect_identical(distlab:::format_pct(log(1e-9), "en-US"), "< 0.01%")
  expect_identical(distlab:::format_pct(log1p(-1e-9), "en-US"), "> 99.99%")
})

# Evaluate generated code in a clean environment.
eval_code <- function(code) eval(parse(text = code), envir = new.env(parent = globalenv()))

test_that("generated R code reproduces the result", {
  cases <- list(
    list("gamma", list(shape = 2, rate = 1), "between", list(a = 1.2, b = 3.4)),
    list("gamma", list(shape = 2, rate = 1), "between", list(a = 6, b = 9)),
    list("normal", list(mean = 0, sd = 1), "gt", list(x = 1.96)),
    list("normal", list(mean = 0, sd = 1), "le", list(x = -1)),
    list("binomial", list(size = 10, prob = 0.5), "between",
         list(a = 2, b = 5, lower_closed = TRUE, upper_closed = FALSE)),
    list("bernoulli", list(prob = 0.3), "gt", list(x = 0)),
    list("betabinomial", list(size = 10, alpha = 2, beta = 3), "between", list(a = 2, b = 5)),
    list("laplace", list(mu = 0, sigma = 1), "gt", list(x = 2)),
    list("discrete_uniform", list(min = 1, max = 6), "le", list(x = 4))
  )
  for (cs in cases) {
    d <- do.call(dl_dist, c(list(cs[[1]]), cs[[2]]))
    res <- do.call(distlab:::calc_result, c(list(d, cs[[3]]), cs[[4]]))
    code <- distlab:::calc_code(d, res)
    expect_equal(eval_code(code), exp(res$log_p), tolerance = 1e-10, info = code)
  }
  g <- dl_dist("gamma", shape = 2, rate = 1)
  res <- distlab:::calc_result(g, "between", a = 6, b = 9)
  expect_match(distlab:::calc_code(g, res), "lower.tail = FALSE")
  res <- distlab:::calc_result(g, "quantile", p = 0.9)
  expect_equal(eval_code(distlab:::calc_code(g, res)), res$q)
  res <- distlab:::calc_result(g, "central", prob = 0.95)
  expect_equal(eval_code(distlab:::calc_code(g, res)), res$q)
  bb <- dl_dist("betabinomial")
  res <- distlab:::calc_result(bb, "quantile", p = 0.5)
  expect_equal(eval_code(distlab:::calc_code(bb, res)), res$q)
  # A tail below the double range also gets its log10 line.
  res <- distlab:::calc_result(dl_dist("normal"), "gt", x = 40)
  code <- distlab:::calc_code(dl_dist("normal"), res)
  expect_match(code, "log.p = TRUE) / log(10)", fixed = TRUE)
  expect_equal(eval_code(code), res$log_p / log(10))
  # Discrete intervals explain the cut points.
  b <- dl_dist("binomial", size = 10, prob = 0.5)
  res <- distlab:::calc_result(b, "between", a = 2, b = 5, lower_closed = TRUE, upper_closed = FALSE)
  expect_match(distlab:::calc_code(b, res), "# P(2 <= X < 5) = P(X <= 4) - P(X <= 1)", fixed = TRUE)
})

test_that("calculator results and sentences follow the mode", {
  g <- dl_dist("gamma", shape = 2, rate = 1)
  res <- distlab:::calc_result(g, "between", a = 1.2, b = 3.4)
  expect_identical(distlab:::calc_sentence(res, g, "pt-BR"),
                   "A probabilidade de X estar entre 1,2 e 3,4 é 0,5158 (51,58%).")
  expect_match(distlab:::calc_tex(res, "pt-BR"), "1{,}2", fixed = TRUE)
  # Continuous ends are always the same probability.
  res2 <- distlab:::calc_result(g, "between", a = 1.2, b = 3.4, lower_closed = TRUE)
  expect_identical(res2$log_p, res$log_p)
  res <- distlab:::calc_result(g, "gt", x = 3)
  expect_equal(exp(res$log_comp), pgamma(3, 2))
  expect_match(distlab:::calc_sentence(res, g, "en-US"), "greater than 3 is 0.1991")
  p <- dl_dist("poisson", lambda = 3)
  res <- distlab:::calc_result(p, "quantile", p = 0.9)
  expect_identical(res$q, qpois(0.9, 3))
  expect_match(distlab:::calc_sentence(res, p, "en-US"), "smallest x with F\\(x\\) ≥ 0.9 is 5")
  res <- distlab:::calc_result(p, "central", prob = 0.9)
  expect_match(distlab:::calc_sentence(res, p, "pt-BR"), "pelo menos 0,9")
  # A value outside the support is flagged; an empty event is exactly 0.
  res <- distlab:::calc_result(g, "le", x = -1)
  expect_true(res$empty)
  expect_true(distlab:::outside_support(g, res$region))
  d <- distlab:::calc_defaults(dl_dist("binomial", size = 10, prob = 0.5))
  expect_identical(d$x, 5)
  expect_lt(d$a, d$b)
  d <- distlab:::calc_defaults(dl_dist("bernoulli", prob = 0.9))
  expect_lt(d$a, d$b)
})

test_that("thousands and decimal marks follow the language", {
  pn <- distlab:::parse_num
  v <- pn("1.234", "pt-BR")
  expect_equal(as.vector(v), 1234)
  expect_true(attr(v, "thousands"))
  v <- pn("1,234", "en-US")
  expect_equal(as.vector(v), 1234)
  expect_true(attr(v, "thousands"))
  expect_equal(as.vector(pn("-1.234.567", "pt-BR")), -1234567)
  expect_null(attr(pn("1.234,5", "pt-BR"), "thousands"))
  # With both marks, the last one must be the decimal mark of the language.
  expect_true(is.na(pn("1,234.5", "pt-BR")))
  expect_true(is.na(pn("1.234,5", "en-US")))
  expect_true(is.na(pn("12.34,5", "pt-BR")))
  # A lone mark that is not a thousands group is a decimal mark.
  expect_identical(pn("0.001", "pt-BR"), 0.001)
  expect_identical(pn("12.34", "pt-BR"), 12.34)
  expect_identical(pn("1,5", "en-US"), 1.5)
  # Overflow is rejected, not read as Inf.
  v <- pn("1e400", "en-US")
  expect_true(is.na(v))
  expect_identical(attr(v, "reason"), "range")
  expect_identical(distlab:::parse_num_list("1.000; 2", "pt-BR"), c(1000, 2))
})

test_that("typed values survive a round trip through the fields", {
  for (lang in dl_languages()) {
    for (x in c(1 / 3, 0.123456789, -1.96, 1234.5678, 1e-7)) {
      txt <- distlab:::format_input(x, lang)
      expect_equal(as.vector(distlab:::parse_num(txt, lang)), x, tolerance = 1e-14,
                   info = paste(lang, txt))
    }
  }
})

test_that("code for an underflowing difference reproduces the shown value", {
  n <- dl_dist("normal")
  res <- distlab:::calc_result(n, "between", a = 40, b = 41)
  code <- distlab:::calc_code(n, res)
  expect_match(code, "log = TRUE) / log(10)", fixed = TRUE)
  expect_identical(exp(res$log_p), 0)
  expect_equal(eval_code(code), res$log_p / log(10))
  expect_identical(distlab:::format_prob(eval_code(code) * log(10), "en-US"),
                   distlab:::format_prob(res$log_p, "en-US"))
})

test_that("an empty complement is shown as 0", {
  b <- dl_dist("beta", shape1 = 2, shape2 = 3)
  res <- distlab:::calc_result(b, "le", x = 1)
  expect_true(res$comp_empty)
  expect_identical(distlab:::format_prob(res$log_comp, "en-US", empty = res$comp_empty), "0")
  res <- distlab:::calc_result(dl_dist("binomial", size = 5, prob = 0.5), "between",
                               a = 0, b = 5, lower_closed = TRUE)
  expect_true(res$comp_empty)
  res <- distlab:::calc_result(dl_dist("normal"), "gt", x = 40)
  expect_false(res$comp_empty)
})
