test_that("every registry entry loads and passes validation", {
  reg <- dl_registry(refresh = TRUE)
  expect_length(reg, 21)
  for (e in reg) expect_identical(distlab:::validate_entry(e), character())
})

test_that("validator reports broken entries", {
  e <- dl_registry()$gamma
  bad <- e
  bad$parameters[[1]]$default <- -1
  expect_match(distlab:::validate_entry(bad), "default of shape", all = FALSE)
  bad <- e
  bad$functions$d <- "stats::not_a_function"
  expect_match(distlab:::validate_entry(bad), "function 'd'", all = FALSE)
  bad <- e
  bad$story[["pt-BR"]] <- NULL
  expect_match(distlab:::validate_entry(bad), "story", all = FALSE)
})

test_that("dl_ids filters by type and group", {
  expect_true("poisson" %in% dl_ids(type = "discrete"))
  expect_false("poisson" %in% dl_ids(type = "continuous"))
  expect_setequal(dl_ids(group = "positive"),
                  c("exponential", "gamma", "chisq", "weibull", "lognormal", "f"))
  expect_identical(dl_ids(group = "binary"), "bernoulli")
})

test_that("in_domain respects open ends and integer flags", {
  par <- list(lower = 0, lower_open = TRUE, upper = Inf)
  expect_false(distlab:::in_domain(0, par))
  expect_true(distlab:::in_domain(1e-12, par))
  expect_false(distlab:::in_domain(NA_real_, par))
  expect_false(distlab:::in_domain(c(1, 2), par))
  expect_false(distlab:::in_domain(2.5, list(lower = 0, upper = 10, integer = TRUE)))
})

test_that("non-finite parameter values are rejected", {
  for (v in c(Inf, -Inf, NaN)) {
    expect_error(dl_dist("normal", mean = v), "outside its domain")
  }
  expect_error(dl_dist("normal", sd = Inf), "outside its domain")
  expect_error(dl_dist("gamma", shape = Inf), "outside its domain")
})

test_that("validator reports malformed fields instead of failing", {
  e <- dl_registry()$gamma
  bad <- e; bad$type <- NULL
  expect_match(distlab:::validate_entry(bad), "missing field: type", all = FALSE)
  bad <- e; bad$group <- c("counts", "positive")
  expect_match(distlab:::validate_entry(bad), "unknown group", all = FALSE)
  bad <- e; bad$tier <- "one"
  expect_match(distlab:::validate_entry(bad), "tier", all = FALSE)
  bad <- e; bad$functions$q <- 42
  expect_match(distlab:::validate_entry(bad), "function 'q'", all = FALSE)
  bad <- e; bad$parameters[[2]]$name <- "rte"; bad$parameters[[2]]$latex <- "\\beta"
  expect_match(distlab:::validate_entry(bad), "no argument", all = FALSE)
  bad <- e; bad$parameters[[2]]$name <- "shape"
  expect_match(distlab:::validate_entry(bad), "duplicated", all = FALSE)
  bad <- e; bad$parameters[[1]]$slider$step <- 0
  expect_match(distlab:::validate_entry(bad), "slider of shape", all = FALSE)
  bad <- e; bad$parameters[[1]]$slider$min <- 0
  expect_match(distlab:::validate_entry(bad), "leaves the domain", all = FALSE)
})

test_that("a file whose id differs from its name is refused", {
  path <- file.path(tempfile(), "other.yml")
  dir.create(dirname(path))
  on.exit(unlink(dirname(path), recursive = TRUE))
  yaml::write_yaml(dl_registry()$poisson, path)
  expect_error(distlab:::read_registry_entry(path), "does not match file name")
})

test_that("the Tier-1 catalog is complete and classified", {
  discrete <- c("bernoulli", "binomial", "geometric", "negbinomial", "poisson",
                "hypergeometric", "betabinomial", "discrete_uniform")
  continuous <- c("uniform", "beta", "exponential", "gamma", "chisq", "weibull",
                  "lognormal", "f", "normal", "student_t", "cauchy", "laplace",
                  "logistic")
  expect_setequal(dl_ids(type = "discrete"), discrete)
  expect_setequal(dl_ids(type = "continuous"), continuous)
  expect_setequal(dl_ids(group = "bounded"), c("uniform", "beta"))
  expect_setequal(dl_ids(group = "real_line"),
                  c("normal", "student_t", "cauchy", "laplace", "logistic"))
  for (e in dl_registry()) expect_identical(e$tier, 1L, info = e$id)
})

test_that("integer parameters have whole-number defaults and sliders", {
  for (e in dl_registry()) {
    for (par in e$parameters) {
      if (!isTRUE(par$integer)) next
      vals <- c(par$default, par$slider$min, par$slider$max, par$slider$step)
      expect_true(all(vals == round(vals)), info = paste(e$id, par$name))
    }
  }
  bad <- dl_registry()$binomial
  bad$parameters[[1]]$slider$step <- 0.5
  expect_match(distlab:::validate_entry(bad), "whole min, max and step", all = FALSE)
})

test_that("support bounds may be expressions of the parameters", {
  e <- dl_registry()$hypergeometric
  expect_identical(distlab:::validate_entry(e), character())
  bad <- e; bad$support$upper <- "min(k, size)"
  expect_match(distlab:::validate_entry(bad), "support upper", all = FALSE)
  bad <- e; bad$support$lower <- "system('ls')"
  expect_match(distlab:::validate_entry(bad), "support lower", all = FALSE)
  bad <- e; bad$support$lower <- "min(k, m)"; bad$support$upper <- "max(0, k - n)"
  expect_match(distlab:::validate_entry(bad), "lower < upper", all = FALSE)
  s <- distlab:::dl_support(dl_dist("hypergeometric", m = 4, n = 3, k = 5))
  expect_identical(c(s$lower, s$upper), c(2, 4))
  s <- distlab:::dl_support(dl_dist("uniform", min = -2, max = 3))
  expect_identical(c(s$lower, s$upper), c(-2, 3))
})

test_that("constraints are validated and enforced", {
  e <- dl_registry()$uniform
  bad <- e; bad$constraints <- list("min < max + q")
  expect_match(distlab:::validate_entry(bad), "not a valid expression", all = FALSE)
  bad <- e; bad$constraints <- list("min > max")
  expect_match(distlab:::validate_entry(bad), "fails at the default", all = FALSE)
  expect_error(dl_dist("uniform", min = 2, max = 1), "must satisfy min < max")
  expect_error(dl_dist("discrete_uniform", min = 3, max = 3), "must satisfy")
  expect_error(dl_dist("hypergeometric", m = 2, n = 2, k = 5), "k <= m \\+ n")
})

test_that("relationships are well formed and point to registry ids", {
  reg <- dl_registry()
  types <- character()
  for (e in reg) {
    for (rel in e$relationships) {
      expect_true(rel$to %in% names(reg), info = paste(e$id, rel$to))
      types <- c(types, rel$type)
    }
  }
  expect_true(all(c("special_case", "transformation", "limit", "sum") %in% types))
  bad <- reg$gamma
  bad$relationships[[1]]$type <- "cousin"
  expect_match(distlab:::validate_entry(bad), "unknown type", all = FALSE)
  bad <- reg$gamma
  bad$relationships[[1]]$condition[["pt-BR"]] <- NULL
  expect_match(distlab:::validate_entry(bad), "condition text", all = FALSE)
  broken <- reg["gamma"]
  broken$gamma$relationships[[1]]$to <- "erlang"
  expect_match(distlab:::validate_relationship_targets(broken), "'erlang'",
               all = FALSE)
  expect_identical(distlab:::validate_relationship_targets(reg), character())
})

test_that("internal helpers resolve as registry functions", {
  expect_true(is.function(distlab:::resolve_fun("distlab::qbbinom")))
  expect_null(distlab:::resolve_fun("distlab::no_such_helper", error = FALSE))
  expect_error(distlab:::resolve_fun("distlab::no_such_helper"), "Cannot find")
})

test_that("validator checks the formula specs under latex", {
  e <- dl_registry()$gamma
  v <- function(x) distlab:::validate_entry(x)
  bad <- e; bad$latex$mean <- NULL
  expect_match(v(bad), "missing latex key: mean", all = FALSE)
  bad <- e; bad$latex[["kurtosis"]] <- list(expr = "x")
  expect_match(v(bad), "unknown latex key: kurtosis", all = FALSE)
  bad <- e; bad$latex$pgf <- list(expr = "z")
  expect_match(v(bad), "only for discrete", all = FALSE)
  pois <- dl_registry()$poisson
  pois$latex$pgf <- NULL
  expect_match(v(pois), "missing latex key: pgf", all = FALSE)
  bad <- e; bad$latex$mean <- list(expr = "a", status = "infinite")
  expect_match(v(bad), "exactly one of expr and status", all = FALSE)
  bad <- e; bad$latex$mean <- list(condition = "\\alpha > 1")
  expect_match(v(bad), "exactly one of expr and status", all = FALSE)
  bad <- e; bad$latex$mean <- list(status = "huge")
  expect_match(v(bad), "status must be one of", all = FALSE)
  bad <- e; bad$latex$mean <- list(expr = 3)
  expect_match(v(bad), "expr must be a string or null", all = FALSE)
  bad <- e; bad$latex$mean <- list(expr = "a", condition = 1)
  expect_match(v(bad), "condition must be a string", all = FALSE)
  bad <- e; bad$latex$mean <- list(expr = "a", note = "b")
  expect_match(v(bad), "unknown field\\(s\\): note", all = FALSE)
  bad <- e; bad$latex$mean <- "\\frac{\\alpha}{\\beta}"
  expect_match(v(bad), "latex mean must be a formula", all = FALSE)
  bad <- e; bad$latex$mean <- list()
  expect_match(v(bad), "latex mean must be a formula", all = FALSE)
  bad <- e; bad$latex$mode <- list(list(expr = "0"), list(expr = "1", condition = "x"))
  expect_match(v(bad), "latex mode\\[1\\] needs a condition", all = FALSE)
  bad <- e; bad$latex$mode <- list(list(expr = "0", condition = "x"), "1")
  expect_match(v(bad), "latex mode must be a formula", all = FALSE)
  bad <- e; bad$latex$cdf <- NULL
  expect_match(v(bad), "latex needs pdf and cdf strings", all = FALSE)
  # A null expr (no closed form) and status pieces with conditions are valid.
  ok <- e
  ok$latex$median <- list(expr = NULL)
  ok$latex$mean <- list(list(expr = "a", condition = "x > 1"),
                        list(status = "infinite", condition = "x \\le 1"))
  expect_identical(v(ok), character())
})
