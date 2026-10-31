test_that("every registry entry loads and passes validation", {
  reg <- dl_registry(refresh = TRUE)
  expect_true(length(reg) >= 3)
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
  expect_identical(dl_ids(group = "positive"), "gamma")
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
