test_that("constraints are parsed into lhs < rhs form", {
  con <- distlab:::parse_constraint("k <= m + n")
  expect_false(con$strict)
  expect_identical(all.vars(con$rhs), c("m", "n"))
  flipped <- distlab:::parse_constraint("max > min")
  expect_true(flipped$strict)
  expect_identical(as.character(flipped$lhs), "min")
  expect_null(distlab:::parse_constraint("min != max"))
})

test_that("resolving moves the parameter the user did not change", {
  u <- dl_entry("uniform")
  expect_identical(distlab:::resolve_constraints(u, list(min = 3, max = 1),
                                                 list(min = 0, max = 1)),
                   list(min = 3, max = 3.1))
  expect_identical(distlab:::resolve_constraints(u, list(min = 0, max = -1),
                                                 list(min = 0, max = 1)),
                   list(min = -1.1, max = -1))
  # When the other slider cannot move far enough, both stop at its end.
  expect_identical(distlab:::resolve_constraints(u, list(min = 10, max = 5),
                                                 list(min = 0, max = 5)),
                   list(min = 9.9, max = 10))
  expect_identical(distlab:::resolve_constraints(u, list(min = 0, max = -10),
                                                 list(min = 0, max = 5)),
                   list(min = -10, max = -9.9))
  # Without previous values the first fix that fits is used.
  expect_identical(distlab:::resolve_constraints(u, list(min = 3, max = 1)),
                   list(min = 3, max = 3.1))
  d <- dl_entry("discrete_uniform")
  expect_identical(distlab:::resolve_constraints(d, list(min = 6, max = 6),
                                                 list(min = 1, max = 6)),
                   list(min = 6, max = 7))
})

test_that("slider values compare with a tolerance, not by type", {
  expect_true(distlab:::same_value(10L, 10))
  expect_true(distlab:::same_value(0.1 + 0.2, 0.3))
  expect_false(distlab:::same_value(1, 1.1))
  expect_false(distlab:::same_value(NULL, 1))
  expect_true(distlab:::same_value(c(1, 5), c(1L, 5L)))
  # An integer from the client is not taken as a change.
  h <- dl_entry("hypergeometric")
  expect_identical(distlab:::resolve_constraints(h, list(m = 2L, n = 3L, k = 9L),
                                                 list(m = 2, n = 3, k = 5))$k, 5)
})

test_that("valid values pass through unchanged", {
  vals <- list(min = 0, max = 1)
  expect_identical(distlab:::resolve_constraints(dl_entry("uniform"), vals), vals)
  g <- list(shape = 2, rate = 1)
  expect_identical(distlab:::resolve_constraints(dl_entry("gamma"), g), g)
})

test_that("k is clamped to m + n and its slider range follows", {
  h <- dl_entry("hypergeometric")
  res <- distlab:::resolve_constraints(h, list(m = 2, n = 3, k = 8),
                                       list(m = 10, n = 3, k = 8))
  expect_identical(res$k, 5)
  lim <- distlab:::slider_limits(h, list(m = 2, n = 3, k = 5))
  expect_identical(lim$k, c(1, 5))
  expect_identical(lim$m, c(1L, 50L))
  wide <- distlab:::slider_limits(h, list(m = 40, n = 30, k = 5))
  expect_equal(wide$k[2], 50)
})

test_that("resolved defaults satisfy every registry constraint", {
  for (did in dl_ids()) {
    e <- dl_entry(did)
    vals <- stats::setNames(lapply(e$parameters, `[[`, "default"),
                            vapply(e$parameters, `[[`, "", "name"))
    res <- distlab:::resolve_constraints(e, vals)
    expect_s3_class(do.call(dl_dist, c(list(did), res)), "dl_dist")
  }
})

test_that("valid fitted parameters extend initial slider windows", {
  lim <- distlab:::slider_limits(dl_entry("normal"), list(mean = 70.8970588235, sd = 13.56996))
  expect_equal(lim$mean, c(-10, 70.8970588235))
  expect_equal(lim$sd, c(.1, 13.56996))
  small <- distlab:::slider_limits(dl_entry("gamma"), list(shape = 2, rate = .001))
  expect_equal(small$rate[1], .001)
  # Coupled constraints still override an invalid structural value.
  h <- distlab:::slider_limits(dl_entry("hypergeometric"), list(m = 100, n = 100, k = 150))
  expect_equal(h$k[2], 150)
})
