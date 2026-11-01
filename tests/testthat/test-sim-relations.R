# Simulation demos of the relationship map: one recipe per registry edge.

all_edges <- function() {
  rel <- dl_relations()
  rel$id <- distlab:::edge_ids(rel)
  rel
}

test_that("every relationship edge has exactly one simulation recipe", {
  rel <- all_edges()
  recipes <- names(distlab:::.dl_sim_recipes)
  expect_setequal(recipes, rel$id)
  for (i in seq_len(nrow(rel))) {
    r <- distlab:::.dl_sim_recipes[[rel$id[i]]]
    kind <- switch(rel$type[i], special_case = "draws", transformation = "draws",
                   sum = "sum", limit = "limit")
    expect_identical(r$kind, kind, info = rel$id[i])
    # The parameters given to the Explorer are valid for each end.
    expect_s3_class(do.call(dl_dist, c(list(rel$from[i]), r$from)), "dl_dist")
    expect_s3_class(do.call(dl_dist, c(list(rel$to[i]), r$to)), "dl_dist")
  }
})

test_that("every demo agrees with its target and its code reproduces it", {
  rel <- all_edges()
  for (i in seq_len(nrow(rel))) {
    id <- rel$id[i]
    d <- dl_relation_demo(rel$from[i], rel$to[i], rel$type[i], n = 3000, seed = 50 + i)
    expect_identical(d$edge$id, id)
    expect_named(d, c("edge", "kind", "frames", "samples", "curves", "distance", "range",
                      "discrete", "params", "seed", "code"))
    nf <- nrow(d$frames)
    expect_identical(d$frames$frame, seq_len(nf), info = id)
    expect_identical(d$distance$frame, seq_len(nf), info = id)
    expect_true(all(is.finite(d$distance$value) & d$distance$value >= 0), info = id)
    expect_setequal(unique(d$curves$frame), seq_len(nf))
    expect_true(all(is.finite(d$range)) && d$range[2] > d$range[1], info = id)
    env <- run_sim_code(d$code)
    metric <- d$distance$metric[1]
    expect_identical(unname(env[[metric]]), d$distance$value, info = id)
    if (d$kind == "limit") {
      expect_null(d$samples)
      expect_identical(d$seed, NA_integer_)
      # The approximation improves along the frames.
      v <- d$distance$value
      expect_true(all(diff(v) < 0), info = paste(id, paste(signif(v, 3), collapse = " ")))
      expect_lt(v[nf], v[1] / 5)
      expect_setequal(unique(d$curves$series), c("approx", "target"))
      next
    }
    expect_identical(d$seed, as.integer(50 + i))
    last <- d$samples$value[d$samples$frame == nf]
    if (d$kind == "draws") {
      expect_identical(env$x, d$samples$value[d$samples$frame == nf], info = id)
      expect_identical(d$frames$value, c(100, 300, 1000, 3000))
      target <- do.call(dl_dist, distlab:::.dl_sim_recipes[[id]]$target)
    } else {
      expect_identical(env$sums[[nf]], last, info = id)
      target <- distlab:::.dl_sim_recipes[[id]]$target(20)
    }
    expect_identical(target$id, rel$to[i])
    expect_sample_law(last, target, info = id)
    if (metric == "ks") {
      expect_equal(d$distance$band, 1.36 / sqrt(if (d$kind == "sum") rep(3000, nf) else d$frames$value))
    }
  }
})

test_that("sum recipes write the same law as their targets", {
  for (id in c("bernoulli__binomial__sum", "exponential__gamma__sum", "geometric__negbinomial__sum")) {
    r <- distlab:::.dl_sim_recipes[[id]]
    f <- eval(parse(text = paste("function(k, q, j)", r$law)))
    for (j in distlab:::.dl_sim_terms) {
      tg <- r$target(j)
      x <- if (tg$entry$type == "discrete") 0:30 else c(0.1, 1, 4, 12)
      val <- if (tg$entry$type == "discrete") f(k = x, j = j) else f(q = x, j = j)
      ref <- if (tg$entry$type == "discrete") dl_density(tg, x) else dl_cdf(tg, x)
      expect_identical(val, ref, info = paste(id, j))
    }
  }
})

test_that("heavy tails are plotted on a quantile range and report draws outside", {
  d <- dl_relation_demo("normal", "cauchy", n = 5000, seed = 1)
  expect_equal(d$range, c(-30, 30))
  expect_true(all(d$frames$outside > 0 & d$frames$outside < 0.05))
  expect_identical(d$frames$label[5], "n = 5000")
  f <- dl_relation_demo("chisq", "f", n = 2000, seed = 1)
  expect_identical(f$range[1], 0)
  expect_equal(f$range[2], qf(0.995, 5, 10))
  u <- dl_relation_demo("beta", "uniform", n = 500, seed = 1)
  expect_identical(u$range, c(0, 1))
  expect_true(all(u$frames$outside == 0))
  expect_identical(u$frames$value, c(100, 300, 500))
})

test_that("limit demos compare the right laws", {
  bp <- dl_relation_demo("binomial", "poisson")
  first <- bp$curves[bp$curves$frame == 1, ]
  expect_equal(first$density[first$series == "approx"], dbinom(0:12, 5, 0.6))
  expect_equal(first$density[first$series == "target"], dpois(0:12, 3))
  expect_identical(bp$frames$label[1], "n = 5")
  # Standardized bars have area 1 (up to the mass beyond 4.5 sd).
  bn <- dl_relation_demo("binomial", "normal")
  bars <- bn$curves[bn$curves$frame == 6 & bn$curves$series == "approx", ]
  expect_equal(sum(bars$density) / sqrt(1000 * 0.3 * 0.7), 1, tolerance = 1e-4)
  tn <- dl_relation_demo("student_t", "normal")
  expect_equal(tn$distance$value[1], max(pnorm(seq(0, 6, by = 1e-4)) - pt(seq(0, 6, by = 1e-4), 1)),
               tolerance = 1e-6)
  expect_identical(tn$distance$metric[1], "kolmogorov")
  hb <- dl_relation_demo("hypergeometric", "binomial")
  expect_identical(hb$frames$label[6], "N = 10000")
})

test_that("dl_relation_demo checks its arguments", {
  expect_error(dl_relation_demo("normal", "gamma"), "No relationship")
  expect_error(dl_relation_demo("normal", "cauchy", type = "sum"), "of type 'sum'")
  expect_error(dl_relation_demo(1, "cauchy"), "distribution ids")
  expect_error(dl_relation_demo("normal", "cauchy", n = 5), "`n`")
  expect_error(dl_relation_demo("normal", "cauchy", seed = 0.5), "`seed`")
  expect_error(distlab:::relation_demo_run("nope__nope__sum"), "No simulation recipe")
  # The seed restores the caller's stream; no seed gives NA.
  set.seed(3)
  before <- .Random.seed
  dl_relation_demo("normal", "chisq", n = 100, seed = 9)
  expect_identical(.Random.seed, before)
  expect_identical(dl_relation_demo("normal", "chisq", n = 100)$seed, NA_integer_)
})
