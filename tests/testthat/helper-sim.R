# Helpers for the simulation tests.

# Evaluate generated code in a fresh environment; returns the environment
# with the value of the last expression as `.value`.
run_sim_code <- function(code) {
  env <- new.env(parent = globalenv())
  env$.value <- eval(parse(text = code), envir = env)
  env
}

# A sample agrees with a law: Kolmogorov-Smirnov for continuous laws and a
# chi-square test on cells with expected count >= 5 (tails merged) for
# discrete ones, at the 0.1% level.
expect_sample_law <- function(x, dist, info = NULL) {
  if (identical(dist$entry$type, "discrete")) {
    s <- distlab:::dl_support(dist)
    lo <- max(s$lower, dl_quantile(dist, 1e-6))
    hi <- min(s$upper, dl_quantile(dist, 1e-6, lower.tail = FALSE))
    k <- seq(lo, hi)
    p <- dl_density(dist, k)
    p[1] <- dl_cdf(dist, lo)
    p[length(p)] <- dl_cdf(dist, hi - 1, lower.tail = FALSE)
    obs <- tabulate(pmin(pmax(x, lo), hi) - lo + 1, nbins = length(k))
    # Merge neighboring cells until each expects at least 5 draws.
    e <- p * length(x)
    grp <- integer(length(e))
    acc <- 0
    g <- 1L
    for (i in seq_along(e)) {
      grp[i] <- g
      acc <- acc + e[i]
      if (acc >= 5 && i < length(e)) {
        g <- g + 1L
        acc <- 0
      }
    }
    # A last group short of 5 joins the one before it.
    if (sum(e[grp == g]) < 5 && g > 1L) grp[grp == g] <- g - 1L
    o <- tapply(obs, grp, sum)
    pe <- tapply(p, grp, sum)
    if (length(o) < 2L) return(invisible(TRUE))
    stat <- suppressWarnings(stats::chisq.test(o, p = pe / sum(pe)))
    expect_gt(stat$p.value, 1e-3, label = paste("chi-square p-value", info))
  } else {
    stat <- suppressWarnings(stats::ks.test(x, function(q) dl_cdf(dist, q)))
    expect_gt(stat$p.value, 1e-3, label = paste("KS p-value", info))
  }
}

# Run `code` with the global random number state removed and restored after.
without_random_seed <- function(code) {
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE)
  if (had) rm(".Random.seed", envir = genv)
  on.exit(if (had) assign(".Random.seed", old, envir = genv))
  code
}
