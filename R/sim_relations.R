# One simulation demo per relationship edge of the registry, keyed by edge
# id ("from__to__type", see edge_ids()). Three kinds:
# * "draws": simulate the construction on the "from" side (the R source in
#   `draw` leaves the draws in `x`) and compare it with the exact `target`;
# * "sum": add 1, 2, 3, ... independent terms (the columns of the matrix `x`
#   made by `draw`); `law` is the R source of the target CDF or mass function
#   with `j` terms, and `target(j)` the same law as a dl_dist;
# * "limit": no draws; `code` computes the distance between the
#   approximating law and the limit law along `values`, and `curves(v)` gives
#   both laws for plotting.
# `params` describes the recipe in R notation; `from` and `to` are parameter
# values that open each end in the Explorer.

# Cumulative numbers of draws shown as frames of a "draws" demo.
.dl_sim_frames <- c(100, 300, 1000, 3000, 10000, 30000, 1e5)

# Number of terms in the frames of a "sum" demo.
.dl_sim_terms <- c(1, 2, 3, 5, 10, 20)

.dl_sim_recipes <- list(
  bernoulli__binomial__sum = list(
    kind = "sum", params = "prob = 0.3",
    draw = "x <- matrix(rbinom(n * 20, size = 1, prob = 0.3), nrow = n)",
    law = "dbinom(k, size = j, prob = 0.3)",
    target = function(j) dl_dist("binomial", size = j, prob = 0.3),
    from = list(prob = 0.3), to = list(size = 20, prob = 0.3)),
  beta__uniform__special_case = list(
    kind = "draws", params = "shape1 = 1, shape2 = 1",
    draw = "x <- rbeta(n, shape1 = 1, shape2 = 1)",
    target = list("uniform", min = 0, max = 1),
    from = list(shape1 = 1, shape2 = 1), to = list(min = 0, max = 1)),
  betabinomial__binomial__limit = list(
    kind = "limit", metric = "tv", params = "size = 10, prob = 0.3, alpha = c * prob, beta = c * (1 - prob)",
    symbol = "\u03b1 + \u03b2", values = c(1, 3, 10, 30, 100, 1000),
    code = c(
      "size <- 10",
      "prob <- 0.3",
      "conc <- {values}",
      "tv <- sapply(conc, function(cc) {",
      "  k <- 0:size",
      "  0.5 * sum(abs(extraDistr::dbbinom(k, size = size, alpha = cc * prob, beta = cc * (1 - prob)) -",
      "    dbinom(k, size = size, prob = prob)))",
      "})"),
    curves = function(v) {
      limit_pmf_curves(dl_dist("betabinomial", size = 10, alpha = v * 0.3, beta = v * (1 - 0.3)),
                       dl_dist("binomial", size = 10, prob = 0.3), 0:10)
    },
    from = list(size = 10, alpha = 3, beta = 7), to = list(size = 10, prob = 0.3)),
  betabinomial__discrete_uniform__special_case = list(
    kind = "draws", params = "size = 10, alpha = 1, beta = 1",
    draw = "x <- extraDistr::rbbinom(n, size = 10, alpha = 1, beta = 1)",
    target = list("discrete_uniform", min = 0, max = 10),
    from = list(size = 10, alpha = 1, beta = 1), to = list(min = 0, max = 10)),
  binomial__bernoulli__special_case = list(
    kind = "draws", params = "size = 1, prob = 0.3",
    draw = "x <- rbinom(n, size = 1, prob = 0.3)",
    target = list("bernoulli", prob = 0.3),
    from = list(size = 1, prob = 0.3), to = list(prob = 0.3)),
  binomial__poisson__limit = list(
    kind = "limit", metric = "tv", params = "lambda = 3, prob = lambda / size",
    symbol = "n", values = c(5, 10, 20, 50, 100, 500),
    code = c(
      "lambda <- 3",
      "size <- {values}",
      "tv <- sapply(size, function(m) {",
      "  k <- 0:m",
      "  0.5 * (sum(abs(dbinom(k, size = m, prob = lambda / m) - dpois(k, lambda = lambda))) +",
      "    ppois(m, lambda = lambda, lower.tail = FALSE))",
      "})"),
    curves = function(v) {
      limit_pmf_curves(dl_dist("binomial", size = v, prob = 3 / v),
                       dl_dist("poisson", lambda = 3), 0:12)
    },
    from = list(size = 50, prob = 0.06), to = list(lambda = 3)),
  binomial__normal__limit = list(
    kind = "limit", metric = "kolmogorov", params = "prob = 0.3",
    symbol = "n", values = c(5, 10, 30, 100, 300, 1000),
    code = c(
      "prob <- 0.3",
      "size <- {values}",
      "kolmogorov <- sapply(size, function(m) {",
      "  k <- 0:m",
      "  z <- (k - m * prob) / sqrt(m * prob * (1 - prob))",
      "  cdf <- pbinom(k, size = m, prob = prob)",
      "  max(abs(cdf - pnorm(z)), abs(c(0, cdf[-length(cdf)]) - pnorm(z)))",
      "})"),
    curves = function(v) {
      limit_std_curves(dl_dist("binomial", size = v, prob = 0.3), 0:v)
    },
    from = list(size = 30, prob = 0.3), to = list(mean = 0, sd = 1)),
  chisq__f__transformation = list(
    kind = "draws", params = "df1 = 5, df2 = 10",
    draw = c("u <- rchisq(n, df = 5)",
             "v <- rchisq(n, df = 10)",
             "x <- (u / 5) / (v / 10)"),
    target = list("f", df1 = 5, df2 = 10),
    from = list(df = 5), to = list(df1 = 5, df2 = 10)),
  exponential__gamma__sum = list(
    kind = "sum", params = "rate = 1",
    draw = "x <- matrix(rexp(n * 20, rate = 1), nrow = n)",
    law = "pgamma(q, shape = j, rate = 1)",
    target = function(j) dl_dist("gamma", shape = j, rate = 1),
    from = list(rate = 1), to = list(shape = 20, rate = 1)),
  exponential__laplace__transformation = list(
    kind = "draws", params = "rate = 1",
    draw = c("x1 <- rexp(n, rate = 1)",
             "x2 <- rexp(n, rate = 1)",
             "x <- x1 - x2"),
    target = list("laplace", mu = 0, sigma = 1),
    from = list(rate = 1), to = list(mu = 0, sigma = 1)),
  gamma__exponential__special_case = list(
    kind = "draws", params = "shape = 1, rate = 2",
    draw = "x <- rgamma(n, shape = 1, rate = 2)",
    target = list("exponential", rate = 2),
    from = list(shape = 1, rate = 2), to = list(rate = 2)),
  gamma__chisq__special_case = list(
    kind = "draws", params = "shape = 2, rate = 0.5",
    draw = "x <- rgamma(n, shape = 2, rate = 0.5)",
    target = list("chisq", df = 4),
    from = list(shape = 2, rate = 0.5), to = list(df = 4)),
  gamma__beta__transformation = list(
    kind = "draws", params = "shape = 2, rate = 1; shape = 3, rate = 1",
    draw = c("g1 <- rgamma(n, shape = 2, rate = 1)",
             "g2 <- rgamma(n, shape = 3, rate = 1)",
             "x <- g1 / (g1 + g2)"),
    target = list("beta", shape1 = 2, shape2 = 3),
    from = list(shape = 2, rate = 1), to = list(shape1 = 2, shape2 = 3)),
  geometric__negbinomial__sum = list(
    kind = "sum", params = "prob = 0.3",
    draw = "x <- matrix(rgeom(n * 20, prob = 0.3), nrow = n)",
    law = "dnbinom(k, size = j, prob = 0.3)",
    target = function(j) dl_dist("negbinomial", size = j, prob = 0.3),
    from = list(prob = 0.3), to = list(size = 20, prob = 0.3)),
  hypergeometric__binomial__limit = list(
    kind = "limit", metric = "tv", params = "k = 10, m = round(0.3 * N), n = N - m",
    symbol = "N", values = c(20, 50, 100, 500, 1000, 10000),
    code = c(
      "draws <- 10",
      "prob <- 0.3",
      "total <- {values}",
      "tv <- sapply(total, function(N) {",
      "  m <- round(N * prob)",
      "  x <- 0:draws",
      "  0.5 * sum(abs(dhyper(x, m = m, n = N - m, k = draws) -",
      "    dbinom(x, size = draws, prob = prob)))",
      "})"),
    curves = function(v) {
      m <- round(v * 0.3)
      limit_pmf_curves(dl_dist("hypergeometric", m = m, n = v - m, k = 10),
                       dl_dist("binomial", size = 10, prob = 0.3), 0:10)
    },
    from = list(m = 6, n = 14, k = 10), to = list(size = 10, prob = 0.3)),
  lognormal__normal__transformation = list(
    kind = "draws", params = "meanlog = 0, sdlog = 0.5",
    draw = "x <- log(rlnorm(n, meanlog = 0, sdlog = 0.5))",
    target = list("normal", mean = 0, sd = 0.5),
    from = list(meanlog = 0, sdlog = 0.5), to = list(mean = 0, sd = 0.5)),
  negbinomial__geometric__special_case = list(
    kind = "draws", params = "size = 1, prob = 0.3",
    draw = "x <- rnbinom(n, size = 1, prob = 0.3)",
    target = list("geometric", prob = 0.3),
    from = list(size = 1, prob = 0.3), to = list(prob = 0.3)),
  normal__normal__transformation = list(
    kind = "draws", params = "mean = 10, sd = 2",
    draw = "x <- (rnorm(n, mean = 10, sd = 2) - 10) / 2",
    target = list("normal", mean = 0, sd = 1),
    from = list(mean = 10, sd = 2), to = list(mean = 0, sd = 1)),
  normal__lognormal__transformation = list(
    kind = "draws", params = "mean = 0, sd = 0.5",
    draw = "x <- exp(rnorm(n, mean = 0, sd = 0.5))",
    target = list("lognormal", meanlog = 0, sdlog = 0.5),
    from = list(mean = 0, sd = 0.5), to = list(meanlog = 0, sdlog = 0.5)),
  normal__chisq__transformation = list(
    kind = "draws", params = "mean = 0, sd = 1",
    draw = "x <- rnorm(n, mean = 0, sd = 1)^2",
    target = list("chisq", df = 1),
    from = list(mean = 0, sd = 1), to = list(df = 1)),
  normal__cauchy__transformation = list(
    kind = "draws", params = "mean = 0, sd = 1",
    draw = c("z1 <- rnorm(n, mean = 0, sd = 1)",
             "z2 <- rnorm(n, mean = 0, sd = 1)",
             "x <- z1 / z2"),
    target = list("cauchy", location = 0, scale = 1),
    from = list(mean = 0, sd = 1), to = list(location = 0, scale = 1)),
  poisson__normal__limit = list(
    kind = "limit", metric = "kolmogorov", params = "lambda",
    symbol = "\u03bb", values = c(1, 3, 10, 30, 100, 1000),
    code = c(
      "lambda <- {values}",
      "kolmogorov <- sapply(lambda, function(l) {",
      "  k <- 0:qpois(1e-12, lambda = l, lower.tail = FALSE)",
      "  z <- (k - l) / sqrt(l)",
      "  cdf <- ppois(k, lambda = l)",
      "  max(abs(cdf - pnorm(z)), abs(c(0, cdf[-length(cdf)]) - pnorm(z)))",
      "})"),
    curves = function(v) {
      limit_std_curves(dl_dist("poisson", lambda = v),
                       0:stats::qpois(1e-12, lambda = v, lower.tail = FALSE))
    },
    from = list(lambda = 30), to = list(mean = 0, sd = 1)),
  student_t__cauchy__special_case = list(
    kind = "draws", params = "df = 1",
    draw = "x <- rt(n, df = 1)",
    target = list("cauchy", location = 0, scale = 1),
    from = list(df = 1), to = list(location = 0, scale = 1)),
  student_t__normal__limit = list(
    kind = "limit", metric = "kolmogorov", params = "df",
    symbol = "\u03bd", values = c(1, 2, 5, 10, 30, 100),
    code = c(
      "df <- {values}",
      "kolmogorov <- sapply(df, function(v) {",
      "  optimize(function(x) pnorm(x) - pt(x, df = v), c(0, 6), maximum = TRUE)$objective",
      "})"),
    curves = function(v) {
      x <- seq(-5, 5, length.out = 201)
      rbind(data.frame(series = "approx", x = x, density = stats::dt(x, df = v),
                       cdf = stats::pt(x, df = v)),
            data.frame(series = "target", x = x, density = stats::dnorm(x),
                       cdf = stats::pnorm(x)))
    },
    from = list(df = 5), to = list(mean = 0, sd = 1)),
  student_t__f__transformation = list(
    kind = "draws", params = "df = 5",
    draw = "x <- rt(n, df = 5)^2",
    target = list("f", df1 = 1, df2 = 5),
    from = list(df = 5), to = list(df1 = 1, df2 = 5)),
  uniform__exponential__transformation = list(
    kind = "draws", params = "min = 0, max = 1; rate = 2",
    draw = "x <- -log(runif(n, min = 0, max = 1)) / 2",
    target = list("exponential", rate = 2),
    from = list(min = 0, max = 1), to = list(rate = 2)),
  uniform__logistic__transformation = list(
    kind = "draws", params = "min = 0, max = 1",
    draw = c("u <- runif(n, min = 0, max = 1)",
             "x <- log(u / (1 - u))"),
    target = list("logistic", location = 0, scale = 1),
    from = list(min = 0, max = 1), to = list(location = 0, scale = 1)),
  weibull__exponential__special_case = list(
    kind = "draws", params = "shape = 1, scale = 2",
    draw = "x <- rweibull(n, shape = 1, scale = 2)",
    target = list("exponential", rate = 0.5),
    from = list(shape = 1, scale = 2), to = list(rate = 0.5))
)

# Mass functions of an approximating and a limit law on the whole numbers x.
limit_pmf_curves <- function(approx, target, x) {
  rbind(data.frame(series = "approx", x = x, density = dl_density(approx, x),
                   cdf = dl_cdf(approx, x)),
        data.frame(series = "target", x = x, density = dl_density(target, x),
                   cdf = dl_cdf(target, x)))
}

# A discrete law standardized to mean 0 and sd 1 against N(0, 1): bars at the
# standardized points z_k with height sd * P(X = k), so their area matches
# the Normal density, and the Normal curve on [-4.5, 4.5].
limit_std_curves <- function(dist, k) {
  m <- dl_moments(dist)
  z <- (k - m[["mean"]]) / m[["sd"]]
  keep <- abs(z) <= 4.5
  grid <- seq(-4.5, 4.5, length.out = 201)
  rbind(data.frame(series = "approx", x = z[keep],
                   density = m[["sd"]] * dl_density(dist, k[keep]),
                   cdf = dl_cdf(dist, k[keep])),
        data.frame(series = "target", x = grid, density = stats::dnorm(grid),
                   cdf = stats::pnorm(grid)))
}

#' Simulation of a relationship between distributions
#'
#' Demonstrates one edge of the relationship map, [dl_relations()], with a
#' fixed recipe:
#' * special cases and transformations simulate the construction on the
#'   "from" side (for example \eqn{Z_1 / Z_2} for two standard Normals, or
#'   \eqn{-\log(U) / \lambda}) and compare the draws with the exact "to" law
#'   after 100, 300, 1000, ... draws (up to `n`), by the Kolmogorov-Smirnov
#'   distance (continuous laws, with the asymptotic 95% value
#'   \eqn{1.36 / \sqrt{n}} as `band`) or the total variation distance
#'   (discrete laws, with the expected distance of an \eqn{n}-draw sample
#'   from the exact mass function \eqn{p} as `band`,
#'   \eqn{\frac{1}{2} \sum_i \sqrt{2 p_i (1 - p_i) / (\pi n)}}, the Normal
#'   approximation of \eqn{E|\hat p_i - p_i|});
#' * sums add 1, 2, 3, 5, 10 and 20 independent terms, `n` draws each, and
#'   compare them with the exact law of the sum, with the same bands;
#' * limits are computed exactly, without draws: the approximating law
#'   (such as Binomial(n, \eqn{\lambda/n})) is compared with the limit law
#'   along a sequence of parameters, by the total variation distance
#'   (discrete limits) or the Kolmogorov distance \eqn{\sup |F - G|} (Normal
#'   limits, where a discrete law is standardized). The distance shrinks
#'   along the frames.
#'
#' Heavy-tailed laws (such as the Cauchy and F) are plotted on their 0.5% to
#' 99.5% quantile range, cut at 15 interquartile ranges from the median (finite
#' support ends are kept); the share of draws outside the range is reported
#' in `frames$outside`.
#'
#' @inheritParams dl_mc_check
#' @param from,to Distribution ids at the two ends of the edge.
#' @param type Relationship type (`"special_case"`, `"transformation"`,
#'   `"sum"` or `"limit"`), needed only when several edges join `from` and
#'   `to`.
#' @param n Number of draws (for sums, per number of terms), from 10 to
#'   \eqn{10^5}.
#'
#' @returns A list with
#'   * `edge`: list with `from`, `to`, `type` and `id`;
#'   * `kind`: `"draws"`, `"sum"` or `"limit"`;
#'   * `frames`: data frame with `frame`, `value` (draws, terms or the
#'     limiting parameter), `label` and `outside` (share of draws outside
#'     `range`; `NA` for limits);
#'   * `samples`: data frame with `frame` and `value`, the draws of each
#'     frame (`NULL` for limits);
#'   * `curves`: data frame with `frame`, `series` (`"target"` for the exact
#'     or limit law, `"approx"` for the approximating law of a limit), `x`,
#'     `density` (mass for discrete laws) and `cdf`;
#'   * `distance`: data frame with `frame`, `value`, `metric` (`"ks"`,
#'     `"tv"` or `"kolmogorov"`) and `band` (the reference level of the
#'     distance under the exact law; `NA` for limits);
#'   * `range`: plotting range; `discrete`: whether the compared laws are
#'     discrete (whole-number bins);
#'   * `params`: the recipe parameters as text; `seed` (`NA` for limits or
#'     when `seed` is `NULL`); `code`: R code that reproduces the draws and
#'     distances exactly.
#'
#' @seealso [dl_relations()], [dl_transform()]
#' @family simulation
#' @export
#' @examples
#' demo <- dl_relation_demo("normal", "cauchy", n = 2000, seed = 7)
#' demo$distance
#' lim <- dl_relation_demo("binomial", "poisson")
#' lim$distance$value
#' cat(lim$code)
dl_relation_demo <- function(from, to, type = NULL, n = 5000, seed = NULL) {
  rel <- dl_relations()
  rel$id <- edge_ids(rel)
  if (!is_string(from) || !is_string(to)) {
    stop("`from` and `to` must be distribution ids; see dl_relations().", call. = FALSE)
  }
  hit <- rel[rel$from == from & rel$to == to, , drop = FALSE]
  if (!is.null(type)) hit <- hit[hit$type %in% type, , drop = FALSE]
  if (!nrow(hit)) {
    stop(sprintf("No relationship from '%s' to '%s'%s; see dl_relations().", from, to,
                 if (is.null(type)) "" else sprintf(" of type '%s'", type)), call. = FALSE)
  }
  if (nrow(hit) > 1L) {
    stop(sprintf("Several relationships join '%s' and '%s'; choose `type` among: %s.",
                 from, to, paste(hit$type, collapse = ", ")), call. = FALSE)
  }
  relation_demo_run(hit$id, n = n, seed = seed)
}

# Demo of one edge by id; the engine of dl_relation_demo().
relation_demo_run <- function(edge_id, n = 5000, seed = NULL) {
  rel <- dl_relations()
  rel$id <- edge_ids(rel)
  r <- rel[rel$id == edge_id, , drop = FALSE]
  recipe <- .dl_sim_recipes[[edge_id]]
  if (!nrow(r) || is.null(recipe)) {
    stop("No simulation recipe for the relationship '", edge_id, "'.", call. = FALSE)
  }
  n <- check_whole(n, "n", 10, 1e5)
  seed <- sim_seed(seed)
  edge <- list(from = r$from, to = r$to, type = r$type, id = edge_id)
  out <- switch(recipe$kind,
                draws = demo_draws(recipe, n, seed),
                sum = demo_sum(recipe, n, seed),
                limit = demo_limit(recipe))
  c(list(edge = edge, kind = recipe$kind), out, list(params = recipe$params))[
    c("edge", "kind", "frames", "samples", "curves", "distance", "range", "discrete",
      "params", "seed", "code")]
}

# Evaluate generated code in a fresh environment that sees base R and
# 'stats' (and other packages through `::`), after the seed when given.
run_code <- function(code, seed) {
  env <- new.env(parent = asNamespace("stats"))
  with_seed(seed, eval(parse(text = code, keep.source = FALSE), envir = env))
  env
}

# Even grid of `n` points for continuous laws or whole numbers for discrete
# ones on `range`; at most about 2000 whole numbers, evenly spread, for very
# wide discrete laws.
law_curve <- function(dist, range, n = 401L) {
  if (identical(dist$entry$type, "discrete")) {
    s <- dl_support(dist)
    lo <- max(range[1], s$lower)
    hi <- min(range[2], s$upper)
    x <- if (hi - lo > 2000) unique(round(seq(lo, hi, length.out = 2001))) else seq(lo, hi)
    return(data.frame(x = x, density = dl_density(dist, x), cdf = dl_cdf(dist, x)))
  }
  curve_grid(dist, range[1], range[2], n = n)
}

# Frame labels such as "n = 1000".
frame_labels <- function(symbol, values) {
  paste(symbol, "=", format(values, scientific = FALSE, trim = TRUE, big.mark = ""))
}

demo_draws <- function(recipe, n, seed) {
  target <- do.call(dl_dist, recipe$target)
  discrete <- identical(target$entry$type, "discrete")
  sizes <- unique(c(.dl_sim_frames[.dl_sim_frames < n], n))
  metric <- if (discrete) "tv" else "ks"
  check <- if (discrete) {
    c(sprintf("pmf <- function(k) %s", code_fn(target, "d", "k")),
      .dl_tv_code,
      "tv <- sapply(frames, function(m) tv_dist(x[seq_len(m)], pmf))",
      # Expected distance of an m-draw sample from the exact mass function.
      sprintf("p <- pmf(%s)", support_code(list(target))),
      "band <- sapply(frames, function(m) 0.5 * sum(sqrt(2 * p * (1 - p) / (pi * m))))")
  } else {
    c(.dl_ks_code,
      sprintf("ks <- sapply(frames, function(m) ks_dist(x[seq_len(m)], function(q) %s))",
              code_fn(target, "p", "q")),
      "band <- 1.36 / sqrt(frames)")
  }
  code <- paste(c(
    code_head(seed),
    sprintf("n <- %s", code_num(n)),
    recipe$draw,
    sprintf("frames <- %s", code_vec(sizes)),
    check
  ), collapse = "\n")
  env <- run_code(code, seed)
  x <- env$x
  rng <- sim_range(target, x)
  curve <- law_curve(target, rng, n = 201L)
  nf <- length(sizes)
  outside <- vapply(sizes, function(m) mean(x[seq_len(m)] < rng[1] | x[seq_len(m)] > rng[2]),
                    numeric(1))
  list(
    frames = data.frame(frame = seq_len(nf), value = sizes, label = frame_labels("n", sizes),
                        outside = outside),
    samples = data.frame(frame = rep(seq_len(nf), sizes), value = unlist(lapply(sizes, function(m) x[seq_len(m)]))),
    curves = data.frame(frame = rep(seq_len(nf), each = nrow(curve)), series = "target",
                        curve[rep(seq_len(nrow(curve)), nf), , drop = FALSE], row.names = NULL),
    distance = data.frame(frame = seq_len(nf), value = env[[metric]], metric = metric,
                          band = env$band),
    range = rng, discrete = discrete, seed = seed, code = code
  )
}

# Whole numbers holding all but 1e-12 of the mass of every law in `targets`,
# as R source ("0:12"), for the expected total variation distance.
support_code <- function(targets) {
  lo <- min(vapply(targets, function(d) dl_support(d)$lower, numeric(1)))
  hi <- max(vapply(targets, function(d) {
    min(dl_support(d)$upper, dl_quantile(d, 1e-12, lower.tail = FALSE))
  }, numeric(1)))
  sprintf("%s:%s", code_num(lo), code_num(hi))
}

demo_sum <- function(recipe, n, seed) {
  terms <- .dl_sim_terms
  targets <- lapply(terms, recipe$target)
  discrete <- identical(targets[[1]]$entry$type, "discrete")
  metric <- if (discrete) "tv" else "ks"
  law <- if (discrete) sprintf("function(k) %s", recipe$law) else sprintf("function(q) %s", recipe$law)
  code <- paste(c(
    code_head(seed),
    sprintf("n <- %s", code_num(n)),
    sprintf("terms <- %s", code_vec(terms)),
    recipe$draw,
    "sums <- lapply(terms, function(j) rowSums(x[, seq_len(j), drop = FALSE]))",
    if (discrete) .dl_tv_code else .dl_ks_code,
    sprintf("%s <- mapply(function(v, j) %s_dist(v, %s), sums, terms)", metric, metric, law),
    if (discrete) {
      c(sprintf("support <- %s", support_code(targets)),
        sprintf("band <- sapply(terms, function(j) {\n  k <- support\n  p <- %s\n  0.5 * sum(sqrt(2 * p * (1 - p) / (pi * n)))\n})",
                recipe$law))
    } else {
      "band <- 1.36 / sqrt(n)"
    }
  ), collapse = "\n")
  env <- run_code(code, seed)
  sums <- env$sums
  ranges <- vapply(seq_along(terms), function(i) sim_range(targets[[i]], sums[[i]]), numeric(2))
  rng <- c(min(ranges[1, ]), max(ranges[2, ]))
  nf <- length(terms)
  curves <- do.call(rbind, lapply(seq_len(nf), function(i) {
    data.frame(frame = i, series = "target", law_curve(targets[[i]], rng, n = 201L))
  }))
  list(
    frames = data.frame(frame = seq_len(nf), value = terms, label = frame_labels("k", terms),
                        outside = vapply(sums, function(v) mean(v < rng[1] | v > rng[2]), numeric(1))),
    samples = data.frame(frame = rep(seq_len(nf), lengths(sums)), value = unlist(sums)),
    curves = curves,
    distance = data.frame(frame = seq_len(nf), value = env[[metric]], metric = metric,
                          band = env$band),
    range = rng, discrete = discrete, seed = seed, code = code
  )
}

demo_limit <- function(recipe) {
  v <- recipe$values
  code <- paste(gsub("{values}", code_vec(v), recipe$code, fixed = TRUE), collapse = "\n")
  env <- run_code(code, NA_integer_)
  nf <- length(v)
  curves <- do.call(rbind, lapply(seq_len(nf), function(i) {
    data.frame(frame = i, recipe$curves(v[i]))
  }))
  discrete <- recipe$metric == "tv"
  list(
    frames = data.frame(frame = seq_len(nf), value = v, label = frame_labels(recipe$symbol, v),
                        outside = NA_real_),
    samples = NULL,
    curves = curves,
    distance = data.frame(frame = seq_len(nf), value = env[[recipe$metric]],
                          metric = recipe$metric, band = NA_real_),
    range = range(curves$x), discrete = discrete, seed = NA_integer_, code = code
  )
}
