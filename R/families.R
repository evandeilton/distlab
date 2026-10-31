# Distribution functions used by registry entries ("distlab::<name>") when no
# imported package provides them with the needed arguments.

# Bernoulli(prob) as a Binomial with one trial. Unlike 'extraDistr', these
# return 0 (not a warning) outside {0, 1}, as the other mass functions do.
dbern <- function(x, prob, log = FALSE) {
  stats::dbinom(x, size = 1, prob = prob, log = log)
}

pbern <- function(q, prob, lower.tail = TRUE, log.p = FALSE) {
  stats::pbinom(q, size = 1, prob = prob, lower.tail = lower.tail, log.p = log.p)
}

qbern <- function(p, prob, lower.tail = TRUE, log.p = FALSE) {
  stats::qbinom(p, size = 1, prob = prob, lower.tail = lower.tail, log.p = log.p)
}

rbern <- function(n, prob) stats::rbinom(n, size = 1, prob = prob)

# Beta-binomial CDF as exact cumulative sums of the mass function, clamped to
# [0, 1]. 'extraDistr::pbbinom' can drift by ~1e-13 (even above 1), which would
# break the p/q round trip; qbbinom() below uses the same sums.
pbbinom <- function(q, size, alpha = 1, beta = 1, lower.tail = TRUE,
                    log.p = FALSE) {
  pmf <- extraDistr::dbbinom(seq(0, size), size, alpha, beta)
  # cdf[i] = P(X <= i - 1); sf[i] = P(X > i - 1), summed from the upper end.
  cdf <- pmin(cumsum(pmf), 1)
  sf <- pmin(c(rev(cumsum(rev(pmf)))[-1], 0), 1)
  k <- floor(q)
  idx <- pmin(pmax(k, 0), size) + 1
  out <- if (lower.tail) {
    ifelse(k < 0, 0, cdf[idx])
  } else {
    ifelse(k < 0, 1, sf[idx])
  }
  if (log.p) log(out) else out
}

# Quantile function of the Beta-binomial, which 'extraDistr' does not provide.
# Smallest x in 0..size with F(x) >= p (or S(x) <= p for the upper tail); the
# upper tail is summed directly so tiny tail probabilities keep precision.
# Parameters are scalars, as passed by the engine.
qbbinom <- function(p, size, alpha = 1, beta = 1, lower.tail = TRUE,
                    log.p = FALSE) {
  if (log.p) p <- exp(p)
  x <- seq(0, size)
  pmf <- extraDistr::dbbinom(x, size, alpha, beta)
  # Same relative fuzz as base R discrete quantiles, against rounding in sums.
  fuzz <- 64 * .Machine$double.eps
  if (lower.tail) {
    cdf <- pmin(cumsum(pmf), 1)
    pick <- function(u) which(cdf >= u * (1 - fuzz))[1]
  } else {
    sf <- c(rev(cumsum(rev(pmf)))[-1], 0)
    pick <- function(u) which(sf <= u * (1 + fuzz))[1]
  }
  vapply(p, function(u) {
    if (is.na(u)) return(NA_real_)
    if (u < 0 || u > 1 + 1e-12) return(NaN)
    i <- pick(u)
    if (is.na(i)) size else x[i]
  }, numeric(1))
}
