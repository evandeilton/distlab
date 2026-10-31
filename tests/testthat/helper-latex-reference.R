# R transcriptions of the registry formulas. For each entry and latex key, a
# closure of the parameters `p` (named list) and the key's variable `v`
# returns the piece that applies and its value: a number, Inf or NA for the
# statuses, or NULL for `expr: null`. Tests compare the piece index with the
# YAML conditions and the value with sums, integrals and stats functions.
# Any edit to a YAML formula must be mirrored here.

# A piece index with its value; `kind` marks an interval or a set of values.
P <- function(i, value = NULL, kind = "point") {
  list(piece = as.integer(i), value = value, kind = kind)
}

is_int <- function(x) abs(x - round(x)) < 1e-9 * max(1, abs(x))

# Gauss hypergeometric series; terminates when an upper parameter is a
# non-positive integer.
F21 <- function(a, b, c, z, nmax = 500L) {
  s <- 0
  term <- 1
  for (j in 0:nmax) {
    s <- s + term
    term <- term * (a + j) * (b + j) / ((c + j) * (j + 1)) * z
    if (all(term == 0)) break
  }
  s
}

# Kummer confluent hypergeometric series 1F1(a; b; z).
F11 <- function(a, b, z, nmax = 2000L) {
  s <- 0
  term <- 1
  for (j in 0:nmax) {
    s <- s + term
    term <- term * (a + j) / ((b + j) * (j + 1)) * z
    if (abs(term) < 1e-17 * abs(s)) break
  }
  s
}

# Moments of a symmetric law with given variance and excess kurtosis.
sym <- function(var, kurt) {
  list(mean = function(p, v) P(1, p$location %||% p$mean %||% p$mu),
       variance = function(p, v) P(1, var(p)),
       skewness = function(p, v) P(1, 0),
       excess_kurtosis = function(p, v) P(1, kurt))
}
`%||%` <- function(x, y) if (is.null(x)) y else x

latex_ref <- list(
  bernoulli = list(
    quantile = function(p, v) P(1, ifelse(v <= 1 - p$prob, 0, 1)),
    mean = function(p, v) P(1, p$prob),
    variance = function(p, v) P(1, p$prob * (1 - p$prob)),
    skewness = function(p, v) with(p, if (prob > 0 && prob < 1)
      P(1, (1 - 2 * prob) / sqrt(prob * (1 - prob))) else P(2, NA)),
    excess_kurtosis = function(p, v) with(p, if (prob > 0 && prob < 1)
      P(1, (1 - 6 * prob * (1 - prob)) / (prob * (1 - prob))) else P(2, NA)),
    raw_moment = function(p, v) {
      if (v > 0) P(1, p$prob) else if (v == 0) P(2, 1) else
        if (p$prob < 1) P(3, Inf) else P(4, 1)
    },
    mgf = function(p, v) P(1, 1 - p$prob + p$prob * exp(v)),
    cf = function(p, v) P(1, 1 - p$prob + p$prob * exp(1i * v)),
    pgf = function(p, v) P(1, 1 - p$prob + p$prob * v),
    median = function(p, v) {
      if (p$prob < 0.5) P(1, 0) else if (p$prob == 0.5) P(2, c(0, 1), "interval") else P(3, 1)
    },
    mode = function(p, v) {
      if (p$prob < 0.5) P(1, 0) else if (p$prob == 0.5) P(2, c(0, 1), "set") else P(3, 1)
    },
    entropy = function(p, v) with(p, if (prob > 0 && prob < 1)
      P(1, -(1 - prob) * log(1 - prob) - prob * log(prob)) else P(2, 0))
  ),
  binomial = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) P(1, p$size * p$prob),
    variance = function(p, v) P(1, p$size * p$prob * (1 - p$prob)),
    skewness = function(p, v) with(p, if (prob > 0 && prob < 1)
      P(1, (1 - 2 * prob) / sqrt(size * prob * (1 - prob))) else P(2, NA)),
    excess_kurtosis = function(p, v) with(p, if (prob > 0 && prob < 1)
      P(1, (1 - 6 * prob * (1 - prob)) / (size * prob * (1 - prob))) else P(2, NA)),
    mgf = function(p, v) P(1, (1 - p$prob + p$prob * exp(v))^p$size),
    cf = function(p, v) P(1, (1 - p$prob + p$prob * exp(1i * v))^p$size),
    pgf = function(p, v) P(1, (1 - p$prob + p$prob * v)^p$size),
    median = function(p, v) {
      np <- p$size * p$prob
      if (is_int(np)) P(1, round(np)) else P(2)
    },
    mode = function(p, v) with(p, {
      c1 <- (size + 1) * prob
      if (prob == 1) P(3, size) else
        if (is_int(c1) && round(c1) >= 1 && round(c1) <= size) {
          P(2, round(c1) - 0:1, "set")
        } else P(1, floor(c1 + 1e-9))
    }),
    entropy = function(p, v) P(1)
  ),
  geometric = list(
    quantile = function(p, v) with(p, {
      if (prob == 1) P(4, 0) else if (v == 0) P(2, 0) else if (v == 1) P(3, Inf) else
        P(1, ceiling(log(1 - v) / log(1 - prob) - 1e-9) - 1)
    }),
    mean = function(p, v) P(1, (1 - p$prob) / p$prob),
    variance = function(p, v) P(1, (1 - p$prob) / p$prob^2),
    skewness = function(p, v) if (p$prob < 1) P(1, (2 - p$prob) / sqrt(1 - p$prob)) else P(2, NA),
    excess_kurtosis = function(p, v) if (p$prob < 1) P(1, 6 + p$prob^2 / (1 - p$prob)) else P(2, NA),
    mgf = function(p, v) with(p, if (v < -log(1 - prob))
      P(1, prob / (1 - (1 - prob) * exp(v))) else P(2, Inf)),
    cf = function(p, v) P(1, p$prob / (1 - (1 - p$prob) * exp(1i * v))),
    pgf = function(p, v) with(p, {
      if (abs(v) < 1 / (1 - prob)) P(1, prob / (1 - (1 - prob) * v)) else
        if (v >= 1 / (1 - prob)) P(2, Inf) else P(3, NA)
    }),
    median = function(p, v) with(p, {
      if (prob == 1) return(P(3, 0))
      r <- log(2) / log(1 - prob)
      if (is_int(r)) P(2, c(-round(r) - 1, -round(r)), "interval") else P(1, ceiling(-r) - 1)
    }),
    mode = function(p, v) P(1, 0),
    entropy = function(p, v) with(p, if (prob < 1)
      P(1, (-(1 - prob) * log(1 - prob) - prob * log(prob)) / prob) else P(2, 0))
  ),
  negbinomial = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) P(1, p$size * (1 - p$prob) / p$prob),
    variance = function(p, v) P(1, p$size * (1 - p$prob) / p$prob^2),
    skewness = function(p, v) with(p, if (prob < 1)
      P(1, (2 - prob) / sqrt(size * (1 - prob))) else P(2, NA)),
    excess_kurtosis = function(p, v) with(p, if (prob < 1)
      P(1, 6 / size + prob^2 / (size * (1 - prob))) else P(2, NA)),
    mgf = function(p, v) with(p, if (v < -log(1 - prob))
      P(1, (prob / (1 - (1 - prob) * exp(v)))^size) else P(2, Inf)),
    cf = function(p, v) P(1, (p$prob / (1 - (1 - p$prob) * exp(1i * v)))^p$size),
    pgf = function(p, v) with(p, {
      if (abs(v) < 1 / (1 - prob)) P(1, (prob / (1 - (1 - prob) * v))^size) else
        if (v >= 1 / (1 - prob)) P(2, Inf) else P(3, NA)
    }),
    median = function(p, v) P(1),
    mode = function(p, v) with(p, {
      c1 <- (size - 1) * (1 - prob) / prob
      if (is_int(c1) && round(c1) >= 1) P(2, round(c1) - 0:1, "set") else
        P(1, max(0, floor(c1 + 1e-9)))
    }),
    entropy = function(p, v) P(1)
  ),
  poisson = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) P(1, p$lambda),
    variance = function(p, v) P(1, p$lambda),
    skewness = function(p, v) P(1, 1 / sqrt(p$lambda)),
    excess_kurtosis = function(p, v) P(1, 1 / p$lambda),
    mgf = function(p, v) P(1, exp(p$lambda * (exp(v) - 1))),
    cf = function(p, v) P(1, exp(p$lambda * (exp(1i * v) - 1))),
    pgf = function(p, v) P(1, exp(p$lambda * (v - 1))),
    median = function(p, v) P(1),
    mode = function(p, v) {
      if (is_int(p$lambda)) P(2, round(p$lambda) - 0:1, "set") else P(1, floor(p$lambda))
    },
    entropy = function(p, v) P(1)
  ),
  hypergeometric = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) P(1, p$k * p$m / (p$m + p$n)),
    variance = function(p, v) with(p, {
      N <- m + n
      P(1, k * m * n * (N - k) / (N^2 * (N - 1)))
    }),
    skewness = function(p, v) with(p, {
      N <- m + n
      if (k == N) P(3, NA) else if (N == 2) P(2, 0) else
        P(1, (n - m) * (N - 2 * k) * sqrt(N - 1) / ((N - 2) * sqrt(k * m * n * (N - k))))
    }),
    excess_kurtosis = function(p, v) with(p, {
      N <- m + n
      if (k == N) P(4, NA) else if (N == 2) P(2, -2) else if (N == 3) P(3, -1.5) else
        P(1, ((N - 1) * N^2 * (N * (N + 1) - 6 * m * n - 6 * k * (N - k)) +
                6 * k * m * n * (N - k) * (5 * N - 6)) /
            (k * m * n * (N - k) * (N - 2) * (N - 3)))
    }),
    mgf = function(p, v) with(p, {
      N <- m + n
      if (k <= n) P(1, choose(n, k) / choose(N, k) * F21(-k, -m, n - k + 1, exp(v))) else
        if (k <= m) P(2, exp(k * v) * choose(m, k) / choose(N, k) * F21(-k, -n, m - k + 1, exp(-v))) else
          P(3, exp(m * v) * choose(n, N - k) / choose(N, k) * F21(k - N, -m, k - m + 1, exp(-v)))
    }),
    cf = function(p, v) with(p, {
      N <- m + n
      if (k <= n) P(1, choose(n, k) / choose(N, k) * F21(-k, -m, n - k + 1, exp(1i * v))) else
        if (k <= m) P(2, exp(1i * k * v) * choose(m, k) / choose(N, k) *
                        F21(-k, -n, m - k + 1, exp(-1i * v))) else
          P(3, exp(1i * m * v) * choose(n, N - k) / choose(N, k) *
              F21(k - N, -m, k - m + 1, exp(-1i * v)))
    }),
    pgf = function(p, v) with(p, {
      N <- m + n
      if (k <= n) P(1, choose(n, k) / choose(N, k) * F21(-k, -m, n - k + 1, v)) else
        if (v == 0) P(4, 0) else
          if (k <= m) P(2, v^k * choose(m, k) / choose(N, k) * F21(-k, -n, m - k + 1, 1 / v)) else
            P(3, v^m * choose(n, N - k) / choose(N, k) * F21(k - N, -m, k - m + 1, 1 / v))
    }),
    median = function(p, v) P(1),
    mode = function(p, v) with(p, {
      c1 <- (k + 1) * (m + 1) / (m + n + 2)
      if (is_int(c1)) P(2, round(c1) - 0:1, "set") else P(1, floor(c1))
    }),
    entropy = function(p, v) P(1)
  ),
  betabinomial = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) P(1, p$size * p$alpha / (p$alpha + p$beta)),
    variance = function(p, v) with(p, {
      s <- alpha + beta
      P(1, size * alpha * beta * (s + size) / (s^2 * (s + 1)))
    }),
    skewness = function(p, v) with(p, {
      s <- alpha + beta
      P(1, (s + 2 * size) * (beta - alpha) / (s + 2) *
          sqrt((1 + s) / (size * alpha * beta * (size + s))))
    }),
    excess_kurtosis = function(p, v) with(p, {
      s <- alpha + beta
      ab <- alpha * beta
      n <- size
      P(1, s^2 * (1 + s) / (n * ab * (s + 2) * (s + 3) * (s + n)) *
          (s * (s - 1 + 6 * n) + 3 * ab * (n - 2) + 6 * n^2 -
             3 * ab * n * (6 - n) / s - 18 * ab * n^2 / s^2) - 3)
    }),
    mgf = function(p, v) P(1, F21(-p$size, p$alpha, p$alpha + p$beta, 1 - exp(v))),
    cf = function(p, v) P(1, F21(-p$size, p$alpha, p$alpha + p$beta, 1 - exp(1i * v))),
    pgf = function(p, v) P(1, F21(-p$size, p$alpha, p$alpha + p$beta, 1 - v)),
    median = function(p, v) P(1),
    mode = function(p, v) P(1),
    entropy = function(p, v) P(1)
  ),
  discrete_uniform = list(
    quantile = function(p, v) with(p, {
      if (v == 0) P(2, min) else P(1, min - 1 + ceiling(v * (max - min + 1)))
    }),
    mean = function(p, v) P(1, (p$min + p$max) / 2),
    variance = function(p, v) P(1, ((p$max - p$min + 1)^2 - 1) / 12),
    skewness = function(p, v) P(1, 0),
    excess_kurtosis = function(p, v) with(p, {
      n2 <- (max - min + 1)^2
      P(1, -6 * (n2 + 1) / (5 * (n2 - 1)))
    }),
    mgf = function(p, v) with(p, {
      if (v == 0) P(2, 1) else
        P(1, (exp(min * v) - exp((max + 1) * v)) / ((max - min + 1) * (1 - exp(v))))
    }),
    cf = function(p, v) with(p, {
      if (is_int(v / (2 * pi))) P(2, 1) else
        P(1, (exp(1i * min * v) - exp(1i * (max + 1) * v)) /
            ((max - min + 1) * (1 - exp(1i * v))))
    }),
    pgf = function(p, v) with(p, {
      f <- function() (v^min - v^(max + 1)) / ((max - min + 1) * (1 - v))
      if (v == 1) P(3, 1) else if (min >= 0) P(1, f()) else if (v == 0) P(4, NA) else P(2, f())
    }),
    median = function(p, v) with(p, {
      if ((max - min) %% 2 == 0) P(1, (min + max) / 2) else
        P(2, c(min + max - 1, min + max + 1) / 2, "interval")
    }),
    mode = function(p, v) P(1, seq(p$min, p$max), "set"),
    entropy = function(p, v) P(1, log(p$max - p$min + 1))
  ),
  uniform = list(
    quantile = function(p, v) P(1, p$min + v * (p$max - p$min)),
    mean = function(p, v) P(1, (p$min + p$max) / 2),
    variance = function(p, v) P(1, (p$max - p$min)^2 / 12),
    skewness = function(p, v) P(1, 0),
    excess_kurtosis = function(p, v) P(1, -6 / 5),
    raw_moment = function(p, v) {
      a <- p$min; b <- p$max; j <- v
      f <- function() (b^(j + 1) - a^(j + 1)) / ((j + 1) * (b - a))
      if (j >= 0 && is_int(j)) P(1, f()) else
        if (a >= 0 && j > -1 && !is_int(j)) P(2, f()) else
          if (a > 0 && j < -1) P(3, f()) else
            if (a > 0 && j == -1) P(4, (log(b) - log(a)) / (b - a)) else
              if (a == 0 && j <= -1) P(5, Inf) else
                if (a < 0 && !is_int(j)) P(6, NA) else
                  if (b < 0 && j <= -2) P(7, f()) else
                    if (b < 0 && j == -1) P(8, (log(-b) - log(-a)) / (b - a)) else
                      if (b >= 0 && j %% 2 == 0) P(9, Inf) else
                        if (b > 0) P(10, NA) else P(11, -Inf)
    },
    mgf = function(p, v) with(p, {
      if (v == 0) P(2, 1) else P(1, (exp(max * v) - exp(min * v)) / (v * (max - min)))
    }),
    cf = function(p, v) with(p, {
      if (v == 0) P(2, 1) else
        P(1, (exp(1i * max * v) - exp(1i * min * v)) / (1i * v * (max - min)))
    }),
    median = function(p, v) P(1, (p$min + p$max) / 2),
    mode = function(p, v) P(1, c(p$min, p$max), "interval"),
    entropy = function(p, v) P(1, log(p$max - p$min))
  ),
  beta = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) P(1, p$shape1 / (p$shape1 + p$shape2)),
    variance = function(p, v) with(p, {
      s <- shape1 + shape2
      P(1, shape1 * shape2 / (s^2 * (s + 1)))
    }),
    skewness = function(p, v) with(p, {
      a <- shape1; b <- shape2; s <- a + b
      P(1, 2 * (b - a) * sqrt(s + 1) / ((s + 2) * sqrt(a * b)))
    }),
    excess_kurtosis = function(p, v) with(p, {
      a <- shape1; b <- shape2; s <- a + b
      P(1, 6 * ((a - b)^2 * (s + 1) - a * b * (s + 2)) / (a * b * (s + 2) * (s + 3)))
    }),
    raw_moment = function(p, v) with(p, {
      if (v > -shape1) P(1, beta(shape1 + v, shape2) / beta(shape1, shape2)) else P(2, Inf)
    }),
    mgf = function(p, v) P(1, F11(p$shape1, p$shape1 + p$shape2, v)),
    cf = function(p, v) P(1, F11(p$shape1, p$shape1 + p$shape2, 1i * v)),
    median = function(p, v) with(p, {
      a <- shape1; b <- shape2
      if (a == b) P(1, 0.5) else if (a == 1) P(2, 1 - 2^(-1 / b)) else
        if (b == 1) P(3, 2^(-1 / a)) else P(4)
    }),
    mode = function(p, v) with(p, {
      a <- shape1; b <- shape2
      if (a > 1 && b > 1) P(1, (a - 1) / (a + b - 2)) else
        if (a <= 1 && b > 1) P(2, 0) else
          if (a < 1 && b == 1) P(3, 0) else
            if (a > 1 && b <= 1) P(4, 1) else
              if (a == 1 && b < 1) P(5, 1) else
                if (a < 1 && b < 1) P(6, c(0, 1), "set") else P(7, c(0, 1), "interval")
    }),
    entropy = function(p, v) with(p, {
      a <- shape1; b <- shape2
      P(1, lbeta(a, b) - (a - 1) * digamma(a) - (b - 1) * digamma(b) +
          (a + b - 2) * digamma(a + b))
    })
  ),
  exponential = list(
    quantile = function(p, v) P(1, -log(1 - v) / p$rate),
    mean = function(p, v) P(1, 1 / p$rate),
    variance = function(p, v) P(1, 1 / p$rate^2),
    skewness = function(p, v) P(1, 2),
    excess_kurtosis = function(p, v) P(1, 6),
    raw_moment = function(p, v) if (v > -1) P(1, gamma(1 + v) / p$rate^v) else P(2, Inf),
    mgf = function(p, v) if (v < p$rate) P(1, p$rate / (p$rate - v)) else P(2, Inf),
    cf = function(p, v) P(1, p$rate / (p$rate - 1i * v)),
    median = function(p, v) P(1, log(2) / p$rate),
    mode = function(p, v) P(1, 0),
    entropy = function(p, v) P(1, 1 - log(p$rate))
  ),
  gamma = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) P(1, p$shape / p$rate),
    variance = function(p, v) P(1, p$shape / p$rate^2),
    skewness = function(p, v) P(1, 2 / sqrt(p$shape)),
    excess_kurtosis = function(p, v) P(1, 6 / p$shape),
    raw_moment = function(p, v) with(p, {
      if (v > -shape) P(1, gamma(shape + v) / (rate^v * gamma(shape))) else P(2, Inf)
    }),
    mgf = function(p, v) with(p, if (v < rate) P(1, (1 - v / rate)^(-shape)) else P(2, Inf)),
    cf = function(p, v) P(1, (1 - 1i * v / p$rate)^(-p$shape)),
    median = function(p, v) P(1),
    mode = function(p, v) if (p$shape >= 1) P(1, (p$shape - 1) / p$rate) else P(2, 0),
    entropy = function(p, v) with(p, {
      P(1, shape - log(rate) + lgamma(shape) + (1 - shape) * digamma(shape))
    })
  ),
  chisq = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) P(1, p$df),
    variance = function(p, v) P(1, 2 * p$df),
    skewness = function(p, v) P(1, sqrt(8 / p$df)),
    excess_kurtosis = function(p, v) P(1, 12 / p$df),
    raw_moment = function(p, v) with(p, {
      if (v > -df / 2) P(1, 2^v * gamma(df / 2 + v) / gamma(df / 2)) else P(2, Inf)
    }),
    mgf = function(p, v) if (v < 0.5) P(1, (1 - 2 * v)^(-p$df / 2)) else P(2, Inf),
    cf = function(p, v) P(1, (1 - 2i * v)^(-p$df / 2)),
    median = function(p, v) P(1),
    mode = function(p, v) if (p$df >= 2) P(1, p$df - 2) else P(2, 0),
    entropy = function(p, v) with(p, {
      P(1, df / 2 + log(2 * gamma(df / 2)) + (1 - df / 2) * digamma(df / 2))
    })
  ),
  weibull = list(
    quantile = function(p, v) P(1, p$scale * (-log(1 - v))^(1 / p$shape)),
    mean = function(p, v) P(1, p$scale * gamma(1 + 1 / p$shape)),
    variance = function(p, v) with(p, {
      g <- gamma(1 + (1:2) / shape)
      P(1, scale^2 * (g[2] - g[1]^2))
    }),
    skewness = function(p, v) {
      g <- gamma(1 + (1:3) / p$shape)
      P(1, (g[3] - 3 * g[1] * g[2] + 2 * g[1]^3) / (g[2] - g[1]^2)^1.5)
    },
    excess_kurtosis = function(p, v) {
      g <- gamma(1 + (1:4) / p$shape)
      P(1, (g[4] - 4 * g[1] * g[3] + 6 * g[1]^2 * g[2] - 3 * g[1]^4) / (g[2] - g[1]^2)^2 - 3)
    },
    raw_moment = function(p, v) with(p, {
      if (v > -shape) P(1, scale^v * gamma(1 + v / shape)) else P(2, Inf)
    }),
    mgf = function(p, v) with(p, {
      if (shape > 1) P(1) else
        if (shape == 1) {
          if (v < 1 / scale) P(2, 1 / (1 - scale * v)) else P(3, Inf)
        } else if (v <= 0) P(4) else P(5, Inf)
    }),
    cf = function(p, v) P(1),
    median = function(p, v) P(1, p$scale * log(2)^(1 / p$shape)),
    mode = function(p, v) with(p, {
      if (shape > 1) P(1, scale * ((shape - 1) / shape)^(1 / shape)) else P(2, 0)
    }),
    entropy = function(p, v) with(p, {
      P(1, -digamma(1) * (1 - 1 / shape) + log(scale / shape) + 1)
    })
  ),
  lognormal = list(
    quantile = function(p, v) P(1, exp(p$meanlog + p$sdlog * stats::qnorm(v))),
    mean = function(p, v) P(1, exp(p$meanlog + p$sdlog^2 / 2)),
    variance = function(p, v) with(p, P(1, (exp(sdlog^2) - 1) * exp(2 * meanlog + sdlog^2))),
    skewness = function(p, v) with(p, P(1, (exp(sdlog^2) + 2) * sqrt(exp(sdlog^2) - 1))),
    excess_kurtosis = function(p, v) with(p, {
      s2 <- sdlog^2
      P(1, exp(4 * s2) + 2 * exp(3 * s2) + 3 * exp(2 * s2) - 6)
    }),
    raw_moment = function(p, v) P(1, exp(v * p$meanlog + v^2 * p$sdlog^2 / 2)),
    mgf = function(p, v) if (v <= 0) P(1) else P(2, Inf),
    cf = function(p, v) P(1),
    median = function(p, v) P(1, exp(p$meanlog)),
    mode = function(p, v) P(1, exp(p$meanlog - p$sdlog^2)),
    entropy = function(p, v) P(1, p$meanlog + log(2 * pi * exp(1) * p$sdlog^2) / 2)
  ),
  f = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) if (p$df2 > 2) P(1, p$df2 / (p$df2 - 2)) else P(2, Inf),
    variance = function(p, v) with(p, {
      d1 <- df1; d2 <- df2
      if (d2 > 4) P(1, 2 * d2^2 * (d1 + d2 - 2) / (d1 * (d2 - 2)^2 * (d2 - 4))) else
        if (d2 > 2) P(2, Inf) else P(3, NA)
    }),
    skewness = function(p, v) with(p, {
      d1 <- df1; d2 <- df2
      if (d2 > 6) P(1, (2 * d1 + d2 - 2) * sqrt(8 * (d2 - 4)) /
                      ((d2 - 6) * sqrt(d1 * (d1 + d2 - 2)))) else
        if (d2 > 4) P(2, Inf) else P(3, NA)
    }),
    excess_kurtosis = function(p, v) with(p, {
      d1 <- df1; d2 <- df2
      if (d2 > 8) P(1, 12 * (d1 * (5 * d2 - 22) * (d1 + d2 - 2) + (d2 - 4) * (d2 - 2)^2) /
                      (d1 * (d2 - 6) * (d2 - 8) * (d1 + d2 - 2))) else
        if (d2 > 4) P(2, Inf) else P(3, NA)
    }),
    raw_moment = function(p, v) with(p, {
      d1 <- df1; d2 <- df2
      if (v > -d1 / 2 && v < d2 / 2) {
        P(1, (d2 / d1)^v * gamma(d1 / 2 + v) * gamma(d2 / 2 - v) /
            (gamma(d1 / 2) * gamma(d2 / 2)))
      } else if (v >= d2 / 2) P(2, Inf) else P(3, Inf)
    }),
    mgf = function(p, v) if (v <= 0) P(1) else P(2, Inf),
    cf = function(p, v) with(p, {
      # Kummer U as two 1F1 terms; this needs b = 1 - d2/2 not an integer.
      a <- df1 / 2; b <- 1 - df2 / 2; z <- -(df2 / df1) * 1i * v
      if (is_int(b) || Mod(z) > 12) return(P(1))
      U <- gamma(1 - b) / gamma(a - b + 1) * F11(a, b, z) +
        gamma(b - 1) / gamma(a) * z^(1 - b) * F11(a - b + 1, 2 - b, z)
      P(1, gamma((df1 + df2) / 2) / gamma(df2 / 2) * U)
    }),
    median = function(p, v) P(1),
    mode = function(p, v) with(p, {
      if (df1 > 2) P(1, (df1 - 2) / df1 * df2 / (df2 + 2)) else P(2, 0)
    }),
    entropy = function(p, v) with(p, {
      d1 <- df1; d2 <- df2
      P(1, log(d2 / d1) + lbeta(d1 / 2, d2 / 2) + (1 - d1 / 2) * digamma(d1 / 2) -
          (1 + d2 / 2) * digamma(d2 / 2) + (d1 + d2) / 2 * digamma((d1 + d2) / 2))
    })
  ),
  normal = c(sym(function(p) p$sd^2, 0), list(
    quantile = function(p, v) P(1, p$mean + p$sd * stats::qnorm(v)),
    mgf = function(p, v) P(1, exp(p$mean * v + p$sd^2 * v^2 / 2)),
    cf = function(p, v) P(1, exp(1i * p$mean * v - p$sd^2 * v^2 / 2)),
    median = function(p, v) P(1, p$mean),
    mode = function(p, v) P(1, p$mean),
    entropy = function(p, v) P(1, log(2 * pi * exp(1) * p$sd^2) / 2)
  )),
  student_t = list(
    quantile = function(p, v) P(1),
    mean = function(p, v) if (p$df > 1) P(1, 0) else P(2, NA),
    variance = function(p, v) with(p, {
      if (df > 2) P(1, df / (df - 2)) else if (df > 1) P(2, Inf) else P(3, NA)
    }),
    skewness = function(p, v) if (p$df > 3) P(1, 0) else P(2, NA),
    excess_kurtosis = function(p, v) with(p, {
      if (df > 4) P(1, 6 / (df - 4)) else if (df > 2) P(2, Inf) else P(3, NA)
    }),
    raw_moment = function(p, v) with(p, {
      j <- v
      if (!is_int(j)) P(5, NA) else
        if (j < 0) {
          if (j %% 2 == 0) P(6, Inf) else P(7, NA)
        } else if (j %% 2 == 1) {
          if (j < df) P(1, 0) else P(3, NA)
        } else if (j < df) {
          P(2, df^(j / 2) * gamma((j + 1) / 2) * gamma((df - j) / 2) / (sqrt(pi) * gamma(df / 2)))
        } else P(4, Inf)
    }),
    mgf = function(p, v) if (v == 0) P(1, 1) else P(2, Inf),
    cf = function(p, v) with(p, {
      if (v == 0) return(P(2, 1))
      x <- sqrt(df) * abs(v)
      P(1, besselK(x, df / 2) * x^(df / 2) / (gamma(df / 2) * 2^(df / 2 - 1)))
    }),
    median = function(p, v) P(1, 0),
    mode = function(p, v) P(1, 0),
    entropy = function(p, v) with(p, {
      P(1, (df + 1) / 2 * (digamma((df + 1) / 2) - digamma(df / 2)) +
          log(sqrt(df) * beta(df / 2, 1 / 2)))
    })
  ),
  cauchy = list(
    quantile = function(p, v) P(1, p$location + p$scale * tan(pi * (v - 0.5))),
    mean = function(p, v) P(1, NA),
    variance = function(p, v) P(1, NA),
    skewness = function(p, v) P(1, NA),
    excess_kurtosis = function(p, v) P(1, NA),
    mgf = function(p, v) if (v == 0) P(1, 1) else P(2, Inf),
    cf = function(p, v) P(1, exp(1i * p$location * v - p$scale * abs(v))),
    median = function(p, v) P(1, p$location),
    mode = function(p, v) P(1, p$location),
    entropy = function(p, v) P(1, log(4 * pi * p$scale))
  ),
  laplace = c(sym(function(p) 2 * p$sigma^2, 3), list(
    quantile = function(p, v) with(p, P(1, mu - sigma * sign(v - 0.5) * log(1 - 2 * abs(v - 0.5)))),
    mgf = function(p, v) with(p, {
      if (abs(v) < 1 / sigma) P(1, exp(mu * v) / (1 - sigma^2 * v^2)) else P(2, Inf)
    }),
    cf = function(p, v) P(1, exp(1i * p$mu * v) / (1 + p$sigma^2 * v^2)),
    median = function(p, v) P(1, p$mu),
    mode = function(p, v) P(1, p$mu),
    entropy = function(p, v) P(1, 1 + log(2 * p$sigma))
  )),
  logistic = c(sym(function(p) p$scale^2 * pi^2 / 3, 6 / 5), list(
    quantile = function(p, v) P(1, p$location + p$scale * log(v / (1 - v))),
    mgf = function(p, v) with(p, {
      if (abs(v) < 1 / scale) P(1, exp(location * v) * beta(1 - scale * v, 1 + scale * v)) else
        P(2, Inf)
    }),
    cf = function(p, v) with(p, {
      if (v == 0) P(2, 1) else
        P(1, exp(1i * location * v) * pi * scale * v / sinh(pi * scale * v))
    }),
    median = function(p, v) P(1, p$location),
    mode = function(p, v) P(1, p$location),
    entropy = function(p, v) P(1, log(p$scale) + 2)
  ))
)
