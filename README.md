# distlab

<!-- badges: start -->
[![R-CMD-check](https://github.com/evandeilton/distlab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/evandeilton/distlab/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

distlab is an interactive laboratory for probability distributions: a
'shiny' dashboard and an engine to explore distributions, in Brazilian
Portuguese and English.

## Installation

``` r
# install.packages("pak")
pak::pak("evandeilton/distlab")
```

## Usage

``` r
library(distlab)

g <- dl_dist("gamma", shape = 3, rate = 2)
dl_moments(g)
dl_cdf(g, 1)

run_app(lang = "pt-BR")
```

Fit catalog distributions to observations or a frequency table:

``` r
data <- dl_fit_data(c(0, 1, 2, 3, 4), frequency = c(8, 18, 14, 7, 3))
fit <- dl_fit(data, c("poisson", "geometric"))
fit$ranking
dl_fit_diagnostics(fit, "poisson")$gof
```

Goodness-of-fit p-values are descriptive when parameters are estimated from
the same data. The Fit page also accepts grouped intervals and exports the
tables and R code needed to reproduce a study.
