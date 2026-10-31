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
