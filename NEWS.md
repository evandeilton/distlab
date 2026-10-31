# distlab 0.0.0.9000

* Initial package structure: distribution registry, engine for d/p/q/r and
  moments, bilingual dictionary, and a 'shiny' app with the Explorer page.
* The registry now holds 21 univariate distributions: Bernoulli, Binomial,
  Geometric, Negative binomial, Poisson, Hypergeometric, Beta-binomial,
  Discrete uniform, Uniform, Beta, Exponential, Gamma, Chi-square, Weibull,
  Lognormal, F, Normal, Student's t, Cauchy, Laplace and Logistic.
* Support bounds may depend on the parameters (e.g. Binomial `0..size`,
  Uniform `min..max`), and entries can declare constraints such as
  `min < max`, checked by `dl_dist()`.
* `dl_moments()` has closed forms for every entry and returns `Inf` for
  infinite moments and `NA` for moments that do not exist; the Explorer shows
  them as "∞" and "does not exist".
* Registry entries list their relationships to other distributions (special
  cases, sums, limits and transformations).
* Numeric moments of continuous distributions integrate over probabilities,
  so densities that are unbounded at a support end (Beta with shapes below 1)
  are handled.
* The Explorer renders sliders while the sidebar is collapsed, so plots and
  moments appear on narrow screens.
* 'extraDistr' is now imported for the Beta-binomial, Discrete uniform and
  Laplace distributions.
