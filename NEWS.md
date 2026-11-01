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
* Registry entries carry LaTeX formulas for the quantile, moments, raw
  moments, mgf, cf, pgf, median, mode and entropy, with conditions and
  infinite/undefined statuses that match `dl_moments()`.
* Registry entries list their relationships to other distributions (special
  cases, sums, limits and transformations).
* Numeric moments of continuous distributions integrate over probabilities,
  so densities that are unbounded at a support end (Beta with shapes below 1)
  are handled.
* The Explorer renders sliders while the sidebar is collapsed, so plots and
  moments appear on narrow screens.
* 'extraDistr' is now imported for the Beta-binomial, Discrete uniform and
  Laplace distributions.
* New brand and app shell: logo, colors and vendored fonts (Space Grotesk,
  Inter, JetBrains Mono), light and dark modes, and a navigation bar with
  Home, Compendium, Explorer, Calculator, Compare, Simulation, Fit and About.
* Home page with a distribution search, quick-start actions and one card per
  data type.
* The interface language can be switched at any time between Portuguese and
  English without losing the open page, distribution or parameter values.
* The Explorer shows a formula sheet rendered with KaTeX (density, CDF,
  quantile, moments, generating functions, median, mode and entropy, with
  their conditions), a "Related distributions" card that opens the related
  entry, and moment tiles and plots in the brand colors.
* Linked sliders keep parameter constraints (Uniform `min < max`,
  Hypergeometric `k <= m + n`) by moving values and ranges.
