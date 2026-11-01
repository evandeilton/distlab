# distlab 0.0.0.9000

* New Fit lab: load CSV/TSV or pasted observations, value/frequency tables,
  grouped intervals, built-in examples or a seeded simulated sample. Data
  summaries and support checks explain which catalog families can be fitted.
* New `dl_fit_data()`, `dl_fit_describe()`, `dl_fit_candidates()`, `dl_fit()`
  and `dl_fit_diagnostics()` expose data preparation, maximum likelihood and
  diagnostics outside Shiny. Frequency counts stay compressed; grouped
  likelihoods use interval probabilities without conditioning away tails.
* Fits provide estimates, observed-information standard errors and Wald
  intervals where regularity permits them, convergence and boundary notes,
  and AIC/BIC rankings separated by comparable likelihoods. Integer design
  parameters can be fixed explicitly; failures remain visible per candidate.
* The bilingual Fit page provides density/PMF and CDF overlays, conditional
  pointwise Q-Q envelopes, P-P plots and descriptive goodness-of-fit results,
  plus CSV tables/data and reproducible R code. Grouped diagnostics retain
  the original classes. Language and theme changes preserve the fitted result.

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
* New Compendium page with four views: a relationship map of every
  distribution (fixed layout, colors by data type, dashed arrows for limits;
  click a node or arrow for details, double-click to open it in the
  Explorer), an accessible and printable list of relations, cards by data
  type and family, and a sortable, searchable table. Filters by group, type
  and relation type apply to every view and survive a language switch.
* Home group cards link to their group in the Compendium.
* New `dl_relations()` and `dl_catalog()` return the relationship edges and
  the catalog of distributions as data frames.
* Registry entries may set a map position (`graph`), a short map label per
  relationship (`label`) and extra groups for cross-listing (`also_in`).
* 'visNetwork' is now imported for the relationship map.
* New `dl_prob()` computes the probability of an interval with open or closed
  ends, taking upper tails with `lower.tail = FALSE` and staying accurate on
  the log scale for tails beyond the double range. New `dl_interval()` gives
  the central interval for a probability, with its exact coverage for
  discrete distributions.
* New Calculator page: P(X ≤ x), P(X > x), P(a < X ≤ b) with a choice of
  `<` or `≤` for discrete distributions, quantiles and central intervals.
  The region is shaded on the density and marked on the CDF, and the result
  comes as a sentence, a percentage and the R code that reproduces it, with a
  copy button. Fields accept a decimal comma.
* New Compare page: two to four distributions overlaid, or one parameter
  swept over several values, with density and CDF plots in colorblind-safe
  colors and line types, a table of moments, median and support, and presets
  such as Binomial vs Poisson and t vs Normal.
* The Explorer and the Compendium open the Calculator and Compare with the
  chosen distribution.
* New Simulation page, where every number comes from a seeded simulation and
  each tab shows the R code that repeats it exactly: a sample checked against
  the exact law (histogram, empirical CDF and a Monte Carlo table of moments
  and quartile probabilities with standard errors and 95% intervals), the law
  of large numbers (running means, also for the Cauchy), the central limit
  theorem (animated, with the distance to N(0, 1)), sums, extremes and
  transformations with their exact law or a Normal approximation, an animated
  demo of every relation of the map, and memorylessness.
* New `dl_mc_check()`, `dl_lln()`, `dl_clt()`, `dl_transform()`,
  `dl_memoryless()` and `dl_relation_demo()` run these simulations outside
  the app. With a `seed` they restore the caller's random number stream and
  return R code that reproduces their numbers exactly.
* The Explorer and the Compendium open the Simulation with the chosen
  distribution, and the Compendium simulates a selected relation.
