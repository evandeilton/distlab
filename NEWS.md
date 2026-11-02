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
* New Fit page: load pasted or uploaded CSV/TSV data (raw values,
  value/frequency tables or grouped classes), an example or a seeded
  simulated sample, then fit catalog distributions by maximum likelihood and
  compare them by AIC and BIC.
* Fits show estimates, standard errors and Wald intervals that stay inside
  the parameter space. Samples without a finite estimate, such as a Cauchy
  sample in which half the values coincide, are listed as failures instead
  of being ranked.
* Density or PMF, CDF, Q-Q and P-P plots and descriptive goodness-of-fit
  tests check each fit. The page exports the tables, the data and R code
  that reproduces the fit.
* Pasted and uploaded tables accept decimal commas and thousands separators
  ("1.234,5" or "1,234.5"), and one line of numbers is read as one sample.
  Files are limited to 5 MB.
* New `dl_fit_data()`, `dl_fit_describe()`, `dl_fit_candidates()`, `dl_fit()`
  and `dl_fit_diagnostics()` do the same outside the app. Frequency tables
  are never expanded, and common families use closed-form estimates, so
  samples of 100,000 values fit in seconds.
* Parameter sliders widen to show values opened from a fit and keep that
  range while you drag.
