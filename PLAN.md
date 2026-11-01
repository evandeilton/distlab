# distlab: project plan

Versioned working document (excluded from the package build via `.Rbuildignore`).

## Vision

distlab is an R package with a Shiny dashboard, in pt-BR and en-US, for undergraduates to study probability
distributions and fit them to data. Each distribution is presented as linked views: story, formulas,
plots, simulation and relationships. Every number on screen is exact (closed form or controlled numerical
integration) and comes with the R code that reproduces it.

## Fixed decisions

| # | Topic | Decision |
|---|---|---|
| 1 | MVP catalog | ~25 distributions: the 20 families of the relationship map + Logistic, Categorical, Multinomial, Dirichlet, MVN (multivariate as view-only cards) |
| 2 | Deployment | Server first (shinyapps.io / Posit Connect); shinylive on GitHub Pages in v1 |
| 3 | Notation | Bussab & Morettin, Magalhães, Casella & Berger; R parameterization shown alongside, with a switch |
| 4 | Languages | Full UI and content in pt-BR and en-US |
| 5 | Fit lab data | Raw numeric columns and frequency/grouped tables. Survival/censored data out of scope |
| 6 | GOF policy | AIC/BIC ranking; asymptotic p-values labeled "descriptive (parameters estimated)"; parametric bootstrap on demand |
| 7 | Learning labs | Full Simulation lab in the MVP. No quizzes |
| 8 | Optional packages | Non-core families live in Suggests; the app offers to install the package when a student selects such a distribution |
| 9 | Identity | Own brand and logo (designed with the owner); gkwdistapp is visual inspiration only |
| 10 | Hosting | GitHub `evandeilton/distlab`, commits with the owner's identity |
| 11 | Data handling | data.table and tidyverse |
| 12 | Study report | Downloadable custom HTML report per distribution, built from the student's own session (parameters, simulations, calculations, fits), in MVP |

## Navigation (bslib `page_navbar`)

1. **Home**: search, one card per data-type group, quick-start presets.
2. **Compendium**, three views:
   - by data type (default): Binary/categorical, Counts, Bounded/proportions, Positive/lifetimes, Real line,
     Directional, Multivariate (+ Mixed/compound later), with family subgroups and cross-listing;
   - relationship graph (visNetwork, fixed layout): node opens the distribution, edge opens a side panel with
     the condition in KaTeX and a simulation demo;
   - searchable table.
3. **Explorer**: sliders + numeric inputs, parameterization switch, PDF/PMF and CDF in separate panels (step CDF
   for discrete), moment value boxes ("does not exist" when undefined), story, formula cards, R syntax.
4. **Calculator**: P(X ≤ x), P(X > x), P(a < X ≤ b), quantiles, shaded areas, `<` vs `≤` for discrete, result
   as a sentence.
5. **Compare**: 2–4 distributions or one parameter sweep; colorblind-safe palette + linetypes; summary table.
6. **Simulation lab** (complete): sample histogram vs true curve, LLN (incl. Cauchy), CLT, sums and
   transformations, limit relations (Binomial → Poisson, etc.), memorylessness, Monte Carlo checks of moments
   and probabilities, every map relation as an animated demo.
7. **Fit lab**: data → descriptives → candidates → fit → ranking → diagnostics → export.
8. **About / Help**: references, notation table, onboarding tour.

## Study report (HTML)

A "Download study report" button (Explorer, Simulation lab and Fit lab) produces one self-contained `.html`
file for the selected distribution, built from what the student did in the session:

1. Cover: distribution, parameters chosen, date, seed, language.
2. Story and generative explanation, with diagram.
3. Formulas (KaTeX): PMF/PDF, CDF, quantile, moments, MGF; parameterizations side by side.
4. Numbers: exact moments vs the student's simulated values, with Monte Carlo error.
5. Interactive plots (plotly htmlwidgets, zoom and hover work offline): PDF/PMF, CDF, quantile, the student's
   simulations (histogram vs curve, LLN, CLT), parameter sweeps as plotly animation frames.
6. Calculator results the student computed, with shaded areas.
7. Relationships: special cases, transformations and limits, each with a simulation figure.
8. Fit results when data were used: estimates, SEs, ranking, diagnostics, descriptive GOF.
9. Reproducible R code appendix that regenerates every number and figure, plus the simulated data embedded
   as CSV.
10. References and session info.

Implementation: the report is assembled by the engine (`dl_report()`, usable outside Shiny) from a session
state object, rendered with htmltools and htmlwidgets, with JS/CSS dependencies inlined so the file opens
offline with no extra folder. No Quarto or pandoc requirement, so it also works on shinyapps.io and later in
shinylive. Shiny server reactivity cannot live in a static file; interactivity in the report comes from plotly
(hover, zoom, animation sliders). Report text in pt-BR or en-US following the UI language.

Global: language toggle, dark mode, presentation mode, "Copy R code" on every card, PNG/SVG export,
bookmarkable URL state (v1).

## Architecture

- **Engine (no shiny dependency)**, exported API: `dl_dist()`, `d/p/q/r` wrappers, `dl_moments()`,
  `dl_relations()`, `dl_fit()`, `dl_compare()`, `dl_plot_*()` (ggplot2), `dl_code()`.
- **Registry**: `inst/registry/<id>.yml`, one file per distribution (names en/pt-BR, group, subgroup, tier,
  support, parameters with domain/default/slider, parameterizations, function references, LaTeX, story,
  relationships, fit hints, references). Validated by a hand-written validator in testthat.
- **Moments**: closed forms as R functions keyed by id; numerical fallback (integrate split at the mode, or a
  truncated sum) with NA on divergence; nonexistent moments flagged explicitly.
- **Optional packages**: `dl_require(pkg)` checks `requireNamespace()`; if missing, a modal asks the user to
  install it (consent required; never silent). Server deployments bundle every Suggests package so nothing
  installs at runtime.
- **Plots**: the engine returns tidy data.tables; `plotly_*()` builders for screen and `gg_*()` builders for
  export/code share that data layer.
- **Modules**: compendium, graph, explore, calc, compare, sim, data, fit, diagnostics, export, settings.
- **Assets**: vendored KaTeX, `_brand.yml`, `inst/i18n/{pt-BR,en}.yml` read by a `tr()` helper.
- **Compiled code**: none at MVP; Rcpp + RcppArmadillo only where profiling shows a need.

## Stack

| Package | Role | Dependency |
|---|---|---|
| shiny, bslib, brand.yml | runtime, UI, theme | Imports |
| plotly | interactive plots | Imports |
| ggplot2 | export plots and generated code | Imports |
| visNetwork | relationship graph | Imports |
| data.table, dplyr, tidyr, purrr | data handling | Imports |
| yaml, jsonlite | registry, state | Imports |
| extraDistr | Laplace, Beta-binomial, Dirichlet, Categorical, discrete uniform | Imports |
| goftest, numDeriv | AD/CvM, Hessian SEs and derivative checks | Imports |
| gkwdist, actuar, mvtnorm, sn, evd, VGAM, gamlss.dist | extended families | Suggests |
| fitdistrplus | Cullen–Frey, cross-checks | Suggests |
| htmltools, htmlwidgets | self-contained HTML study report | Imports |
| mirai | async bootstrap | Suggests |
| testthat, shinytest2, distributions3 | tests and numeric cross-checks | Suggests |

## Catalog

- **MVP (T1, ~25)**: Bernoulli, Binomial, Geometric, Negative binomial, Poisson, Hypergeometric, Beta-binomial,
  Discrete uniform, Uniform, Beta, Exponential, Gamma, Chi-square, Weibull, Lognormal, F, Normal, Student t,
  Cauchy, Laplace, Logistic, Categorical, Multinomial, Dirichlet, MVN.
- **v1 (T2, ~35)**: Erlang, inverse gamma, Rayleigh, Maxwell, Gompertz, inverse Gaussian, log-logistic, Pareto,
  Lomax, Burr, Fréchet, Gumbel, GEV, GPD, skew-normal, skew-t, truncated normal, triangular, arcsine,
  Kumaraswamy and the gkwdist family, ZIP/ZINB, Skellam, zeta, log-series, beta-negative-binomial, von Mises,
  MVT, Wishart.
- **v2 (T3)**: remaining entries of the pt.wikipedia list reachable on CRAN; directional, matrix-valued,
  mixtures and compound distributions.

## Fit lab

1. Input: CSV/TSV upload (`fread`, decimal comma), paste, frequency/grouped tables, built-in datasets,
   simulation from any registry entry with a seed.
2. Descriptives: n, NA, ties, zeros, negatives, integer flag, moments, histogram, ECDF.
3. Candidates suggested by support; families whose support is violated are blocked with the reason.
4. MLE on an unconstrained scale (log/logit), `nlminb` with registry starts + multistart; numDeriv Hessian
   SEs, delta method, Wald CIs; boundary estimates flagged.
5. Ranking: logLik, k, AIC, BIC, ΔAIC, Akaike weights, only among comparable likelihoods.
6. Diagnostics: density/PMF overlay, ECDF vs CDF, Q-Q with envelope, P-P.
7. GOF: AD/CvM/KS (continuous), chi-square with merged bins (discrete); descriptive labels; bootstrap on
   demand.
8. Export: tables, plots, R code.

## Quality

- Engine tests: d/p/q/r consistency, densities integrate (PMFs sum) to 1, moments closed form vs numeric vs
  Monte Carlo, cross-checks against distributions3/extraDistr, every registry relationship verified
  numerically, edge cases (extreme parameters, Beta shapes < 1, heavy tails, tiny n, constant data, NA/Inf),
  numDeriv checks for derivatives, registry validator.
- `testServer()` for modules, shinytest2 smoke tests (`skip_on_cran()`), accessibility checks.
- GitHub Actions matrix (Linux/macOS/Windows; release/devel/oldrel). One branch/PR per batch; merge on green CI.

## Content rules

- Stories and diagrams written from scratch (the reference cheat sheet is copyrighted); pt.wikipedia used only
  as a list of names.
- Every relationship edge re-derived and cited (Leemis & McQueston, 2008) and verified in tests. The reference
  map mixes rate and scale for λ, has a garbled N(0,1) → χ² label and misses Z² → χ²₁.

## Roadmap

- **MVP (0.1.0)**: T1 catalog; Compendium (3 views); Explorer; Calculator; Compare; full Simulation lab; Fit lab
  (raw + frequency data, AIC/BIC, diagnostics, descriptive GOF); pt-BR/en-US; dark mode; own brand and logo;
  Copy R code; PNG/SVG export; presentation mode; optional-package install flow; HTML study report.
- **v1**: T2 catalog + gkwdist family; bootstrap GOF and CIs; quantile/hazard plots; URL state; shinylive build;
  onboarding tour.
- **v2**: T3 catalog; multivariate and directional visuals and fitting; mixtures; printable cheat sheet
  generated from the registry.

## Delivery batches (one branch/PR each, merge on green CI)

1. `feat/t1-catalog`: 21 univariate T1 entries, parameter-dependent support, moment existence (Inf vs
   undefined), relationship edges verified numerically, Explorer fix for collapsed sidebar.
2. `feat/brand-shell`: brand and logo, `_brand.yml`, navbar shell, Home, live language toggle, KaTeX formulas,
   "Related distributions" card in the Explorer (every registry edge, with condition in KaTeX, click to open).
3. `feat/compendium` (priority): relationship graph of all distributions (node opens the distribution, edge shows
   type and condition), data-type view, family subgroups, search table.
4. `feat/calculator-compare`: probability calculator with shaded areas; Compare page.
5. `feat/simulation-lab`: LLN, CLT, sums/transformations, limits, memorylessness, Monte Carlo checks.
6. `feat/fit-lab`: data input (raw and frequency tables), MLE engine, ranking, diagnostics, GOF.
7. `feat/multivariate`: Categorical, Multinomial, Dirichlet, MVN cards.
8. `feat/study-report`: self-contained HTML report.
9. `feat/export-presentation`: Copy R code, PNG/SVG export, presentation mode, optional-package install flow.

## Open items

- Brand and logo are implemented (batch 2); owner feedback can still lead to refinements.
- Confirm license (proposal: code MIT, content CC BY 4.0).
- Confirm CRAN as a target.

## Status (2026-11-01)

| Batch | Branch | PR | State |
|---|---|---|---|
| 1 | feat/t1-catalog | #1 | merged |
| 2 | feat/brand-shell | #2 | merged |
| 3 | feat/compendium | #3 | merged |
| 4 | feat/calculator-compare | #4 | merged |
| 5 | feat/simulation-lab (included in codex/fit-lab) | #5 | implemented through 21b1fc5; published with batch 6; review/CI/merge pending |
| 6 | codex/fit-lab | #5 | implemented, validated and committed as e9f2df6; PR open against main; CI started |
| 7 | feat/multivariate | — | not implemented; four catalog entries/cards pending |
| 8 | feat/study-report | — | not implemented; dl_report() and session export pending |
| 9 | feat/export-presentation | — | partial groundwork: copy-code controls exist; remaining scope pending |

Status includes the Simulation base at `21b1fc5`, Fit commit `e9f2df6` and live GitHub inspection on
2026-11-01. Batches 1–4 are merged; batches 5/6 are published together in
[PR #5](https://github.com/evandeilton/distlab/pull/5), with five CI jobs started; batches 7–9 remain queued.
Branch `codex/fit-lab` includes the unmerged Simulation base. No merge has been performed.

### Remaining MVP work, in delivery order

**5. Close Simulation (`feat/simulation-lab`).** The engine and six-tab page are implemented.

- [ ] Review the complete diff against main and address findings.
- [ ] Validate the UI in pt-BR/en-US, light/dark mode and desktop/mobile layouts.
- [x] Install the package, run the browser smoke test and R CMD check (verified on `codex/fit-lab`).
- [ ] Obtain green results in all five CI jobs and merge (PR #5 is open).
- [ ] Update the handoff and restart the preview after landing.

**6. Fit lab (`codex/fit-lab`).** Locally implemented and validated on 2026-11-01.
This branch starts from `21b1fc5` to preserve the Simulation work; closing batch 5 remains a separate task.
Three specialists cover estimation, data/diagnostics and bilingual UI; Sol/Codex orchestrates integration
and validation. No assistant coauthor trailers are permitted.

- [x] Raw CSV/TSV/pasted data, frequency/grouped tables, example data and seeded simulated data.
- [x] Descriptive summaries, histogram/ECDF and support-based candidate selection.
- [x] MLE engine (`dl_fit()`), parameter constraints, multistart, standard errors and confidence intervals.
- [x] Comparable-likelihood ranking: logLik, AIC, BIC, delta AIC and Akaike weights.
- [x] Density/PMF, ECDF/CDF, Q-Q and P-P diagnostics; descriptive GOF with estimated-parameter labels.
- [x] Bilingual Shiny page, reproducible R code, validation and tests. Bootstrap GOF/CIs remain v1 scope.

Local validation: complete devtools suite passed (17,893 expectations, no failures/warnings; installed-only
browser test skipped there), followed by additional regression checks, successful installed-package Chrome
smoke test and final full R CMD check with 0 errors, 0 warnings and 0 notes. Desktop/mobile, language/theme,
CSV/R downloads, script reproduction and fitted-model navigation were checked. Preview: port 4322;
the pre-existing process on 4321 was preserved. See `COLLABORATION.md` and `docs/architecture.md`.

Published in [PR #5](https://github.com/evandeilton/distlab/pull/5), against `main`, with the existing
Simulation commits preserved and Fit commit `e9f2df6` authored/committed solely by the owner. The PR was
created by `evandeilton`; no assistant coauthor trailers were added to the new commits. Five remote CI jobs
have started and must pass before merge. The plan and collaboration handoff are versioned with the code.

**7. Complete T1 catalog (`feat/multivariate`).**

- [ ] Add Categorical, Multinomial, Dirichlet and multivariate Normal entries/cards.
- [ ] Include bilingual stories, support, parameters, formulas and references.
- [ ] Integrate cards into catalog navigation and tests; multivariate cards remain view-only in the MVP.

**8. Study report (`feat/study-report`).**

- [ ] Capture the student's session state: parameters, calculations, simulations and fits.
- [ ] Implement `dl_report()` and download actions in Explorer, Simulation and Fit.
- [ ] Produce one self-contained bilingual HTML file with offline interactive plots, formulas,
  reproducible R code, embedded CSV data, references and session information, without Quarto/pandoc.
- [ ] Validate report completeness and offline operation.

**9. Export and presentation (`feat/export-presentation`).**

- [ ] Audit existing copy-code controls and cover the remaining cards/results.
- [ ] Add PNG/SVG export and presentation mode.
- [ ] Implement the optional-package availability/install flow with explicit consent.
- [ ] Validate bilingual behavior, accessibility and desktop/mobile layouts across the completed MVP.

Before declaring 0.1.0 complete, reconcile the final implementation with the MVP roadmap and planned
public API, including the parameterization switch and plot/code helpers. Existing copy-code controls
do not mean the entire export batch is complete. T2/T3 catalogs, shinylive, URL state, onboarding and
bootstrap GOF/CIs remain in the v1/v2 roadmap; they are not additional MVP delivery batches.

Workflow per batch: specialist agent implements on the batch branch -> independent reviewer -> fixes ->
push -> PR with Auto-fix -> merge when all 5 CI jobs are green -> next batch from main. Restart the preview
(`.claude/launch.json`, config "distlab", port 4321) whenever features land.

Simulation base (5, `feat/simulation-lab`): engine, six-tab UI, relation demos, seeded R code and refinements
are implemented in four commits after main. The owner requested starting batch 6 on `codex/fit-lab`,
which now includes the locally validated Fit lab. Local package/browser checks cover this combined state;
the remaining Simulation visual review and remote CI/merge still require closure.
Next delivery step: review PR #5 and its CI, address findings, and merge only after the appropriate review;
then proceed to batch 7. No merge has been authorized or performed in this publication step.
See `COLLABORATION.md` for the shared handoff and validation record;
all assistants must follow the owner's identity and change-log instructions in `AGENTS.md` / `CLAUDE.md`.

Known limitations to revisit:
- Slider tick labels use a decimal point (ion.rangeSlider has no decimal-mark option).
- Compendium: selecting an edge from another view does not zoom in; mobile and print layouts not screenshotted.
- Long supports (Hypergeometric) scroll sideways in Compendium cards.
- F characteristic function verified numerically only for odd d2.
- Calculator does not link onward to Compare/Explorer; sweep accepts 2–5 values.
- `also_in` cross-listing implemented but unused by any entry.

Assets: brand canvas https://claude.ai/artifact/KuEjuVBGitRfRHZYoR81c1; brand files in inst/brand and
inst/app/www. Remaining open items: license confirmation (MIT code / CC BY 4.0 content), CRAN target, T2 catalog,
shinylive build.
