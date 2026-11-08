# Multivariate reference cards: registry entries, their formulas and
# relationships checked numerically, their exclusion from the univariate
# tools and their routing in the app.

mv_ids <- c("categorical", "dirichlet", "multinomial", "mvnormal")
mv_entry <- function(id) dl_entry(id, multivariate = TRUE)

# All count vectors of length k that add to n.
compositions <- function(n, k) {
  if (k == 1L) return(matrix(n, 1L, 1L))
  do.call(rbind, lapply(0:n, function(j) cbind(j, compositions(n - j, k - 1L))))
}

# Bivariate normal density, written from the registry formula with base R.
dmvn <- function(x, mu, sigma) {
  d <- x - mu
  q <- sum(d * solve(sigma, d))
  (2 * pi)^(-length(mu) / 2) * det(sigma)^(-1 / 2) * exp(-q / 2)
}

test_that("the multivariate entries load, validate and stay out of the default registry", {
  all <- dl_registry(refresh = TRUE, dimension = c("univariate", "multivariate"))
  expect_length(all, 25)
  expect_setequal(dl_ids(dimension = "multivariate"), mv_ids)
  expect_false(any(mv_ids %in% dl_ids()))
  expect_false(any(mv_ids %in% names(dl_registry())))
  for (id in mv_ids) {
    e <- all[[id]]
    expect_identical(distlab:::validate_entry(e), character(), info = id)
    expect_identical(e$group, "multivariate")
    expect_identical(e$tier, 1L)
  }
  expect_error(dl_ids(dimension = "both"), "dimension")
  expect_error(dl_registry(dimension = character()), "dimension")
})

test_that("dl_catalog() and dl_relations() include the multivariate entries", {
  cat <- dl_catalog()
  expect_identical(cat$id, dl_ids(dimension = c("univariate", "multivariate")))
  expect_setequal(cat$id[cat$dimension == "multivariate"], mv_ids)
  expect_identical(cat$n_params[cat$id == "mvnormal"], 2L)
  rel <- dl_relations()
  mv_rel <- rel[rel$from %in% mv_ids, ]
  expect_setequal(paste(mv_rel$from, mv_rel$to, mv_rel$type), c(
    "categorical multinomial sum", "categorical bernoulli special_case",
    "multinomial categorical special_case", "multinomial binomial transformation",
    "dirichlet beta special_case", "dirichlet beta transformation",
    "mvnormal normal transformation"))
  expect_true(all(nzchar(mv_rel$label)))
  expect_identical(nrow(dl_relations("dirichlet")), 2L)
  expect_true(any(dl_relations("binomial")$from == "multinomial"))
})

test_that("the multivariate validator reports broken entries", {
  e <- mv_entry("multinomial")
  check <- function(bad, pattern) {
    expect_match(distlab:::validate_entry(bad), pattern, all = FALSE)
  }
  bad <- e; bad$parameters[[2]]$description[["pt-BR"]] <- NULL
  check(bad, "needs a description")
  bad <- e; bad$parameters[[2]]$example <- c(-0.2, 0.7, 0.5)
  check(bad, "below its lower bound")
  bad <- e; bad$parameters[[1]]$example <- 2.5
  check(bad, "must be whole")
  bad <- e; bad$parameters[[2]]$example <- c(0.2, 0.2, 0.2)
  check(bad, "fails at the example values")
  bad <- e; bad$constraints <- list("system('ls') == 0")
  check(bad, "not a valid expression")
  bad <- e; bad$latex$covariance <- NULL
  check(bad, "missing latex key: covariance")
  bad <- e; bad$latex$variance <- list(expr = "n")
  check(bad, "unknown latex key")
  bad <- e; bad$latex$pdf <- "\\foo{x}"
  check(bad, "unsupported macro")
  bad <- e; bad$group <- "counts"
  check(bad, "multivariate group")
  bad <- e; bad$functions$q <- "stats::qbinom"
  check(bad, "may only be d and r")
  bad <- e; bad$functions$d <- "stats::dpois"
  check(bad, "no argument")
  bad <- e; bad$support <- list(lower = 0)
  check(bad, "support needs a latex string")
  bad <- e; bad$dimension <- "matrix"
  check(bad, "dimension must be")
  bad <- e; bad$functions_note <- list(`en-US` = "Only English.")
  check(bad, "functions_note")
  # Covariance examples must be positive definite, which det > 0 misses.
  m <- mv_entry("mvnormal")
  bad <- m
  bad$parameters[[1]]$example <- c(0, 0, 0)
  bad$parameters[[2]]$example <- list(c(1, 2, 2), c(2, 1, 2), c(2, 2, 1))
  expect_gt(det(matrix(c(1, 2, 2, 2, 1, 2, 2, 2, 1), 3)), 0)
  check(bad, "symmetric positive definite")
  bad$parameters[[2]]$example <- list(c(1, 0.5), c(0.4, 1))
  bad$parameters[[1]]$example <- c(0, 0)
  check(bad, "symmetric positive definite")
  expect_null(distlab:::mv_illustration_data(bad))
  # The vector macros stay out of univariate entries.
  u <- dl_entry("normal")
  u$latex$pdf <- "f(x) = x^{\\top}"
  expect_match(distlab:::validate_entry(u), "unsupported macro", all = FALSE)
})

test_that("multivariate LaTeX is balanced and conditions are math only", {
  macros <- c(distlab:::.dl_latex_macros, distlab:::.dl_mv_latex_macros)
  for (id in mv_ids) {
    e <- mv_entry(id)
    expect_identical(distlab:::validate_mv_latex(e$latex, macros), character(), info = id)
    expect_identical(distlab:::latex_string_problems(distlab:::support_tex(e), id,
                                                     macros = macros), character())
    expect_identical(distlab:::latex_string_problems(distlab:::params_tex(e), id,
                                                     macros = macros), character())
  }
})

test_that("Multinomial formulas and relations hold exactly", {
  n <- 6
  p <- c(0.2, 0.5, 0.3)
  x <- compositions(n, 3)
  w <- apply(x, 1, stats::dmultinom, size = n, prob = p)
  expect_equal(sum(w), 1)
  # Mean, covariance and the mgf by enumeration.
  m <- unname(colSums(w * x))
  expect_equal(m, n * p)
  cov <- unname(crossprod(x * sqrt(w))) - tcrossprod(m)
  want <- -n * tcrossprod(p)
  diag(want) <- n * p * (1 - p)
  expect_equal(cov, want)
  t <- c(0.3, -0.4, 0.1)
  expect_equal(sum(w * exp(x %*% t)), sum(p * exp(t))^n)
  # Each count is Binomial(n, p_i).
  for (i in 1:3) {
    marg <- vapply(0:n, function(j) sum(w[x[, i] == j]), numeric(1))
    expect_equal(marg, stats::dbinom(0:n, n, p[i]), info = i)
  }
  # One trial is a Categorical draw.
  for (i in 1:3) {
    e <- replace(numeric(3), i, 1)
    expect_equal(stats::dmultinom(e, 1, p), extraDistr::dcat(i, p))
  }
})

test_that("Categorical formulas and relations hold exactly", {
  p <- c(0.1, 0.4, 0.3, 0.2)
  k <- length(p)
  onehot <- diag(k)
  expect_equal(extraDistr::dcat(1:k, p), p)
  m <- colSums(p * onehot)
  expect_equal(m, p)
  cov <- crossprod(onehot * sqrt(p)) - tcrossprod(m)
  want <- -tcrossprod(p)
  diag(want) <- p * (1 - p)
  expect_equal(cov, want)
  t <- c(0.2, -0.5, 1, 0.3)
  expect_equal(sum(p * exp(onehot %*% t)), sum(p * exp(t)))
  # With k = 2 the first coordinate is Bernoulli(p_1).
  q <- c(0.35, 0.65)
  expect_equal(extraDistr::dcat(1, q), dl_density(dl_dist("bernoulli", prob = q[1]), 1))
  expect_equal(extraDistr::dcat(2, q), dl_density(dl_dist("bernoulli", prob = q[1]), 0))
  # The sum of n one-hot vectors is Multinomial(n, p): all k^n sequences.
  n <- 4
  seqs <- as.matrix(expand.grid(rep(list(1:3), n)))
  q3 <- c(0.2, 0.5, 0.3)
  prob <- apply(seqs, 1, function(s) prod(q3[s]))
  counts <- t(apply(seqs, 1, tabulate, nbins = 3))
  key <- apply(counts, 1, paste, collapse = "-")
  got <- tapply(prob, key, sum)
  x <- compositions(n, 3)
  want <- apply(x, 1, stats::dmultinom, size = n, prob = q3)
  names(want) <- apply(x, 1, paste, collapse = "-")
  expect_equal(as.vector(got[names(want)]), unname(want))
})

test_that("Dirichlet formulas and relations hold", {
  a <- c(2, 3, 4)
  a0 <- sum(a)
  # With k = 2 the density of X_1 is Beta(alpha_1, alpha_2).
  for (t in c(0.1, 0.35, 0.8)) {
    expect_equal(extraDistr::ddirichlet(c(t, 1 - t), c(2.5, 1.5)),
                 stats::dbeta(t, 2.5, 1.5), info = t)
  }
  # The marginal of X_1 integrates the joint density over the other parts.
  for (t in c(0.05, 0.3, 0.6)) {
    f <- function(u) extraDistr::ddirichlet(cbind(t, u, 1 - t - u), a)
    marg <- stats::integrate(f, 0, 1 - t, rel.tol = 1e-10)$value
    expect_equal(marg, stats::dbeta(t, a[1], a0 - a[1]), tolerance = 1e-7, info = t)
  }
  # Mean and covariance by integration of the Beta marginals and E(X_1 X_2).
  expect_equal(stats::integrate(function(t) t * stats::dbeta(t, a[2], a0 - a[2]), 0, 1)$value,
               a[2] / a0, tolerance = 1e-8)
  cross <- stats::integrate(function(s) vapply(s, function(t) {
    stats::integrate(function(u) t * u * extraDistr::ddirichlet(cbind(t, u, 1 - t - u), a),
                     0, 1 - t, rel.tol = 1e-10)$value
  }, numeric(1)), 0, 1, rel.tol = 1e-8)$value
  cov12 <- cross - a[1] * a[2] / a0^2
  expect_equal(cov12, -a[1] * a[2] / (a0^2 * (a0 + 1)), tolerance = 1e-6)
  var1 <- a[1] * (a0 - a[1]) / (a0^2 * (a0 + 1))
  b <- c(a[1], a0 - a[1])
  expect_equal(var1, prod(b) / (sum(b)^2 * (sum(b) + 1)))
})

test_that("Multivariate normal formulas and relations hold", {
  mu <- c(1, -2)
  sigma <- matrix(c(2, 0.9, 0.9, 1.5), 2)
  # The registry density integrates over x2 to the Normal marginal of x1.
  for (x1 in c(-1, 0.5, 2.7)) {
    f <- function(x2) vapply(x2, function(v) dmvn(c(x1, v), mu, sigma), numeric(1))
    marg <- stats::integrate(f, -Inf, Inf, rel.tol = 1e-10)$value
    expect_equal(marg, stats::dnorm(x1, mu[1], sqrt(sigma[1, 1])), tolerance = 1e-8)
  }
  # A linear combination a'X is N(a'mu, a'Sigma a): draws through chol().
  set.seed(71)
  z <- matrix(stats::rnorm(2 * 2e4), 2)
  x <- t(mu + t(chol(sigma)) %*% z)
  a <- c(0.7, -1.2)
  y <- drop(x %*% a)
  d <- dl_dist("normal", mean = sum(a * mu), sd = sqrt(drop(a %*% sigma %*% a)))
  stat <- suppressWarnings(stats::ks.test(y, function(q) dl_cdf(d, q))$statistic)
  expect_lt(unname(stat), 1.95 / sqrt(length(y)))
  # The mgf matches the Gaussian integral of exp(t'x) over the density.
  t <- c(0.2, -0.1)
  # Integrate over 12 standard deviations around the mean; the rest is negligible.
  h <- 12 * sqrt(diag(sigma))
  inner <- function(v1) vapply(v1, function(u) {
    stats::integrate(function(v2) vapply(v2, function(w) {
      exp(sum(t * c(u, w))) * dmvn(c(u, w), mu, sigma)
    }, numeric(1)), mu[2] - h[2], mu[2] + h[2], rel.tol = 1e-10)$value
  }, numeric(1))
  mgf <- stats::integrate(inner, mu[1] - h[1], mu[1] + h[1], rel.tol = 1e-9)$value
  expect_equal(mgf, exp(sum(t * mu) + drop(t %*% sigma %*% t) / 2), tolerance = 1e-6)
})

test_that("illustration data match the example values and keep the RNG state", {
  set.seed(5)
  before <- .Random.seed
  d <- distlab:::mv_illustration_data(mv_entry("mvnormal"))
  expect_identical(.Random.seed, before)
  sigma <- matrix(c(1, 0.6, 0.6, 1), 2)
  for (i in seq_along(d$ellipses)) {
    dev <- sweep(d$ellipses[[i]], 2, d$mean)
    q <- rowSums((dev %*% solve(sigma)) * dev)
    expect_equal(q, rep(stats::qchisq(d$levels[i], 2), nrow(dev)), info = i)
  }
  expect_identical(dim(d$draws), c(300L, 2L))
  expect_identical(d, distlab:::mv_illustration_data(mv_entry("mvnormal")))
  dd <- distlab:::mv_illustration_data(mv_entry("dirichlet"))
  expect_equal(rowSums(dd$draws), rep(1, 300))
  expect_equal(dd$mean, c(2, 3, 4) / 9)
  dm <- distlab:::mv_illustration_data(mv_entry("multinomial"))
  expect_equal(dm$y, c(2, 5, 3))
  expect_equal(dm$sd, sqrt(10 * c(0.2, 0.5, 0.3) * c(0.8, 0.5, 0.7)))
  expect_equal(distlab:::mv_illustration_data(mv_entry("categorical"))$y, c(0.1, 0.4, 0.3, 0.2))
})

test_that("univariate tools refuse or leave out the multivariate ids", {
  for (id in mv_ids) {
    expect_error(dl_dist(id), "multivariate distribution", info = id)
  }
  expect_error(dl_fit(c(0.2, 0.5, 0.3), "dirichlet"), "multivariate distribution")
  expect_error(dl_relation_demo("multinomial", "binomial"), "multivariate distribution")
  expect_error(dl_relations("nope"), "Unknown distribution id")
  ids <- unlist(lapply(c("en-US", "pt-BR"), distlab:::dist_choices))
  expect_false(any(mv_ids %in% ids))
  expect_setequal(ids, dl_ids())
  expect_false(any(mv_ids %in% dl_fit_candidates(c(1, 2, 3))$id))
  edges <- unlist(distlab:::edge_choices("en-US"))
  expect_false(any(grepl(paste(mv_ids, collapse = "|"), edges)))
  expect_setequal(edges, names(distlab:::.dl_sim_recipes))
  # Home search and the data-type tiles do list them.
  expect_true(all(mv_ids %in% distlab:::search_choices("pt-BR")))
  expect_true("multivariate" %in% distlab:::registry_groups())
  expect_setequal(distlab:::group_members("multivariate"), mv_ids)
})

test_that("the card renders in both languages", {
  for (lang in c("en-US", "pt-BR")) {
    for (id in mv_ids) {
      e <- mv_entry(id)
      html <- as.character(distlab:::mv_card_tag(e, lang, "c-open"))
      expect_match(html, distlab:::tr("card.badge", lang), fixed = TRUE, info = id)
      expect_match(html, '<svg class="dl-mv-plot"[^>]*role="img"', info = id)
      expect_match(html, sprintf('aria-labelledby="dl-mv-cap-%s"', id), fixed = TRUE)
      expect_match(html, sprintf('<figcaption id="dl-mv-cap-%s"', id), fixed = TRUE)
      expect_match(html, distlab:::tr("formula.covariance", lang), fixed = TRUE)
      expect_match(html, "\\operatorname{Cov}(X_i, X_j)", fixed = TRUE)
      expect_match(html, substr(e$story[[lang]], 1, 30), fixed = TRUE)
      expect_match(html, 'data-input="c-open"', fixed = TRUE)
      expect_no_match(html, "card\\.|formula\\.")
    }
  }
  pt <- as.character(distlab:::mv_card_tag(mv_entry("mvnormal"), "pt-BR", "o"))
  expect_match(pt, "μ = (0; 0), variâncias 1 e 1 e correlação 0,6", fixed = TRUE)
  expect_match(pt, 'aria-label="Abrir Normal no Explorador"', fixed = TRUE)
  en <- as.character(distlab:::mv_card_tag(mv_entry("multinomial"), "en-US", "o"))
  expect_match(en, "n = 10 and p = (0.2, 0.5, 0.3)", fixed = TRUE)
  expect_match(en, 'aria-label="Open the Categorical reference card"', fixed = TRUE)
  expect_match(en, "stats::dmultinom", fixed = TRUE)
  expect_no_match(as.character(distlab:::mv_card_tag(mv_entry("mvnormal"), "en-US", "o")),
                  "R functions")
  # Every draw of the bivariate Normal stays inside the window.
  svg <- as.character(distlab:::mv_figure_tag(mv_entry("mvnormal"), "en-US"))
  expect_identical(lengths(regmatches(svg, gregexpr('class="dl-mv-point"', svg))), 300L)
  cat_pt <- as.character(distlab:::mv_card_tag(mv_entry("categorical"), "pt-BR", "o"))
  expect_match(cat_pt, "dcat() e rcat() trabalham com o índice da categoria", fixed = TRUE)
})

test_that("the card dialog opens over any page and follows the language", {
  req <- shiny::reactiveVal(NULL)
  lang <- shiny::reactiveVal("en-US")
  shiny::testServer(distlab:::mod_card_server, args = list(lang = lang, request = req), {
    req(list(card = "dirichlet", n = 1))
    session$flushReact()
    expect_identical(card()$id, "dirichlet")
    expect_identical(output$card_title, "Dirichlet")
    expect_match(output$card$html, "\\mathrm{Beta}(\\alpha_i, \\alpha_0 - \\alpha_i)",
                 fixed = TRUE)
    expect_null(session$returned())
    lang("pt-BR")
    session$flushReact()
    expect_match(output$card$html, "Formulário")
    # Unknown or univariate ids do not open a card.
    req(list(card = "gamma", n = 2))
    session$flushReact()
    expect_identical(card()$id, "dirichlet")
    # Links inside the card open another card or a univariate law.
    session$setInputs(open = "mvnormal")
    expect_identical(card()$id, "mvnormal")
    session$setInputs(open = "normal")
    expect_identical(session$returned()[c("tab", "id")], list(tab = "explorer", id = "normal"))
    expect_null(card())
    session$setInputs(open = "<script>")
    expect_identical(session$returned()$id, "normal")
  })
  # The dialog is named by its title.
  html <- as.character(distlab:::card_modal(shiny::NS("card"), "en-US"))
  expect_match(html, 'role="dialog"', fixed = TRUE)
  expect_match(html, 'aria-modal="true"', fixed = TRUE)
  expect_match(html, 'aria-labelledby="card-card_title"', fixed = TRUE)
  expect_match(html, 'id="card-card_title"', fixed = TRUE)
})

test_that("the Compendium asks for cards and keeps tools to univariate laws", {
  lang <- shiny::reactiveVal("en-US")
  shiny::testServer(distlab:::mod_compendium_server, args = list(lang = lang), {
    session$setInputs(open = "dirichlet")
    expect_identical(session$returned()$card, "dirichlet")
    expect_null(session$returned()$tab)
    session$setInputs(dblclick = "mvnormal")
    expect_identical(session$returned()$card, "mvnormal")
    n <- session$returned()$n
    # The interactive pages are not offered for a multivariate law.
    session$setInputs(calc = "dirichlet", compare = "dirichlet", sim = "dirichlet",
                      sim_edge = "dirichlet__beta__special_case")
    expect_identical(session$returned()$n, n)
    session$setInputs(open = "beta")
    expect_identical(session$returned()[c("tab", "id")], list(tab = "explorer", id = "beta"))
    lang("pt-BR")
    session$setInputs(search = "multinomial")
    expect_identical(sel(), list(kind = "node", id = "multinomial"))
    expect_match(output$panel$html, "Abrir a ficha de consulta")
    expect_no_match(output$panel$html, "Calculadora")
  })
  html <- as.character(distlab:::edge_panel(
    {
      r <- dl_relations()
      r$id <- distlab:::edge_ids(r)
      r[r$id == "multinomial__binomial__transformation", ]
    }, "en-US", "o", sim_id = "s"))
  expect_no_match(html, "Simulate")
  expect_match(html, "Open the Multinomial reference card", fixed = TRUE)
})

test_that("Home and the Explorer ask for the card without leaving the page", {
  shiny::testServer(distlab:::mod_home_server, args = list(lang = "en-US"), {
    session$setInputs(search = "dirichlet")
    expect_identical(session$returned()$card, "dirichlet")
    expect_null(session$returned()$tab)
    expect_identical(output$status, "")
    session$setInputs(pick = "mvnormal")
    expect_identical(session$returned()$card, "mvnormal")
  })
  html <- as.character(distlab:::group_picker(shiny::NS("home"), "pt-BR"))
  expect_match(html, 'id="home-tile_multivariate"', fixed = TRUE)
  expect_match(html, "Ex.: votos divididos entre partidos", fixed = TRUE)
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "binomial")
    session$setInputs(goto = "multinomial")
    expect_identical(session$returned$route()$card, "multinomial")
    expect_null(session$returned$route()$tab)
    expect_identical(input$dist, "binomial")
  })
  html <- as.character(distlab:::related_list_tag("binomial", "en-US", "x"))
  expect_match(html, "Open the Multinomial reference card", fixed = TRUE)
})

test_that("the study report lists relations to multivariate laws as text", {
  rel <- distlab:::report_edges("binomial")
  expect_true("multinomial__binomial__transformation" %in% rel$id)
  ctx <- distlab:::report_context("en-US", "binomial")
  sec <- distlab:::report_relations(ctx, dl_dist("binomial"), list())
  html <- as.character(sec[[length(sec)]])
  expect_match(html, "Multinomial")
  expect_match(html, "described in its reference card", fixed = TRUE)
  sec <- distlab:::report_story(ctx, dl_dist("binomial"))
  html <- as.character(sec[[length(sec)]])
  expect_match(html, "Multinomial")
})

test_that("the map places the new nodes and curves parallel edges apart", {
  n <- distlab:::graph_nodes("en-US")
  expect_true(all(mv_ids %in% n$id))
  expect_identical(n$shape[n$id == "mvnormal"], "ellipse")
  expect_identical(n$shape[n$id == "multinomial"], "box")
  e <- distlab:::graph_edges("en-US")
  db <- e[e$from == "dirichlet" & e$to == "beta", ]
  expect_identical(nrow(db), 2L)
  expect_identical(sum(db$smooth.enabled), 1L)
  expect_identical(db$smooth.type[db$smooth.enabled], "curvedCCW")
  expect_true(all(e$smooth.enabled[e$from == "categorical" & e$to == "multinomial"]))
  # Three or more parallel edges get distinct curves, also next to a reverse edge.
  reg <- distlab:::catalog_registry()
  extra <- reg$dirichlet$relationships[[2]]
  reg$dirichlet$relationships <- c(reg$dirichlet$relationships,
                                   list(modifyList(extra, list(type = "limit")),
                                        modifyList(extra, list(type = "sum"))))
  reg$beta$relationships <- c(reg$beta$relationships,
                              list(modifyList(extra, list(to = "dirichlet"))))
  e <- distlab:::graph_edges("en-US", reg = reg)
  db <- e[e$from == "dirichlet" & e$to == "beta", ]
  expect_identical(nrow(db), 4L)
  curve <- paste(db$smooth.enabled, db$smooth.type, db$smooth.roundness)
  expect_false(anyDuplicated(curve) > 0)
  back <- e[e$from == "beta" & e$to == "dirichlet", ]
  # The reverse edge curves to the counterclockwise side of dirichlet -> beta.
  expect_false(any(db$smooth.type == "curvedCCW" & db$smooth.roundness == back$smooth.roundness))
})
