# Session record of the study and the report downloads of the app pages.

test_that("the store keeps the last questions and merges quick edits", {
  st <- distlab:::study_store(max_calc = 3, window = 2)
  d <- dl_dist("normal")
  q <- function(x, mode = "le") {
    distlab:::calc_question(distlab:::calc_result(d, mode, x = x, p = 0.5, prob = 0.5), d)
  }
  t0 <- as.POSIXct("2026-01-01 10:00:00", tz = "UTC")
  expect_true(st$add_calc(q(1), now = t0))
  # The same mode within the window replaces the question (typing, sliders).
  st$add_calc(q(1.5), now = t0 + 1)
  calcs <- st$get("normal")$calculations
  expect_length(calcs, 1L)
  expect_identical(calcs[[1]]$x, 1.5)
  st$add_calc(q(2), now = t0 + 10)
  st$add_calc(q(0.5, "gt"), now = t0 + 11)
  expect_false(st$add_calc(q(2), now = t0 + 30))
  st$add_calc(q(3), now = t0 + 40)
  calcs <- st$get("normal")$calculations
  expect_identical(vapply(calcs, `[[`, "", "mode"), c("le", "gt", "le"))
  expect_identical(vapply(calcs, `[[`, numeric(1), "x"), c(2, 0.5, 3))
  expect_length(st$get("gamma")$calculations, 0L)
  # The questions are what dl_report() takes.
  recs <- distlab:::report_calculations(d, calcs)
  expect_equal(exp(recs[[3]]$result$log_p), pnorm(3))
  st$clear()
  expect_length(st$get("normal")$calculations, 0L)
})

test_that("the store keeps the latest simulation of each kind, every demo and fits", {
  st <- distlab:::study_store()
  d <- dl_dist("exponential")
  st$add_sim(distlab:::mc_check_run(d, 100, 1))
  st$add_sim(dl_lln(d, 100, seed = 2))
  st$add_sim(dl_lln(d, 50, seed = 3))
  st$add_sim(dl_relation_demo("exponential", "gamma", n = 100, seed = 4))
  st$add_sim(dl_relation_demo("binomial", "poisson"))
  expect_false(st$add_sim(list(a = 1)))
  rec <- st$get("exponential")
  expect_identical(vapply(rec$simulations, distlab:::sim_kind, ""), c("mc_check", "lln", "relation"))
  expect_identical(rec$simulations[[2]]$seed, 3L)
  # Limits have no seed, so the last seed stays the one of the gamma demo.
  expect_identical(rec$seed, 4L)
  expect_length(st$get("poisson")$simulations, 1L)
  expect_length(st$get("gamma")$simulations, 1L)
  fit <- dl_fit(c(1.2, 0.5, 2.2, 0.9), c("exponential", "gamma"))
  st$add_fit(fit)
  expect_identical(st$get("gamma")$fit, fit)
  expect_identical(st$get("exponential")$fit, fit)
  expect_null(st$get("normal")$fit)
  st$set_params("gamma", list(shape = 3, rate = 1))
  expect_identical(st$get("gamma")$params$shape, 3)
})

test_that("the Calculator records each result shown, also after a language switch", {
  st <- distlab:::study_store()
  lang <- shiny::reactiveVal("en-US")
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = lang, store = st), {
    session$setInputs(dist = "gamma", p_gamma_shape = 2, p_gamma_rate = 1, mode = "le", x = "2")
    session$elapse(200)
    expect_match(output$result$html, "0.594", fixed = TRUE)
    session$setInputs(mode = "quantile", p = "0.9")
    session$elapse(200)
    expect_match(output$result$html, "Q(0.9)", fixed = TRUE)
    lang("pt-BR")
    session$flushReact()
    session$setInputs(p = "0,9")
    session$elapse(200)
    expect_match(output$result$html, "Q(0{,}9)", fixed = TRUE)
  })
  calcs <- st$get("gamma")$calculations
  expect_identical(vapply(calcs, `[[`, "", "mode"), c("le", "quantile"))
  expect_identical(calcs[[2]]$p, 0.9)
  expect_identical(calcs[[1]]$dist$params, list(shape = 2, rate = 1))
})

test_that("the Simulation records the result of each tab it shows", {
  st <- distlab:::study_store()
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US", store = st), {
    session$setInputs(dist = "poisson", p_poisson_lambda = 3, seed = 11, n = "1000", tab = "sample")
    session$elapse(400)
    output$sample_code
    session$setInputs(tab = "lln", paths = 2)
    session$elapse(400)
    output$lln_code
    session$setInputs(tab = "relations", edge = "binomial__poisson__limit")
    output$rel_code
  })
  rec <- st$get("poisson")
  expect_identical(vapply(rec$simulations, distlab:::sim_kind, ""), c("mc_check", "lln", "relation"))
  expect_identical(rec$simulations[[1]]$seed, 11L)
  expect_identical(rec$simulations[[1]]$dist$params$lambda, 3)
  expect_length(rec$simulations[[1]]$sample, 1000L)
  expect_identical(rec$seed, 11L)
  expect_length(st$get("binomial")$simulations, 1L)
})

test_that("the report downloads build the page of the open distribution", {
  st <- distlab:::study_store()
  day <- format(Sys.Date(), "%Y-%m-%d")
  st$add_calc(distlab:::calc_question(
    distlab:::calc_result(dl_dist("bernoulli", prob = 0.3), "le", x = 0), dl_dist("bernoulli", prob = 0.3)))
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "pt-BR", store = st), {
    session$setInputs(dist = "bernoulli", p_bernoulli_prob = 0.3)
    session$elapse(200)
    output$moments
    path <- output$report
    expect_identical(basename(path), sprintf("distlab-bernoulli-%s.html", day))
    html <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    expect_match(html, "Relatório de estudo: Bernoulli", fixed = TRUE)
    expect_match(html, "Pergunta 1", fixed = TRUE)
  })
  expect_identical(st$get("bernoulli")$params, list(prob = 0.3))
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US", store = st), {
    session$setInputs(dist = "bernoulli", p_bernoulli_prob = 0.4, seed = 7, n = "500",
                      tab = "sample")
    session$elapse(400)
    output$sample_code
    path <- output$report
    html <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    expect_match(html, "Study report: Bernoulli", fixed = TRUE)
    expect_match(html, "set.seed(7)", fixed = TRUE)
    expect_match(html, "Bernoulli(prob = 0.4)", fixed = TRUE)
  })
})

test_that("the Fit lab records each fit and reports the selected family", {
  st <- distlab:::study_store()
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = "en-US", store = st), {
    session$setInputs(source = "example", example = "counts", level = ".95", load = 0, run = 0)
    session$setInputs(load = 1)
    session$setInputs(candidates = c("poisson", "geometric"), run = 1)
    session$setInputs(diagnostic_id = "poisson")
    path <- output$report
    html <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    expect_match(html, '<section class="dl-report-section" id="fit">', fixed = TRUE)
    expect_match(html, "Study report: Poisson", fixed = TRUE)
  })
  expect_s3_class(st$get("poisson")$fit, "dl_fit")
  expect_s3_class(st$get("geometric")$fit, "dl_fit")
})

test_that("the Explorer, Simulation and Fit pages offer the report", {
  for (lang in dl_languages()) {
    label <- distlab:::tr("report.download", lang)
    ui <- c(explore = as.character(distlab:::mod_explore_ui("explore", lang)),
            sim = as.character(distlab:::mod_sim_ui("sim", lang)),
            fit = as.character(distlab:::mod_fit_ui("fit", lang)))
    for (page in names(ui)) {
      expect_match(ui[[page]], sprintf('id="%s-report"', page), fixed = TRUE, info = page)
      expect_match(ui[[page]], 'data-i18n="report.download"', fixed = TRUE, info = page)
      expect_match(ui[[page]], label, fixed = TRUE, info = page)
    }
  }
  expect_identical(distlab:::tr("report.download", "pt-BR"), "Baixar relatório de estudo")
})
