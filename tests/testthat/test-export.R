# Plot downloads and code cards.

# Run generated code in a fresh environment and return that environment.
run_snippet <- function(code) {
  env <- new.env(parent = globalenv())
  eval(parse(text = code), envir = env)
  env
}

test_that("every plot gets translated PNG and SVG download buttons", {
  d <- dl_dist("normal")
  for (lang in dl_languages()) {
    p <- distlab:::explore_density_plot(dl_curve(d), d, lang, distlab:::plot_theme())
    cfg <- plotly::plotly_build(p)$x$config
    buttons <- Filter(is.list, cfg$modeBarButtonsToAdd)
    expect_identical(vapply(buttons, `[[`, "", "name"), c("dl-download-png", "dl-download-svg"))
    expect_identical(vapply(buttons, `[[`, "", "title"),
                     c(distlab:::tr("plot.download_png", lang), distlab:::tr("plot.download_svg", lang)))
    for (b in buttons) {
      expect_s3_class(b$click, "JS_EVAL")
      expect_s3_class(b$icon, "JS_EVAL")
    }
    expect_match(buttons[[1]]$click, "format: 'png'", fixed = TRUE)
    expect_match(buttons[[2]]$click, "format: 'svg'", fixed = TRUE)
    # The snapshot button of plotly would keep a transparent background.
    expect_true("toImage" %in% cfg$modeBarButtonsToRemove)
    expect_identical(cfg$toImageButtonOptions[c("width", "height", "scale")],
                     list(width = 1200, height = 700, scale = 2))
  }
  # The buttons survive the serialization of the widget.
  html <- htmltools::renderTags(htmltools::as.tags(p))$html
  expect_match(html, "config.modeBarButtonsToAdd.1.click", fixed = TRUE)
})

test_that("downloaded plots are named after page, distributions and plot", {
  f <- distlab:::plot_file_name
  expect_identical(f("explorer", "student_t", "density"), "distlab-explorer-student_t-density")
  expect_identical(f("compare", c("normal", "student_t", "student_t"), "cdf"),
                   "distlab-compare-normal-student_t-cdf")
  expect_identical(f("fit", "data", "histogram"), "distlab-fit-data-histogram")
  expect_identical(f("report", NULL, "figure 1/2"), "distlab-report-figure-1-2")
  p <- distlab:::plot_file(plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers"),
                           "simulation", "gamma", "lln")
  expect_identical(p$x$config$toImageButtonOptions$filename, "distlab-simulation-gamma-lln")
})

test_that("module plots carry their file names", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "gamma", p_gamma_shape = 3, p_gamma_rate = 2)
    session$elapse(200)
    expect_match(output$density, "distlab-explorer-gamma-density", fixed = TRUE)
    expect_match(output$cdf, "distlab-explorer-gamma-cdf", fixed = TRUE)
  })
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "poisson", p_poisson_lambda = 3, mode = "le", x = "2")
    session$elapse(200)
    expect_match(output$density, "distlab-calculator-poisson-density", fixed = TRUE)
  })
})

test_that("report figures carry numbered file names", {
  d <- dl_dist("poisson", lambda = 3)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path))
  dl_report(d, path, lang = "en-US", relations = FALSE, simulations = list())
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "distlab-report-poisson-figure-1", fixed = TRUE)
  expect_match(html, "dl-download-svg", fixed = TRUE)
})

test_that("the Explorer code card reproduces the moments and curves shown", {
  for (id in c("normal", "gamma", "poisson", "bernoulli", "laplace", "betabinomial")) {
    d <- dl_dist(id)
    code <- distlab:::explore_code(d)
    env <- run_snippet(code)
    expect_identical(dl_moments(env$d), dl_moments(d), info = id)
    expect_identical(env$curve, dl_curve(d), info = id)
  }
  d <- dl_dist("gamma", shape = 0.3, rate = 1 / 3)
  env <- run_snippet(distlab:::explore_code(d))
  expect_identical(env$d$params, d$params)
  expect_match(distlab:::explore_code(dl_dist("normal")),
               "# R functions: dnorm(x, mean = 0, sd = 1) and pnorm(x, mean = 0, sd = 1)", fixed = TRUE)
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "pt-BR"), {
    session$setInputs(dist = "gamma", p_gamma_shape = 3, p_gamma_rate = 2)
    session$elapse(200)
    html <- output$code$html
    expect_match(html, "distlab::dl_dist(\"gamma\", shape = 3, rate = 2)", fixed = TRUE)
    expect_match(html, "Copiar", fixed = TRUE)
  })
})

test_that("the Compare code card reproduces the curves and the summary table", {
  check <- function(dists) {
    series <- lapply(dists, function(d) list(dist = d))
    curves <- distlab:::compare_curves(series)
    env <- run_snippet(distlab:::compare_code(series))
    for (i in seq_along(dists)) {
      cv <- curves[[i]]$curve
      ok <- is.finite(env$density[[i]])
      expect_identical(env$grids[[i]][ok], cv$x)
      expect_identical(env$density[[i]][ok], cv$density)
      expect_identical(env$cdf[[i]][ok], cv$cdf)
    }
    tab <- eval(parse(text = sub("^.*\n(t\\(sapply[^\n]*)$", "\\1", distlab:::compare_code(series))),
                envir = env)
    for (i in seq_along(dists)) {
      expect_identical(unname(tab[i, 1:5]), unname(dl_moments(dists[[i]])))
      expect_identical(unname(tab[i, "median"]), dl_quantile(dists[[i]], 0.5))
    }
  }
  check(list(dl_dist("normal"), dl_dist("student_t", df = 1), dl_dist("student_t", df = 5)))
  check(list(dl_dist("binomial", size = 50, prob = 0.06), dl_dist("poisson", lambda = 3)))
  check(list(dl_dist("normal", mean = 3, sd = 1.5), dl_dist("poisson", lambda = 3)))
  check(lapply(c(0.5, 1, 2), function(a) dl_dist("beta", shape1 = a, shape2 = 2)))
  shiny::testServer(distlab:::mod_compare_server, args = list(lang = "en-US"), {
    session$setInputs(mode = "overlay",
                      s1_dist = "normal", s1_p_normal_mean = 0, s1_p_normal_sd = 1,
                      s2_dist = "student_t", s2_p_student_t_df = 1,
                      s3_dist = "student_t", s3_p_student_t_df = 5)
    session$elapse(200)
    expect_match(output$code$html, "distlab::dl_dist(\"student_t\", df = 5)", fixed = TRUE)
    expect_match(output$density, "distlab-compare-normal-student_t-density", fixed = TRUE)
  })
})

test_that("the Fit code card rebuilds the tables shown for the selected family", {
  data <- dl_fit_data(x = datasets::faithful$waiting)
  fit <- dl_fit(data, ids = c("normal", "gamma"))
  code <- distlab:::fit_view_code(fit, "gamma")
  env <- new.env(parent = globalenv())
  exprs <- parse(text = code)
  values <- lapply(exprs, function(e) eval(e, envir = env))
  n <- length(values)
  expect_equal(values[[n - 2L]], dl_fit_describe(data))
  expect_equal(values[[n - 1L]], fit$fits$gamma$estimates)
  expect_equal(values[[n]], dl_fit_diagnostics(fit, id = "gamma")$gof)
  expect_false(grepl("estimates", distlab:::fit_view_code(fit)))
})
