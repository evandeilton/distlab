# Plot downloads.

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
