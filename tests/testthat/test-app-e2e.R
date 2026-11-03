# Browser test of the live language switch. Runs the installed package, so it
# is skipped under devtools::load_all() and on CRAN.
test_that("the language switch keeps tab, distribution and parameters", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if(exists(".__DEVTOOLS__", envir = asNamespace("distlab")),
          "needs the installed package")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
          "Chrome is not available")
  app <- shinytest2::AppDriver$new(distlab::run_app("pt-BR"), width = 1200,
                                   height = 800, load_timeout = 30000)
  # Close the browser too, so Chrome removes its temporary profile.
  on.exit({
    app$stop()
    try(chromote::default_chromote_object()$close(), silent = TRUE)
  }, add = TRUE)
  # The navbar stays on one line at every desktop width, in both languages;
  # where labels do not fit, it shows icons with labels for screen readers.
  # Spread of the vertical centers of brand, links and toggles: one line is
  # a few pixels, a wrapped navbar is a whole row.
  spread <- paste0(
    "(function () { var c = Array.prototype.map.call(document.querySelectorAll(",
    "'.navbar-brand, .bslib-page-navbar > .navbar .nav-link, .dl-lang-toggle'), ",
    "function (e) { var r = e.getBoundingClientRect(); return r.top + r.height / 2; });",
    " return Math.max.apply(null, c) - Math.min.apply(null, c); })()")
  for (w in c(1000, 1200, 1400)) {
    app$set_window_size(w, 800)
    app$wait_for_idle()
    expect_lt(app$get_js(spread), 10, label = paste("navbar spread at", w))
  }
  app$set_window_size(1200, 800)
  expect_identical(app$get_js("document.querySelector('[data-value=\"compendium\"] .dl-nav-label').title"),
                   "Compêndio")
  # Home: a data-type tile opens its panel, Esc closes it; one panel at a time.
  expanded <- "document.querySelector('#home-tile_positive').getAttribute('aria-expanded')"
  app$run_js("document.querySelector('#home-tile_positive').click()")
  expect_identical(app$get_js(expanded), "true")
  expect_false(app$get_js("document.querySelector('#home-panel_positive').hidden"))
  app$run_js("document.querySelector('#home-tile_counts').click()")
  expect_identical(app$get_js(expanded), "false")
  app$run_js("document.querySelector('#home-tile_counts').dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape', bubbles: true}))")
  expect_true(app$get_js("document.querySelector('#home-panel_counts').hidden"))
  app$run_js("document.querySelector('#home-tile_positive').click()")
  app$wait_for_js("document.querySelector('#home-panel_positive .dl-goto[data-dist=\"gamma\"]') !== null")
  # On a phone the Home page fits the screen width.
  app$set_window_size(375, 812)
  app$wait_for_idle()
  expect_lte(app$get_js("document.documentElement.scrollWidth"), app$get_js("window.innerWidth"))
  app$set_window_size(1200, 800)
  app$run_js("document.querySelector('#home-panel_positive .dl-goto[data-dist=\"gamma\"]').click()")
  app$wait_for_idle()
  expect_identical(app$get_value(input = "nav"), "explorer")
  expect_identical(app$get_value(input = "explore-dist"), "gamma")
  app$set_inputs(`explore-p_gamma_shape` = 4.5)
  app$run_js("document.querySelector('.dl-lang-btn[data-lang=\"en-US\"]').click()")
  app$wait_for_idle()
  expect_identical(app$get_value(input = "lang"), "en-US")
  expect_identical(app$get_value(input = "nav"), "explorer")
  expect_identical(app$get_value(input = "explore-dist"), "gamma")
  expect_equal(app$get_value(input = "explore-p_gamma_shape"), 4.5)
  expect_identical(app$get_js("document.querySelector('[data-i18n=\"nav.home\"]').textContent"),
                   "Home")
  expect_identical(app$get_js("document.documentElement.lang"), "en-US")
  # The browser remembers the open distribution; Home offers to resume it.
  app$wait_for_js("document.querySelector('.dl-resume-btn') !== null", timeout = 5000)
  expect_match(app$get_js("document.querySelector('.dl-resume-btn').textContent"), "Gamma")
  expect_match(app$get_js("window.localStorage.getItem('distlab.last')"), '"id":"gamma"', fixed = TRUE)
  dark_label <- "document.querySelector('bslib-input-dark-mode').shadowRoot.querySelector('button').getAttribute('aria-label')"
  expect_match(app$get_js(dark_label), "^Switch to (dark|light) mode$")
  # Formulas are typeset by KaTeX in the browser.
  expect_gt(app$get_js("document.querySelectorAll('.dl-math .katex').length"), 0)
  # The Compendium draws the map and typesets the list of relations.
  app$run_js("document.querySelector('[data-value=\"compendium\"]').click()")
  app$wait_for_idle()
  expect_gt(app$get_js("document.querySelectorAll('#graphcompendium-graph canvas').length"), 0)
  app$run_js("document.querySelector('[data-value=\"relations\"]').click()")
  app$wait_for_idle()
  expect_gt(app$get_js("document.querySelectorAll('.dl-rel-table .katex').length"), 20)
  # The Calculator typesets the event and shows the R code.
  app$run_js("document.querySelector('.navbar [data-value=\"calculator\"]').click()")
  app$wait_for_idle()
  expect_gt(app$get_js("document.querySelectorAll('.dl-calc-event .katex').length"), 0)
  expect_match(app$get_js("document.querySelector('.dl-code-block').textContent"), "^pnorm\\(")
  # Compare starts with three overlaid distributions.
  app$run_js("document.querySelector('.navbar [data-value=\"compare\"]').click()")
  app$wait_for_idle()
  expect_identical(app$get_js("document.querySelectorAll('.dl-slot:not([hidden])').length"), 3L)
  # The Simulation draws the sample check with its seeded code, then a
  # relation with its formula typeset.
  app$run_js("document.querySelector('.navbar [data-value=\"simulation\"]').click()")
  app$wait_for_idle()
  expect_identical(app$get_js("document.querySelectorAll('.dl-mc-table tbody tr').length"), 8L)
  expect_match(app$get_js("document.querySelector('.dl-sim .dl-code-block').textContent"),
               "^set\\.seed\\(2026\\)")
  app$run_js("document.querySelector('.dl-sim [data-value=\"relations\"]').click()")
  app$wait_for_idle()
  expect_gt(app$get_js("document.querySelectorAll('.dl-sim-rel-head .katex').length"), 0)
  expect_gt(app$get_js("document.querySelectorAll('.dl-sim .js-plotly-plot').length"), 0)
  # On a phone the open tab, its table and its code stay inside the screen.
  app$run_js("document.querySelector('.dl-sim [data-value=\"sample\"]').click()")
  app$set_window_size(375, 812)
  app$wait_for_idle()
  pane <- ".dl-sim .tab-content > .tab-pane.active"
  expect_lte(app$get_js(sprintf("document.querySelector('%s').getBoundingClientRect().right", pane)),
             app$get_js("window.innerWidth"))
  app$set_window_size(1200, 800)
  # The Fit lab loads an example explicitly, ranks the requested families
  # and keeps its fitted values when the language changes.
  app$run_js("document.querySelector('.navbar [data-value=\"fit\"]').click()")
  app$set_inputs(`fit-source` = "example", `fit-example` = "counts")
  app$click("fit-load")
  app$wait_for_idle()
  app$set_inputs(`fit-candidates` = c("poisson", "geometric"))
  app$click("fit-run")
  app$wait_for_idle()
  expect_match(app$get_js("document.querySelector('#fit-ranking').textContent"), "Poisson")
  app$set_inputs(`fit-tab` = "diagnostics")
  app$wait_for_js("document.querySelector('#fit-density').classList.contains('js-plotly-plot')")
  expect_gt(app$get_js("document.querySelectorAll('#fit-density.js-plotly-plot').length"), 0)
  app$set_inputs(`fit-tab` = "export")
  app$wait_for_js("document.querySelector('#fit-code .dl-code-block') !== null")
  fit_code <- app$get_js("document.querySelector('#fit-code .dl-code-block').textContent")
  app$run_js("document.querySelector('.dl-lang-btn[data-lang=\"pt-BR\"]').click()")
  app$wait_for_js("document.documentElement.lang === 'pt-BR'")
  expect_identical(app$get_js("document.querySelector('#fit-code .dl-code-block').textContent"), fit_code)
  app$set_inputs(`fit-tab` = "results")
  app$wait_for_js("document.querySelector('#fit-ranking').textContent.includes('Geométrica')")
  expect_match(app$get_js("document.querySelector('#fit-ranking').textContent"), "Geométrica")
  # Fitted values can lie outside the catalog's initial teaching windows.
  app$set_inputs(`fit-source` = "example", `fit-example` = "faithful")
  app$click("fit-load")
  app$wait_for_idle()
  app$set_inputs(`fit-candidates` = "normal")
  app$click("fit-run")
  app$wait_for_js("document.getElementById('fit-diagnostic_id').value === 'normal'")
  app$click("fit-open_explorer")
  app$wait_for_idle()
  expect_identical(app$get_value(input = "nav"), "explorer")
  expect_equal(app$get_value(input = "explore-p_normal_mean"), mean(datasets::faithful$waiting),
               tolerance = 1e-6)
})
