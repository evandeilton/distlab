# Browser test of the live language switch. Runs the installed package, so it
# is skipped under devtools::load_all() and on CRAN.
test_that("the language switch keeps tab, distribution and parameters", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if(exists(".__DEVTOOLS__", envir = asNamespace("distlab")),
          "needs the installed package")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
          "Chrome is not available")
  # Chrome leaves folders in its TMPDIR: give it one that the test removes
  # (short, since Chrome puts a socket in it).
  chrome_tmp <- tempfile("chr")
  dir.create(chrome_tmp)
  on.exit(unlink(chrome_tmp, recursive = TRUE), add = TRUE)
  old_tmp <- Sys.getenv("TMPDIR", unset = NA)
  if (nchar(chrome_tmp) < 60) Sys.setenv(TMPDIR = chrome_tmp)
  app <- tryCatch(
    shinytest2::AppDriver$new(distlab::run_app("pt-BR"), width = 1200, height = 800,
                              load_timeout = 30000),
    finally = if (is.na(old_tmp)) Sys.unsetenv("TMPDIR") else Sys.setenv(TMPDIR = old_tmp))
  # Close the browser too, before its folder is removed.
  on.exit({
    app$stop()
    try(chromote::default_chromote_object()$close(), silent = TRUE)
  }, add = TRUE, after = FALSE)
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
  # Home search opens a multivariate law as its reference card over Home, a
  # dialog named by its title, with formulas typeset and a static
  # illustration; it fits a phone screen and its links open univariate laws
  # in the Explorer.
  app$run_js("document.querySelector('.navbar [data-value=\"home\"]').click()")
  app$set_inputs(`home-search` = "mvnormal")
  app$wait_for_js("document.querySelector('.modal.show .dl-mv-card .katex') !== null")
  expect_identical(app$get_value(input = "nav"), "home")
  expect_identical(app$get_js(paste0(
    "document.getElementById(document.querySelector('.modal.show').getAttribute('aria-labelledby'))",
    ".textContent.trim()")), "Multivariate normal")
  expect_true(app$get_js(
    "document.querySelector('.modal.show svg.dl-mv-plot[role=\"img\"] .dl-mv-ellipse') !== null"))
  app$set_window_size(375, 812)
  app$wait_for_idle()
  body <- "document.querySelector('.modal.show .modal-body')"
  expect_lte(app$get_js(paste0(body, ".scrollWidth")), app$get_js(paste0(body, ".clientWidth")))
  expect_lte(app$get_js("document.querySelector('.modal.show .modal-dialog').getBoundingClientRect().right"),
             app$get_js("window.innerWidth"))
  app$set_window_size(1200, 800)
  app$run_js("document.querySelector('.modal.show .dl-mv-related .dl-goto[data-dist=\"normal\"]').click()")
  app$wait_for_js("document.querySelector('.modal.show') === null")
  app$wait_for_idle()
  expect_identical(app$get_value(input = "nav"), "explorer")
  expect_identical(app$get_value(input = "explore-dist"), "normal")
  # The related card of the Explorer opens the card over the Explorer, and
  # closing it returns focus to the link.
  link <- "document.querySelector('#explore-related .dl-goto[data-dist=\"mvnormal\"]')"
  app$run_js(paste0(link, ".focus(); ", link, ".click()"))
  app$wait_for_js("document.querySelector('.modal.show .dl-mv-card') !== null")
  expect_identical(app$get_value(input = "nav"), "explorer")
  # Bootstrap ignores a close while the dialog is still fading in.
  Sys.sleep(0.6)
  app$run_js("document.querySelector('.modal.show .modal-footer button').click()")
  app$wait_for_js("document.querySelector('.modal.show') === null && !document.body.classList.contains('modal-open')")
  app$wait_for_js(paste0("document.activeElement === ", link))
  # The Calculator typesets the event and shows the R code.
  app$run_js("document.querySelector('.navbar [data-value=\"calculator\"]').click()")
  app$wait_for_idle()
  expect_gt(app$get_js("document.querySelectorAll('.dl-calc-event .katex').length"), 0)
  expect_match(app$get_js("document.querySelector('.dl-calc-result .dl-code-block').textContent"), "^pnorm\\(")
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
  # Plots download as PNG or SVG on the background of the page, named after
  # the page, distribution and plot.
  app$wait_for_js("document.querySelector('#explore-density .modebar-btn[data-title]') !== null")
  svg_btn <- "#explore-density .modebar-btn[data-title=\"Baixar gráfico em SVG (vetorial)\"]"
  expect_true(app$get_js(sprintf("document.querySelector('%s') !== null", svg_btn)))
  app$run_js(paste0(
    "window.__dlSaved = Plotly.downloadImage; window.__dlGot = null;",
    "Plotly.downloadImage = function (fig, o) { window.__dlGot = { bg: fig.layout.paper_bgcolor,",
    " format: o.format, name: o.filename, width: o.width }; return Promise.resolve(); };",
    sprintf("document.querySelector('%s').click();", svg_btn),
    "Plotly.downloadImage = window.__dlSaved;"))
  got <- app$get_js("window.__dlGot")
  expect_identical(got$format, "svg")
  expect_identical(got$name, "distlab-explorer-normal-density")
  expect_identical(got$width, 1200L)
  expect_match(got$bg, "^rgb\\(")
  expect_match(app$get_js(paste0(
    "(function () { var gd = document.getElementById('explore-density');",
    " return Plotly.toImage({ data: gd.data, layout: gd.layout }, { format: 'svg', width: 600,",
    " height: 350 }); })()")), "^data:image/svg\\+xml")
  # The mean label never runs under the toolbar.
  overlap <- paste0(
    "(function () { var gd = document.getElementById('explore-density');",
    " var a = gd.querySelector('.modebar').getBoundingClientRect();",
    " var b = gd.querySelector('.annotation').getBoundingClientRect();",
    " return a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom; })()")
  expect_false(app$get_js(overlap))
  # Presentation mode: larger type and plot fonts, thicker lines, closed
  # sidebars; it survives a language switch and a reload, and Esc leaves it.
  base_font <- "parseFloat(getComputedStyle(document.documentElement).fontSize)"
  plot_font <- "document.getElementById('explore-density').layout.font.size"
  mean_line <- "document.getElementById('explore-density').layout.shapes[0].line.width"
  expect_equal(app$get_js(base_font), 16)
  app$run_js("document.getElementById('dl_present').click()")
  app$wait_for_js(paste(plot_font, "> 12"))
  expect_equal(app$get_js(mean_line), 1.5 * 1.6)
  Sys.sleep(0.5)
  expect_false(app$get_js(overlap))
  # Downloads keep the sizes of the normal view.
  png_btn <- "#explore-density .modebar-btn[data-title=\"Baixar gráfico em PNG\"]"
  app$run_js(paste0(
    "window.__dlSaved = Plotly.downloadImage; window.__dlGot = null;",
    "Plotly.downloadImage = function (fig, o) { window.__dlGot = { font: fig.layout.font.size,",
    " line: fig.data[0].line.width, shape: fig.layout.shapes[0].line.width, format: o.format };",
    " return Promise.resolve(); };",
    sprintf("document.querySelector('%s').click();", png_btn),
    "Plotly.downloadImage = window.__dlSaved;"))
  got <- app$get_js("window.__dlGot")
  expect_identical(got$format, "png")
  expect_equal(c(got$font, got$line, got$shape), c(12, 2.5, 1.5))
  expect_gt(app$get_js(plot_font), 12)
  # Esc that closes a menu keeps the mode, and a held P key does not flicker.
  present_on <- "document.documentElement.classList.contains('dl-present')"
  app$run_js(paste0(
    "var m = document.createElement('div'); m.id = 'dl-test-menu'; m.className = 'dropdown-menu show';",
    " document.body.appendChild(m);",
    " document.body.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape', bubbles: true}));"))
  expect_true(app$get_js(present_on))
  app$run_js("document.getElementById('dl-test-menu').remove()")
  app$run_js("document.body.dispatchEvent(new KeyboardEvent('keydown', {key: 'p', repeat: true, bubbles: true}))")
  expect_true(app$get_js(present_on))
  expect_gt(app$get_js(base_font), 16)
  expect_lt(app$get_js(spread), 10, label = "navbar spread while presenting")
  expect_identical(app$get_js("document.getElementById('dl_present').getAttribute('aria-pressed')"), "true")
  collapsed <- paste0("document.getElementById('explore-dist').closest('.bslib-sidebar-layout')",
                      ".classList.contains('sidebar-collapsed')")
  expect_true(app$get_js(collapsed))
  app$run_js("document.querySelector('.dl-lang-btn[data-lang=\"en-US\"]').click()")
  app$wait_for_js("document.documentElement.lang === 'en-US'")
  app$wait_for_idle()
  expect_true(app$get_js("document.documentElement.classList.contains('dl-present')"))
  expect_match(app$get_js("document.getElementById('dl_present').title"), "^Presentation mode")
  app$wait_for_js(paste(plot_font, "> 12"))
  # It still fits a phone screen.
  app$set_window_size(375, 812)
  app$wait_for_idle()
  expect_lte(app$get_js("document.documentElement.scrollWidth"), app$get_js("window.innerWidth"))
  app$set_window_size(1200, 800)
  # After a reload the driver only reads the page.
  app$run_js("location.reload()")
  app$wait_for_js("document.readyState === 'complete' && !!window.Shiny && !!window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected()",
                  timeout = 20000)
  expect_true(app$get_js("document.documentElement.classList.contains('dl-present')"))
  expect_gt(app$get_js(base_font), 16)
  app$wait_for_js(collapsed)
  app$run_js("document.body.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape', bubbles: true}))")
  expect_false(app$get_js("document.documentElement.classList.contains('dl-present')"))
  expect_equal(app$get_js(base_font), 16)
  expect_false(app$get_js(collapsed))
  expect_identical(app$get_js("window.localStorage.getItem('distlab.present')"), "0")
  # "P" switches it on and off.
  app$run_js("document.body.dispatchEvent(new KeyboardEvent('keydown', {key: 'p', bubbles: true}))")
  expect_true(app$get_js("document.documentElement.classList.contains('dl-present')"))
  app$run_js("document.body.dispatchEvent(new KeyboardEvent('keydown', {key: 'p', bubbles: true}))")
  expect_false(app$get_js("document.documentElement.classList.contains('dl-present')"))
})
