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
  app$run_js("document.querySelector('.dl-goto[data-dist=\"gamma\"]').click()")
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
})
