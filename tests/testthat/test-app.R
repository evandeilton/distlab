app_for_test <- function(lang = "pt-BR") {
  shiny::shinyApp(distlab:::app_ui(lang), distlab:::app_server(lang))
}

test_that("the navbar has every section, with icons and translated labels", {
  html <- as.character(distlab:::app_ui("pt-BR"))
  for (s in c("home", "compendium", "explorer", "calculator", "compare",
              "simulation", "fit", "about")) {
    expect_match(html, sprintf('data-value="%s"', s), info = s)
    expect_match(html, sprintf('data-i18n="nav.%s"', s), info = s)
  }
  expect_match(html, "Compêndio")
  expect_match(html, 'aria-label="Idioma da interface"', fixed = TRUE)
  expect_match(html, "dl_lang")
  expect_match(html, "favicon.svg")
  expect_match(distlab:::upcoming_panel(list(value = "fit", icon = "chart-line"), "en-US") |>
                 as.character(), "upcoming release")
})

test_that("every translatable key in the UI exists in both dictionaries", {
  for (lang in dl_languages()) {
    html <- as.character(distlab:::app_ui(lang))
    keys <- unique(sub('^data-i18n(-[a-z-]+)?="([^"]+)"$', "\\2",
                       regmatches(html, gregexpr('data-i18n(-(title|aria-label|placeholder))?="[^"]+"',
                                                 html))[[1]]))
    expect_gt(length(keys), 20)
    for (l in dl_languages()) {
      expect_true(all(keys %in% names(distlab:::dl_dictionary(l))), info = l)
    }
  }
})

test_that("switching language keeps distribution and parameter values", {
  sess <- recording_session()
  shiny::testServer(app_for_test("pt-BR"), session = sess, {
    session$setInputs(`explore-dist` = "gamma", `explore-p_gamma_shape` = 3.5,
                      `explore-p_gamma_rate` = 2, nav = "explorer")
    session$elapse(200)
    expect_match(output$`explore-moments`$html, "1,75")
    session$setInputs(lang = "en-US")
    session$elapse(200)
    expect_identical(explore$dist()$id, "gamma")
    expect_equal(explore$dist()$params$shape, 3.5)
    expect_match(output$`explore-moments`$html, "1.75", fixed = TRUE)
    expect_match(output$`explore-story`, "Events arrive")
    expect_match(output$`explore-formulas_moments`$html, "Variance")
    expect_match(output$`explore-related`$html, "Special case")
    # The client gets the new dictionary; the selection is kept, not reset.
    custom <- Filter(function(m) identical(m$type, "dl-lang"), attr(sess, "log")$custom)
    expect_length(custom, 1L)
    expect_identical(custom[[1]]$message$dict[["nav.home"]], "Home")
    # Other pages relabel their own (unset) selectors too; keep the Explorer's.
    sel <- Filter(function(m) !is.null(m$message$value), input_messages(sess, "dist"))
    expect_identical(sel[[length(sel)]]$message$value, "gamma")
    expect_false(any(vapply(attr(sess, "log")$input, function(m) identical(m$id, "nav"),
                            logical(1))))
  })
})

test_that("Home navigation opens the Explorer with the chosen distribution", {
  sess <- recording_session()
  shiny::testServer(app_for_test("en-US"), session = sess, {
    session$setInputs(`home-pick` = "cauchy")
    nav <- input_messages(sess, "nav")
    expect_identical(nav[[length(nav)]]$message$value, "explorer")
    sel <- input_messages(sess, "dist")
    expect_identical(sel[[length(sel)]]$message$value, "cauchy")
    session$setInputs(`home-go_fit` = 1)
    nav <- input_messages(sess, "nav")
    expect_identical(nav[[length(nav)]]$message$value, "fit")
  })
})

test_that("run_app sets the initial language", {
  app <- run_app(lang = "en-US")
  expect_s3_class(app, "shiny.appobj")
  expect_error(run_app(lang = "fr-FR"), "must be one of")
  expect_identical(distlab:::coerce_lang(NULL, "pt-BR"), "pt-BR")
  expect_identical(distlab:::coerce_lang("xx", "en-US"), "en-US")
  expect_identical(distlab:::coerce_lang("en-US", "pt-BR"), "en-US")
})

test_that("the theme and palette come from the brand file", {
  expect_identical(distlab:::brand_color("primary"), "#2B2D6E")
  expect_identical(distlab:::brand_color("secondary"), "#14A3A0")
  expect_length(distlab:::dl_palette(10), 10)
  expect_true(bslib::is_bs_theme(distlab:::dl_theme()))
  expect_identical(bslib::bs_get_variables(distlab:::dl_theme(), "primary")[[1]], "#2B2D6E")
})

test_that("the dark-mode switch follows the OS and has translated labels", {
  html <- as.character(distlab:::dark_toggle("pt-BR"))
  expect_match(html, 'data-label-dark="Mudar para o modo escuro"', fixed = TRUE)
  expect_match(html, 'data-i18n-label-light="nav.to_light"', fixed = TRUE)
  expect_no_match(html, 'mode="light"', fixed = TRUE)
})

test_that("every data-type group has a name and description in both languages", {
  for (lang in dl_languages()) {
    dict <- distlab:::dl_dictionary(lang)
    for (g in distlab:::.dl_groups) {
      expect_true(is.character(dict[[paste0("group.", g)]]), info = paste(lang, g))
      expect_true(is.character(dict[[paste0("group.", g, ".desc")]]), info = paste(lang, g))
    }
  }
})

test_that("building the UI registers the resource path for logo and favicon", {
  shiny::removeResourcePath("distlab-www")
  distlab:::app_ui("en-US")
  expect_true("distlab-www" %in% names(shiny::resourcePaths()))
})

test_that("the Compendium opens distributions in the Explorer", {
  sess <- recording_session()
  shiny::testServer(app_for_test("en-US"), session = sess, {
    session$setInputs(nav = "compendium", `compendium-open` = "weibull")
    nav <- input_messages(sess, "nav")
    expect_identical(nav[[length(nav)]]$message$value, "explorer")
    sel <- input_messages(sess, "dist")
    expect_identical(sel[[length(sel)]]$message$value, "weibull")
  })
})

test_that("Home group links open that group in the Compendium", {
  sess <- recording_session()
  shiny::testServer(app_for_test("en-US"), session = sess, {
    session$setInputs(`home-compendium` = "positive")
    nav <- input_messages(sess, "nav")
    expect_identical(nav[[length(nav)]]$message$value, "compendium")
    g <- input_messages(sess, "groups")
    expect_identical(g[[length(g)]]$message$value, "positive")
    expect_length(input_messages(sess, "dist"), 0L)
  })
})

test_that("the Explorer opens the Calculator and Compare with its values", {
  sess <- recording_session()
  shiny::testServer(app_for_test("en-US"), session = sess, {
    session$setInputs(`explore-dist` = "gamma", `explore-p_gamma_shape` = 3.5,
                      `explore-p_gamma_rate` = 2, nav = "explorer")
    session$setInputs(`explore-to_calc` = 1)
    nav <- input_messages(sess, "nav")
    expect_identical(nav[[length(nav)]]$message$value, "calculator")
    sel <- input_messages(sess, "dist")
    expect_identical(sel[[length(sel)]]$message$value, "gamma")
    session$setInputs(`calc-dist` = "gamma")
    expect_match(output$`calc-params`$html, 'data-from="3.5"', fixed = TRUE)
    session$setInputs(`explore-to_compare` = 1)
    nav <- input_messages(sess, "nav")
    expect_identical(nav[[length(nav)]]$message$value, "compare")
    s1 <- input_messages(sess, "s1_dist")
    expect_identical(s1[[length(s1)]]$message$value, "gamma")
  })
})

test_that("the Compendium opens the Calculator and Compare", {
  sess <- recording_session()
  shiny::testServer(app_for_test("en-US"), session = sess, {
    session$setInputs(nav = "compendium", `compendium-calc` = "poisson")
    nav <- input_messages(sess, "nav")
    expect_identical(nav[[length(nav)]]$message$value, "calculator")
    sel <- input_messages(sess, "dist")
    expect_identical(sel[[length(sel)]]$message$value, "poisson")
    session$setInputs(`compendium-compare` = "binomial")
    nav <- input_messages(sess, "nav")
    expect_identical(nav[[length(nav)]]$message$value, "compare")
    s1 <- input_messages(sess, "s1_dist")
    expect_identical(s1[[length(s1)]]$message$value, "binomial")
  })
})

test_that("switching language keeps the Calculator and Compare state", {
  shiny::testServer(app_for_test("pt-BR"), {
    session$setInputs(`calc-dist` = "gamma", `calc-p_gamma_shape` = 2, `calc-p_gamma_rate` = 1,
                      `calc-mode` = "between", `calc-a` = "1,2", `calc-b` = "3,4",
                      `compare-mode` = "sweep", `compare-sw_dist` = "poisson",
                      `compare-sw_param` = "lambda", `compare-sw_values` = "1; 4; 10",
                      `compare-sw_p_poisson_lambda` = 3, nav = "calculator")
    session$elapse(200)
    expect_match(output$`calc-result`$html, "0,5158")
    session$setInputs(lang = "en-US")
    session$elapse(200)
    expect_match(output$`calc-result`$html, "0.5158", fixed = TRUE)
    expect_equal(exp(calc$result()$log_p), pgamma(3.4, 2) - pgamma(1.2, 2))
    expect_identical(compare$mode(), "sweep")
    expect_length(compare$series(), 3L)
    expect_match(output$`compare-summary`$html, "Variance")
  })
})

test_that("the Explorer, the Compendium and the Simulation link to each other", {
  sess <- recording_session()
  last <- function(id) {
    m <- input_messages(sess, id)
    m[[length(m)]]$message$value
  }
  shiny::testServer(app_for_test("en-US"), session = sess, {
    # The Explorer sends its distribution and values.
    session$setInputs(`explore-dist` = "gamma", `explore-p_gamma_shape` = 3.5,
                      `explore-p_gamma_rate` = 2, nav = "explorer")
    session$setInputs(`explore-to_sim` = 1)
    expect_identical(last("nav"), "simulation")
    expect_identical(last("dist"), "gamma")
    session$setInputs(`sim-dist` = "gamma")
    expect_match(output$`sim-params`$html, 'data-from="3.5"', fixed = TRUE)
    # The Compendium simulates a distribution or a relation.
    session$setInputs(nav = "compendium", `compendium-sim` = "poisson")
    expect_identical(last("nav"), "simulation")
    expect_identical(last("dist"), "poisson")
    session$setInputs(`compendium-sim_edge` = "beta__uniform__special_case")
    expect_identical(last("nav"), "simulation")
    expect_identical(last("edge"), "beta__uniform__special_case")
    expect_identical(last("tab"), "relations")
    n_nav <- length(input_messages(sess, "nav"))
    session$setInputs(`compendium-sim_edge` = "nope")
    expect_length(input_messages(sess, "nav"), n_nav)
    # An end of the relation opens in the Explorer with the recipe values.
    session$setInputs(nav = "simulation", `sim-tab` = "relations",
                      `sim-edge` = "beta__uniform__special_case", `sim-seed` = 2026)
    session$setInputs(`sim-open_end` = "beta")
    expect_identical(last("nav"), "explorer")
    expect_identical(last("dist"), "beta")
    session$setInputs(`explore-dist` = "beta")
    expect_match(output$`explore-params`$html, 'data-from="1"', fixed = TRUE)
  })
})

test_that("Compendium panels offer the Simulation", {
  html <- as.character(distlab:::node_panel(dl_entry("gamma"), "en-US", "c-open", "c-edge",
                                            sim_id = "c-sim"))
  expect_match(html, 'data-input="c-sim"', fixed = TRUE)
  expect_match(html, "Simulate Gamma")
  rel <- dl_relations()
  rel$id <- distlab:::edge_ids(rel)
  r <- rel[rel$id == "normal__cauchy__transformation", ]
  html <- as.character(distlab:::edge_panel(r, "pt-BR", "c-open", sim_id = "c-sim-edge"))
  expect_match(html, 'data-dist="normal__cauchy__transformation"', fixed = TRUE)
  expect_match(html, "Simular esta relação")
  expect_no_match(as.character(distlab:::edge_panel(r, "en-US", "c-open")), "Simulate")
})
