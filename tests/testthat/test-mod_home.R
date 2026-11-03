test_that("Home has task tiles, data-type tiles with panels and exam cards", {
  html <- as.character(distlab:::mod_home_ui("home", "en-US"))
  expect_match(html, "<h1[^>]*>.*What do you want to do today")
  for (k in names(distlab:::.dl_home_tasks)) {
    expect_match(html, sprintf('data-dist="%s" data-input="home-task"', k), info = k)
  }
  groups <- distlab:::registry_groups()
  expect_identical(groups, c("binary", "counts", "bounded", "positive", "real_line"))
  for (g in groups) {
    expect_match(html, sprintf('id="home-tile_%s"[^>]*aria-expanded="false" aria-controls="home-panel_%s"',
                               g, g), info = g)
    expect_match(html, sprintf('id="home-panel_%s"[^>]*hidden', g), info = g)
    expect_match(html, sprintf('data-dist="%s" data-input="home-compendium"', g), info = g)
  }
  expect_match(html, "See all 7 in the Compendium")
  expect_match(html, "See it in the Compendium")
  for (k in names(distlab:::.dl_home_exams)) {
    expect_match(html, sprintf('data-dist="%s" data-input="home-exam"', k), info = k)
  }
  # Mini plots are decorative; the tagline lives on the About page.
  expect_identical(lengths(regmatches(html, gregexpr('<svg[^>]*aria-hidden="true"', html))), 4L)
  expect_no_match(html, "app.tagline")
  expect_match(as.character(distlab:::about_panel("en-US")), 'data-i18n="app.tagline"')
})

test_that("group chips list every member, starters first", {
  for (g in distlab:::registry_groups()) {
    html <- as.character(distlab:::group_chips(g, "en-US", "home-pick"))
    for (did in distlab:::group_members(g)) {
      expect_match(html, sprintf('data-dist="%s"', did), info = did)
    }
  }
  html <- as.character(distlab:::group_chips("counts", "pt-BR", "home-pick"))
  expect_lt(regexpr('data-dist="poisson"', html), regexpr('data-dist="geometric"', html))
  expect_match(html, "comece por aqui")
})

test_that("search choices use names of the current language", {
  pt <- distlab:::search_choices("pt-BR")
  expect_true("Gama" %in% names(pt))
  expect_identical(unname(pt[["Gama"]]), "gamma")
  expect_true("Gamma" %in% names(distlab:::search_choices("en-US")))
})

test_that("the remembered distribution is checked before use", {
  ok <- distlab:::home_last(list(tab = "explorer", id = "gamma",
                                 params = list(rate = 1, shape = 2.5, junk = 3)))
  expect_identical(ok, list(tab = "explorer", id = "gamma",
                            params = list(shape = 2.5, rate = 1)))
  # Incomplete or invalid values keep the distribution and drop the values.
  expect_identical(distlab:::home_last(list(tab = "calculator", id = "gamma",
                                            params = list(shape = 2)))$params, list())
  expect_identical(distlab:::home_last(list(tab = "explorer", id = "gamma",
                                            params = list(shape = -1, rate = 1)))$params, list())
  expect_null(distlab:::home_last(list(tab = "fit", id = "gamma")))
  expect_null(distlab:::home_last(list(tab = "explorer", id = "nope")))
  expect_null(distlab:::home_last("gamma"))
  expect_null(distlab:::home_last(NULL))
})

test_that("search and chips request navigation", {
  shiny::testServer(distlab:::mod_home_server, args = list(lang = "en-US"), {
    session$setInputs(search = "poisson")
    expect_identical(session$returned()[c("tab", "id")], list(tab = "explorer", id = "poisson"))
    expect_identical(output$status, "Opening Poisson in the Explorer…")
    session$setInputs(pick = "beta")
    expect_identical(session$returned()$id, "beta")
    n <- session$returned()$n
    session$setInputs(pick = "beta")
    expect_gt(session$returned()$n, n)
    session$setInputs(search = "not_a_distribution")
    expect_identical(session$returned()$id, "beta")
    session$setInputs(compendium = "counts")
    expect_identical(session$returned()[c("tab", "group")],
                     list(tab = "compendium", group = "counts"))
    expect_identical(output$status, "")
    session$setInputs(compendium = "nope")
    expect_identical(session$returned()$group, "counts")
  })
})

test_that("task tiles open each page, reusing the last distribution", {
  shiny::testServer(distlab:::mod_home_server, args = list(lang = "en-US"), {
    route <- function(key) {
      session$setInputs(task = key)
      r <- session$returned()
      r[setdiff(names(r), "n")]
    }
    expect_identical(route("explore"), list(tab = "explorer", id = "normal"))
    expect_identical(route("calc"), list(tab = "calculator", id = "binomial"))
    expect_identical(route("compare"), list(tab = "compare", preset = "t_normal"))
    expect_identical(route("simulate"), list(tab = "simulation"))
    expect_identical(route("fit"), list(tab = "fit"))
    expect_identical(route("map"), list(tab = "compendium"))
    session$setInputs(last = list(tab = "explorer", id = "gamma",
                                  params = list(shape = 3, rate = 2)))
    expect_identical(route("calc"), list(tab = "calculator", id = "gamma",
                                         params = list(shape = 3, rate = 2)))
    expect_identical(route("explore")$id, "gamma")
    n <- session$returned()$n
    session$setInputs(task = "nope")
    expect_identical(session$returned()$n, n)
  })
})

test_that("exam cards open a relation, a simulation tab or a Compare preset", {
  shiny::testServer(distlab:::mod_home_server, args = list(lang = "en-US"), {
    route <- function(key) {
      session$setInputs(exam = key)
      r <- session$returned()
      r[setdiff(names(r), "n")]
    }
    expect_identical(route("binom_pois"),
                     list(tab = "simulation", edge = "binomial__poisson__limit"))
    expect_identical(route("clt"), list(tab = "simulation", sim_tab = "clt"))
    expect_identical(route("t_normal"), list(tab = "compare", preset = "t_normal"))
    expect_identical(route("beta_shapes"), list(tab = "compare", preset = "beta_shapes"))
  })
})

test_that("the resume strip reopens the last distribution", {
  shiny::testServer(distlab:::mod_home_server, args = list(lang = "pt-BR"), {
    expect_null(output$resume$html)
    session$setInputs(resume = "x")
    expect_null(session$returned())
    session$setInputs(last = list(tab = "calculator", id = "gamma",
                                  params = list(shape = 2.5, rate = 1)))
    html <- output$resume$html
    expect_match(html, "Continuar:")
    expect_match(html, "Gama")
    expect_match(html, "\\alpha = 2{,}5;\\ \\beta = 1", fixed = TRUE)
    expect_match(html, "na Calculadora")
    session$setInputs(resume = "gamma")
    expect_identical(session$returned()[c("tab", "id", "params")],
                     list(tab = "calculator", id = "gamma", params = list(shape = 2.5, rate = 1)))
    expect_identical(output$status, "Abrindo Gama na Calculadora…")
  })
})

test_that("chips and the resume strip follow the language", {
  lang <- shiny::reactiveVal("pt-BR")
  shiny::testServer(distlab:::mod_home_server, args = list(lang = lang), {
    session$setInputs(last = list(tab = "explorer", id = "gamma", params = list()))
    expect_match(output$chips_positive$html, "Gama")
    expect_match(output$popular$html, "Exponencial")
    expect_match(output$resume$html, "no Explorador")
    lang("en-US")
    session$flushReact()
    expect_match(output$chips_positive$html, "Gamma")
    expect_match(output$popular$html, "Exponential")
    expect_match(output$resume$html, "in the Explorer")
    # Without stored values, the strip shows the defaults.
    expect_match(output$resume$html, "\\alpha = 2,\\ \\beta = 1", fixed = TRUE)
  })
})
