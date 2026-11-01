test_that("Home has one card per registry group, with every distribution", {
  html <- as.character(distlab:::group_cards("en-US", "home-pick"))
  groups <- distlab:::registry_groups()
  expect_identical(groups, c("binary", "counts", "bounded", "positive", "real_line"))
  for (g in groups) expect_match(html, paste0("dl-group-", g))
  for (did in dl_ids()) expect_match(html, sprintf('data-dist="%s"', did), info = did)
  html <- as.character(distlab:::group_cards("en-US", "home-pick", "home-compendium"))
  for (g in groups) {
    expect_match(html, sprintf('data-dist="%s" data-input="home-compendium"', g), info = g)
  }
})

test_that("search choices use names of the current language", {
  pt <- distlab:::search_choices("pt-BR")
  expect_true("Gama" %in% names(pt))
  expect_identical(unname(pt[["Gama"]]), "gamma")
  expect_true("Gamma" %in% names(distlab:::search_choices("en-US")))
})

test_that("search, chips and quick start request navigation", {
  shiny::testServer(distlab:::mod_home_server, args = list(lang = "en-US"), {
    session$setInputs(search = "poisson")
    expect_identical(session$returned()[c("tab", "id")], list(tab = "explorer", id = "poisson"))
    session$setInputs(pick = "beta")
    expect_identical(session$returned()$id, "beta")
    n <- session$returned()$n
    session$setInputs(pick = "beta")
    session$setInputs(go_normal = 1)
    expect_identical(session$returned()$id, "normal")
    expect_gt(session$returned()$n, n)
    session$setInputs(go_fit = 1)
    expect_identical(session$returned()$tab, "fit")
    session$setInputs(search = "not_a_distribution")
    expect_identical(session$returned()$tab, "fit")
    session$setInputs(compendium = "counts")
    expect_identical(session$returned()[c("tab", "group")],
                     list(tab = "compendium", group = "counts"))
    session$setInputs(compendium = "nope")
    expect_identical(session$returned()$group, "counts")
  })
})

test_that("group cards follow the language", {
  lang <- shiny::reactiveVal("pt-BR")
  shiny::testServer(distlab:::mod_home_server, args = list(lang = lang), {
    expect_match(output$groups$html, "Contagens")
    lang("en-US")
    session$flushReact()
    expect_match(output$groups$html, "Counts")
  })
})
