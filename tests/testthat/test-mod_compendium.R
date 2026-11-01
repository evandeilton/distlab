# Custom messages sent to the map widget, by handler name.
vis_messages <- function(session, type) {
  Filter(function(m) identical(m$type, type), attr(session, "log")$custom)
}

test_that("clicking a node or an edge fills the side panel", {
  shiny::testServer(distlab:::mod_compendium_server, args = list(lang = "en-US"), {
    expect_match(output$panel$html, "21")
    session$setInputs(click = list(kind = "node", id = "gamma"))
    expect_identical(sel(), list(kind = "node", id = "gamma"))
    expect_match(output$panel$html, "Gamma")
    expect_match(output$panel$html, "Open in the Explorer")
    expect_match(output$panel$html, "x \\in (0, \\infty)", fixed = TRUE)
    session$setInputs(click = list(kind = "edge", id = "binomial__poisson__limit"))
    expect_identical(sel()$kind, "edge")
    html <- output$panel$html
    expect_match(html, "Binomial")
    expect_match(html, "Limit")
    expect_match(html, "\\mathrm{Poisson}(\\lambda)", fixed = TRUE)
    expect_match(html, 'data-dist="poisson"', fixed = TRUE)
    expect_match(html, 'data-dist="binomial"', fixed = TRUE)
    session$setInputs(click = list(kind = "none", id = NULL))
    expect_null(sel())
    # Unknown ids are ignored.
    session$setInputs(click = list(kind = "node", id = "<script>"))
    expect_null(sel())
  })
})

test_that("open buttons and double clicks route to the Explorer", {
  shiny::testServer(distlab:::mod_compendium_server, args = list(lang = "en-US"), {
    session$setInputs(open = "beta")
    expect_identical(session$returned()[c("tab", "id")], list(tab = "explorer", id = "beta"))
    n <- session$returned()$n
    session$setInputs(dblclick = "cauchy")
    expect_identical(session$returned()$id, "cauchy")
    expect_gt(session$returned()$n, n)
    session$setInputs(open = "not_a_distribution")
    expect_identical(session$returned()$id, "cauchy")
  })
})

test_that("search, list buttons and reset drive the map", {
  sess <- recording_session()
  shiny::testServer(distlab:::mod_compendium_server, args = list(lang = "pt-BR"),
                    session = sess, {
    session$setInputs(search = "poisson")
    expect_identical(sel(), list(kind = "node", id = "poisson"))
    m <- vis_messages(sess, "dl-map-select")
    expect_length(m, 1L)
    expect_identical(m[[1]]$message[c("kind", "id")], list(kind = "node", id = "poisson"))
    v <- input_messages(sess, "view")
    expect_identical(v[[length(v)]]$message$value, "graph")
    session$setInputs(pick_edge = "gamma__beta__transformation")
    expect_identical(sel()$id, "gamma__beta__transformation")
    m <- vis_messages(sess, "dl-map-select")
    expect_identical(m[[2]]$message$kind, "edge")
    expect_identical(unlist(m[[2]]$message$ends), c("gamma", "beta"))
    session$setInputs(groups = "counts", types = c("discrete", "continuous"))
    expect_identical(filters()$groups, "counts")
    upd <- vis_messages(sess, "visShinyUpdateNodes")
    nodes <- upd[[length(upd)]]$message$nodes
    expect_identical(nodes$opacity == 1, nodes$dl_group == "counts")
    session$setInputs(reset = 1)
    expect_null(sel())
    expect_length(vis_messages(sess, "visShinyFit"), 1L)
    resets <- input_messages(sess, "groups")
    expect_setequal(resets[[length(resets)]]$message$value,
                    distlab:::registry_groups())
  })
})

test_that("filters apply to every view", {
  shiny::testServer(distlab:::mod_compendium_server, args = list(lang = "en-US"), {
    session$setInputs(groups = "binary", types = c("discrete", "continuous"),
                      rel_types = c("sum"))
    expect_match(output$bytype$html, "Bernoulli")
    expect_no_match(output$bytype$html, "Poisson")
    expect_match(output$table$html, "Bernoulli")
    expect_no_match(output$table$html, ">Normal<")
    # Bernoulli -> Binomial needs both ends active.
    expect_match(output$relations$html, "No distribution matches the filters")
    session$setInputs(groups = c("binary", "counts"))
    rel <- output$relations$html
    expect_match(rel, "Sum")
    expect_match(rel, 'data-dist="bernoulli__binomial__sum"', fixed = TRUE)
    expect_no_match(rel, "Special case")
    # Nothing checked means nothing shown, not everything.
    session$setInputs(groups = NULL)
    expect_match(output$table$html, "No distribution matches the filters")
  })
})

test_that("switching language keeps view, selection and filters", {
  lang <- shiny::reactiveVal("pt-BR")
  sess <- recording_session()
  shiny::testServer(distlab:::mod_compendium_server, args = list(lang = lang),
                    session = sess, {
    session$setInputs(groups = c("counts", "positive"), types = "discrete",
                      click = list(kind = "node", id = "geometric"))
    expect_match(output$panel$html, "Geométrica")
    expect_match(output$bytype$html, "Contagens")
    lang("en-US")
    session$flushReact()
    expect_identical(sel(), list(kind = "node", id = "geometric"))
    expect_identical(filters()$groups, c("counts", "positive"))
    expect_match(output$panel$html, "Geometric")
    expect_match(output$bytype$html, "Counts")
    upd <- vis_messages(sess, "visShinyUpdateNodes")
    nodes <- upd[[length(upd)]]$message$nodes
    expect_identical(nodes$label[nodes$id == "geometric"], "Geometric")
    expect_false(any(vapply(attr(sess, "log")$input, function(m) identical(m$id, "view"),
                            logical(1))))
  })
})

test_that("a group request from Home opens the cards view with that group", {
  req <- shiny::reactiveVal(NULL)
  sess <- recording_session()
  shiny::testServer(distlab:::mod_compendium_server,
                    args = list(lang = "en-US", request = req), session = sess, {
    req(list(tab = "compendium", group = "bounded", n = 1))
    session$flushReact()
    g <- input_messages(sess, "groups")
    expect_identical(g[[length(g)]]$message$value, "bounded")
    v <- input_messages(sess, "view")
    expect_identical(v[[length(v)]]$message$value, "bytype")
    req(list(tab = "explorer", id = "beta", n = 2))
    session$flushReact()
    expect_length(input_messages(sess, "view"), 1L)
  })
})

test_that("panels, cards and tables escape registry text", {
  reg <- dl_registry()
  reg$gamma$names[["en-US"]] <- "<b>bad</b>"
  reg$gamma$story[["en-US"]] <- "<script>x</script>"
  html <- as.character(distlab:::bytype_tag("en-US", "c-open", reg = reg))
  expect_no_match(html, "<b>bad", fixed = TRUE)
  html <- as.character(distlab:::catalog_table("en-US", "c-open", reg = reg))
  expect_no_match(html, "<b>bad", fixed = TRUE)
  html <- as.character(distlab:::node_panel(reg$gamma, "en-US", "c-open", "c-edge"))
  expect_no_match(html, "<script>", fixed = TRUE)
})

test_that("cross-listed entries appear under each of their groups", {
  reg <- dl_registry()
  reg$beta$also_in <- "positive"
  html <- as.character(distlab:::bytype_tag("en-US", "c-open", reg = reg))
  expect_identical(lengths(regmatches(html, gregexpr('data-dist="beta"', html))), 2L)
  expect_match(html, "dl-cross")
  expect_true("beta" %in% distlab:::group_members("positive", reg))
})

test_that("the catalog table is sortable and searchable", {
  html <- as.character(distlab:::catalog_table("pt-BR", "c-open"))
  expect_match(html, "dl-sortable")
  expect_identical(lengths(regmatches(html, gregexpr("<tr data-search", html))),
                   length(dl_ids()))
  expect_match(html, 'aria-sort="ascending"', fixed = TRUE)
  expect_match(html, "stats::dgamma", fixed = TRUE)
  expect_match(html, "Família gama")
})

test_that("selecting an edge of a hidden type switches that type back on", {
  sess <- recording_session()
  shiny::testServer(distlab:::mod_compendium_server, args = list(lang = "en-US"),
                    session = sess, {
    session$setInputs(rel_types = "sum")
    session$setInputs(pick_edge = "binomial__poisson__limit")
    r <- input_messages(sess, "rel_types")
    expect_setequal(r[[length(r)]]$message$value, c("sum", "limit"))
    expect_identical(sel()$id, "binomial__poisson__limit")
  })
})

test_that("a note shows while any filter is narrowed", {
  lang <- shiny::reactiveVal("en-US")
  shiny::testServer(distlab:::mod_compendium_server, args = list(lang = lang), {
    expect_null(output$filter_note)
    session$setInputs(groups = "counts")
    expect_match(output$filter_note$html, "Filtered view")
    lang("pt-BR")
    session$flushReact()
    expect_match(output$filter_note$html, "Visão filtrada")
    session$setInputs(groups = distlab:::registry_groups(), rel_types = "limit")
    expect_match(output$filter_note$html, "Visão filtrada")
    session$setInputs(rel_types = c("special_case", "transformation", "limit", "sum"))
    expect_null(output$filter_note)
  })
})

test_that("the list of relations and the map use the same edge rule", {
  reg <- dl_registry()
  for (g in distlab:::registry_groups()) {
    e <- distlab:::graph_edges("en-US", groups = g)
    html <- as.character(distlab:::relations_table("en-US", "o", "p", groups = g))
    ids <- e$id[e$dl_active]
    n <- lengths(regmatches(html, gregexpr('data-input="p"', html)))
    expect_identical(n, length(ids), info = g)
    for (i in ids) expect_match(html, sprintf('data-dist="%s"', i), fixed = TRUE, info = i)
  }
})

test_that("cross-listed entries pass group filters in every view", {
  reg <- dl_registry()
  reg$beta$also_in <- "positive"
  act <- distlab:::node_active(reg, "positive", c("discrete", "continuous"))
  expect_true(act[["beta"]])
  expect_false(act[["uniform"]])
  expect_identical(names(act)[act],
                   distlab:::group_members("positive", reg))
  n <- distlab:::graph_nodes("en-US", groups = "positive", reg = reg)
  expect_identical(n$opacity[n$id == "beta"], 1)
  html <- as.character(distlab:::catalog_table("en-US", "o", groups = "positive", reg = reg))
  expect_match(html, 'data-dist="beta"', fixed = TRUE)
  html <- as.character(distlab:::bytype_tag("en-US", "o", groups = "positive", reg = reg))
  expect_match(html, 'data-dist="beta"', fixed = TRUE)
  e <- distlab:::graph_edges("en-US", groups = c("positive"), reg = reg)
  expect_true(e$dl_active[e$from == "gamma" & e$to == "beta"])
})

test_that("the Support column is not sortable", {
  html <- as.character(distlab:::catalog_table("en-US", "o"))
  expect_no_match(html, 'data-col="4"', fixed = TRUE)
  expect_match(html, "<th scope=\"col\">Support</th>")
})

test_that("the node panel links to the Calculator and Compare", {
  html <- as.character(distlab:::node_panel(dl_entry("gamma"), "pt-BR", "c-open", "c-edge",
                                            calc_id = "c-calc", compare_id = "c-compare"))
  expect_match(html, 'data-input="c-calc"', fixed = TRUE)
  expect_match(html, 'data-input="c-compare"', fixed = TRUE)
  expect_match(html, "Comparar com")
  html <- as.character(distlab:::node_panel(dl_entry("gamma"), "en-US", "c-open", "c-edge"))
  expect_no_match(html, "c-calc", fixed = TRUE)
})
