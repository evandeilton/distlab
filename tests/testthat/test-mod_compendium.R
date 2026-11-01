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
    expect_length(vis_messages(sess, "visShinyFocus"), 1L)
    expect_identical(unlist(vis_messages(sess, "visShinySelectNodes")[[1]]$message$selid), "poisson")
    session$setInputs(pick_edge = "gamma__beta__transformation")
    expect_identical(sel()$id, "gamma__beta__transformation")
    expect_length(vis_messages(sess, "visShinySelectEdges"), 1L)
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
    rel <- output$relations$html
    expect_match(rel, "Sum")
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
