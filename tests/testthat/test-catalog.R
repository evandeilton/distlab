test_that("dl_relations lists every registry edge exactly once", {
  rel <- dl_relations()
  expect_named(rel, c("from", "to", "type", "condition_en", "condition_pt", "latex", "label"))
  n <- sum(vapply(dl_registry(), function(e) length(e$relationships), integer(1)))
  expect_identical(nrow(rel), n)
  expect_false(anyDuplicated(distlab:::edge_ids(rel)) > 0)
  expect_false(anyNA(rel))
  expect_true(all(rel$from %in% dl_ids()) && all(rel$to %in% dl_ids()))
  expect_true(all(rel$type %in% c("special_case", "transformation", "sum", "limit")))
  expect_true(all(nzchar(rel$label)))
})

test_that("dl_relations filters by id and checks it", {
  p <- dl_relations("poisson")
  expect_true(all(p$from == "poisson" | p$to == "poisson"))
  expect_true(any(p$from == "binomial" & p$to == "poisson" & p$type == "limit"))
  # A self loop counts once.
  expect_identical(sum(dl_relations("normal")$to == "normal" &
                         dl_relations("normal")$from == "normal"), 1L)
  expect_error(dl_relations("nope"), "Unknown distribution id")
})

test_that("dl_catalog has one complete row per entry", {
  cat <- dl_catalog()
  expect_identical(cat$id, dl_ids())
  expect_named(cat, c("id", "name_en", "name_pt", "group", "subgroup", "type", "tier",
                      "n_params", "n_relations"))
  expect_false(anyNA(cat))
  expect_identical(cat$name_pt[cat$id == "gamma"], "Gama")
  rel <- dl_relations()
  for (i in cat$id) {
    expect_identical(cat$n_relations[cat$id == i], sum(rel$from == i | rel$to == i), info = i)
  }
  expect_identical(cat$n_params[cat$id == "hypergeometric"], 3L)
})

test_that("supports and parameters render as valid TeX", {
  reg <- dl_registry()
  expect_identical(distlab:::support_tex(reg$bernoulli), "\\{0, 1\\}")
  expect_identical(distlab:::support_tex(reg$poisson), "\\{0, 1, 2, \\ldots\\}")
  expect_identical(distlab:::support_tex(reg$normal), "(-\\infty, \\infty)")
  expect_identical(distlab:::support_tex(reg$gamma), "(0, \\infty)")
  expect_identical(distlab:::support_tex(reg$uniform), "[{a}, {b}]")
  expect_match(distlab:::support_tex(reg$hypergeometric), "\\max(0, {k} - {n})", fixed = TRUE)
  for (e in reg) {
    expect_identical(distlab:::latex_string_problems(distlab:::support_tex(e), e$id),
                     character(), info = e$id)
    expect_identical(distlab:::latex_string_problems(distlab:::params_tex(e), e$id),
                     character(), info = e$id)
  }
})

test_that("every subgroup and tier has a name in both languages", {
  for (lang in dl_languages()) {
    for (sg in unique(dl_catalog()$subgroup)) {
      expect_false(identical(distlab:::subgroup_label(sg, lang), gsub("_", " ", sg)),
                   info = paste(lang, sg))
    }
    expect_false(identical(distlab:::tr("tier.1", lang), "tier.1"))
  }
  expect_identical(distlab:::subgroup_label("new_family", "en-US"), "new family")
})

test_that("excerpts cut at a sentence or a word", {
  expect_identical(distlab:::text_excerpt("Short."), "Short.")
  long <- paste(rep("One sentence here.", 30), collapse = " ")
  out <- distlab:::text_excerpt(long, 60)
  expect_lte(nchar(out), 60)
  expect_match(out, "\\.$")
  out <- distlab:::text_excerpt(strrep("word ", 80), 50)
  expect_match(out, "…$")
})

test_that("registry validation covers graph, label and also_in", {
  e <- dl_registry()$gamma
  bad <- e; bad$graph <- list(xy = c(1, NA))
  expect_match(distlab:::validate_entry(bad), "graph needs xy", all = FALSE)
  bad <- e; bad$relationships[[1]]$label <- "\\alpha = 1"
  expect_match(distlab:::validate_entry(bad), "plain-text label", all = FALSE)
  bad <- e; bad$also_in <- "positive"
  expect_match(distlab:::validate_entry(bad), "also_in", all = FALSE)
  ok <- e; ok$also_in <- "bounded"; ok$graph <- NULL
  expect_identical(distlab:::validate_entry(ok), character())
})
