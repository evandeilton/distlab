test_that("pieces show math, infinity, nonexistence and missing closed forms", {
  html <- function(x) as.character(x)
  p <- distlab:::formula_piece_tag(list(expr = "\\frac{1}{2}", condition = "x > 0"),
                                   "\\operatorname{E}(X)", "en-US")
  expect_match(html(p), "\\operatorname{E}(X) = \\frac{1}{2}", fixed = TRUE)
  expect_match(html(p), "dl-piece-cond")
  expect_match(html(p), ">if<")
  inf <- distlab:::formula_piece_tag(list(status = "infinite"), "M_X(t)", "pt-BR")
  expect_match(html(inf), "M_X(t) = \\infty", fixed = TRUE)
  und <- distlab:::formula_piece_tag(list(status = "undefined"), "\\gamma_1", "pt-BR")
  expect_match(html(und), "não existe")
  none <- distlab:::formula_piece_tag(list(expr = NULL), "F^{-1}(u)", "en-US")
  expect_match(html(none), "no closed form")
  expect_no_match(html(none), "dl-piece-cond")
})

test_that("decimals use a comma in pt-BR math", {
  expect_identical(distlab:::tex_decimal("x = 0.5", "pt-BR"), "x = 0{,}5")
  expect_identical(distlab:::tex_decimal("x = 0.5", "en-US"), "x = 0.5")
})

test_that("rows keep every piece with its condition", {
  e <- dl_entry("gamma")
  row <- as.character(distlab:::formula_row(e, "mode", "en-US"))
  expect_identical(lengths(regmatches(row, gregexpr('class="dl-piece"', row))), 2L)
  expect_match(row, "\\alpha \\ge 1", fixed = TRUE)
  expect_match(row, "\\alpha &lt; 1", fixed = TRUE)
  expect_match(as.character(distlab:::formula_row(e, "pdf", "en-US")), "Density")
  pois <- as.character(distlab:::formula_row(dl_entry("poisson"), "pdf", "en-US"))
  expect_match(pois, "Probability mass function")
})

test_that("the generating tab has the pgf only for discrete entries", {
  expect_true("pgf" %in% distlab:::formula_tabs("discrete")$generating)
  expect_false("pgf" %in% distlab:::formula_tabs("continuous")$generating)
  html <- as.character(distlab:::formula_tab_content(dl_entry("binomial"), "generating", "en-US"))
  expect_match(html, "G_X(z)", fixed = TRUE)
})

test_that("every formula key of every entry is placed in a tab", {
  for (did in dl_ids()) {
    e <- dl_entry(did)
    shown <- unlist(distlab:::formula_tabs(e$type))
    expect_true(all(names(e$latex) %in% shown), info = did)
  }
})

test_that("slider labels carry the LaTeX symbol and the R name", {
  par <- dl_entry("gamma")$parameters[[1]]
  html <- as.character(distlab:::param_label(par))
  expect_match(html, "dl-math")
  expect_match(html, "\\alpha", fixed = TRUE)
  expect_match(html, "<code>shape</code>", fixed = TRUE)
})
