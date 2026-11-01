test_that("related edges list outgoing and incoming relationships", {
  edges <- distlab:::related_edges("exponential")
  dirs <- vapply(edges, `[[`, "", "direction")
  others <- vapply(edges, `[[`, "", "other")
  out_n <- length(dl_entry("exponential")$relationships)
  expect_identical(sum(dirs == "out"), out_n)
  # Gamma lists Exponential as a special case, so it comes in.
  expect_true(any(dirs == "in" & others == "gamma"))
  expect_true(all(others %in% dl_ids()))
})

test_that("every registry edge appears from both ends", {
  reg <- dl_registry()
  for (e in reg) {
    for (rel in e$relationships) {
      back <- distlab:::related_edges(rel$to)
      hit <- vapply(back, function(ed) identical(ed$from, e$id) &&
                      identical(ed$to, rel$to) && identical(ed$type, rel$type), logical(1))
      expect_true(any(hit), info = paste(e$id, "->", rel$to))
    }
  }
})

test_that("the related card shows translated types, conditions and LaTeX", {
  html <- as.character(distlab:::related_list_tag("gamma", "pt-BR", "x-goto"))
  expect_match(html, "Caso particular")
  expect_match(html, "Transformação")
  expect_match(html, "Esperar por um único evento")
  expect_match(html, "\\mathrm{Gamma}(1, \\beta)", fixed = TRUE)
  expect_match(html, 'data-input="x-goto"', fixed = TRUE)
  expect_match(html, 'aria-label="Abrir Exponencial no Explorador"', fixed = TRUE)
  en <- as.character(distlab:::related_list_tag("gamma", "en-US", "x-goto"))
  expect_match(en, "Special case")
})
