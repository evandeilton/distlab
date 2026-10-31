test_that("dictionaries have the same keys in every language", {
  keys <- lapply(dl_languages(), function(l) sort(names(distlab:::dl_dictionary(l))))
  expect_identical(keys[[1]], keys[[2]])
})

test_that("tr fills placeholders and falls back to the key", {
  expect_match(distlab:::tr("error.invalid_params", "en-US", message = "boom"), "boom")
  expect_identical(distlab:::tr("no.such.key", "pt-BR"), "no.such.key")
  expect_error(distlab:::tr("app.title", "fr-FR"), "must be one of")
})

test_that("numbers use the decimal mark of the language", {
  expect_identical(distlab:::format_num(1.5, "pt-BR"), "1,5")
  expect_identical(distlab:::format_num(1.5, "en-US"), "1.5")
  expect_identical(distlab:::format_num(NA_real_, "en-US"), "—")
})
