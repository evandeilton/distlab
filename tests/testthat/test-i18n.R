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

test_that("moments show infinity and translated nonexistence", {
  expect_identical(distlab:::format_moment(Inf, "en-US"), "∞")
  expect_identical(distlab:::format_moment(-Inf, "pt-BR"), "-∞")
  expect_identical(distlab:::format_moment(NA_real_, "en-US"), "does not exist")
  expect_identical(distlab:::format_moment(NA_real_, "pt-BR"), "não existe")
  expect_identical(distlab:::format_moment(2.5, "pt-BR"), "2,5")
})

test_that("numbers show at most 4 decimal places and tiny values as powers of ten", {
  f <- distlab:::format_num
  v <- distlab:::format_value
  expect_identical(v(1.959964, "pt-BR"), "1,96")
  expect_identical(v(1.959964, "en-US"), "1.96")
  expect_identical(f(0.99997, "pt-BR"), "1")
  expect_identical(v(12.345678, "pt-BR"), "12,3457")
  expect_identical(f(c(12.345678, 0.5158, 2.5), "en-US"), c("12.35", "0.5158", "2.5"))
  expect_identical(f(1234.56789, "en-US", digits = 5), "1234.6")
  # Values that would round to 0 at 4 decimals: 2 significant digits.
  expect_identical(f(1.6544e-8, "pt-BR"), "1,7 × 10⁻⁸")
  expect_identical(f(1.3217e-43, "en-US"), "1.3 × 10⁻⁴³")
  expect_identical(f(-1.3e-40, "pt-BR"), "-1,3 × 10⁻⁴⁰")
  expect_identical(f(9.96e-6, "en-US"), "1.0 × 10⁻⁵")
  expect_identical(f(4.99e-5, "en-US"), "5.0 × 10⁻⁵")
  expect_identical(f(4.9e-324, "en-US"), "4.9 × 10⁻³²⁴")
  # Below 0.0001 powers of ten; from 0.0001 on, 4 decimal places.
  expect_identical(f(c(5e-5, 0.00009, 0.000123, -0.00005, 1e-4), "pt-BR"),
                   c("5,0 × 10⁻⁵", "9,0 × 10⁻⁵", "0,0001", "-5,0 × 10⁻⁵", "0,0001"))
  expect_identical(f(c(0, -0), "en-US"), c("0", "0"))
  # Vectorized, with NA and infinite values as before.
  expect_identical(f(c(NA, Inf, 1.5), "pt-BR"), c("—", "—", "1,5"))
  expect_identical(v(c(Inf, -Inf, NA, 2e-6), "en-US"),
                   c("∞", "-∞", "—", "2.00 × 10⁻⁶"))
  expect_identical(distlab:::format_moment(1.3e-7, "pt-BR"), "1,3 × 10⁻⁷")
  # Text fields keep 15 digits so a value parses back; TeX gets its own form.
  expect_identical(distlab:::format_input(1.959963984540054, "pt-BR"), "1,95996398454005")
  expect_identical(distlab:::format_input(Inf, "en-US"), "Inf")
  expect_identical(distlab:::tex_num(1.3e-40, "pt-BR"), "1{,}30 \\times 10^{-40}")
  expect_identical(distlab:::tex_num(1.959964, "en-US"), "1.96")
})

test_that("values that name a point or a parameter keep 3 significant digits", {
  v <- distlab:::format_value
  expect_identical(v(c(0.0123, 0.00999, 0.00012, -0.00012), "en-US"),
                   c("0.0123", "9.99 × 10⁻³", "1.20 × 10⁻⁴",
                     "-1.20 × 10⁻⁴"))
  expect_identical(v(0.000128155, "pt-BR"), "1,28 × 10⁻⁴")
  expect_identical(v(123456.789, "en-US"), "123457")
  # Normal(0, 1e-4): the Calculator states the event it computed.
  d <- dl_dist("normal", mean = 0, sd = 1e-4)
  le <- distlab:::calc_result(d, "le", x = 0.00012)
  q <- distlab:::calc_result(d, "quantile", p = 0.9)
  expect_identical(distlab:::calc_tex(le, "en-US"), "P(X \\le 1.20 \\times 10^{-4})")
  expect_identical(distlab:::calc_tex(q, "pt-BR"), "Q(0{,}9) = 1{,}28 \\times 10^{-4}")
  expect_match(distlab:::calc_sentence(le, d, "en-US"), "equal to 1.20 × 10⁻⁴ is 0.8849",
               fixed = TRUE)
  expect_match(distlab:::calc_sentence(q, d, "pt-BR"), "O quantil de ordem 0,9 é 1,28 × 10⁻⁴",
               fixed = TRUE)
  html <- as.character(distlab:::calc_result_tag(q, d, "en-US", code_id = NULL))
  expect_match(html, "1.28 × 10⁻⁴", fixed = TRUE)
  # Labels of nearby parameter values stay distinct.
  for (lang in dl_languages()) {
    a <- distlab:::series_label(dl_dist("normal", sd = 0.00012), lang)
    b <- distlab:::series_label(dl_dist("normal", sd = 0.00014), lang)
    expect_false(identical(a, b), info = lang)
  }
})
