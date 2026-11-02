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
  # Values that round to exactly 0.0001, and zeros.
  expect_identical(f(c(5e-5, 0.00009, 0.000123, -0.00005), "pt-BR"),
                   c("0,0001", "0,0001", "0,0001", "-0,0001"))
  expect_identical(f(c(0, -0), "en-US"), c("0", "0"))
  # Vectorized, with NA and infinite values as before.
  expect_identical(f(c(NA, Inf, 1.5), "pt-BR"), c("—", "—", "1,5"))
  expect_identical(v(c(Inf, -Inf, NA, 2e-6), "en-US"),
                   c("∞", "-∞", "—", "2.0 × 10⁻⁶"))
  expect_identical(distlab:::format_moment(1.3e-7, "pt-BR"), "1,3 × 10⁻⁷")
  # Text fields keep 15 digits so a value parses back; TeX gets its own form.
  expect_identical(distlab:::format_input(1.959963984540054, "pt-BR"), "1,95996398454005")
  expect_identical(distlab:::format_input(Inf, "en-US"), "Inf")
  expect_identical(distlab:::tex_num(1.3e-40, "pt-BR"), "1{,}3 \\times 10^{-40}")
  expect_identical(distlab:::tex_num(1.959964, "en-US"), "1.96")
})
