test_that("the Fit UI exposes bilingual input, diagnostics and export controls", {
  en <- as.character(distlab:::mod_fit_ui("fit", "en-US"))
  pt <- as.character(distlab:::mod_fit_ui("fit", "pt-BR"))
  expect_match(en, "Fit selected families", fixed = TRUE)
  expect_match(pt, "Ajustar famílias selecionadas", fixed = TRUE)
  expect_match(en, "Grouped intervals", fixed = TRUE)
  expect_match(pt, "Simulação com semente", fixed = TRUE)
  for (id in c("load", "run", "histogram", "ecdf", "density", "cdf", "qq", "pp",
               "download_ranking", "download_estimates", "download_code", "download_data")) {
    expect_match(en, paste0('id="fit-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("loading and fitting are explicit, and both preserve the prior fit", {
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = "en-US"), {
    session$setInputs(source = "example", example = "iris", level = ".95", load = 0, run = 0)
    expect_null(loaded())
    expect_null(result())
    session$setInputs(load = 1)
    expect_equal(loaded()$n, 150)
    expect_match(output$descriptives$html, "Mean", fixed = TRUE)
    expect_null(result())
    session$setInputs(candidates = c("normal", "gamma"), run = 1)
    expect_s3_class(result(), "dl_fit")
    expect_setequal(names(result()$fits), c("normal", "gamma"))
    expect_match(output$ranking$html, "Akaike weight", fixed = TRUE)
    first <- result()
    session$setInputs(example = "faithful", candidates = "normal")
    expect_identical(result(), first)
    session$setInputs(load = 2)
    expect_equal(loaded()$n, 272)
    expect_identical(result(), first)
    expect_match(output$status$html, "previous data", fixed = TRUE)
    session$setInputs(run = 2)
    expect_equal(result()$data$n, 272)
  })
})

test_that("invalid tables report errors without destroying the loaded data", {
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = "pt-BR"), {
    session$setInputs(source = "example", example = "iris", load = 0)
    session$setInputs(load = 1)
    original <- loaded()
    session$setInputs(source = "paste", kind = "raw", text = "name\nhello\nworld", value_col = "name", load = 2)
    expect_match(output$status$html, "Não foi possível carregar", fixed = TRUE)
    expect_identical(loaded(), original)
    session$setInputs(text = "value\n1\n2\nInf", value_col = "value", load = 3)
    expect_match(output$status$html, "Não foi possível carregar", fixed = TRUE)
    expect_identical(loaded(), original)
  })
})

test_that("decimal-comma frequency tables retain exact weighted descriptives", {
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = "pt-BR"), {
    session$setInputs(source = "paste", kind = "frequency", decimal = ",",
      text = "valor;contagem\n1,5;2\n2,5;3\n4,5;1", value_col = "valor", freq_col = "contagem", load = 0)
    session$setInputs(load = 1)
    expect_identical(loaded()$type, "frequency")
    expect_equal(loaded()$n, 6)
    expect_equal(distlab:::dl_fit_describe(loaded())$mean, mean(c(1.5, 1.5, 2.5, 2.5, 2.5, 4.5)))
    expect_match(output$descriptives$html, "2,5", fixed = TRUE)
    expect_type(output$histogram, "character")
    expect_type(output$ecdf, "character")
    session$setInputs(candidates = "poisson", run = 0)
    session$setInputs(run = 1)
    expect_null(result())
    expect_match(output$status$html, "família elegível", fixed = TRUE)
  })
})

test_that("grouped input retains intervals and disables individual diagnostics", {
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = "en-US"), {
    session$setInputs(source = "paste", kind = "grouped", decimal = ".",
      text = "lower,upper,n\n-Inf,-1,12\n-1,0,32\n0,1,40\n1,Inf,16",
      lower_col = "lower", upper_col = "upper", freq_col = "n", load = 0, run = 0)
    session$setInputs(load = 1)
    expect_identical(loaded()$type, "grouped")
    expect_match(output$descriptives$html, "approximate", fixed = TRUE)
    session$setInputs(candidates = "normal", run = 1)
    expect_s3_class(result(), "dl_fit")
    expect_match(output$ranking$html, "Interval probabilities", fixed = TRUE)
    expect_type(output$density, "character")
    expect_match(output$gof$html, "Descriptive", fixed = TRUE)
    expect_error(output$qq, "unavailable")
    expect_match(output$diagnostic_notes$html, "Individual Q–Q and P–P plots are unavailable", fixed = TRUE)
  })
})

test_that("changing language and dark mode does not repeat estimation", {
  calls <- 0L
  real <- distlab:::fit_models
  local_mocked_bindings(fit_models = function(...) { calls <<- calls + 1L; real(...) },
                        .package = "distlab")
  lang <- shiny::reactiveVal("en-US")
  dark <- shiny::reactiveVal(FALSE)
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = lang, dark = dark), {
    session$setInputs(source = "example", example = "iris", load = 0, run = 0)
    session$setInputs(load = 1)
    session$setInputs(candidates = "normal", run = 1)
    expect_identical(calls, 1L)
    first <- result()
    expect_type(output$density, "character")
    expect_type(output$qq, "character")
    expect_type(output$pp, "character")
    expect_match(output$estimates$html, "Std. error", fixed = TRUE)
    lang("pt-BR")
    dark(TRUE)
    session$flushReact()
    expect_match(output$estimates$html, "Erro-padrão", fixed = TRUE)
    expect_match(output$ranking$html, "Peso de Akaike", fixed = TRUE)
    expect_type(output$density, "character")
    expect_identical(calls, 1L)
    expect_identical(result(), first)
    session$setInputs(open_explorer = 1)
    expect_identical(route()$tab, "explorer")
    expect_identical(route()$params, first$fits$normal$dist$params)
  })
})

test_that("fixed integer designs are passed explicitly to the fitting engine", {
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = "en-US"), {
    session$setInputs(source = "example", example = "counts", load = 0, run = 0)
    session$setInputs(load = 1)
    session$setInputs(candidates = c("poisson", "binomial"))
    expect_match(output$fixed$html, "fixed_binomial_size", fixed = TRUE)
    session$setInputs(fixed_binomial_size = 10, run = 1)
    expect_equal(result()$fits$binomial$dist$params$size, 10)
    expect_true(result()$fits$binomial$estimates$fixed[1])
    expect_match(output$ranking$html, "Discrete probabilities", fixed = TRUE)
  })
})

test_that("seeded data loading preserves the session RNG and is reproducible", {
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = "en-US"), {
    session$setInputs(source = "simulate", sim_dist = "normal", sim_n = 100, seed = 17,
                      p_normal_mean = 2, p_normal_sd = 1, load = 0)
    session$flushReact()
    set.seed(99)
    rng <- .Random.seed
    session$setInputs(load = 1)
    expect_identical(.Random.seed, rng)
    first <- loaded()
    session$setInputs(load = 2)
    expect_identical(loaded(), first)
    expect_equal(first$n, 100)
  })
})

test_that("fit failures, warnings and input errors follow the live language", {
  lang <- shiny::reactiveVal("en-US")
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = lang), {
    session$setInputs(source = "paste", kind = "raw", text = "x\n1\n1\n1", value_col = "x", load = 0, run = 0)
    session$setInputs(load = 1)
    session$setInputs(candidates = "normal", run = 1)
    expect_match(output$ranking$html, "Constant observations", fixed = TRUE)
    lang("pt-BR")
    session$flushReact()
    expect_match(output$ranking$html, "Observações constantes", fixed = TRUE)
    expect_no_match(output$ranking$html, "Constant data", fixed = TRUE)
    session$setInputs(source = "example", example = "iris", load = 2)
    session$setInputs(candidates = "student_t", run = 2)
    expect_match(output$fit_notes$html, "estima apenas df", fixed = TRUE)
    session$setInputs(source = "paste", kind = "frequency", text = "x,n\n1,-1\n2,2",
                      value_col = "x", freq_col = "n", load = 3)
    expect_match(output$status$html, "frequências devem ser inteiros", fixed = TRUE)
    lang("en-US")
    session$flushReact()
    expect_match(output$status$html, "Frequencies must be finite", fixed = TRUE)
  })
  expect_match(distlab:::fit_message_text("An unfamiliar optimizer error", "pt-BR"),
               "Não foi possível concluir", fixed = TRUE)
  expect_match(distlab:::fit_message_text("Fixed parameter `size` is outside its domain.", "pt-BR"),
               "parâmetro fixo está fora do domínio", fixed = TRUE)
})

test_that("pasted decimal-comma columns keep their decimals in both languages", {
  for (lg in c("pt-BR", "en-US")) {
    shiny::testServer(distlab:::mod_fit_server, args = list(lang = lg), {
      session$setInputs(source = "paste", kind = "raw", decimal = "auto",
                        text = "tempo\n1,5\n2,7\n3,1\n4,9", load = 0)
      expect_match(output$columns$html, ">tempo<", fixed = TRUE)
      expect_no_match(output$columns$html, "V1", fixed = TRUE)
      session$setInputs(load = 1)
      expect_equal(loaded()$x, c(1.5, 2.7, 3.1, 4.9))
      session$setInputs(text = "valor\n1,5\nNA\n4,1", value_col = "valor", load = 2)
      expect_equal(loaded()$x, c(1.5, 4.1))
      expect_equal(loaded()$removed, 1)
      session$setInputs(text = "1,5 2,7 3,1", value_col = "V1", load = 3)
      expect_equal(loaded()$x, c(1.5, 2.7, 3.1))
      before <- loaded()
      session$setInputs(text = "x\n1\n2\n1,2,3", value_col = "x", load = 4)
      expect_identical(loaded(), before)
      expect_match(output$status$html, if (lg == "pt-BR") "números diferentes de campos" else
        "different numbers of fields", fixed = TRUE)
    })
  }
})

test_that("tables report removed frequencies and estimates explain their intervals", {
  shiny::testServer(distlab:::mod_fit_server, args = list(lang = "en-US"), {
    session$setInputs(source = "paste", kind = "frequency", decimal = ".",
      text = "x,n\n1,4\nNA,30\n2,6", value_col = "x", freq_col = "n", load = 0)
    session$setInputs(load = 1)
    expect_equal(loaded()$removed_frequency, 30)
    expect_match(output$descriptives$html, "Frequency in removed rows", fixed = TRUE)
    session$setInputs(source = "paste", kind = "grouped",
      text = "lower,upper,n\n0,10,5\n10,20,10\n20,NA,30",
      lower_col = "lower", upper_col = "upper", freq_col = "n", load = 2)
    expect_match(output$status$html, "Type -Inf or Inf", fixed = TRUE)
  })
  ui <- as.character(distlab:::mod_fit_ui("fit", "pt-BR"))
  expect_match(ui, "escala logit das probabilidades", fixed = TRUE)
  expect_match(ui, "Abrir no Explorador", fixed = TRUE)
})

test_that("every note of a combined message is translated", {
  msg <- paste("Support endpoints are nonregular; Wald uncertainty is unavailable and",
               "information criteria need caution. Observed information is singular or",
               "unstable; Wald uncertainty is unavailable.")
  pt <- distlab:::fit_message_text(msg, "pt-BR")
  expect_match(pt, "Limites de suporte estimados", fixed = TRUE)
  expect_match(pt, "informação observada é singular", fixed = TRUE)
  expect_lt(regexpr("Limites", pt), regexpr("informação", pt))
  constant <- dl_fit(rep(2, 5), "uniform")$failures$uniform
  expect_match(distlab:::fit_message_text(constant, "en-US"), "Constant observations", fixed = TRUE)
  for (id in c("cauchy", "betabinomial")) {
    fit <- dl_fit(c(3, 3, 3, 4), id, fixed = if (id == "betabinomial") list(betabinomial = list(size = 10)) else list())
    expect_no_match(distlab:::fit_message_text(fit$failures[[id]], "pt-BR"), "Não foi possível concluir",
                    fixed = TRUE)
  }
  expect_match(distlab:::fit_message_text("Input exceeds 5 MB.", "pt-BR"), "5 MB", fixed = TRUE)
  expect_match(distlab:::fit_message_text(
    "Singular or non-positive observed information: no stable finite interior MLE.", "pt-BR"),
    "não é positiva definida", fixed = TRUE)
  expect_match(distlab:::tr("fit.gof_help", "en-US"), "assume known parameters", fixed = TRUE)
  expect_match(distlab:::tr("fit.gof_help", "pt-BR"), "supõem parâmetros conhecidos", fixed = TRUE)
})

test_that("open classes are drawn inside the plotted range", {
  expect_equal(distlab:::fit_finite_steps(c(10, 20, Inf), c(-Inf, 10, 20, 10, 20, Inf)),
               c(10, 20, 20.5))
  expect_equal(distlab:::fit_finite_steps(c(1, 2), c(1, 2)), c(1, 2))
  g <- dl_fit_data(lower = c(-Inf, 0, 1), upper = c(0, 1, Inf), frequency = c(2, 3, 4))
  built <- plotly::plotly_build(distlab:::fit_preview_plot(g, "ecdf", "en-US", distlab:::plot_theme(FALSE)))
  expect_true(all(is.finite(built$x$data[[1]]$x)))
  expect_equal(max(built$x$data[[1]]$y), 1)
  d <- dl_fit_diagnostics(dl_fit(g, "normal", fixed = list(normal = list(mean = .5, sd = 1))))
  built <- plotly::plotly_build(distlab:::fit_diagnostic_plot(d, "cdf", "en-US", distlab:::plot_theme(FALSE)))
  expect_true(all(is.finite(built$x$data[[1]]$x)))
  expect_equal(max(built$x$data[[1]]$y), 1)
})

test_that("ambiguous commas follow the table structure in both languages", {
  for (lg in c("pt-BR", "en-US")) {
    shiny::testServer(distlab:::mod_fit_server, args = list(lang = lg), {
      session$setInputs(source = "paste", kind = "frequency", decimal = "auto",
                        text = "0,15\n1,32\n2,27", load = 0)
      session$setInputs(value_col = "V1", freq_col = "V2", load = 1)
      expect_identical(loaded()$type, "frequency")
      expect_equal(loaded()$x, c(0, 1, 2))
      expect_equal(loaded()$n, 74)
      session$setInputs(kind = "raw", text = "1,3\n2,4\n5,6", value_col = "V1", load = 2)
      expect_equal(loaded()$x, c(1.3, 2.4, 5.6))
      session$setInputs(text = "1,5, 2,7, 3,1", load = 3)
      expect_equal(loaded()$x, c(1.5, 2.7, 3.1))
      before <- loaded()
      session$setInputs(text = "x\n1,5\nabc\n2,5", value_col = "x", load = 4)
      expect_identical(loaded(), before)
      expect_match(output$status$html, if (lg == "pt-BR") "A coluna contém texto, como “abc”" else
        "The column contains text, such as “abc”", fixed = TRUE)
      session$setInputs(kind = "frequency", text = "x;n\n1,5;3\nn/d;2", value_col = "x",
                        freq_col = "n", load = 5)
      expect_identical(loaded(), before)
      expect_match(output$status$html, "“n/d”", fixed = TRUE)
    })
  }
})
