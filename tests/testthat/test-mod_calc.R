test_that("the calculator computes each mode from the fields", {
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "gamma", p_gamma_shape = 2, p_gamma_rate = 1,
                      mode = "between", a = "1.2", b = "3.4")
    session$elapse(200)
    expect_equal(exp(res()$log_p), pgamma(3.4, 2, 1) - pgamma(1.2, 2, 1))
    html <- output$result$html
    expect_match(html, "between 1.2 and 3.4 is 0.5158 (51.58%)", fixed = TRUE)
    expect_match(html, "pgamma(3.4, shape = 2, rate = 1) - pgamma(1.2, shape = 2, rate = 1)",
                 fixed = TRUE)
    expect_match(html, "dl-copy")
    session$setInputs(mode = "gt", x = "50")
    session$elapse(200)
    expect_equal(exp(res()$log_p), pgamma(50, 2, 1, lower.tail = FALSE))
    expect_match(output$result$html, "lower.tail = FALSE", fixed = TRUE)
    session$setInputs(mode = "le", x = "2")
    session$elapse(200)
    expect_equal(exp(res()$log_p), pgamma(2, 2, 1))
    session$setInputs(mode = "quantile", p = "0.9")
    session$elapse(200)
    expect_equal(res()$q, qgamma(0.9, 2, 1))
    session$setInputs(mode = "central", prob = "0.95")
    session$elapse(200)
    expect_equal(res()$q, qgamma(c(0.025, 0.975), 2, 1))
    expect_type(output$density, "character")
    expect_type(output$cdf, "character")
  })
})

test_that("discrete intervals use the chosen ends and explain them", {
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = "pt-BR"), {
    session$setInputs(dist = "binomial", p_binomial_size = 10, p_binomial_prob = 0.5,
                      mode = "between", a = "2", b = "5")
    session$elapse(200)
    expect_equal(exp(res()$log_p), sum(dbinom(3:5, 10, 0.5)))
    expect_match(output$endpoints$html, "lower_end")
    session$setInputs(lower_end = "closed", upper_end = "open")
    session$elapse(200)
    expect_equal(exp(res()$log_p), sum(dbinom(2:4, 10, 0.5)))
    html <- output$result$html
    expect_match(html, "(2 ≤ X &lt; 5)", fixed = TRUE)
    expect_match(html, "P(X &lt;= 4) - P(X &lt;= 1)", fixed = TRUE)
    session$setInputs(mode = "quantile", p = "0,9")
    session$elapse(200)
    expect_identical(res()$q, qbinom(0.9, 10, 0.5))
    expect_match(output$result$html, "menor x com F\\(x\\)")
    # Continuous distributions show a note instead of the end choices.
    session$setInputs(dist = "normal", p_normal_mean = 0, p_normal_sd = 1)
    expect_no_match(output$endpoints$html, "lower_end")
    expect_match(output$endpoints$html, "mesma")
  })
})

test_that("fields accept a decimal comma and report invalid input", {
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = "pt-BR"), {
    session$setInputs(dist = "normal", p_normal_mean = 0, p_normal_sd = 1,
                      mode = "between", a = "-1,96", b = "1,96")
    session$elapse(200)
    expect_equal(exp(res()$log_p), diff(pnorm(c(-1.96, 1.96))))
    expect_match(output$result$html, "0,95")
    session$setInputs(a = "abc")
    expect_error(res(), "Digite um número")
    session$setInputs(a = "3", b = "1")
    expect_error(res(), "não pode ser maior")
    session$setInputs(mode = "quantile", p = "1,5")
    expect_error(res(), "estritamente entre 0 e 1")
  })
})

test_that("tiny tails show their size, not zero", {
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "normal", p_normal_mean = 0, p_normal_sd = 1,
                      mode = "gt", x = "40")
    session$elapse(200)
    html <- output$result$html
    expect_match(html, "3.656 × 10⁻³⁵⁰", fixed = TRUE)
    expect_match(html, "&lt; 0.01%", fixed = TRUE)
  })
})

test_that("values are kept per distribution and restored on return", {
  sess <- recording_session()
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = "en-US"), session = sess, {
    session$setInputs(dist = "gamma", p_gamma_shape = 2, p_gamma_rate = 1, a = "0.7", b = "4")
    session$setInputs(dist = "poisson", p_poisson_lambda = 3)
    # Poisson starts at its quartiles.
    a_msgs <- input_messages(sess, "a")
    expect_identical(a_msgs[[length(a_msgs)]]$message$value, "2")
    session$setInputs(dist = "gamma")
    a_msgs <- input_messages(sess, "a")
    expect_identical(a_msgs[[length(a_msgs)]]$message$value, "0.7")
  })
})

test_that("a language switch rewrites the numbers with the new decimal mark", {
  lang <- shiny::reactiveVal("pt-BR")
  sess <- recording_session()
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = lang), session = sess, {
    session$setInputs(dist = "gamma", p_gamma_shape = 2, p_gamma_rate = 1,
                      mode = "between", a = "1,2", b = "3,4")
    session$elapse(200)
    lang("en-US")
    session$flushReact()
    a_msgs <- input_messages(sess, "a")
    expect_identical(a_msgs[[length(a_msgs)]]$message$value, "1.2")
    session$setInputs(a = "1.2", b = "3.4")
    session$elapse(200)
    expect_identical(input$mode, "between")
    expect_equal(dist()$params$shape, 2)
    expect_match(output$result$html, "between 1.2 and 3.4")
  })
})

test_that("a request from the Explorer opens the distribution with its values", {
  req <- shiny::reactiveVal(NULL)
  sess <- recording_session()
  shiny::testServer(distlab:::mod_calc_server, args = list(lang = "en-US", select = req),
                    session = sess, {
    session$setInputs(dist = "normal", p_normal_mean = 0, p_normal_sd = 1)
    req(list(tab = "calculator", id = "gamma", params = list(shape = 3.5, rate = 2)))
    session$flushReact()
    sel <- input_messages(sess, "dist")
    expect_identical(sel[[length(sel)]]$message$value, "gamma")
    session$setInputs(dist = "gamma")
    expect_match(output$params$html, 'data-from="3.5"', fixed = TRUE)
    # Requests for other pages are ignored.
    n <- length(input_messages(sess, "dist"))
    req(list(tab = "explorer", id = "poisson"))
    session$flushReact()
    expect_length(input_messages(sess, "dist"), n)
  })
})
