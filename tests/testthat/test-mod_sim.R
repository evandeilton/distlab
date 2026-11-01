sim_inputs <- list(dist = "gamma", p_gamma_shape = 2, p_gamma_rate = 1, seed = 2026,
                   n = "1000", tab = "sample")

test_that("the sample tab shows plots, the Monte Carlo table and its code", {
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US"), {
    do.call(session$setInputs, sim_inputs)
    session$elapse(400)
    res <- mc()
    expect_identical(nrow(res$table), 8L)
    expect_length(res$sample, 1000L)
    html <- output$mc_table$html
    expect_match(html, "Monte Carlo check")
    expect_match(html, "P(X ≤ 0.9613)", fixed = TRUE)
    expect_match(html, "Holds the exact value")
    expect_match(output$hist_title, "exact density")
    expect_match(output$sample_hist, '"type":"bar"', fixed = TRUE)
    expect_type(output$sample_ecdf, "character")
    code <- output$sample_code$html
    expect_match(code, "set.seed(2026)", fixed = TRUE)
    expect_match(code, "rgamma(1000, shape = 2, rate = 1)", fixed = TRUE)
    expect_match(code, "dl-copy")
    # Discrete distributions draw relative frequencies.
    session$setInputs(dist = "poisson", p_poisson_lambda = 3)
    session$elapse(400)
    expect_match(output$hist_title, "Relative frequencies")
    # Heavy tails: the share of draws off the plot is reported.
    session$setInputs(dist = "cauchy", p_cauchy_location = 0, p_cauchy_scale = 1)
    session$elapse(400)
    expect_match(output$sample_note$html, "outside the plotted range")
    expect_match(output$mc_table$html, "does not exist")
  })
})

test_that("an invalid seed gives a message and the dice draws a new one", {
  sess <- recording_session()
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "pt-BR"), session = sess, {
    do.call(session$setInputs, sim_inputs)
    session$elapse(400)
    for (bad in list(0, 1.5, NA, 3e9)) {
      session$setInputs(seed = bad)
      session$elapse(400)
      expect_error(output$mc_table, "semente deve ser um número inteiro")
    }
    session$setInputs(dice = 1)
    msg <- input_messages(sess, "seed")
    v <- as.numeric(msg[[length(msg)]]$message$value)
    expect_true(v >= 1 && v <= 2147483647 && v == round(v))
    session$setInputs(seed = 7)
    session$elapse(400)
    expect_identical(mc()$seed, 7L)
    expect_match(output$sample_code$html, "set.seed(7)", fixed = TRUE)
  })
})

test_that("the LLN and CLT tabs explain heavy tails", {
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US"), {
    do.call(session$setInputs, sim_inputs)
    session$setInputs(tab = "lln", paths = 2, lln_log = TRUE)
    session$elapse(400)
    expect_identical(max(lln()$paths$path), 2L)
    expect_match(output$lln_note$html, "μ = 2")
    expect_match(output$lln_plot, '"type":"log"', fixed = TRUE)
    session$setInputs(tab = "clt", reps = "500")
    expect_true(clt()$standardized)
    expect_match(output$clt_plot, '"frames"', fixed = TRUE)
    expect_match(output$clt_plot, '"label":"Play"', fixed = TRUE)
    expect_match(output$clt_table$html, "Distance to N(0, 1)", fixed = TRUE)
    session$setInputs(dist = "cauchy", p_cauchy_location = 0, p_cauchy_scale = 1)
    session$elapse(400)
    expect_false(clt()$standardized)
    expect_null(output$clt_table)
    expect_match(output$clt_note$html, "again the same Cauchy")
    session$setInputs(tab = "lln")
    expect_match(output$lln_note$html, "no mean")
    session$setInputs(dist = "student_t", p_student_t_df = 1.5)
    session$elapse(400)
    expect_match(output$lln_note$html, "variance is infinite")
  })
})

test_that("the transformation tab names the exact law or the approximation", {
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US"), {
    do.call(session$setInputs, sim_inputs)
    session$setInputs(dist = "exponential", p_exponential_rate = 1, tab = "transform",
                      op = "sum", k = 3, a = 0, b = 2)
    session$elapse(400)
    expect_identical(tf()$target$id, "gamma")
    expect_match(output$tf_summary$html, "Exact law of Y: Gamma(shape = 3, rate = 1)", fixed = TRUE)
    expect_match(output$tf_plot, '"text":"f(y)"', fixed = TRUE)
    expect_match(output$tf_code$html, "rowSums(x)", fixed = TRUE)
    expect_type(output$tf_plot, "character")
    session$setInputs(op = "max")
    expect_match(output$tf_summary$html, "No catalog distribution")
    session$setInputs(dist = "uniform", p_uniform_min = 0, p_uniform_max = 1, op = "sum", k = 12)
    session$elapse(400)
    expect_match(output$tf_summary$html, "Normal approximation: Normal(mean = 6, sd = 1)", fixed = TRUE)
    session$setInputs(op = "affine", a = 1, b = NA)
    expect_error(output$tf_summary, "Enter numbers for a and b between")
    session$setInputs(b = 1001)
    expect_error(output$tf_summary, "Enter numbers for a and b between")
    session$setInputs(b = 0)
    expect_error(output$tf_summary, "must not be 0")
    session$setInputs(dist = "poisson", p_poisson_lambda = 3, op = "log")
    session$elapse(400)
    expect_error(output$tf_summary, "needs X > 0")
    # Sums of a discrete law keep whole-number bins.
    session$setInputs(op = "sum", k = 2)
    expect_identical(tf_view()$step, 1)
    expect_match(output$tf_plot, "P(Y = y)", fixed = TRUE)
    session$setInputs(dist = "cauchy", p_cauchy_location = 0, p_cauchy_scale = 1, op = "mean", k = 10)
    session$elapse(400)
    expect_match(output$tf_summary$html, "same Cauchy law")
  })
})

test_that("the relations tab animates each kind of demo and opens its ends", {
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US"), {
    do.call(session$setInputs, sim_inputs)
    session$setInputs(tab = "relations", edge = "normal__cauchy__transformation")
    head <- output$rel_head$html
    expect_match(head, "Normal")
    expect_match(head, "Cauchy")
    expect_match(head, "dl-math")
    expect_match(head, "Transformation")
    expect_match(output$rel_table$html, "95% noise level")
    expect_match(output$rel_note$html, "outside the plotted range")
    expect_match(output$rel_plot, '"label":"n = 1000"', fixed = TRUE)
    expect_match(output$rel_code$html, "z1 / z2", fixed = TRUE)
    session$setInputs(edge = "binomial__poisson__limit")
    expect_match(output$rel_note$html, "No draws")
    expect_no_match(output$rel_table$html, "noise level")
    # A limit demo needs no seed.
    session$setInputs(seed = 0)
    session$elapse(400)
    expect_identical(demo()$kind, "limit")
    session$setInputs(open_end = "poisson")
    expect_identical(route()$tab, "explorer")
    expect_identical(route()$id, "poisson")
    expect_identical(route()$params, list(lambda = 3))
    session$setInputs(open_end = "gamma")
    expect_identical(route()$id, "poisson")
  })
})

test_that("the memorylessness tab gives a verdict or offers the Exponential", {
  sess <- recording_session()
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "pt-BR"), session = sess, {
    do.call(session$setInputs, sim_inputs)
    session$setInputs(dist = "exponential", p_exponential_rate = 1, tab = "memoryless")
    session$elapse(400)
    expect_match(output$mem_controls$html, "mem_s")
    session$setInputs(mem_s = 0.8)
    expect_identical(output$mem_ok, "yes")
    html <- output$mem_verdict$html
    expect_match(html, "Sem memória")
    expect_match(html, "dl-sim-verdict-yes")
    expect_match(html, "sorteios passaram de s")
    expect_match(output$mem_code$html, "x[x &gt; s] - s", fixed = TRUE)
    expect_null(output$mem_note)
    expect_type(output$mem_surv, "character")
    expect_type(output$mem_hist, "character")
    session$setInputs(dist = "weibull", p_weibull_shape = 2, p_weibull_scale = 1)
    session$elapse(400)
    expect_match(output$mem_verdict$html, "Com memória")
    session$setInputs(dist = "geometric", p_geometric_prob = 0.3, mem_s = 2)
    session$elapse(400)
    expect_true(mem()$memoryless)
    expect_match(output$mem_note$html, "fracassos antes do primeiro sucesso")
    session$setInputs(dist = "normal", p_normal_mean = 0, p_normal_sd = 1)
    session$elapse(400)
    expect_identical(output$mem_ok, "")
    expect_match(output$mem_controls$html, "suporte")
    session$setInputs(to_exp = 1)
    sel <- input_messages(sess, "dist")
    expect_identical(sel[[length(sel)]]$message$value, "exponential")
    # A threshold far in the tail leaves too few draws.
    session$setInputs(dist = "exponential", p_exponential_rate = 1, mem_s = 20)
    session$elapse(400)
    expect_error(output$mem_verdict, "Menos de 30 sorteios")
  })
})

test_that("every kind of relation demo draws an animated plot", {
  skip_on_cran()
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US"), {
    session$setInputs(seed = 2026, n = "1000", tab = "relations")
    for (e in c("binomial__normal__limit", "student_t__normal__limit",
                "exponential__gamma__sum", "bernoulli__binomial__sum",
                "betabinomial__discrete_uniform__special_case")) {
      session$setInputs(edge = e)
      expect_match(output$rel_plot, '"frames"', fixed = TRUE, info = e)
      expect_match(output$rel_table$html, "Distance", info = e)
    }
  })
})

test_that("requests open a distribution with its values, or a relation", {
  req <- shiny::reactiveVal(NULL)
  sess <- recording_session()
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US", select = req),
                    session = sess, {
    do.call(session$setInputs, sim_inputs)
    req(list(tab = "simulation", id = "binomial", params = list(size = 20, prob = 0.1)))
    session$flushReact()
    sel <- input_messages(sess, "dist")
    expect_identical(sel[[length(sel)]]$message$value, "binomial")
    session$setInputs(dist = "binomial")
    expect_match(output$params$html, 'data-from="20"', fixed = TRUE)
    req(list(tab = "simulation", edge = "gamma__beta__transformation"))
    session$flushReact()
    e <- input_messages(sess, "edge")
    expect_identical(e[[length(e)]]$message$value, "gamma__beta__transformation")
    tabs <- input_messages(sess, "tab")
    expect_identical(tabs[[length(tabs)]]$message$value, "relations")
    # Requests for other pages or unknown ids are ignored.
    n_before <- length(input_messages(sess, "dist"))
    req(list(tab = "calculator", id = "normal"))
    session$flushReact()
    req(list(tab = "simulation", id = "nope"))
    session$flushReact()
    expect_length(input_messages(sess, "dist"), n_before)
  })
})

test_that("a language switch relabels the selectors and the numbers", {
  lang <- shiny::reactiveVal("en-US")
  sess <- recording_session()
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = lang), session = sess, {
    do.call(session$setInputs, sim_inputs)
    session$setInputs(p_gamma_shape = 2.5)
    session$elapse(400)
    expect_match(output$mc_table$html, "Std. error")
    lang("pt-BR")
    session$elapse(400)
    expect_match(output$mc_table$html, "Erro-padrão")
    expect_match(output$mc_table$html, "2,5")
    ops <- input_messages(sess, "op")
    expect_true("Soma de k cópias" %in% names(unlist(ops[[length(ops)]]$message$options)) ||
                  grepl("Soma de k cópias", paste(unlist(ops[[length(ops)]]$message), collapse = " ")))
    edges <- input_messages(sess, "edge")
    expect_match(paste(unlist(edges[[length(edges)]]$message), collapse = " "), "Binomial → Poisson")
    expect_identical(mc()$table$exact[1], 2.5)
  })
})

test_that("the UI has every tab, the seed controls and translated labels", {
  html <- as.character(distlab:::mod_sim_ui("sim", "pt-BR"))
  for (t in distlab:::.dl_sim_tabs) expect_match(html, sprintf('data-value="%s"', t), info = t)
  expect_match(html, "Lei dos Grandes Números")
  expect_match(html, "Teorema Central do Limite")
  expect_match(html, "Falta de memória")
  expect_match(html, "Semente")
  expect_match(html, 'value="2026"', fixed = TRUE)
  expect_match(html, "input.tab == &#39;relations&#39;|input.tab == 'relations'")
  ch <- distlab:::edge_choices("en-US")
  expect_setequal(unname(unlist(ch)), names(distlab:::.dl_sim_recipes))
  expect_true("Binomial → Poisson" %in% names(ch$Limit))
  expect_identical(names(distlab:::transform_choices("pt-BR"))[1], "Soma de k cópias")
  expect_null(distlab:::valid_seed(0))
  expect_identical(distlab:::valid_seed(2147483647), 2147483647L)
  expect_identical(distlab:::format_share(0.0123, "pt-BR"), "1,2%")
  expect_identical(distlab:::format_share(1e-5, "en-US"), "< 0.1%")
})

test_that("lattice transforms keep a bounded number of bars", {
  # a + bX with a huge slope: bars on the lattice of step b, not one per integer.
  p30 <- dl_dist("poisson", lambda = 30)
  y <- dl_transform(p30, "affine", n = 2000, seed = 1, a = 0, b = 1e4)$sample
  v <- distlab:::sim_view(y, NULL, mode = distlab:::transform_bin_mode(p30, "affine", 0, 1e4))
  expect_identical(v$step, 1e4)
  expect_lte(nrow(v$bins), 2000L)
  expect_equal(sum(v$bins$proportion), 1)
  # X^2 of a wide Geometric: too many whole numbers, so continuous bins.
  g <- dl_dist("geometric", prob = 0.01)
  y <- dl_transform(g, "square", n = 2000, seed = 1)$sample
  v <- distlab:::sim_view(y, NULL, mode = distlab:::transform_bin_mode(g, "square", 0, 1))
  expect_true(is.na(v$step))
  expect_lte(nrow(v$bins), 80L)
})

test_that("means of discrete draws sit on their lattice", {
  # Poisson(1) mean of 3 copies: bars of width 1/3 against the Normal approximation.
  res <- dl_transform(dl_dist("poisson", lambda = 1), "mean", k = 3, n = 5000, seed = 1)
  v <- distlab:::sim_view(res$sample, res$approx, mode = "auto")
  expect_equal(v$step, 1 / 3)
  peak <- dnorm(0, sd = 1 / sqrt(3))
  ratio <- max(v$bins$density) / peak
  expect_true(ratio > 1 / 1.3 && ratio < 1.3)
  # Standardized Bernoulli(0.5) means of 30 draws: spacing 1/(sigma sqrt(30)).
  b <- dl_dist("bernoulli", prob = 0.5)
  clt <- dl_clt(b, sizes = c(1, 30), reps = 2000, seed = 1)
  bins <- distlab:::clt_bins(clt, b)
  last <- bins[bins$frame == 2, ]
  expect_equal(last$width[1], 1 / (0.5 * sqrt(30)), tolerance = 1e-9)
  ratio <- max(last$density) / dnorm(0)
  expect_true(ratio > 1 / 1.3 && ratio < 1.3)
  # Continuous laws keep the fixed 0.2-wide bins.
  cn <- distlab:::clt_bins(dl_clt(dl_dist("exponential"), sizes = 5, reps = 200, seed = 1),
                           dl_dist("exponential"))
  expect_equal(unique(round(cn$width, 10)), 0.2)
})

test_that("a very wide discrete law still shows its law and moments", {
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "negbinomial", p_negbinomial_size = 50, p_negbinomial_prob = 0.05,
                      seed = 2026, n = "1000", tab = "transform", op = "sum", k = 30)
    session$elapse(400)
    expect_identical(tf()$target$params$size, 1500)
    expect_match(output$tf_summary$html, "Negative binomial(size = 1500, prob = 0.05)", fixed = TRUE)
    expect_type(output$tf_plot, "character")
  })
  expect_identical(distlab:::sim_error_text(simpleError("internal detail"), "pt-BR"),
                   "Não foi possível rodar a simulação com estes valores.")
})

test_that("special cases of the Exponential and Geometric are named", {
  html <- as.character(distlab:::mem_verdict_tag(
    dl_memoryless(dl_dist("gamma", shape = 1, rate = 2), s = 0.5, n = 500, seed = 1),
    dl_dist("gamma", shape = 1, rate = 2), 500, "en-US"))
  expect_match(html, "dl-sim-verdict-yes")
  expect_match(html, "With these parameters the Gamma is the Exponential.")
  html <- as.character(distlab:::mem_verdict_tag(
    dl_memoryless(dl_dist("negbinomial", size = 1, prob = 0.3), s = 2, n = 500, seed = 1),
    dl_dist("negbinomial", size = 1, prob = 0.3), 500, "pt-BR"))
  expect_match(html, "a Binomial negativa é a Geométrica")
  expect_identical(distlab:::format_gap(0, "en-US"), "0")
  expect_identical(distlab:::format_gap(3e-17, "en-US"), "< 10⁻¹²")
  expect_identical(distlab:::format_gap(0.1839, "pt-BR"), "0,1839")
})

test_that("the threshold slider starts above 0", {
  for (d in list(dl_dist("exponential"), dl_dist("weibull", shape = 0.2, scale = 1),
                 dl_dist("geometric", prob = 0.9), dl_dist("poisson", lambda = 0.5))) {
    sl <- distlab:::mem_slider(d)
    expect_gt(sl$min, 0)
    expect_gte(sl$value, sl$min)
    expect_lte(sl$value, sl$max)
    expect_gt(sl$max, sl$min)
  }
})

test_that("the LLN plot shows the band from k = 1 unless the variance is infinite", {
  r <- dl_lln(dl_dist("normal"), n = 1000, paths = 3, seed = 1)
  y <- distlab:::lln_ylim(r$paths$mean, r$paths$mean[r$paths$k >= 50], 0, 1)
  expect_lte(y[1], -2.2)
  expect_gte(y[2], 2.2)
  expect_lte(y[1], min(r$paths$mean))
  # Heavy tails keep the late running means in view.
  cy <- dl_lln(dl_dist("cauchy"), n = 2000, seed = 3)
  late <- cy$paths$mean[cy$paths$k >= 100]
  y <- distlab:::lln_ylim(cy$paths$mean, late, NA, NA)
  expect_true(is.null(y) || diff(y) < diff(range(cy$paths$mean)) + 1e-9)
})

test_that("a language switch does not re-run the simulation", {
  calls <- 0L
  real <- distlab:::mc_check_run
  local_mocked_bindings(mc_check_run = function(...) {
    calls <<- calls + 1L
    real(...)
  }, .package = "distlab")
  lang <- shiny::reactiveVal("en-US")
  shiny::testServer(distlab:::mod_sim_server, args = list(lang = lang), {
    do.call(session$setInputs, sim_inputs)
    session$elapse(400)
    first <- mc()
    n_calls <- calls
    expect_gte(n_calls, 1L)
    lang("pt-BR")
    session$elapse(400)
    expect_match(output$mc_table$html, "Grandeza")
    expect_identical(calls, n_calls)
    expect_identical(mc(), first)
  })
})

test_that("count choices use the thousands separator of the language", {
  expect_no_warning(pt <- distlab:::count_choices(c(500, 100000), "pt-BR"))
  expect_identical(names(pt), c("500", "100.000"))
  expect_identical(unname(distlab:::count_choices(c(500, 100000), "en-US")), c("500", "100000"))
  expect_identical(names(distlab:::count_choices(100000, "en-US")), "100,000")
  html <- as.character(distlab:::mod_sim_ui("sim", "en-US"))
  expect_match(html, "heavy-tailed laws")
  expect_match(html, ">100,000<", fixed = TRUE)
})

test_that("the Simulation stylesheet keeps tabs inside their card on phones", {
  css <- paste(readLines(system.file("app", "www", "distlab.css", package = "distlab")),
               collapse = "\n")
  expect_match(css, ".dl-sim .tab-content > .tab-pane", fixed = TRUE)
  expect_match(css, "min-width: 0; }", fixed = TRUE)
})
