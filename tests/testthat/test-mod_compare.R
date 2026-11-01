overlay_inputs <- list(
  mode = "overlay",
  s1_dist = "normal", s1_p_normal_mean = 0, s1_p_normal_sd = 1,
  s2_dist = "student_t", s2_p_student_t_df = 1,
  s3_dist = "student_t", s3_p_student_t_df = 5,
  s4_dist = "gamma", s4_p_gamma_shape = 2, s4_p_gamma_rate = 1
)

test_that("overlay mode draws the active slots and a summary table", {
  shiny::testServer(distlab:::mod_compare_server, args = list(lang = "en-US"), {
    do.call(session$setInputs, overlay_inputs)
    session$elapse(200)
    s <- series()
    expect_length(s, 3L)
    expect_identical(vapply(s, `[[`, "", "label"),
                     c("Normal(mean = 0, sd = 1)", "Student's t(df = 1)", "Student's t(df = 5)"))
    expect_identical(vapply(s, `[[`, "", "dash"), c("solid", "dash", "dot"))
    html <- output$summary$html
    expect_match(html, "does not exist")
    expect_match(html, "(-∞, ∞)", fixed = TRUE)
    expect_type(output$density, "character")
    expect_type(output$cdf, "character")
    # The Cauchy tail does not stretch the shared range far beyond the Normal.
    rng <- range(curves()[[1]]$curve$x)
    expect_lt(diff(rng), 30)
  })
})

test_that("slots can be added and removed, keeping at least two", {
  sess <- recording_session()
  shiny::testServer(distlab:::mod_compare_server, args = list(lang = "en-US"), session = sess, {
    do.call(session$setInputs, overlay_inputs)
    session$setInputs(add = 1)
    expect_identical(active(), 1:4)
    session$elapse(200)
    expect_length(series(), 4L)
    session$setInputs(add = 2)
    expect_identical(active(), 1:4)
    session$setInputs(remove2 = 1)
    expect_identical(active(), c(1L, 3L, 4L))
    session$elapse(200)
    # A slot keeps its color when another one leaves.
    expect_identical(series()[[2]]$color, distlab:::series_colors(4)[3])
    session$setInputs(remove3 = 1)
    session$setInputs(remove4 = 1)
    expect_identical(active(), c(1L, 4L))
    expect_null(preset())
    shows <- Filter(function(m) identical(m$type, "dl-show"), attr(sess, "log")$custom)
    expect_gt(length(shows), 0L)
  })
})

test_that("presets fill the slots or the sweep in one click", {
  sess <- recording_session()
  shiny::testServer(distlab:::mod_compare_server, args = list(lang = "pt-BR"), session = sess, {
    do.call(session$setInputs, overlay_inputs)
    session$setInputs(preset = "binom_pois")
    expect_identical(preset(), "binom_pois")
    expect_identical(active(), 1:2)
    d1 <- input_messages(sess, "s1_dist")
    expect_identical(d1[[length(d1)]]$message$value, "binomial")
    d2 <- input_messages(sess, "s2_dist")
    expect_identical(d2[[length(d2)]]$message$value, "poisson")
    session$setInputs(s1_dist = "binomial", s2_dist = "poisson")
    expect_match(output$s1_params$html, 'data-from="50"', fixed = TRUE)
    expect_match(output$preset_hint$html, "Poisson\\(3\\)")
    session$setInputs(preset = "gamma_shape")
    m <- input_messages(sess, "mode")
    expect_identical(m[[length(m)]]$message$value, "sweep")
    v <- input_messages(sess, "sw_values")
    expect_identical(v[[length(v)]]$message$value, "0,5; 1; 2; 5")
    for (key in names(distlab:::.dl_compare_presets)) {
      session$setInputs(preset = key)
      expect_identical(preset(), key)
    }
    # Unknown presets are ignored.
    session$setInputs(preset = "nope")
    expect_identical(preset(), "poisson_lambda")
  })
})

test_that("every preset builds valid distributions", {
  for (key in names(distlab:::.dl_compare_presets)) {
    pr <- distlab:::.dl_compare_presets[[key]]
    if (pr$mode == "overlay") {
      ds <- lapply(pr$slots, function(s) do.call(dl_dist, c(list(s[[1]]), s[[2]])))
    } else {
      ds <- lapply(pr$values, function(v) {
        vals <- pr$params
        vals[[pr$param]] <- v
        do.call(dl_dist, c(list(pr$id), vals))
      })
    }
    expect_gte(length(ds), 2L)
    curves <- distlab:::compare_curves(lapply(ds, function(d) list(dist = d)))
    expect_true(all(vapply(curves, function(s) nrow(s$curve) > 1, logical(1))), info = key)
  }
})

test_that("sweep mode varies one parameter and validates the values", {
  shiny::testServer(distlab:::mod_compare_server, args = list(lang = "pt-BR"), {
    session$setInputs(mode = "sweep", sw_dist = "gamma", sw_param = "shape",
                      sw_values = "0,5; 1; 2; 5", sw_p_gamma_shape = 2, sw_p_gamma_rate = 1)
    session$elapse(200)
    s <- series()
    expect_length(s, 4L)
    expect_equal(vapply(s, function(x) x$dist$params$shape, 0), c(0.5, 1, 2, 5))
    expect_identical(s[[1]]$label, "Gama(shape = 0,5; rate = 1)")
    # The swept parameter's own slider is hidden.
    expect_match(output$sw_params$html, "dl-param-hidden")
    # The spike of shape 0.5 does not flatten the other curves.
    expect_equal(distlab:::compare_ylim(curves()), c(0, 1.15))
    session$setInputs(sw_values = "1")
    expect_error(series(), "dois a cinco")
    session$setInputs(sw_values = "1; 2; 3; 4; 5; 6")
    expect_error(series(), "dois a cinco")
    session$setInputs(sw_values = "1; -2")
    expect_error(series(), "-2 está fora do domínio")
    session$setInputs(sw_values = "1; x")
    expect_error(series(), "dois a cinco")
  })
})

test_that("mixing discrete and continuous shows bars, curves and a note", {
  shiny::testServer(distlab:::mod_compare_server, args = list(lang = "en-US"), {
    do.call(session$setInputs, overlay_inputs)
    session$setInputs(s1_dist = "poisson", s1_p_poisson_lambda = 4)
    session$elapse(200)
    s <- series()
    expect_true(s[[1]]$discrete)
    expect_false(s[[2]]$discrete)
    expect_match(output$note$html, "Bars are probabilities")
    expect_match(output$density, '"type":"bar"', fixed = TRUE)
    expect_match(output$summary$html, "{0, 1, …}", fixed = TRUE)
  })
})

test_that("a language switch keeps slots and relabels them", {
  lang <- shiny::reactiveVal("en-US")
  sess <- recording_session()
  shiny::testServer(distlab:::mod_compare_server, args = list(lang = lang), session = sess, {
    do.call(session$setInputs, overlay_inputs)
    session$setInputs(s2_dist = "gamma", s2_p_gamma_shape = 2.5, s2_p_gamma_rate = 1)
    session$elapse(200)
    lang("pt-BR")
    session$elapse(200)
    s <- series()
    expect_identical(s[[2]]$label, "Gama(shape = 2,5; rate = 1)")
    expect_identical(active(), 1:3)
    expect_match(output$summary$html, "Variância")
    sel <- input_messages(sess, "s2_dist")
    expect_identical(sel[[length(sel)]]$message$value, "gamma")
  })
})

test_that("requests from other pages fill the first slot and a related one", {
  req <- shiny::reactiveVal(NULL)
  sess <- recording_session()
  shiny::testServer(distlab:::mod_compare_server, args = list(lang = "en-US", select = req),
                    session = sess, {
    do.call(session$setInputs, overlay_inputs)
    req(list(tab = "compare", id = "binomial", params = list(size = 20, prob = 0.1)))
    session$flushReact()
    d1 <- input_messages(sess, "s1_dist")
    expect_identical(d1[[length(d1)]]$message$value, "binomial")
    d2 <- input_messages(sess, "s2_dist")
    expect_true(d2[[length(d2)]]$message$value %in% dl_ids())
    expect_identical(active(), 1:2)
    session$setInputs(s1_dist = "binomial")
    expect_match(output$s1_params$html, 'data-from="20"', fixed = TRUE)
  })
})

test_that("support text and labels follow the language", {
  expect_identical(distlab:::support_text(dl_dist("binomial", size = 10, prob = 0.5), "pt-BR"),
                   "{0; 1; …; 10}")
  expect_identical(distlab:::support_text(dl_dist("bernoulli"), "en-US"), "{0, 1}")
  expect_identical(distlab:::support_text(dl_dist("gamma"), "en-US"), "(0, ∞)")
  expect_identical(distlab:::support_text(dl_dist("uniform", min = 0, max = 1.5), "pt-BR"),
                   "[0; 1,5]")
  expect_identical(distlab:::sweep_defaults(dl_entry("gamma"), "shape"), c(1, 2, 4, 8))
  for (id in dl_ids()) {
    e <- dl_entry(id)
    for (par in e$parameters) {
      v <- distlab:::sweep_defaults(e, par$name)
      expect_gte(length(v), 2L)
    }
  }
})

test_that("sweep defaults respect constraints with the other parameters", {
  for (id in dl_ids()) {
    e <- dl_entry(id)
    for (par in e$parameters) {
      for (v in distlab:::sweep_defaults(e, par$name)) {
        vals <- distlab:::default_params(e)
        vals[[par$name]] <- v
        expect_s3_class(do.call(dl_dist, c(list(id), vals)), "dl_dist")
      }
    }
  }
  expect_true(all(distlab:::sweep_defaults(dl_entry("uniform"), "min") < 1))
  expect_true(all(distlab:::sweep_defaults(dl_entry("discrete_uniform"), "max") >= 1))
  # Values far from the defaults still give a sweep.
  v <- distlab:::sweep_defaults(dl_entry("uniform"), "max", list(min = 9, max = 9.5))
  expect_gte(length(v), 2L)
  expect_true(all(v > 9))
})
