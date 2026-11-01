test_that("explorer builds the distribution from slider inputs", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "gamma", p_gamma_shape = 3, p_gamma_rate = 2)
    expect_identical(dist()$id, "gamma")
    expect_equal(dist()$params$rate, 2)
    expect_false(is_discrete())
    session$setInputs(dist = "poisson", p_poisson_lambda = 4)
    expect_true(is_discrete())
    expect_equal(dl_moments(dist())[["mean"]], 4)
  })
})

test_that("opening a fitted model expands its sliders and keeps useful precision", {
  request <- shiny::reactiveVal(NULL)
  sess <- recording_session()
  shiny::testServer(distlab:::mod_explore_server,
    args = list(lang = "en-US", select = request), session = sess, {
      session$setInputs(dist = "normal", p_normal_mean = 0, p_normal_sd = 1)
      request(list(tab = "explorer", id = "normal",
                   params = list(mean = 70.8970588235, sd = 13.56996)))
      session$flushReact()
      mean <- tail(input_messages(sess, "p_normal_mean"), 1)[[1]]$message
      expect_equal(as.numeric(mean$value), 70.8970588235)
      ranges <- Filter(function(m) !is.null(m$message$max), input_messages(sess, "p_normal_mean"))
      expect_equal(as.numeric(tail(ranges, 1)[[1]]$message$max), 70.8970588235)
      expect_equal(as.numeric(mean$step), .000001)
      request(list(tab = "explorer", id = "normal",
                   params = list(mean = 5.843333333, sd = .8253013)))
      session$flushReact()
      mean <- tail(input_messages(sess, "p_normal_mean"), 1)[[1]]$message
      expect_equal(as.numeric(mean$value), 5.843333333)
      expect_equal(as.numeric(mean$step), .000001)
    })
})

test_that("stale sliders from an earlier visit cannot overwrite a routed fit", {
  request <- shiny::reactiveVal(NULL)
  shiny::testServer(distlab:::mod_explore_server,
    args = list(lang = "en-US", select = request), {
      session$setInputs(dist = "gamma", p_gamma_shape = 3, p_gamma_rate = 2,
                        p_normal_mean = 0, p_normal_sd = 1)
      request(list(tab = "explorer", id = "normal",
                   params = list(mean = 70.8970588235, sd = 13.56996)))
      session$flushReact()
      session$setInputs(dist = "normal")
      expect_equal(pars()$vals$mean, 70.8970588235)
      # Echo of the programmatic update, then an ordinary student edit.
      session$setInputs(p_normal_mean = 70.897059, p_normal_sd = 13.56996)
      session$setInputs(p_normal_mean = 72)
      expect_equal(dist()$params$mean, 72)
    })
})

test_that("switching back to a distribution never reads another one's sliders", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "gamma", p_gamma_shape = 3, p_gamma_rate = 2)
    session$setInputs(dist = "normal", p_normal_mean = 1, p_normal_sd = 0.5)
    expect_equal(dist()$params$sd, 0.5)
    session$setInputs(dist = "gamma")
    expect_identical(dist()$id, "gamma")
    expect_equal(dist()$params$shape, 3)
  })
})

test_that("invalid parameters give a translated validation message", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "pt-BR"), {
    session$setInputs(dist = "gamma", p_gamma_shape = 0, p_gamma_rate = 1)
    err <- tryCatch(dist(), error = function(e) e)
    expect_s3_class(err, "shiny.silent.error")
    expect_match(conditionMessage(err), "fora do domínio")
  })
})

test_that("density_ylim caps unbounded spikes only", {
  spike <- dl_curve(dl_dist("gamma", shape = 0.1, rate = 1))
  expect_length(distlab:::density_ylim(spike), 2)
  smooth <- dl_curve(dl_dist("normal"))
  expect_null(distlab:::density_ylim(smooth))
})

test_that("app UI builds in both languages", {
  for (lang in dl_languages()) {
    expect_s3_class(distlab:::app_ui(lang), "shiny.tag.list")
  }
})

test_that("moments wait for parameters instead of showing NA", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "normal")
    expect_error(output$moments)
  })
})

test_that("moment boxes mark infinite and nonexistent moments", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "student_t", p_student_t_df = 1.5)
    session$elapse(200)
    html <- output$moments$html
    expect_match(html, "∞")
    expect_match(html, "does not exist")
  })
})

test_that("every entry renders sliders, plots and moments in the explorer", {
  # `did`, not `id`: the module's own `id` argument is visible in the test body.
  for (did in dl_ids()) {
    shiny::testServer(distlab:::mod_explore_server, args = list(lang = "pt-BR"), {
      e <- dl_entry(did)
      vals <- lapply(e$parameters, `[[`, "default")
      names(vals) <- distlab:::param_input_id(did, vapply(e$parameters, `[[`, "", "name"))
      do.call(session$setInputs, c(list(dist = did), vals))
      session$elapse(200)
      expect_match(output$params$html, "js-range-slider", info = did)
      expect_s3_class(curve(), "data.frame")
      expect_type(output$density, "character")
      expect_type(output$cdf, "character")
      expect_type(output$moments$html, "character")
    })
  }
})

test_that("integer parameters get whole-number sliders", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "hypergeometric")
    html <- output$params$html
    steps <- regmatches(html, gregexpr('data-step="[^"]*"', html))[[1]]
    expect_identical(steps, rep('data-step="1"', 3))
  })
})

test_that("linked sliders keep min < max by pushing the other value", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "uniform", p_uniform_min = 0, p_uniform_max = 1)
    session$setInputs(p_uniform_min = 3)
    expect_equal(dist()$params$min, 3)
    expect_equal(dist()$params$max, 3.1)
    session$setInputs(p_uniform_max = -2)
    expect_equal(dist()$params$min, -2.1)
    expect_equal(dist()$params$max, -2)
  })
})

test_that("linked sliders clamp k to m + n in the Hypergeometric", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "hypergeometric", p_hypergeometric_m = 10,
                      p_hypergeometric_n = 15, p_hypergeometric_k = 8)
    session$setInputs(p_hypergeometric_m = 2, p_hypergeometric_n = 3)
    expect_equal(dist()$params$k, 5)
    session$setInputs(p_hypergeometric_k = 40)
    expect_equal(dist()$params$k, 5)
  })
})

test_that("the module follows a reactive language", {
  lang <- shiny::reactiveVal("pt-BR")
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = lang), {
    session$setInputs(dist = "gamma", p_gamma_shape = 2.5, p_gamma_rate = 1)
    session$elapse(200)
    expect_match(output$moments$html, "2,5")
    expect_match(output$story, "Eventos")
    lang("en-US")
    session$elapse(200)
    expect_match(output$moments$html, "2.5", fixed = TRUE)
    expect_match(output$story, "Events")
    expect_equal(dist()$params$shape, 2.5)
  })
})

test_that("formula tabs and the related card render for every entry", {
  for (did in dl_ids()) {
    shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
      session$setInputs(dist = did)
      for (tab in c("functions", "moments", "generating", "summaries")) {
        expect_match(output[[paste0("formulas_", tab)]]$html, "dl-formula", info = did)
      }
      expect_type(output$related$html, "character")
    })
  }
})

test_that("clicking a related distribution opens it", {
  sess <- recording_session()
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"),
                    session = sess, {
    session$setInputs(dist = "gamma")
    expect_match(output$related$html, 'data-dist="exponential"')
    session$setInputs(goto = "exponential")
    msgs <- input_messages(sess, "dist")
    expect_identical(msgs[[length(msgs)]]$message$value, "exponential")
  })
})

test_that("plots and moments wait for the debounced distribution to catch up", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "gamma", p_gamma_shape = 3, p_gamma_rate = 2)
    session$elapse(200)
    expect_identical(dist_now()$id, "gamma")
    session$setInputs(dist = "poisson", p_poisson_lambda = 4)
    # Inside the debounce window the old Gamma must not reach the outputs.
    err <- tryCatch(dist_now(), error = function(e) e)
    expect_s3_class(err, "shiny.silent.error")
    expect_error(output$moments)
    session$elapse(200)
    expect_identical(dist_now()$id, "poisson")
    expect_match(output$moments$html, ">4<")
  })
})
