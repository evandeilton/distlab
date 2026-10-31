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

test_that("parameter combinations that break a constraint give a message", {
  shiny::testServer(distlab:::mod_explore_server, args = list(lang = "en-US"), {
    session$setInputs(dist = "uniform", p_uniform_min = 3, p_uniform_max = 1)
    err <- tryCatch(dist(), error = function(e) e)
    expect_s3_class(err, "shiny.silent.error")
    expect_match(conditionMessage(err), "min < max")
  })
})
