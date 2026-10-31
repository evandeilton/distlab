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
