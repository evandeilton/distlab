# Browser test of a study report opened from disk with the network off:
# everything it needs is in the file.
test_that("a report opens from disk with its plots, math and fonts", {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
          "Chrome is not available")
  d <- dl_dist("poisson", lambda = 3)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  dl_report(d, path, lang = "pt-BR", relations = FALSE,
            simulations = list(dl_mc_check(d, n = 300, seed = 1)),
            calculations = list(list(mode = "le", x = 2)))
  # Chrome leaves folders in its TMPDIR: give it one that the test removes
  # (short, since Chrome puts a socket in it).
  chrome_tmp <- tempfile("chr")
  dir.create(chrome_tmp)
  on.exit(unlink(chrome_tmp, recursive = TRUE), add = TRUE)
  old_tmp <- Sys.getenv("TMPDIR", unset = NA)
  if (nchar(chrome_tmp) < 60) Sys.setenv(TMPDIR = chrome_tmp)
  chrome <- tryCatch(chromote::Chromote$new(), finally = {
    if (is.na(old_tmp)) Sys.unsetenv("TMPDIR") else Sys.setenv(TMPDIR = old_tmp)
  })
  b <- chrome$new_session(width = 1100, height = 800)
  # Close the browser before its folder is removed.
  on.exit({
    b$close()
    try(chrome$close(), silent = TRUE)
  }, add = TRUE, after = FALSE)
  # Network off: the page must not need it.
  b$Network$enable()
  b$Network$emulateNetworkConditions(offline = TRUE, latency = 0, downloadThroughput = -1,
                                     uploadThroughput = -1)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file://", normalizePath(path)), wait_ = FALSE)
  b$wait_for(loaded)
  ev <- function(js, await = FALSE) {
    b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)$result$value
  }
  widgets <- ev("document.querySelectorAll('.html-widget').length")
  expect_gt(widgets, 5)
  for (i in 1:50) {
    if (identical(ev("document.querySelectorAll('.js-plotly-plot').length"), widgets)) break
    Sys.sleep(0.1)
  }
  expect_identical(ev("typeof window.Plotly"), "object")
  expect_identical(ev("document.querySelectorAll('.js-plotly-plot').length"), widgets)
  # Every figure offers PNG and SVG downloads with Portuguese tooltips.
  expect_identical(ev("document.querySelectorAll('.modebar-btn[data-title=\"Baixar gráfico em SVG (vetorial)\"]').length"),
                   widgets)
  expect_gt(ev("document.querySelectorAll('.dl-math .katex').length"), 10)
  expect_identical(ev("document.querySelectorAll('.dl-math:not([data-rendered])').length"), 0L)
  # KaTeX and brand fonts load from data URIs inside the page.
  fonts <- paste("document.fonts.ready.then(() => [...document.fonts]",
                 ".filter(f => f.status === 'loaded').map(f => f.family).join('|'))")
  families <- ev(fonts, await = TRUE)
  expect_match(families, "KaTeX_Main")
  expect_match(families, "Inter")
  # The page fetched nothing over the network.
  remote <- "performance.getEntriesByType('resource').filter(e => /^https?:/.test(e.name)).length"
  expect_identical(ev(remote), 0L)
  # On a phone the page fits the screen.
  b$Emulation$setDeviceMetricsOverride(width = 375, height = 812, deviceScaleFactor = 1, mobile = TRUE)
  Sys.sleep(0.5)
  expect_lte(ev("document.documentElement.scrollWidth"), 375)
})
