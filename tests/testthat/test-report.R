# Study report: sections, self-contained page, numbers in each language and
# an R code appendix that reproduces what the page shows.

report_cases <- list(gamma = list(shape = 2.5, rate = 2), poisson = list(lambda = 2.5),
                     cauchy = list())

# The page without its inline scripts and styles.
page_markup <- function(html) {
  html <- gsub("<script[^>]*>[\\s\\S]*?</script>", "", html, perl = TRUE)
  gsub("<style[^>]*>[\\s\\S]*?</style>", "", html, perl = TRUE)
}

test_that("reports have every section and inline every script, style and font", {
  for (id in names(report_cases)) for (lang in dl_languages()) {
    info <- paste(id, lang)
    html <- cached_report(id, lang, report_cases[[id]])
    expect_true(startsWith(html, "<!DOCTYPE html>"), info = info)
    expect_match(html, sprintf('<html lang="%s">', lang), fixed = TRUE, info = info)
    for (s in c("story", "formulas", "numbers", "plots", "calculations", "relations", "code",
                "references")) {
      expect_match(html, sprintf('<section class="dl-report-section" id="%s">', s), fixed = TRUE,
                   info = paste(info, s))
      expect_true(shown(html, distlab:::tr(paste0("report.section.", s), lang)), info = paste(info, s))
    }
    expect_no_match(html, 'id="fit"', fixed = TRUE)
    markup <- page_markup(html)
    expect_no_match(markup, "<script[^>]*\\ssrc=", perl = TRUE)
    expect_no_match(markup, "<link\\b", perl = TRUE)
    expect_no_match(markup, "(src|href)=\"(https?:)?//", perl = TRUE)
    styles <- regmatches(html, gregexpr("<style[^>]*>[\\s\\S]*?</style>", html, perl = TRUE))[[1]]
    expect_false(any(grepl("url\\(\\s*['\"]?(https?:)?//", styles, perl = TRUE)), info = info)
    expect_true(any(grepl("url(\"data:font/woff2;base64,", styles, fixed = TRUE)), info = info)
    count <- function(p) lengths(regmatches(html, gregexpr(p, html, fixed = TRUE)))
    expect_identical(count('<script data-dependency="katex@'), 1L, info = info)
    expect_identical(count('<style data-dependency="katex@'), 1L, info = info)
    expect_identical(count('<script data-dependency="plotly-main@'), 1L, info = info)
    expect_identical(count('<script data-dependency="distlab-report@'), 1L, info = info)
    expect_gt(count('class="dl-math'), 10L)
    expect_gt(count('class="plotly html-widget'), 8L)
    expect_lt(nchar(html, type = "bytes"), 8e6)
  }
})

test_that("numbers, words and plot separators follow the language", {
  g_en <- cached_report("gamma", "en-US", report_cases$gamma)
  g_pt <- cached_report("gamma", "pt-BR", report_cases$gamma)
  # Gamma(2.5, 2): mean 1.25, variance 0.625.
  expect_true(shown(g_en, "1.25"))
  expect_true(shown(g_en, "0.625"))
  expect_true(shown(g_pt, "1,25"))
  expect_true(shown(g_pt, "0,625"))
  expect_false(shown(g_pt, "1.25"))
  expect_match(g_en, '"separators":".,"', fixed = TRUE)
  expect_match(g_pt, '"separators":",."', fixed = TRUE)
  expect_match(g_pt, "Relatório de estudo: Gama", fixed = TRUE)
  expect_match(g_en, "Study report: Gamma", fixed = TRUE)
  p_pt <- cached_report("poisson", "pt-BR", report_cases$poisson)
  expect_true(shown(p_pt, "2,5"))
  expect_match(p_pt, distlab:::tr("report.numbers.discrete", "pt-BR"), fixed = TRUE)
  # The Cauchy has no moments: named, not shown as numbers, and explained.
  for (lang in dl_languages()) {
    c_html <- cached_report("cauchy", lang, report_cases$cauchy)
    expect_true(shown(c_html, distlab:::tr("moments.undefined", lang)), info = lang)
    expect_match(c_html, distlab:::tr("report.numbers.missing", lang), fixed = TRUE, info = lang)
    expect_match(c_html, distlab:::tr("sim.lln.note.no_mean", lang), fixed = TRUE, info = lang)
  }
})

test_that("the code appendix parses and reproduces the numbers shown", {
  for (id in names(report_cases)) for (lang in dl_languages()) {
    info <- paste(id, lang)
    html <- cached_report(id, lang, report_cases[[id]])
    blocks <- report_code_blocks(html)
    expect_true(all(c("dist", "mc_check", "sweep", "lln", "clt", "calc", "relation") %in% blocks$kind),
                info = info)
    for (i in seq_len(nrow(blocks))) {
      code <- blocks$code[i]
      expect_no_error(parse(text = code))
      env <- eval_block(code)
      what <- paste(info, blocks$kind[i], i)
      switch(blocks$kind[i],
        dist = {
          for (v in env$moments) expect_true(shown(html, distlab:::format_moment(v, lang)), info = what)
        },
        mc_check = {
          tab <- env$.value
          est <- tab$estimate[!is.na(tab$estimate)]
          for (v in est) expect_true(shown(html, distlab:::format_num(v, lang)), info = what)
          se <- tab$se[!is.na(tab$se)]
          for (v in se) expect_true(shown(html, distlab:::format_num(v, lang)), info = what)
        },
        clt = {
          if (is.numeric(env$.value)) {
            for (v in env$.value) expect_true(shown(html, distlab:::format_num(v, lang)), info = what)
          }
        },
        relation = {
          # Each demo computes one distance: ks, tv or kolmogorov.
          dist_v <- c(env$ks, env$tv, env$kolmogorov)
          for (v in dist_v) expect_true(shown(html, distlab:::format_num(v, lang)), info = what)
        },
        calc = {
          v <- env$.value
          text <- if (grepl("^q", code)) distlab:::format_value(v, lang) else
            distlab:::format_prob(log(v), lang, empty = FALSE)
          expect_true(shown(html, text), info = what)
        },
        sweep = expect_true(is.matrix(env$density) && all(is.finite(env$density)), info = what),
        lln = expect_true(is.matrix(env$running) && ncol(env$running) == 3L, info = what)
      )
    }
  }
})

test_that("the default simulations match their engines", {
  d <- dl_dist("gamma", shape = 2.5, rate = 2)
  html <- cached_report("gamma", "en-US", report_cases$gamma)
  blocks <- report_code_blocks(html)
  lln <- eval_block(blocks$code[blocks$kind == "lln"])$running
  expect_identical(as.vector(lln), dl_lln(d, 5000, paths = 3, seed = 2026)$paths$mean)
  mc <- eval_block(blocks$code[blocks$kind == "mc_check"])$.value
  expect_equal(mc, dl_mc_check(d, 5000, seed = 2026), ignore_attr = TRUE)
  expect_match(html, '<dd>2026</dd>', fixed = TRUE)
})

test_that("a report file is written in UTF-8 and the page is returned invisibly", {
  d <- dl_dist("poisson", lambda = 2)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path))
  out <- withVisible(dl_report(d, path, lang = "pt-BR", relations = FALSE,
                               simulations = list(dl_mc_check(d, n = 200, seed = 1))))
  expect_false(out$visible)
  raw <- readBin(path, "raw", file.size(path))
  expect_identical(raw, charToRaw(out$value))
  text <- rawToChar(raw)
  Encoding(text) <- "UTF-8"
  expect_true(validUTF8(text))
  expect_match(text, "História e distribuições relacionadas", fixed = TRUE)
  expect_match(text, "<meta charset=\"utf-8\">", fixed = TRUE)
})

test_that("relations = FALSE leaves the demos out and empty sessions say so", {
  d <- dl_dist("exponential", rate = 2)
  html <- dl_report(d, simulations = list(dl_mc_check(d, n = 200, seed = 1)), relations = FALSE)
  blocks <- report_code_blocks(html)
  expect_false("relation" %in% blocks$kind)
  expect_false(any(c("lln", "clt") %in% blocks$kind))
  expect_match(html, distlab:::tr("report.rel.no_demo", "en-US"), fixed = TRUE)
  expect_match(html, distlab:::tr("report.calc.none", "en-US"), fixed = TRUE)
  # Every relationship of the entry is still listed with its formula.
  rel <- dl_relations("exponential")
  expect_identical(lengths(regmatches(html, gregexpr(distlab:::tr("report.rel.no_demo", "en-US"), html,
                                                     fixed = TRUE))), nrow(rel))
})

test_that("given simulations are reported, and a seeded check gets its draws back", {
  d <- dl_dist("exponential", rate = 2)
  sims <- list(dl_transform(d, "sum", k = 3, n = 300, seed = 2),
               dl_memoryless(d, s = 0.5, n = 300, seed = 3),
               dl_relation_demo("exponential", "gamma", n = 300, seed = 4),
               dl_mc_check(d, n = 300, seed = 5))
  html <- dl_report(d, simulations = sims, relations = FALSE, lang = "pt-BR")
  blocks <- report_code_blocks(html)
  expect_setequal(unique(blocks$kind), c("dist", "mc_check", "sweep", "transform", "memoryless",
                                         "relation"))
  expect_match(html, distlab:::tr("sim.hist.density", "pt-BR"), fixed = TRUE)
  expect_match(html, distlab:::tr("sim.mem.verdict.yes", "pt-BR"), fixed = TRUE)
  # Seeds of the results shown; the default seed was not used.
  expect_match(html, "<dd>2, 3, 4, 5</dd>", fixed = TRUE)
  # Its simulated values are offered as CSV files.
  expect_gte(lengths(regmatches(html, gregexpr("href=\"data:text/csv", html, fixed = TRUE))), 4L)
  # Without a seed the draws of a check cannot be rebuilt.
  unseeded <- dl_report(d, simulations = list(dl_mc_check(d, n = 100)), relations = FALSE)
  expect_match(unseeded, distlab:::tr("report.sim.no_draws", "en-US"), fixed = TRUE)
  expect_match(unseeded, distlab:::tr("report.sim.no_seed", "en-US"), fixed = TRUE)
})

test_that("only demos of the distribution are added, and its own results kept", {
  d <- dl_dist("exponential", rate = 1)
  demo <- dl_relation_demo("exponential", "gamma", n = 200, seed = 9)
  recs <- distlab:::report_simulations(d, list(demo), seed = 1, relations = FALSE)
  # A demo alone is not a simulation of the distribution: the default set runs.
  expect_identical(vapply(recs, `[[`, "", "kind"), c("mc_check", "lln", "clt", "relation"))
  recs <- distlab:::report_simulations(d, list(dl_lln(d, 100, seed = 1)), seed = 1, relations = TRUE)
  kinds <- vapply(recs, `[[`, "", "kind")
  expect_identical(kinds[1:2], c("mc_check", "lln"))
  expect_identical(sum(kinds == "relation"),
                   sum(distlab:::report_edges("exponential")$id %in% names(distlab:::.dl_sim_recipes)))
})

test_that("the Fit section appears for a fitted family, also when its fit failed", {
  x <- c(0.8, 1.3, 2.1, 0.4, 1.7, 2.9, 1.1, 0.6, 1.9, 1.4)
  fit <- dl_fit(x, c("gamma", "normal"))
  g <- dl_dist("gamma", shape = 2, rate = 1)
  small <- function(d) list(dl_mc_check(d, n = 200, seed = 1))
  html <- dl_report(g, fit = fit, simulations = small(g), relations = FALSE)
  expect_match(html, '<section class="dl-report-section" id="fit">', fixed = TRUE)
  expect_match(html, distlab:::tr("report.fit.gof_note", "en-US"), fixed = TRUE)
  expect_match(html, "Best fit: Gamma — lowest AIC", fixed = TRUE)
  expect_match(html, "Maximum likelihood fit of the Gamma and the other candidate families", fixed = TRUE)
  blocks <- report_code_blocks(html)
  env <- eval_block(blocks$code[blocks$kind == "fit"])
  est <- env$fit$fits$gamma$estimates$estimate
  for (v in est) expect_true(shown(html, distlab:::format_num(v, "en-US", digits = 5)))
  expect_s3_class(env$.value, "data.frame")
  # A family that was not a candidate has no Fit section.
  e <- dl_dist("exponential")
  expect_no_match(dl_report(e, fit = fit, simulations = small(e), relations = FALSE), 'id="fit"',
                  fixed = TRUE)
  # A failed fit is reported in words, without diagnostics.
  bad <- dl_fit(c(1, 1, 2), c("normal", "cauchy"))
  expect_true("cauchy" %in% names(bad$failures))
  cy <- dl_dist("cauchy")
  failed <- dl_report(cy, fit = bad, simulations = small(cy), relations = FALSE, lang = "pt-BR")
  expect_match(failed, 'id="fit"', fixed = TRUE)
  expect_match(failed, "O ajuste da Cauchy falhou", fixed = TRUE)
  expect_no_match(failed, distlab:::tr("report.fit.diagnostics", "pt-BR"), fixed = TRUE)
})

test_that("arguments are checked before anything is simulated", {
  d <- dl_dist("normal")
  expect_error(dl_report(list()), "dl_dist")
  expect_error(dl_report(d, lang = "fr-FR"), "must be one of")
  expect_error(dl_report(d, seed = NULL), "`seed`")
  expect_error(dl_report(d, seed = 1.5), "`seed`")
  expect_error(dl_report(d, fit = list()), "dl_fit")
  expect_error(dl_report(d, relations = NA), "TRUE or FALSE")
  expect_error(dl_report(d, file = 1), "file path")
  expect_error(dl_report(d, calculations = list(list(mode = "xx"))), "`mode`")
  expect_error(dl_report(d, calculations = list(list(mode = "le"))), "`x`")
  expect_error(dl_report(d, calculations = list(list(mode = "quantile", p = 1))), "in \\(0, 1\\)")
  expect_error(dl_report(d, calculations = list(list(mode = "between", a = 3, b = 1))), "greater")
  expect_error(dl_report(d, calculations = list(list(mode = "le", x = 1, dist = dl_dist("gamma")))),
               "about the distribution 'normal'")
  expect_error(dl_report(d, calculations = list(1)), "`mode`")
  expect_error(dl_report(d, simulations = dl_mc_check(d, n = 20, seed = 1)), "list of simulation results")
  expect_error(dl_report(d, simulations = list(list(a = 1))), "is not a result")
  expect_error(dl_report(d, simulations = list(dl_lln(dl_dist("gamma"), 20, seed = 1))),
               "simulates 'gamma', not 'normal'")
  expect_error(dl_report(d, simulations = list(dl_relation_demo("binomial", "poisson"))),
               "does not involve 'normal'")
})

test_that("stylesheets inline local files and refuse remote ones", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  writeBin(as.raw(1:10), file.path(dir, "f.woff2"))
  writeLines(c('@font-face { src: url(f.woff2?v=1#x) format("woff2"); }',
               '.a { background: url("data:image/png;base64,AA"); }'), file.path(dir, "a.css"))
  css <- distlab:::inline_css(file.path(dir, "a.css"))
  expect_match(css, 'url("data:font/woff2;base64,AQIDBAUGBwgJCg==")', fixed = TRUE)
  expect_match(css, 'url("data:image/png;base64,AA")', fixed = TRUE)
  writeLines(".b { background: url(https://example.com/x.png); }", file.path(dir, "b.css"))
  expect_error(distlab:::inline_css(file.path(dir, "b.css")), "remote file")
  writeLines(".c { background: url(missing.png); }", file.path(dir, "c.css"))
  expect_error(distlab:::inline_css(file.path(dir, "c.css")), "missing file")
  expect_identical(distlab:::safe_script("a</script>b<!--c"), "a<\\/script>b<\\!--c")
  # Long base64 stays on one line, so it cannot end a CSS string.
  expect_false(grepl("\n", distlab:::base64_line(as.raw(rep(1:255, 10)))))
})

test_that("embedded data are shared out within the caps", {
  expect_identical(distlab:::csv_allocate(c(10, 20, 30), total = 40, cap = 100), c(10, 15, 15))
  expect_identical(distlab:::csv_allocate(c(5, 5), total = 100, cap = 4), c(4, 4))
  expect_identical(distlab:::csv_allocate(numeric(0)), numeric(0))
  expect_identical(distlab:::csv_text(data.frame(x = c(1, 0.1 + 0.2, NA), k = c("a", "b", "c"))),
                   "x,k\n1,a\n0.3,b\nNA,c\n")
})

test_that("plot data keep 15 significant digits", {
  json <- as.character(distlab:::report_json(list(x = 1e6 + c(0, 1e-5), y = 1 / 3)))
  expect_match(json, "1000000.00001", fixed = TRUE)
  expect_match(json, "0.333333333333333", fixed = TRUE)
  expect_no_match(json, "0.3333333333333333", fixed = TRUE)
})

test_that("every distribution of the catalog gets a report", {
  for (id in dl_ids()) {
    d <- dl_dist(id)
    html <- dl_report(d, simulations = list(dl_mc_check(d, n = 100, seed = 1)), relations = FALSE,
                      calculations = list(list(mode = "central", prob = 0.9)))
    expect_match(html, "</html>", fixed = TRUE, info = id)
    expect_no_match(html, distlab:::tr("report.plot_unavailable", "en-US"), fixed = TRUE, info = id)
  }
})

# One section of a report page.
report_section_html <- function(html, id) {
  regmatches(html, regexpr(sprintf('<section class="dl-report-section" id="%s">[\\s\\S]*?</section>', id),
                           html, perl = TRUE))
}

test_that("the numbers come from a check with the parameters of the report", {
  g2 <- dl_dist("gamma", shape = 2, rate = 1)
  other <- distlab:::mc_check_run(dl_dist("gamma", shape = 5, rate = 1), 500, 1)
  html <- dl_report(g2, simulations = list(other), relations = FALSE)
  numbers <- report_section_html(html, "numbers")
  expect_match(numbers, "Gamma(shape = 2, rate = 1) · 5,000 draws · seed 2026", fixed = TRUE)
  expect_match(numbers, '<td class="dl-cell-num">2</td>', fixed = TRUE)
  expect_no_match(numbers, "shape = 5", fixed = TRUE)
  # The session's check of other values stays in the Simulations section.
  plots <- report_section_html(html, "plots")
  expect_match(plots, "Gamma(shape = 5, rate = 1) · 500 draws · seed 1", fixed = TRUE)
  expect_no_match(plots, "Gamma(shape = 2, rate = 1) · 5,000 draws", fixed = TRUE)
  expect_match(html, "<dd>1, 2026</dd>", fixed = TRUE)
  # With the same values the session's check is used and no other is run.
  same <- dl_report(g2, simulations = list(dl_mc_check(g2, n = 300, seed = 4, level = 0.9)),
                    relations = FALSE)
  expect_match(report_section_html(same, "numbers"), "300 draws · seed 4", fixed = TRUE)
  expect_identical(sum(report_code_blocks(same)$kind == "mc_check"), 1L)
  expect_match(same, "<dd>4</dd>", fixed = TRUE)
  # Its level labels the intervals.
  expect_match(same, "90% interval", fixed = TRUE)
  expect_match(same, "90% intervals", fixed = TRUE)
})

test_that("an unseeded check keeps its distribution and number of draws", {
  g9 <- dl_dist("gamma", shape = 9, rate = 1)
  html <- dl_report(g9, simulations = list(dl_mc_check(g9, n = 300)), relations = FALSE)
  expect_match(html, "Gamma(shape = 9, rate = 1) · 300 draws · no seed", fixed = TRUE)
  expect_error(dl_report(dl_dist("normal"), simulations = list(dl_mc_check(g9, n = 30))),
               "simulates 'gamma', not 'normal'")
})

test_that("large fits stay small: a thinned CDF and data read from a CSV", {
  x <- distlab:::with_seed(1, stats::rnorm(1e5, 50, 3))
  fit <- dl_fit(x, "normal")
  d <- dl_dist("normal")
  small <- list(dl_mc_check(d, n = 100, seed = 1))
  html <- dl_report(d, fit = fit, simulations = small, relations = FALSE)
  expect_lt(nchar(html, type = "bytes"), 8e6)
  blocks <- report_code_blocks(html)
  fit_code <- blocks$code[blocks$kind == "fit"]
  expect_match(fit_code, 'tab <- read.csv("distlab-data.csv")', fixed = TRUE)
  expect_lt(nchar(fit_code), 2000)
  expect_match(html, "100,000 rows", fixed = TRUE)
  expect_match(html, "Maximum likelihood fit of the Normal to the data", fixed = TRUE)
  cdf <- plotly::plotly_build(distlab:::fit_diagnostic_plot(dl_fit_diagnostics(fit), "cdf", "en-US",
                                                             distlab:::plot_theme()))
  expect_true(all(vapply(cdf$x$data, function(t) length(t$x), 1L) <= 2000L))
  # A middle-sized table is embedded with digits that read back exactly.
  y <- round(x[1:5000], 4)
  fit2 <- dl_fit(y, "normal")
  html2 <- dl_report(d, fit = fit2, simulations = small, relations = FALSE)
  link <- regmatches(html2, regexpr('data:text/csv;charset=utf-8;base64,[^"]+(?=" download="distlab-data.csv")',
                                    html2, perl = TRUE))
  expect_length(link, 1L)
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  writeBin(jsonlite::base64_dec(sub("^.*base64,", "", link)), file.path(dir, "distlab-data.csv"))
  blocks2 <- report_code_blocks(html2)
  old <- setwd(dir)
  env <- eval_block(blocks2$code[blocks2$kind == "fit"])
  setwd(old)
  expect_identical(env$fit$fits$normal$estimates, fit2$fits$normal$estimates)
})

test_that("figures are captioned and named for screen readers", {
  html <- cached_report("gamma", "en-US", report_cases$gamma)
  count <- function(p) lengths(regmatches(html, gregexpr(p, html, perl = TRUE)))
  widgets <- count('class="plotly html-widget')
  expect_identical(count('<figure class="dl-report-plot">\\s*<figcaption'), count("<figure"))
  expect_identical(count('class="plotly html-widget"[^>]*role="img" aria-label="[^"]+"|role="img" aria-label="[^"]+"[^>]*class="plotly html-widget'),
                   widgets)
  expect_match(html, distlab:::tr("report.fig.clt", "en-US"), fixed = TRUE)
  expect_match(html, "3 paths", fixed = TRUE)
  expect_match(html, "2,000 samples per size", fixed = TRUE)
  pt <- cached_report("gamma", "pt-BR", report_cases$gamma)
  expect_match(pt, "3 trajetórias", fixed = TRUE)
  expect_match(pt, "2.000 amostras por tamanho", fixed = TRUE)
  expect_match(pt, "Complemento: 1 − P", fixed = TRUE)
  expect_no_match(pt, "Complementar", fixed = TRUE)
})
