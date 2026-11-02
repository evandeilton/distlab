# Study report: one self-contained HTML page about a distribution, built from
# its registry entry, exact numbers, seeded simulations, the calculations and
# fit of a session, with the R code of every result. No shiny here: the app
# only passes what it recorded.

# Draws of the default simulations and relation demos.
.dl_report_n <- 5000
# Running-mean paths and samples per size of the default LLN and CLT.
.dl_report_paths <- 3
.dl_report_reps <- 2000
# Rows of simulated data embedded as CSV, over the whole report and per file;
# data URIs above about 2 MB fail in some browsers.
.dl_report_csv_max <- 5e4
.dl_report_csv_file_max <- 2.5e4
# Rows of fit data written into the code; larger tables are read from a CSV
# file, embedded in the page up to the second limit.
.dl_report_fit_literal <- 2000
.dl_report_fit_csv <- 25000
# Code blocks longer than this many characters start collapsed.
.dl_report_fold <- 20000
# Largest R script offered as a single download link.
.dl_report_script_max <- 1.5e6

# Order of the kinds of simulation in the report.
.dl_report_kinds <- c("mc_check", "lln", "clt", "transform", "memoryless", "relation")

#' Study report of a distribution
#'
#' Writes one HTML file that explains a distribution: the story and related
#' distributions, the formula sheet, exact moments against a seeded Monte
#' Carlo check, interactive plots (density or mass function, CDF, quantile
#' function, a parameter sweep and every simulation), calculator results,
#' a demonstration of each relationship, an optional fit, and an appendix of
#' R code that reproduces every number. Scripts, styles and fonts are
#' inlined, so the file opens offline in a browser, without a folder of
#' assets and without 'pandoc'.
#'
#' Text is in `lang`, and numbers use its decimal mark, with at most 4
#' decimal places. Exact moments that are infinite show as the infinity
#' sign, and those that do not exist are named so.
#'
#' Each block of the appendix runs on its own in a fresh R session; blocks
#' with random draws start with [set.seed()]. Fit data longer than 2000 rows
#' are read from a CSV file, embedded in the page up to 25,000 rows.
#' Simulated values are also embedded as CSV download links, up to 50,000
#' rows over the whole report (25,000 per file); the code regenerates all
#' of them.
#'
#' @param dist A `dl_dist` object from [dl_dist()].
#' @param file Path of the HTML file to write, in UTF-8, or `NULL` to only
#'   return the page.
#' @param lang Language of the report, one of [dl_languages()].
#' @param calculations A list of Calculator questions about `dist`. Each one
#'   is a list with `mode` and the values it needs:
#'   * `"le"` (\eqn{P(X \le x)}) and `"gt"` (\eqn{P(X > x)}): `x`;
#'   * `"between"` (\eqn{P(a < X \le b)}): `a` and `b`, and for discrete
#'     distributions optionally `lower_closed` (default `FALSE`) and
#'     `upper_closed` (default `TRUE`);
#'   * `"quantile"`: `p`, in (0, 1);
#'   * `"central"` (central interval): `prob`, in (0, 1).
#'
#'   A question may hold its own `dist`, of the same family as `dist` with
#'   other parameter values.
#' @param simulations A list of results of [dl_mc_check()], [dl_lln()],
#'   [dl_clt()], [dl_transform()] and [dl_memoryless()] for the family of
#'   `dist`, and of [dl_relation_demo()] for edges that start or end at it.
#'   Without a simulation of `dist` itself (relation demos aside), a Monte
#'   Carlo check, the law of large numbers and the central limit theorem are
#'   simulated with `seed` (5000 draws, 3 paths, 2000 samples per size).
#'   The exact and simulated numbers come from a Monte Carlo check with the
#'   parameter values of `dist`: one of `simulations` when its parameters
#'   match, otherwise one run with `seed`.
#' @param fit A [dl_fit()] result, or `NULL`. The report has a Fit section
#'   when the family of `dist` is one of its candidates, also when its fit
#'   failed.
#' @param seed Seed of the default simulations and relation demos, a whole
#'   number.
#' @param relations `TRUE` to simulate every relationship of `dist` (5000
#'   draws, `seed`) that `simulations` does not hold.
#'
#' @returns The HTML page as a character string; invisibly when `file` is
#'   given.
#'
#' @seealso [run_app()], whose Explorer, Simulation and Fit pages download
#'   this report for the work of the session.
#' @family app
#' @export
#' @examples
#' g <- dl_dist("gamma", shape = 2, rate = 1)
#' path <- tempfile(fileext = ".html")
#' dl_report(g, path, lang = "en-US",
#'           calculations = list(list(mode = "le", x = 3),
#'                               list(mode = "quantile", p = 0.9)),
#'           simulations = list(dl_mc_check(g, n = 500, seed = 1),
#'                              dl_lln(g, n = 500, seed = 1)),
#'           relations = FALSE)
#' file.size(path)
#' unlink(path)
dl_report <- function(dist, file = NULL, lang = "en-US", calculations = list(),
                      simulations = list(), fit = NULL, seed = 2026, relations = TRUE) {
  dist <- check_dist(dist)
  lang <- match_lang(lang)
  if (is.null(seed)) stop("`seed` must be a single whole number.", call. = FALSE)
  seed <- sim_seed(seed)
  if (!is.null(file) && !is_string(file)) {
    stop("`file` must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(relations) || length(relations) != 1L || is.na(relations)) {
    stop("`relations` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(fit) && !inherits(fit, "dl_fit")) {
    stop("`fit` must be NULL or a result of dl_fit().", call. = FALSE)
  }
  calcs <- report_calculations(dist, calculations)
  sims <- report_simulations(dist, simulations, seed, relations)
  html <- report_html(dist, lang, calcs, sims, fit, seed, progress = getOption("distlab.report_progress"))
  if (is.null(file)) return(html)
  con <- base::file(file, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(html), con)
  invisible(html)
}

# ---- Inputs ------------------------------------------------------------------

# Questions as results of calc_result(), each with the distribution it is for.
report_calculations <- function(dist, calculations) {
  if (!is.list(calculations) || is.data.frame(calculations)) {
    stop("`calculations` must be a list of questions.", call. = FALSE)
  }
  lapply(seq_along(calculations), function(i) {
    q <- calculations[[i]]
    where <- sprintf("`calculations[[%d]]`", i)
    if (!is.list(q) || !is_string(q$mode) || !q$mode %in% .dl_calc_modes) {
      stop(where, " must be a list with `mode` one of: ",
           paste(.dl_calc_modes, collapse = ", "), ".", call. = FALSE)
    }
    d <- q$dist %||% dist
    if (!inherits(d, "dl_dist") || !identical(d$id, dist$id)) {
      stop(where, " must be about the distribution '", dist$id, "'.", call. = FALSE)
    }
    num <- function(name) {
      v <- q[[name]]
      if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
        stop(where, " needs a single number `", name, "`.", call. = FALSE)
      }
      as.numeric(v)
    }
    prob <- function(name) {
      v <- num(name)
      if (!(v > 0 && v < 1)) stop(where, ": `", name, "` must be in (0, 1).", call. = FALSE)
      v
    }
    args <- switch(q$mode,
      le = , gt = list(x = num("x")),
      between = list(a = num("a"), b = num("b"), lower_closed = isTRUE(q$lower_closed),
                     upper_closed = !isFALSE(q$upper_closed)),
      quantile = list(p = prob("p")),
      central = list(prob = prob("prob")))
    if (q$mode == "between" && args$a > args$b) {
      stop(where, ": `a` must not be greater than `b`.", call. = FALSE)
    }
    res <- tryCatch(do.call(calc_result, c(list(d, q$mode), args)), error = function(err) {
      stop(where, " cannot be computed: ", conditionMessage(err), call. = FALSE)
    })
    list(dist = d, result = res)
  })
}

# Question of a calc_result() result, in the form dl_report() takes.
calc_question <- function(res, dist) {
  r <- res$region
  vals <- switch(res$mode,
    le = list(x = r$upper), gt = list(x = r$lower),
    between = list(a = r$lower, b = r$upper, lower_closed = r$lower_closed,
                   upper_closed = r$upper_closed),
    quantile = list(p = res$p), central = list(prob = res$prob))
  c(list(mode = res$mode), vals, list(dist = dist))
}

# Kind of a simulation result, from its structure; NA when unknown.
sim_kind <- function(x) {
  if (is.data.frame(x)) {
    ok <- identical(names(x)[1], "quantity") && !is.null(attr(x, "code", exact = TRUE))
    return(if (ok) "mc_check" else NA_character_)
  }
  if (!is.list(x)) return(NA_character_)
  has <- function(...) all(c(...) %in% names(x))
  if (has("table", "sample", "code")) return("mc_check")
  if (has("edge", "frames", "distance")) return("relation")
  if (has("paths", "mean", "code")) return("lln")
  if (has("means", "standardized", "distance")) return("clt")
  if (has("op", "sample", "code")) return("transform")
  if (has("residual", "exact", "code")) return("memoryless")
  NA_character_
}

# Monte Carlo check of a dl_mc_check() table, with its distribution, number
# of draws and level; a seed gives the draws back, drawn as the check drew
# them. Tables without a `dist` attribute rebuild it by running their code.
mc_from_table <- function(tab) {
  seed <- attr(tab, "seed", exact = TRUE) %||% NA_integer_
  out <- list(table = tab, sample = NULL, seed = seed, code = attr(tab, "code", exact = TRUE),
              dist = attr(tab, "dist", exact = TRUE), n = attr(tab, "n", exact = TRUE), level = attr(tab, "level", exact = TRUE) %||% 0.95)
  if (is.na(seed)) return(out)
  if (inherits(out$dist, "dl_dist") && !is.null(out$n)) {
    out$sample <- with_seed(seed, dl_sample(out$dist, out$n))
    return(out)
  }
  env <- tryCatch(run_code(out$code, seed), error = function(err) NULL)
  if (!is.null(env) && inherits(env$d, "dl_dist")) {
    out$sample <- env$x
    out$dist <- env$d
    out$n <- length(env$x)
  }
  out
}

# TRUE when a record simulates `dist` with the same parameter values.
same_params <- function(rec, dist) {
  !is.null(rec$dist) && isTRUE(all.equal(rec$dist$params, dist$params, tolerance = 0))
}

# One record per simulation: list(kind, res, dist), where `res` may be an
# engine error kept for a note.
report_simulations <- function(dist, simulations, seed, relations) {
  if (!is.list(simulations) || is.data.frame(simulations)) {
    stop("`simulations` must be a list of simulation results, such as list(dl_mc_check(d)).",
         call. = FALSE)
  }
  recs <- lapply(seq_along(simulations), function(i) {
    x <- simulations[[i]]
    where <- sprintf("`simulations[[%d]]`", i)
    kind <- sim_kind(x)
    if (is.na(kind)) {
      stop(where, " is not a result of dl_mc_check(), dl_lln(), dl_clt(), dl_transform(), ",
           "dl_memoryless() or dl_relation_demo().", call. = FALSE)
    }
    if (kind == "relation") {
      if (!dist$id %in% c(x$edge$from, x$edge$to)) {
        stop(where, " is a relationship that does not involve '", dist$id, "'.", call. = FALSE)
      }
      return(list(kind = kind, res = x, dist = NULL))
    }
    if (is.data.frame(x)) x <- mc_from_table(x)
    d <- x$dist %||% dist
    if (!identical(d$id, dist$id)) {
      stop(where, " simulates '", d$id, "', not '", dist$id, "'.", call. = FALSE)
    }
    list(kind = kind, res = x, dist = d)
  })
  kinds <- vapply(recs, `[[`, "", "kind")
  run <- function(kind, expr) list(kind = kind, res = sim_run(expr), dist = dist)
  # Relation demos aside, no simulation of `dist`: the default set.
  if (all(kinds == "relation")) {
    recs <- c(recs, list(
      run("lln", dl_lln(dist, .dl_report_n, paths = .dl_report_paths, seed = seed)),
      run("clt", dl_clt(dist, sizes = .dl_clt_sizes, reps = .dl_report_reps, seed = seed))))
  }
  # Numbers shows a check of `dist` itself: the session's when it has the
  # same parameters, else a default one, kept out of the Simulations section
  # when the session has checks of other parameter values.
  mc <- which(kinds == "mc_check")
  own <- mc[vapply(recs[mc], same_params, logical(1), dist = dist)]
  if (length(own)) {
    recs[[own[1]]]$numbers <- TRUE
  } else {
    rec <- run("mc_check", mc_check_run(dist, .dl_report_n, seed))
    rec$numbers <- TRUE
    rec$hidden <- length(mc) > 0L
    recs <- c(list(rec), recs)
  }
  if (relations) {
    done <- unlist(lapply(recs, function(r) if (r$kind == "relation") r$res$edge$id))
    for (e in setdiff(report_edges(dist$id)$id, done)) {
      if (is.null(.dl_sim_recipes[[e]])) next
      recs[[length(recs) + 1L]] <- list(kind = "relation", dist = NULL,
                                        res = sim_run(relation_demo_run(e, .dl_report_n, seed)),
                                        edge = e)
    }
  }
  kinds <- vapply(recs, `[[`, "", "kind")
  recs <- recs[order(match(kinds, .dl_report_kinds))]
  # An index ties each record to its code block across sections.
  for (i in seq_along(recs)) recs[[i]]$index <- i
  recs
}

# Relationship edges that start or end at `id`, with their edge ids.
report_edges <- function(id) {
  rel <- relations_frame(dl_registry())
  rel$id <- edge_ids(rel)
  rel[rel$from == id | rel$to == id, , drop = FALSE]
}

# ---- Context -----------------------------------------------------------------

# Mutable state of one report: language, plot colors and counters, the code
# blocks and the simulated data, in the order they appear.
report_context <- function(lang) {
  ctx <- new.env(parent = emptyenv())
  ctx$lang <- lang
  ctx$th <- plot_theme(FALSE)
  ctx$plots <- 0L
  ctx$blocks <- list()
  ctx$sim_blocks <- list()
  ctx$data <- list()
  ctx
}

# Add a code block; returns its number.
add_block <- function(ctx, title, code, kind) {
  n <- length(ctx$blocks) + 1L
  ctx$blocks[[n]] <- list(title = title, code = code, kind = kind)
  n
}

# Add a simulated data set to the CSV downloads.
add_data <- function(ctx, label, frame, name) {
  if (is.null(frame) || !nrow(frame)) return(invisible())
  ctx$data[[length(ctx$data) + 1L]] <- list(label = label, frame = frame, name = name)
  invisible()
}

# Links to code blocks of the appendix.
code_ref <- function(ctx, n) {
  links <- lapply(n, function(i) {
    shiny::tags$a(href = paste0("#code-", i), tr("report.code.ref", ctx$lang, n = i))
  })
  shiny::tags$p(class = "dl-report-ref", Reduce(function(a, b) shiny::tagList(a, " \u00b7 ", b), links))
}

# Interactive figure with its title, which also names the plot for screen
# readers; a note when the plot cannot be drawn. `expr` is evaluated here, so
# its errors become the note.
report_plot <- function(ctx, expr, title, height = 320) {
  w <- tryCatch({
    ctx$plots <- ctx$plots + 1L
    b <- report_widget(expr, sprintf("dl-plot-%d", ctx$plots), height)
    htmltools::tagQuery(htmltools::as.tags(b))$filter(".html-widget")$
      addAttrs(role = "img", `aria-label` = title)$allTags()
  }, error = function(err) NULL)
  shiny::tags$figure(
    class = "dl-report-plot",
    shiny::tags$figcaption(class = "dl-report-plot-title", title),
    w %||% shiny::tags$p(class = "dl-calc-note", tr("report.plot_unavailable", ctx$lang))
  )
}

# Note paragraphs in the style of the app.
report_notes <- function(...) {
  notes <- unlist(list(...))
  if (!length(notes)) return(NULL)
  shiny::tagList(lapply(notes, function(t) shiny::tags$p(class = "dl-calc-note", t)))
}

# Caption of a simulation: the law, number of draws (or `extra` text) and seed.
sim_caption <- function(ctx, dist, n = NULL, seed = NA, extra = NULL) {
  lang <- ctx$lang
  parts <- c(series_label(dist, lang),
             if (!is.null(n) && is.finite(n)) tr("report.sim.draws", lang, n = format_count(n, lang)),
             extra,
             if (!is.na(seed)) tr("report.sim.seed", lang, seed = seed) else tr("report.sim.no_seed", lang))
  shiny::tags$p(class = "dl-report-caption", paste(parts, collapse = " \u00b7 "))
}

# Whole number with the thousands separator of the language.
format_count <- function(n, lang) {
  pt <- match_lang(lang) == "pt-BR"
  formatC(n, format = "d", big.mark = if (pt) "." else ",", decimal.mark = if (pt) "," else ".")
}

# A section: anchor id, numbered title and content.
report_section <- function(id, title, ...) {
  list(id = id, title = title,
       tag = shiny::tags$section(class = "dl-report-section", id = id,
                                 shiny::tags$h2(title), ...))
}

# ---- Page --------------------------------------------------------------------

# `progress(share)` (optional) is called after each section with the share
# of sections built, for the app's progress bar.
report_html <- function(dist, lang, calcs, sims, fit, seed, progress = NULL) {
  ctx <- report_context(lang)
  name <- entry_text(dist$entry, "names", lang)
  fit_part <- report_fit_part(fit, dist$id)
  builders <- list(
    function() report_story(ctx, dist),
    function() report_formulas(ctx, dist),
    function() report_numbers(ctx, dist, sims),
    function() report_plots(ctx, dist, sims),
    function() report_calculations_section(ctx, calcs),
    function() report_relations(ctx, dist, sims),
    function() if (!is.null(fit_part)) report_fit(ctx, dist, fit_part),
    function() report_code(ctx, dist),
    function() report_references(ctx, dist)
  )
  sections <- lapply(seq_along(builders), function(i) {
    s <- builders[[i]]()
    if (is.function(progress)) progress(i / length(builders))
    s
  })
  sections <- Filter(Negate(is.null), sections)
  # Seeds of the results in the report, not the default when it was unused.
  seeds <- sort(unique(unlist(lapply(sims, function(r) {
    if (!inherits(r$res, "error")) r$res$seed
  }))))
  toc <- shiny::tags$nav(
    class = "dl-report-toc", `aria-label` = tr("report.toc", lang),
    shiny::tags$h2(tr("report.toc", lang)),
    shiny::tags$ol(lapply(sections, function(s) {
      shiny::tags$li(shiny::tags$a(href = paste0("#", s$id), s$title))
    }))
  )
  body <- shiny::tags$main(
    class = "dl-report",
    report_cover(ctx, dist, seeds[!is.na(seeds)]),
    toc,
    lapply(sections, `[[`, "tag")
  )
  report_page(body, lang, tr("report.title", lang, name = name))
}

# Cover: name, type and group, parameters with their symbols and R names,
# date, language, seeds and version.
report_cover <- function(ctx, dist, seeds) {
  lang <- ctx$lang
  entry <- dist$entry
  rows <- lapply(entry$parameters, function(par) {
    shiny::tags$tr(
      shiny::tags$td(math_tag(par$latex)),
      shiny::tags$td(shiny::tags$code(par$name)),
      shiny::tags$td(class = "dl-cell-num", format_value(dist$params[[par$name]], lang, digits = 10))
    )
  })
  meta <- function(key, value) {
    shiny::tags$div(shiny::tags$dt(tr(key, lang)), shiny::tags$dd(value))
  }
  lang_key <- if (lang == "pt-BR") "nav.lang_pt" else "nav.lang_en"
  shiny::tags$header(
    class = "dl-report-cover",
    shiny::tags$p(class = "dl-report-kicker", tr("report.kicker", lang)),
    shiny::tags$h1(entry_text(entry, "names", lang)),
    shiny::tags$div(
      class = "dl-report-badges",
      shiny::tags$span(class = "badge", tr(paste0("type.", entry$type), lang)),
      shiny::tags$span(class = "badge", tr(paste0("group.", entry$group), lang))
    ),
    shiny::tags$p(class = "dl-report-sub", tr("report.subtitle", lang)),
    shiny::tags$div(
      class = "table-responsive",
      shiny::tags$table(
        shiny::tags$caption(tr("explorer.parameters", lang)),
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(scope = "col", tr("report.param.symbol", lang)),
          shiny::tags$th(scope = "col", tr("report.param.name", lang)),
          shiny::tags$th(scope = "col", class = "dl-cell-num", tr("report.param.value", lang))
        )),
        shiny::tags$tbody(rows)
      )
    ),
    shiny::tags$dl(
      class = "dl-report-meta",
      meta("report.meta.support", support_text(dist, lang)),
      meta("report.meta.date", format(Sys.Date(), "%Y-%m-%d")),
      meta("report.meta.language", tr(lang_key, lang)),
      meta("report.meta.seeds", if (length(seeds)) paste(seeds, collapse = ", ") else "\u2014"),
      meta("report.meta.version", as.character(utils::packageVersion("distlab")))
    ),
    shiny::tags$button(type = "button", class = "dl-print dl-report-print", tr("report.print", lang))
  )
}

# ---- Sections ----------------------------------------------------------------

# Story of the entry and its related distributions with their conditions.
report_story <- function(ctx, dist) {
  lang <- ctx$lang
  reg <- dl_registry()
  rel <- report_edges(dist$id)
  name <- function(i) entry_text(reg[[i]], "names", lang)
  items <- lapply(seq_len(nrow(rel)), function(i) {
    r <- rel[i, ]
    shiny::tags$li(
      relation_badge(r$type, lang), " ",
      shiny::tags$span(class = "dl-rel-path", name(r$from),
                       shiny::tags$span(class = "dl-rel-arrow", `aria-hidden` = "true", "\u2192"),
                       shiny::tags$span(class = "visually-hidden", tr("related.to", lang)),
                       name(r$to)),
      shiny::tags$p(class = "dl-rel-cond", if (lang == "pt-BR") r$condition_pt else r$condition_en)
    )
  })
  report_section(
    "story", tr("report.section.story", lang),
    shiny::tags$p(class = "dl-story", entry_text(dist$entry, "story", lang)),
    shiny::tags$h3(tr("related.title", lang)),
    if (length(items)) shiny::tags$ul(class = "dl-report-rel", items) else
      shiny::tags$p(class = "dl-muted", tr("related.none", lang))
  )
}

# Formula sheet of the Explorer, the support and the R parameterization.
report_formulas <- function(ctx, dist) {
  lang <- ctx$lang
  entry <- dist$entry
  tabs <- lapply(names(formula_tabs(entry$type)), function(tab) {
    shiny::tagList(shiny::tags$h3(tr(paste0("formula.tab.", tab), lang)),
                   formula_tab_content(entry, tab, lang))
  })
  report_section(
    "formulas", tr("report.section.formulas", lang),
    tabs,
    shiny::tags$h3(tr("report.parameterization", lang)),
    shiny::tags$p(tr("report.support", lang), " ", math_tag(support_tex(entry)), " ",
                  shiny::tags$span(class = "dl-muted",
                                   tr("report.support_values", lang, support = support_text(dist, lang)))),
    shiny::tags$p(tr("report.functions", lang), " ",
                  lapply(c("d", "p", "q", "r"), function(k) {
                    shiny::tagList(shiny::tags$code(entry$functions[[k]]), " ")
                  })),
    shiny::tags$p(class = "dl-muted", tr("report.param_note", lang))
  )
}

# R code of the exact moments and of the curves of the plots.
dist_code <- function(dist) {
  u <- quantile_grid(dist)
  paste(c(
    code_dist(dist, exact = TRUE),
    "moments <- distlab::dl_moments(d)",
    "curve <- distlab::dl_curve(d)  # x, density, cdf",
    sprintf("u <- seq(%s, %s, length.out = %d)", code_num(u[1]), code_num(u[length(u)]), length(u)),
    "q <- distlab::dl_quantile(d, u)"
  ), collapse = "\n")
}

# Exact moments against the Monte Carlo check (the first one recorded).
report_numbers <- function(ctx, dist, sims) {
  lang <- ctx$lang
  b_dist <- add_block(ctx, tr("report.code.dist", lang), dist_code(dist), "dist")
  m <- tryCatch(dl_moments(dist), error = function(err) NULL)
  mc <- Filter(function(r) isTRUE(r$numbers), sims)[[1]]
  res <- mc$res
  body <- if (inherits(res, "error")) {
    shiny::tagList(moments_table(m, lang),
                   report_notes(tr("report.numbers.failed", lang, message = sim_error_text(res, lang))))
  } else {
    block <- add_block(ctx, tr("sim.sample.check", lang), res$code, "mc_check")
    ctx$sim_blocks[[as.character(mc$index)]] <- block
    n <- res$n %||% length(res$sample)
    shiny::tagList(sim_caption(ctx, mc$dist, if (length(n)) n, res$seed),
                   mc_table_tag(res$table, lang, res$level %||% 0.95))
  }
  # The note follows the exact moments in the table shown.
  exact <- if (inherits(res, "error")) m else res$table$exact[1:5]
  missing <- !is.null(exact) && any(!is.finite(exact))
  report_section(
    "numbers", tr("report.section.numbers", lang),
    shiny::tags$p(tr("report.numbers.intro", lang,
                     level = format_num(100 * (if (inherits(res, "error")) 0.95 else res$level %||% 0.95), lang))),
    body,
    report_notes(if (missing) tr("report.numbers.missing", lang),
                 if (identical(dist$entry$type, "discrete")) tr("report.numbers.discrete", lang)),
    code_ref(ctx, c(b_dist, ctx$sim_blocks[[as.character(mc$index)]]))
  )
}

# Exact moments alone, when no Monte Carlo check is available.
moments_table <- function(m, lang) {
  keys <- c("mean", "variance", "sd", "skewness", "excess_kurtosis")
  shiny::tags$div(
    class = "table-responsive",
    shiny::tags$table(
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(scope = "col", tr("sim.col.quantity", lang)),
        shiny::tags$th(scope = "col", class = "dl-cell-num", tr("sim.col.exact", lang)))),
      shiny::tags$tbody(lapply(keys, function(k) {
        shiny::tags$tr(shiny::tags$th(scope = "row", tr(paste0("moments.", k), lang)),
                       shiny::tags$td(class = "dl-cell-num",
                                      if (is.null(m)) "\u2014" else format_moment(m[[k]], lang)))
      }))
    )
  )
}

# Grid of u for the quantile plot: the central 99.8%, or 98% for heavy tails.
quantile_grid <- function(dist) {
  edge <- if (heavy_tailed(dist)) 0.01 else 0.001
  seq(edge, 1 - edge, length.out = 401)
}

# Quantile function u -> F^-1(u); a step function for discrete laws.
quantile_plot <- function(dist, lang, th) {
  u <- quantile_grid(dist)
  q <- dl_quantile(dist, u)
  ok <- is.finite(q)
  discrete <- identical(dist$entry$type, "discrete")
  p <- plotly::plot_ly(x = u[ok], y = q[ok], type = "scatter", mode = "lines",
                       line = list(color = th$line, width = 2.5, shape = if (discrete) "hv" else "linear"),
                       hovertemplate = "u = %{x:.3f}<br>F\u207b\u00b9(u) = %{y:.4f}<extra></extra>")
  plot_layout(p, "u", "F\u207b\u00b9(u)", th, lang)
}

# Density, CDF and quantile function, the parameter sweep and every
# simulation except the relationship demos.
report_plots <- function(ctx, dist, sims) {
  lang <- ctx$lang
  th <- ctx$th
  discrete <- identical(dist$entry$type, "discrete")
  curve <- tryCatch(dl_curve(dist), error = function(err) NULL)
  functions <- shiny::tags$div(
    class = "dl-report-grid",
    report_plot(ctx, explore_density_plot(curve, dist, lang, th),
                tr(if (discrete) "explorer.pmf" else "explorer.density", lang)),
    report_plot(ctx, explore_cdf_plot(curve, dist, lang, th), tr("explorer.cdf", lang)),
    report_plot(ctx, quantile_plot(dist, lang, th), tr("report.quantile", lang))
  )
  blocks <- lapply(Filter(function(r) r$kind != "relation" && !isTRUE(r$hidden), sims), function(r) {
    report_sim_block(ctx, r)
  })
  report_section(
    "plots", tr("report.section.plots", lang),
    shiny::tags$h3(tr("report.plots.functions", lang)),
    functions,
    report_sweep(ctx, dist),
    shiny::tags$h3(tr("report.plots.simulations", lang)),
    shiny::tags$p(class = "dl-muted", tr("report.plots.simulations_help", lang)),
    blocks
  )
}

# Values of the first parameter that can be swept, with the current value.
sweep_values <- function(dist) {
  entry <- dist$entry
  for (par in entry$parameters) {
    cur <- dist$params[[par$name]]
    vals <- tryCatch(sweep_defaults(entry, par$name, dist$params), error = function(err) numeric(0))
    vals <- sort(unique(c(vals, cur)))
    if (length(vals) >= 2L) return(list(param = par$name, values = vals, current = cur))
  }
  NULL
}

# Animated sweep of one parameter (the others at their values), with one
# frame per value on a shared range.
report_sweep <- function(ctx, dist) {
  lang <- ctx$lang
  sw <- sweep_values(dist)
  if (is.null(sw)) return(NULL)
  dists <- lapply(sw$values, function(v) {
    p <- dist$params
    p[[sw$param]] <- v
    tryCatch(do.call(dl_dist, c(list(dist$id), p)), error = function(err) NULL)
  })
  keep <- !vapply(dists, is.null, logical(1))
  dists <- dists[keep]
  vals <- sw$values[keep]
  if (length(vals) < 2L) return(NULL)
  rng <- tryCatch(vapply(shared_range(dists), code_value, numeric(1)), error = function(err) NULL)
  discrete <- identical(dist$entry$type, "discrete")
  head <- shiny::tagList(
    shiny::tags$h3(tr("report.plots.sweep", lang, param = sw$param)),
    shiny::tags$p(class = "dl-muted", tr("report.plots.sweep_help", lang, param = sw$param))
  )
  if (is.null(rng)) return(shiny::tagList(head, report_notes(tr("report.plot_unavailable", lang))))
  curves <- lapply(dists, function(d) curve_grid(d, rng[1], rng[2], n = 401L))
  labels <- vapply(vals, function(v) {
    lab <- paste(sw$param, "=", format_value(v, lang))
    if (isTRUE(all.equal(v, sw$current))) paste0(lab, " (", tr("report.plots.current", lang), ")") else lab
  }, "")
  fig <- report_plot(ctx, sweep_plot(curves, discrete, labels, lang, ctx$th),
                     tr("report.fig.sweep", lang), height = 380)
  fn <- sprintf("distlab::dl_density(distlab::dl_dist(\"%s\", %s), x)", dist$id,
                paste(vapply(names(dist$params), function(nm) {
                  paste(nm, "=", if (nm == sw$param) "v" else code_num(dist$params[[nm]], exact = TRUE))
                }, ""), collapse = ", "))
  code <- paste(c(
    sprintf("values <- %s", code_vec(vals)),
    if (discrete) sprintf("x <- seq(%s, %s)", code_num(floor(rng[1])), code_num(ceiling(rng[2]))) else
      sprintf("x <- seq(%s, %s, length.out = 401)", code_num(rng[1], exact = TRUE),
              code_num(rng[2], exact = TRUE)),
    sprintf("density <- sapply(values, function(v) %s)", fn)
  ), collapse = "\n")
  shiny::tagList(head, fig, code_ref(ctx, add_block(ctx, tr("report.code.sweep", lang, param = sw$param),
                                                   code, "sweep")))
}

# Frames of the sweep: bars for discrete laws, filled curves otherwise; the
# y range holds the body of every frame.
sweep_plot <- function(curves, discrete, labels, lang, th) {
  data <- do.call(rbind, lapply(seq_along(curves), function(i) data.frame(frame = i, curves[[i]])))
  series <- lapply(curves, function(cv) list(curve = cv))
  ylim <- compare_ylim(series) %||% c(0, 1.08 * max(data$density))
  p <- if (discrete) {
    plotly::plot_ly(data, x = ~x, y = ~density, frame = ~frame, type = "bar",
                    marker = list(color = th$fill_solid),
                    hovertemplate = "x = %{x}<br>P = %{y:.4f}<extra></extra>")
  } else {
    plotly::plot_ly(data, x = ~x, y = ~density, frame = ~frame, type = "scatter", mode = "lines",
                    fill = "tozeroy", fillcolor = th$fill, line = list(color = th$line, width = 2.5),
                    hovertemplate = "x = %{x:.4f}<br>f(x) = %{y:.4f}<extra></extra>")
  }
  ylab <- tr(if (discrete) "plot.probability" else "plot.density", lang)
  p <- plot_layout(p, tr("plot.x", lang), ylab, th, lang, ylim = ylim)
  p <- plotly::layout(p, margin = list(l = 56, r = 12, t = 24, b = 40))
  sim_animate(p, labels, lang, th)
}

# One simulation of the Plots section: caption, figures, notes, tables and
# the link to its code; an engine error becomes a note.
report_sim_block <- function(ctx, r) {
  lang <- ctx$lang
  th <- ctx$th
  title <- tr(paste0("sim.tab.", c(mc_check = "sample", lln = "lln", clt = "clt",
                                    transform = "transform", memoryless = "memoryless")[[r$kind]]), lang)
  res <- r$res
  if (inherits(res, "error")) {
    return(shiny::tags$div(class = "dl-report-block", shiny::tags$h4(title),
                           report_notes(tr("report.sim.failed", lang, message = sim_error_text(res, lang)))))
  }
  d <- r$dist
  discrete <- identical(d$entry$type, "discrete")
  name <- d$id
  body <- switch(r$kind,
    mc_check = {
      add_data(ctx, title, if (length(res$sample)) data.frame(value = res$sample), paste0(name, "-sample"))
      if (!length(res$sample)) {
        report_notes(tr("report.sim.no_draws", lang))
      } else {
        v <- tryCatch(sample_view_of(res$sample, d), error = function(err) NULL)
        shiny::tagList(
          shiny::tags$div(
            class = "dl-report-grid",
            report_plot(ctx, sim_hist_plot(v, lang, th, tr("sim.exact", lang)),
                        tr(if (discrete) "sim.hist.pmf" else "sim.hist.density", lang)),
            report_plot(ctx, sim_ecdf_plot(res$sample, d, v$range, lang, th), tr("sim.ecdf", lang))
          ),
          if (!is.null(v)) outside_note(v$outside, lang)
        )
      }
    },
    lln = {
      add_data(ctx, title, res$paths, paste0(name, "-lln"))
      shiny::tagList(report_plot(ctx, lln_plot(res, TRUE, lang, th, lower = dl_support(d)$lower),
                                 tr("report.fig.lln", lang), height = 360),
                     lln_note(res, lang))
    },
    clt = {
      add_data(ctx, title, res$means, paste0(name, "-clt"))
      shiny::tagList(report_plot(ctx, clt_plot(res, d, lang, th), tr("report.fig.clt", lang),
                                 height = 420),
                     clt_note(res, discrete, lang),
                     if (res$standardized) clt_table_tag(res, lang))
    },
    transform = {
      add_data(ctx, title, data.frame(y = res$sample), paste0(name, "-transform"))
      v <- tryCatch(tf_view_of(res, d), error = function(err) NULL)
      shiny::tagList(shiny::tags$p(class = "dl-sim-law", transform_text(res, lang)),
                     report_plot(ctx, tf_plot(res, v, lang, th), tr("sim.transform.title", lang)),
                     tf_summary_tag(res, d, v, lang))
    },
    memoryless = {
      add_data(ctx, title, data.frame(residual = res$residual), paste0(name, "-memoryless"))
      v <- tryCatch(sample_view_of(res$residual, d), error = function(err) NULL)
      shiny::tagList(
        mem_verdict_tag(res, d, res$n %||% res$kept, lang),
        shiny::tags$div(
          class = "dl-report-grid",
          report_plot(ctx, mem_surv_plot(res, d, lang, th), tr("sim.mem.survival", lang)),
          report_plot(ctx, mem_hist_plot(v, lang, th), tr("sim.mem.hist", lang))
        ),
        if (discrete) report_notes(tr("sim.mem.note.discrete", lang))
      )
    })
  n <- switch(r$kind, mc_check = res$n %||% length(res$sample),
              lln = max(res$paths$k), clt = NULL, transform = length(res$sample) + res$dropped,
              memoryless = res$n)
  extra <- if (r$kind == "clt") {
    tr("report.sim.reps", lang, n = format_count(nrow(res$means) / nrow(res$distance), lang))
  } else if (r$kind == "lln") {
    paths <- max(res$paths$path)
    if (paths == 1) tr("report.sim.paths_one", lang) else tr("report.sim.paths", lang, n = paths)
  }
  block <- ctx$sim_blocks[[as.character(r$index)]] %||% add_block(ctx, title, res$code, r$kind)
  shiny::tags$div(class = "dl-report-block", shiny::tags$h4(title),
                  sim_caption(ctx, d, if (length(n)) n, res$seed, extra), body, code_ref(ctx, block))
}

# The operation of a transform with its k, or a and b.
transform_text <- function(res, lang) {
  label <- tr(paste0("sim.op.", res$op), lang)
  if (res$op %in% .dl_transform_multi) return(paste0(label, " (k = ", format_count(res$k, lang), ")"))
  if (res$op == "affine") {
    sep <- if (lang == "pt-BR") "; " else ", "
    return(paste0(label, " (a = ", format_value(res$a, lang), sep, "b = ", format_value(res$b, lang), ")"))
  }
  label
}

# Calculator questions: event, value, sentence and shaded plots.
report_calculations_section <- function(ctx, calcs) {
  lang <- ctx$lang
  th <- ctx$th
  items <- lapply(seq_along(calcs), function(i) {
    d <- calcs[[i]]$dist
    res <- calcs[[i]]$result
    curve <- tryCatch(calc_curve(d, res), error = function(err) NULL)
    block <- add_block(ctx, tr("report.code.calc", lang, n = i), calc_code(d, res), "calc")
    shiny::tags$div(
      class = "dl-report-block",
      shiny::tags$h3(tr("report.calc.item", lang, n = i)),
      shiny::tags$p(class = "dl-report-caption", series_label(d, lang)),
      calc_result_tag(res, d, lang, code_id = NULL),
      shiny::tags$div(
        class = "dl-report-grid",
        report_plot(ctx, calc_density_plot(curve, res, d, lang, th),
                    tr(if (identical(d$entry$type, "discrete")) "explorer.pmf" else "explorer.density", lang)),
        report_plot(ctx, calc_cdf_plot(curve, res, d, lang, th), tr("explorer.cdf", lang))
      ),
      code_ref(ctx, block)
    )
  })
  report_section(
    "calculations", tr("report.section.calculations", lang),
    if (length(items)) items else shiny::tags$p(class = "dl-muted", tr("report.calc.none", lang))
  )
}

# Every relationship of the distribution with its condition, formula and
# demonstration.
report_relations <- function(ctx, dist, sims) {
  lang <- ctx$lang
  reg <- dl_registry()
  rel <- report_edges(dist$id)
  demos <- Filter(function(r) r$kind == "relation", sims)
  ids <- vapply(demos, function(r) r$edge %||% r$res$edge$id, "")
  name <- function(i) entry_text(reg[[i]], "names", lang)
  items <- lapply(seq_len(nrow(rel)), function(i) {
    r <- rel[i, ]
    hit <- match(r$id, ids)
    demo <- if (!is.na(hit)) demos[[hit]]$res
    body <- if (is.null(demo)) {
      report_notes(tr("report.rel.no_demo", lang))
    } else if (inherits(demo, "error")) {
      report_notes(tr("report.sim.failed", lang, message = sim_error_text(demo, lang)))
    } else {
      report_demo(ctx, demo, paste(name(r$from), "\u2192", name(r$to)))
    }
    shiny::tags$div(
      class = "dl-report-block",
      shiny::tags$h3(name(r$from), " \u2192 ", name(r$to), " ", relation_badge(r$type, lang)),
      shiny::tags$p(class = "dl-rel-cond", if (lang == "pt-BR") r$condition_pt else r$condition_en),
      shiny::tags$div(class = "dl-report-math", math_tag(r$latex, display = TRUE)),
      body
    )
  })
  report_section(
    "relations", tr("report.section.relations", lang),
    if (length(items)) items else shiny::tags$p(class = "dl-muted", tr("related.none", lang))
  )
}

# Figure, notes, distance table and code of one relation demo.
report_demo <- function(ctx, demo, label) {
  lang <- ctx$lang
  nf <- nrow(demo$frames)
  if (!is.null(demo$samples)) {
    s <- demo$samples
    frame <- if (demo$kind == "sum") {
      data.frame(terms = demo$frames$value[s$frame], value = s$value)
    } else {
      data.frame(value = s$value[s$frame == nf])
    }
    add_data(ctx, label, frame, paste0("relation-", demo$edge$id))
  }
  block <- add_block(ctx, tr("report.code.relation", lang, edge = label), demo$code, "relation")
  shiny::tagList(
    shiny::tags$p(class = "dl-report-caption",
                  paste(c(paste(tr("sim.rel.params", lang), demo$params),
                          if (!is.na(demo$seed)) tr("report.sim.seed", lang, seed = demo$seed)),
                        collapse = " \u00b7 ")),
    report_plot(ctx, rel_plot(demo, lang, ctx$th), tr("report.fig.demo", lang, edge = label),
                height = 420),
    rel_note(demo, lang),
    rel_table_tag(demo, lang),
    code_ref(ctx, block)
  )
}

# The fit of the family of `id` in `fit`: NULL when it was not a candidate.
report_fit_part <- function(fit, id) {
  if (is.null(fit) || !id %in% fit$ranking$id) return(NULL)
  list(fit = fit, model = fit$fits[[id]], failure = fit$failures[[id]])
}

# Ranking, estimates, diagnostics and descriptive goodness of fit.
report_fit <- function(ctx, dist, part) {
  lang <- ctx$lang
  th <- ctx$th
  fit <- part$fit
  id <- dist$id
  name <- entry_text(dist$entry, "names", lang)
  model <- part$model
  diag <- if (!is.null(model)) tryCatch(dl_fit_diagnostics(fit, id = id), error = function(err) NULL)
  views <- c(density = "fit.density", cdf = "fit.cdf", qq = "fit.qq", pp = "fit.pp")
  plots <- if (!is.null(diag)) {
    shown <- Filter(function(v) NROW(diag[[v]]) > 0, names(views))
    shiny::tags$div(class = "dl-report-grid", lapply(shown, function(v) {
      report_plot(ctx, fit_diagnostic_plot(diag, v, lang, th), tr(views[[v]], lang))
    }))
  }
  data <- report_fit_data(fit, lang)
  code <- paste(c(data$code,
                  if (!is.null(model)) c(sprintf("fit$fits[[\"%s\"]]$estimates", id),
                                         sprintf("distlab::dl_fit_diagnostics(fit, id = \"%s\")$gof", id))),
                collapse = "\n")
  block <- add_block(ctx, tr("report.code.fit", lang), code, "fit")
  intro <- if (nrow(fit$ranking) > 1L) "report.fit.intro" else "report.fit.intro_one"
  report_section(
    "fit", tr("report.section.fit", lang),
    shiny::tags$p(tr(intro, lang, name = name, n = format_count(fit$data$n, lang),
                     kind = tr(paste0("fit.", fit$data$type), lang))),
    shiny::tags$h3(tr("fit.ranking", lang)),
    fit_ranking_tag(fit, lang),
    if (!is.null(model)) {
      shiny::tagList(
        shiny::tags$h3(tr("fit.estimates", lang)),
        shiny::tags$p(class = "dl-report-caption", series_label(model$dist, lang)),
        fit_estimates_tag(model, lang),
        shiny::tags$p(class = "dl-muted", tr("fit.estimates_help", lang)),
        fit_notes_tag(model, lang),
        shiny::tags$h3(tr("report.fit.diagnostics", lang)),
        plots %||% report_notes(tr("report.plot_unavailable", lang)),
        if (!is.null(diag)) shiny::tagList(
          shiny::tags$h3(tr("fit.gof", lang)),
          report_notes(tr("report.fit.gof_note", lang)),
          fit_gof_tag(diag, model, lang),
          fit_diagnostic_notes_tag(diag, lang))
      )
    } else {
      report_notes(tr("report.fit.failed", lang, name = name,
                      message = fit_message_text(part$failure, lang)))
    },
    code_ref(ctx, block),
    data$note
  )
}

# Data of the fit code: a literal for small tables; larger ones are read
# from distlab-data.csv, embedded here with 17 significant digits (so the
# doubles read back exactly) up to .dl_report_fit_csv rows, and otherwise
# the table saved by the Fit page.
report_fit_data <- function(fit, lang) {
  data <- fit$data
  tab <- fit_data_table(data)
  rows <- nrow(tab)
  if (rows <= .dl_report_fit_literal) return(list(code = fit$code, note = NULL))
  file <- "distlab-data.csv"
  read <- switch(data$type,
    raw = "data <- distlab::dl_fit_data(x = rep(tab$value, tab$frequency))",
    frequency = "data <- distlab::dl_fit_data(x = tab$value, frequency = tab$frequency)",
    grouped = "data <- distlab::dl_fit_data(lower = tab$lower, upper = tab$upper, frequency = tab$frequency)")
  code <- sub(fit_data_code(data), paste(c(sprintf("tab <- read.csv(\"%s\")", file), read),
                                         collapse = "\n"), fit$code, fixed = TRUE)
  n <- format_count(rows, lang)
  note <- if (rows <= .dl_report_fit_csv) {
    shiny::tags$p(class = "dl-calc-note", tr("report.fit.data_file", lang, n = n), " ",
                  shiny::tags$a(href = text_data_uri(csv_text(tab, digits = 17), "text/csv"),
                                download = file, file))
  } else {
    report_notes(tr("report.fit.data_large", lang, n = n, file = file,
                    button = tr("fit.download_data", lang)))
  }
  list(code = code, note = note)
}

# Code appendix: every block in order, the whole script and the simulated
# data as CSV downloads.
report_code <- function(ctx, dist) {
  lang <- ctx$lang
  blocks <- lapply(seq_along(ctx$blocks), function(i) {
    b <- ctx$blocks[[i]]
    box <- code_box(b$code, sprintf("code-text-%d", i), b$kind, lang)
    if (nchar(b$code) > .dl_report_fold) {
      lines <- length(strsplit(b$code, "\n", fixed = TRUE)[[1]])
      box <- shiny::tags$details(shiny::tags$summary(tr("report.code.show", lang,
                                                        n = format_count(lines, lang))), box)
    }
    shiny::tags$div(class = "dl-report-code", id = paste0("code-", i),
                    shiny::tags$h3(sprintf("%d. %s", i, b$title)), box)
  })
  script <- paste(vapply(seq_along(ctx$blocks), function(i) {
    paste0("# ---- ", i, ". ", ctx$blocks[[i]]$title, " ----\n", ctx$blocks[[i]]$code, "\n")
  }, ""), collapse = "\n")
  script_link <- if (nchar(script, type = "bytes") <= .dl_report_script_max) {
    shiny::tags$p(shiny::tags$a(href = text_data_uri(script, "text/plain"),
                                download = sprintf("distlab-%s.R", dist$id),
                                tr("report.code.script", lang)))
  }
  report_section(
    "code", tr("report.section.code", lang),
    shiny::tags$p(tr("report.code.intro", lang)),
    script_link,
    blocks,
    report_data(ctx, dist)
  )
}

# R code card with a copy button (distlab.js copies the text of `id`).
code_box <- function(code, id, kind, lang) {
  shiny::tags$div(
    class = "dl-code",
    shiny::tags$div(
      class = "dl-code-head",
      shiny::tags$span(class = "dl-code-title", "R"),
      shiny::tags$button(type = "button", class = "dl-copy", `data-target` = id,
                         `data-done` = tr("calc.copied", lang),
                         shiny::tags$span(class = "dl-copy-label", tr("calc.copy", lang)))
    ),
    shiny::tags$pre(class = "dl-code-block", .noWS = "inside",
                    shiny::tags$code(id = id, `data-block` = kind, .noWS = c("inside", "outside"), code))
  )
}

# Rows of each data set embedded as CSV: the smaller sets whole, the larger
# ones an equal share of what is left, within the per-file cap.
csv_allocate <- function(sizes, total = .dl_report_csv_max, cap = .dl_report_csv_file_max) {
  out <- numeric(length(sizes))
  left <- total
  o <- order(sizes)
  for (j in seq_along(o)) {
    i <- o[j]
    share <- floor(left / (length(o) - j + 1))
    out[i] <- min(sizes[i], share, cap)
    left <- left - out[i]
  }
  out
}

# CSV text: a header and one row per observation, numbers with `digits`
# significant digits.
csv_text <- function(df, digits = 15) {
  cols <- lapply(df, function(v) {
    if (!is.numeric(v)) return(as.character(v))
    out <- trimws(formatC(v, digits = digits, format = "g"))
    out[is.na(v)] <- "NA"
    out
  })
  paste0(paste(names(df), collapse = ","), "\n",
         paste(do.call(paste, c(cols, sep = ",")), collapse = "\n"), "\n")
}

# Download links of the simulated data, with the share embedded.
report_data <- function(ctx, dist) {
  lang <- ctx$lang
  if (!length(ctx$data)) return(NULL)
  sizes <- vapply(ctx$data, function(d) nrow(d$frame), numeric(1))
  keep <- csv_allocate(sizes)
  items <- lapply(seq_along(ctx$data), function(i) {
    d <- ctx$data[[i]]
    frame <- d$frame[seq_len(keep[i]), , drop = FALSE]
    shiny::tags$li(
      shiny::tags$a(href = text_data_uri(csv_text(frame), "text/csv"),
                    download = sprintf("distlab-%s.csv", gsub("[^A-Za-z0-9_-]+", "-", d$name)),
                    d$label),
      " ",
      shiny::tags$span(class = "dl-muted",
                       if (keep[i] < sizes[i]) {
                         tr("report.data.cap", lang, kept = format_count(keep[i], lang),
                            n = format_count(sizes[i], lang))
                       } else {
                         tr("report.data.rows", lang, n = format_count(sizes[i], lang))
                       })
    )
  })
  shiny::tagList(
    shiny::tags$h3(tr("report.data.title", lang)),
    shiny::tags$p(class = "dl-muted", tr("report.data.intro", lang)),
    shiny::tags$ul(class = "dl-report-data", items)
  )
}

# Registry references and the R session that built the report.
report_references <- function(ctx, dist) {
  lang <- ctx$lang
  pkgs <- c("distlab", "plotly", "extraDistr", "htmltools")
  versions <- vapply(pkgs, function(p) as.character(utils::packageVersion(p)), "")
  row <- function(key, value) {
    shiny::tags$tr(shiny::tags$th(scope = "row", tr(key, lang)), shiny::tags$td(value))
  }
  report_section(
    "references", tr("report.section.references", lang),
    shiny::tags$h3(tr("report.refs", lang)),
    shiny::tags$ol(class = "dl-report-refs", lapply(dist$entry$references, shiny::tags$li)),
    shiny::tags$h3(tr("report.session", lang)),
    shiny::tags$div(class = "table-responsive", shiny::tags$table(shiny::tags$tbody(
      row("report.session.r", R.version.string),
      row("report.session.platform", R.version$platform),
      row("report.session.packages", paste(pkgs, versions, collapse = ", ")),
      row("report.session.date", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
    )))
  )
}
