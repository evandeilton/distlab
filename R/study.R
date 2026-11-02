# What the student did in one app session, per distribution id, for the study
# report: the last Calculator questions, the latest result of each
# Simulation tab, the relation demos opened and the latest fit that included
# the distribution. Plain environments, not reactive values, so recording
# never invalidates an output or repeats a computation.

# Store of one session. Calculator questions keep the last `max_calc` per
# distribution; a question in the same mode within `window` seconds of the
# previous one replaces it, so typing a number or dragging a slider leaves
# one question, not one per keystroke. Simulations, which hold their draws,
# are kept for the `max_ids` distributions simulated last and demos for the
# `max_demos` relations opened last, so memory stays bounded.
study_store <- function(max_calc = 10L, window = 2, max_ids = 4L, max_demos = 6L) {
  env <- new.env(parent = emptyenv())
  reset <- function() {
    env$calc <- list()
    env$sims <- list()
    env$demos <- list()
    env$fits <- list()
    env$seed <- NULL
  }
  reset()

  # A question as taken by dl_report(), with its `dist`; repeats are ignored.
  add_calc <- function(question, now = Sys.time()) {
    id <- question$dist$id
    recs <- env$calc[[id]] %||% list()
    if (any(vapply(recs, function(r) identical(r$question, question), logical(1)))) {
      return(invisible(FALSE))
    }
    rec <- list(question = question, time = now)
    n <- length(recs)
    recent <- n > 0L && identical(recs[[n]]$question$mode, question$mode) &&
      as.numeric(difftime(now, recs[[n]]$time, units = "secs")) < window
    if (recent) recs[[n]] <- rec else recs[[n + 1L]] <- rec
    if (length(recs) > max_calc) recs <- utils::tail(recs, max_calc)
    env$calc[[id]] <- recs
    invisible(TRUE)
  }

  # Move `key` to the end of a list (most recent last) and keep the last `n`.
  keep_recent <- function(lst, key, value, n) {
    lst[[key]] <- NULL
    lst[[key]] <- value
    if (length(lst) > n) lst <- utils::tail(lst, n)
    lst
  }

  # The latest result of each kind of simulation; demos by relation edge.
  add_sim <- function(res) {
    kind <- sim_kind(res)
    if (is.na(kind)) return(invisible(FALSE))
    if (kind == "relation") {
      env$demos <- keep_recent(env$demos, res$edge$id, res, max_demos)
    } else {
      id <- res$dist$id
      sims <- env$sims[[id]] %||% list()
      sims[[kind]] <- res
      env$sims <- keep_recent(env$sims, id, sims, max_ids)
    }
    if (!is.null(res$seed) && !is.na(res$seed)) env$seed <- res$seed
    invisible(TRUE)
  }

  # A fit is kept for every family among its candidates.
  add_fit <- function(fit) {
    for (id in fit$ranking$id) env$fits[[id]] <- fit
    invisible()
  }

  # Everything recorded for `id`, in the arguments of dl_report().
  get <- function(id) {
    sims <- env$sims[[id]] %||% list()
    sims <- unname(sims[intersect(.dl_report_kinds, names(sims))])
    demos <- Filter(function(d) id %in% c(d$edge$from, d$edge$to), env$demos)
    list(calculations = lapply(env$calc[[id]], `[[`, "question"),
         simulations = c(sims, unname(demos)),
         fit = env$fits[[id]],
         seed = env$seed)
  }

  list(add_calc = add_calc, add_sim = add_sim, add_fit = add_fit, get = get, clear = reset)
}

# Download of the study report of `dist()` in `lang()`, built from what
# `store` recorded for it; `fit()` (when not NULL) replaces the recorded fit.
report_download <- function(store, dist, lang, fit = function() NULL) {
  shiny::downloadHandler(
    filename = function() {
      sprintf("distlab-%s-%s.html", dist()$id, format(Sys.Date(), "%Y-%m-%d"))
    },
    content = function(file) {
      d <- dist()
      lg <- lang()
      rec <- store$get(d$id)
      # Building takes a few seconds; the app shows its progress, and a
      # translated message when it fails.
      shiny::withProgress(message = tr("report.building", lg), value = 0.2, {
        tryCatch(
          dl_report(d, file, lang = lg, calculations = rec$calculations,
                    simulations = rec$simulations, fit = fit() %||% rec$fit,
                    seed = rec$seed %||% 2026),
          error = function(err) {
            shiny::showNotification(tr("report.failed", lg), type = "error", duration = 8)
            stop(err)
          })
        shiny::setProgress(1)
      })
    }
  )
}

# Button that downloads the study report, labeled in `lang`.
report_button <- function(id, lang) {
  shiny::downloadButton(id, i18n_span("report.download", lang),
                        class = "btn-sm btn-outline-primary dl-report-button",
                        icon = shiny::icon("file-arrow-down", `aria-hidden` = "true"))
}
