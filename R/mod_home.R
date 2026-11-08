# Home: resume strip, search, task tiles, a data-type picker and exam-prep
# cards. Every control sends a navigation request handled by the target page.

# Icon of each data-type group.
.dl_group_icons <- c(binary = "toggle-on", counts = "list-ol", bounded = "percent",
                     positive = "hourglass-half", real_line = "arrows-left-right",
                     directional = "compass", multivariate = "table-cells",
                     mixed = "shapes")

# Chips under the search box.
.dl_home_popular <- c("normal", "binomial", "poisson", "exponential", "beta")

# Distributions marked "start here" in each data-type panel, shown first.
.dl_home_starters <- list(binary = "bernoulli", counts = c("binomial", "poisson"),
                          bounded = c("beta", "uniform"),
                          positive = c("exponential", "gamma"),
                          real_line = c("normal", "student_t"))

# Task tiles and the page each one opens (its navbar icon is reused).
.dl_home_tasks <- c(explore = "explorer", calc = "calculator", compare = "compare",
                    simulate = "simulation", fit = "fit", map = "compendium")

# Exam-prep cards and their requests.
.dl_home_exams <- list(
  binom_pois = list(tab = "simulation", edge = "binomial__poisson__limit"),
  clt = list(tab = "simulation", sim_tab = "clt"),
  t_normal = list(tab = "compare", preset = "t_normal"),
  beta_shapes = list(tab = "compare", preset = "beta_shapes")
)

mod_home_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "dl-home", `data-last-input` = ns("last"),
    shiny::uiOutput(ns("resume")),
    shiny::tags$section(
      class = "dl-home-entry",
      shiny::tags$h1(class = "dl-home-title", i18n_span("home.title", lang)),
      shiny::tags$div(
        class = "dl-home-search",
        shiny::tags$span(class = "dl-home-search-icon",
                         shiny::icon("magnifying-glass", `aria-hidden` = "true")),
        shiny::selectizeInput(ns("search"), i18n_span("home.search_label", lang),
                              choices = search_choices(lang), selected = "", width = "100%",
                              options = home_search_options(lang))
      ),
      shiny::tags$div(
        class = "dl-home-popular", role = "group", `aria-labelledby` = ns("popular_label"),
        shiny::tags$span(id = ns("popular_label"), class = "dl-quick-label",
                         i18n_span("home.popular", lang)),
        shiny::uiOutput(ns("popular"), inline = TRUE)
      ),
      shiny::tags$p(class = "dl-home-status", role = "status", `aria-live` = "polite",
                    shiny::textOutput(ns("status"), inline = TRUE))
    ),
    shiny::tags$section(
      class = "dl-home-band", `aria-labelledby` = ns("tasks_title"),
      shiny::tags$h2(id = ns("tasks_title"), class = "visually-hidden",
                     i18n_span("home.tasks", lang)),
      shiny::tags$div(class = "dl-task-grid",
                      lapply(names(.dl_home_tasks), task_tile, input_id = ns("task"),
                             lang = lang))
    ),
    shiny::tags$section(
      class = "dl-home-band dl-bytype", `aria-labelledby` = ns("bytype_title"),
      shiny::tags$h2(id = ns("bytype_title"), class = "dl-section-title",
                     i18n_span("home.bytype", lang)),
      group_picker(ns, lang)
    ),
    shiny::tags$section(
      class = "dl-home-band", `aria-labelledby` = ns("exam_title"),
      shiny::tags$h2(id = ns("exam_title"), class = "dl-section-title",
                     i18n_span("home.exam", lang)),
      shiny::tags$div(class = "dl-exam-row",
                      lapply(names(.dl_home_exams), exam_card, input_id = ns("exam"),
                             lang = lang))
    )
  )
}

# Selectize options of the search: translated placeholder; the list opens
# when the student types, not on focus.
home_search_options <- function(lang) {
  short <- encodeString(tr("home.search_placeholder_short", lang), quote = '"')
  list(
    placeholder = tr("home.search_placeholder", lang), openOnFocus = FALSE,
    # Phones get a shorter placeholder that fits the box.
    onInitialize = I(paste0(
      "function() { if (window.matchMedia('(max-width: 575.98px)').matches) { ",
      "this.settings.placeholder = ", short, "; this.updatePlaceholder(); } }"
    ))
  )
}

# Search choices: every distribution name in `lang`, sorted, multivariate
# reference cards included.
search_choices <- function(lang) {
  reg <- catalog_registry()
  labels <- vapply(reg, function(e) entry_text(e, "names", lang), "")
  o <- order(labels, method = "radix")
  c(stats::setNames("", ""), stats::setNames(names(reg)[o], labels[o]))
}

# Data-type groups present in the registry, in their canonical order.
registry_groups <- function(reg = catalog_registry()) {
  intersect(.dl_groups, unique(unlist(lapply(reg, entry_groups))))
}

# Chips of distributions `ids`, labeled in `lang`.
dist_chips <- function(ids, lang, input_id, reg = catalog_registry()) {
  lapply(ids, function(i) {
    name <- entry_text(reg[[i]], "names", lang)
    dist_button(i, name, input_id, label = open_label(reg[[i]], lang))
  })
}

# One task tile: navbar icon, verb and an example line.
task_tile <- function(key, input_id, lang) {
  tab <- .dl_home_tasks[[key]]
  icon <- Filter(function(s) identical(s$value, tab), .dl_sections)[[1]]$icon
  shiny::tags$button(
    type = "button", class = "dl-goto dl-task-tile", `data-dist` = key,
    `data-input` = input_id,
    shiny::tags$span(class = "dl-icon-chip", shiny::icon(icon, `aria-hidden` = "true")),
    shiny::tags$span(class = "dl-tile-text",
                     shiny::tags$span(class = "dl-tile-title",
                                      i18n_span(paste0("home.task.", key), lang)),
                     shiny::tags$span(class = "dl-tile-example",
                                      i18n_span(paste0("home.task.", key, ".ex"), lang)))
  )
}

# Data-type tiles, each a disclosure button followed by its panel. distlab.js
# keeps one panel open at a time; the chips are outputs so they follow the
# language without closing the panel.
group_picker <- function(ns, lang, reg = catalog_registry()) {
  items <- lapply(registry_groups(reg), function(g) {
    n <- length(group_members(g, reg))
    tile_id <- ns(paste0("tile_", g))
    panel_id <- ns(paste0("panel_", g))
    count_key <- if (n == 1L) "home.count_one" else "home.count"
    more_key <- if (n == 1L) "home.see_one" else "home.see_all"
    list(
      shiny::tags$button(
        type = "button", id = tile_id, class = paste0("dl-group-tile dl-group-", g),
        `aria-expanded` = "false", `aria-controls` = panel_id,
        shiny::tags$span(class = "dl-icon-chip",
                         shiny::icon(.dl_group_icons[[g]], `aria-hidden` = "true")),
        shiny::tags$span(class = "dl-tile-text",
                         shiny::tags$span(class = "dl-tile-title",
                                          i18n_span(paste0("group.", g), lang)),
                         shiny::tags$span(class = "dl-tile-example",
                                          i18n_span(paste0("home.example.", g), lang))),
        shiny::tags$span(class = "dl-count-badge",
                         shiny::tags$span(`data-i18n` = count_key, `data-i18n-n` = n,
                                          tr(count_key, lang, n = n))),
        shiny::tags$span(class = "dl-tile-caret",
                         shiny::icon("chevron-down", `aria-hidden` = "true"))
      ),
      shiny::tags$div(
        id = panel_id, class = paste0("dl-group-panel dl-group-", g), role = "region",
        `aria-labelledby` = tile_id, hidden = NA,
        shiny::uiOutput(ns(paste0("chips_", g)), class = "dl-chips"),
        shiny::tags$div(
          class = "dl-group-more",
          shiny::tags$button(
            type = "button", class = "dl-goto dl-link-btn", `data-dist` = g,
            `data-input` = ns("compendium"),
            shiny::tags$span(`data-i18n` = more_key, `data-i18n-n` = n,
                             tr(more_key, lang, n = n))
          )
        )
      )
    )
  })
  shiny::tags$div(class = "dl-group-grid", unlist(items, recursive = FALSE))
}

# Chips of one group: starters first, with a badge, then the rest by name.
group_chips <- function(g, lang, input_id, reg = catalog_registry()) {
  ids <- group_members(g, reg)
  starters <- intersect(.dl_home_starters[[g]] %||% character(0), ids)
  rest <- setdiff(ids, starters)
  labels <- vapply(rest, function(i) entry_text(reg[[i]], "names", lang), "")
  rest <- rest[order(labels, method = "radix")]
  start_tags <- lapply(starters, function(i) {
    name <- entry_text(reg[[i]], "names", lang)
    shiny::tags$button(
      type = "button", class = "dl-goto dl-chip dl-chip-start", `data-dist` = i,
      `data-input` = input_id, name,
      shiny::tags$span(class = "dl-start-badge", tr("home.start_here", lang))
    )
  })
  shiny::tagList(start_tags, dist_chips(rest, lang, input_id, reg))
}

# One exam-prep card: a static mini plot, a title and an example line.
exam_card <- function(key, input_id, lang) {
  shiny::tags$button(
    type = "button", class = "dl-goto dl-exam-card", `data-dist` = key,
    `data-input` = input_id,
    shiny::HTML(exam_svg(key)),
    shiny::tags$span(class = "dl-tile-title", i18n_span(paste0("home.exam.", key), lang)),
    shiny::tags$span(class = "dl-tile-example",
                     i18n_span(paste0("home.exam.", key, ".ex"), lang))
  )
}

# SVG path through the points (x, y), scaled into the 120 x 56 mini-plot box
# with y up; `ymax` sets the common vertical scale.
mini_path <- function(x, y, ymax = max(y), xlim = range(x)) {
  px <- 4 + (x - xlim[1]) / diff(xlim) * 112
  py <- 52 - pmin(y / ymax, 1) * 46
  paste0("M", paste(sprintf("%.1f,%.1f", px, py), collapse = " L"))
}

# Bars of heights y at positions x, in the same box as mini_path().
mini_bars <- function(x, y, ymax, xlim, width, class) {
  px <- 4 + (x - xlim[1]) / diff(xlim) * 112
  h <- pmin(y / ymax, 1) * 46
  paste(sprintf('<rect class="%s" x="%.1f" y="%.1f" width="%.1f" height="%.1f"/>',
                class, px - width / 2, 52 - h, width, h), collapse = "")
}

# Static mini plot of an exam-prep card, drawn from the exact densities.
# Decorative: the card text says what it shows.
exam_svg <- function(key) {
  curve <- function(d, class) sprintf('<path class="%s" d="%s"/>', class, d)
  body <- switch(key,
    binom_pois = {
      k <- 0:10
      yb <- stats::dbinom(k, 10, 0.3)
      yp <- stats::dpois(k, 3)
      paste0(mini_bars(k, yb, 0.3, c(-0.5, 10.5), 6, "dl-mini-bar"),
             curve(mini_path(k, yp, 0.3, c(-0.5, 10.5)), "dl-mini-line dl-mini-b"))
    },
    clt = {
      mids <- seq(-2.75, 2.75, by = 0.5)
      x <- seq(-3, 3, length.out = 61)
      paste0(mini_bars(mids, stats::dnorm(mids), 0.42, c(-3, 3), 8, "dl-mini-bar"),
             curve(mini_path(x, stats::dnorm(x), 0.42), "dl-mini-line dl-mini-a"))
    },
    t_normal = {
      x <- seq(-4, 4, length.out = 81)
      paste0(curve(mini_path(x, stats::dnorm(x), 0.42), "dl-mini-line dl-mini-a"),
             curve(mini_path(x, stats::dt(x, 1), 0.42), "dl-mini-line dl-mini-b dl-mini-dash"))
    },
    beta_shapes = {
      x <- seq(0.02, 0.98, length.out = 49)
      shapes <- list(c(0.5, 0.5), c(2, 5), c(5, 2), c(2, 2))
      cls <- c("dl-mini-a", "dl-mini-b", "dl-mini-c", "dl-mini-a dl-mini-dash")
      paste(vapply(seq_along(shapes), function(i) {
        s <- shapes[[i]]
        curve(mini_path(x, stats::dbeta(x, s[1], s[2]), 3, c(0, 1)),
              paste("dl-mini-line", cls[i]))
      }, ""), collapse = "")
    })
  paste0('<svg class="dl-mini-plot" viewBox="0 0 120 56" aria-hidden="true" ',
         'focusable="false"><line class="dl-mini-axis" x1="4" y1="52.5" x2="116" ',
         'y2="52.5"/>', body, "</svg>")
}

# Last distribution reported by the browser, checked against the registry:
# list(tab, id, params), with params dropped when they are not a complete,
# valid set. NULL when there is nothing usable.
home_last <- function(x) {
  if (!is.list(x) || !is_string(x$tab) || !x$tab %in% c("explorer", "calculator") ||
      !is_string(x$id) || !x$id %in% dl_ids()) {
    return(NULL)
  }
  e <- dl_entry(x$id)
  nm <- vapply(e$parameters, `[[`, "", "name")
  p <- if (is.list(x$params)) x$params[intersect(names(x$params), nm)] else list()
  ok <- vapply(p, function(v) is.numeric(v) && length(v) == 1L && is.finite(v), logical(1))
  p <- p[ok]
  valid <- setequal(names(p), nm) &&
    !inherits(tryCatch(do.call(dl_dist, c(list(x$id), p)), error = function(err) err), "error")
  list(tab = x$tab, id = x$id, params = if (valid) p[nm] else list())
}

# Resume strip for the last distribution: name, parameter values as math and
# the page it was open on.
resume_tag <- function(last, lang, input_id) {
  if (is.null(last)) return(NULL)
  e <- dl_entry(last$id)
  vals <- if (length(last$params)) last$params else default_params(e)
  sep <- if (lang == "pt-BR") ";\\ " else ",\\ "
  tex <- paste(vapply(e$parameters, function(par) {
    paste(par$latex, "=", format_num(vals[[par$name]], "en-US"))
  }, ""), collapse = sep)
  shiny::tags$div(
    class = "dl-resume",
    shiny::tags$button(
      type = "button", class = "dl-goto dl-resume-btn", `data-dist` = last$id,
      `data-input` = input_id,
      shiny::icon("clock-rotate-left", `aria-hidden` = "true"),
      shiny::tags$span(tr("home.resume", lang)),
      shiny::tags$strong(entry_text(e, "names", lang)),
      math_tag(paste0("(", tex_decimal(tex, lang), ")")),
      shiny::tags$span(tr(paste0("home.resume_in.", last$tab), lang)),
      shiny::tags$span(class = "dl-resume-arrow", `aria-hidden` = "true", "\u2192")
    )
  )
}

# Request of a task tile; the Explorer and the Calculator reopen the last
# distribution when there is one.
home_task_route <- function(key, last = NULL) {
  switch(key,
         explore = list(tab = "explorer", id = last$id %||% "normal", params = last$params),
         calc = list(tab = "calculator", id = last$id %||% "binomial", params = last$params),
         compare = list(tab = "compare", preset = "t_normal"),
         simulate = list(tab = "simulation"),
         fit = list(tab = "fit"),
         map = list(tab = "compendium"),
         NULL)
}

# Returns a reactive with the latest navigation request: list(tab, n) plus
# id/params (Explorer, Calculator), group (Compendium), preset (Compare) or
# edge/sim_tab (Simulation), or list(card, n) without a tab for the reference
# card of a multivariate entry; `n` increases so repeated requests still fire.
# `active` is a reactive that is TRUE while Home is the open page.
mod_home_server <- function(id, lang, active = shiny::reactive(NULL)) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    route <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(NULL)
    count <- 0L
    go <- function(r) {
      if (is.null(r)) return()
      r <- Filter(Negate(is.null), r)
      if (!length(r$params)) r$params <- NULL
      count <<- count + 1L
      route(c(r, n = count))
      status(if (is_string(r$id) && is_string(r$tab) && r$tab %in% c("explorer", "calculator")) {
        r[c("tab", "id")]
      })
    }
    last <- shiny::reactive(home_last(input$last))
    # The "opening" message belongs to the last request; clear it on return.
    shiny::observeEvent(active(), if (isTRUE(active())) status(NULL))

    output$resume <- shiny::renderUI(resume_tag(last(), lang_r(), ns("resume")))
    # The last distribution changes while other pages are open; keep it current.
    shiny::outputOptions(output, "resume", suspendWhenHidden = FALSE)
    output$popular <- shiny::renderUI(dist_chips(.dl_home_popular, lang_r(), ns("pick")))
    for (g in registry_groups()) local({
      gg <- g
      out <- paste0("chips_", gg)
      output[[out]] <- shiny::renderUI(group_chips(gg, lang_r(), ns("pick")))
      # Panels start hidden; their chips must be ready when one opens.
      shiny::outputOptions(output, out, suspendWhenHidden = FALSE)
    })
    output$status <- shiny::renderText({
      s <- status()
      if (is.null(s)) return("")
      tr(paste0("home.opening.", s$tab), lang_r(),
         name = entry_text(dl_entry(s$id), "names", lang_r()))
    })

    shiny::observeEvent(lang_r(), {
      shiny::updateSelectizeInput(session, "search", choices = search_choices(lang_r()),
                                  selected = "", options = home_search_options(lang_r()))
    }, ignoreInit = TRUE)

    # A univariate law opens in the Explorer, a multivariate one as its
    # reference card over Home.
    open_entry <- function(did) {
      if (!is_string(did)) return(FALSE)
      if (did %in% dl_ids()) {
        go(list(tab = "explorer", id = did))
      } else if (did %in% dl_ids(dimension = "multivariate")) {
        go(list(card = did))
      } else {
        return(FALSE)
      }
      TRUE
    }
    shiny::observeEvent(input$search, {
      if (open_entry(input$search)) {
        shiny::updateSelectizeInput(session, "search", selected = "")
      }
    })
    shiny::observeEvent(input$pick, open_entry(input$pick))
    shiny::observeEvent(input$compendium, {
      if (is_string(input$compendium) && input$compendium %in% .dl_groups) {
        go(list(tab = "compendium", group = input$compendium))
      }
    })
    shiny::observeEvent(input$task, {
      if (is_string(input$task)) go(home_task_route(input$task, last()))
    })
    shiny::observeEvent(input$exam, {
      if (is_string(input$exam)) go(.dl_home_exams[[input$exam]])
    })
    shiny::observeEvent(input$resume, {
      l <- last()
      if (!is.null(l) && identical(input$resume, l$id)) go(l)
    })

    route
  })
}
