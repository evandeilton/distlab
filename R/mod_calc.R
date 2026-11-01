# Calculator: probabilities of events, quantiles and central intervals for
# one distribution, with the region shaded on the density and CDF.

mod_calc_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  mode_names <- list(
    math_tag("P(X \\le x)"), math_tag("P(X > x)"), math_tag("P(a < X \\le b)"),
    i18n_span("calc.mode.quantile", lang), i18n_span("calc.mode.central", lang)
  )
  d0 <- dl_dist("normal")
  v0 <- calc_defaults(d0)
  init <- function(name) format_input(v0[[name]], lang)
  bslib::layout_sidebar(
    class = "dl-explorer dl-calc",
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("dist"), i18n_span("explorer.distribution", lang),
                         choices = dist_choices(lang), selected = "normal"),
      shiny::tags$h2(class = "dl-sidebar-heading", i18n_span("explorer.parameters", lang)),
      shiny::uiOutput(ns("params"))
    ),
    shiny::uiOutput(ns("header")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)), fill = FALSE,
      bslib::card(
        class = "dl-calc-question",
        bslib::card_header(shiny::tags$span(
          class = "dl-card-title", shiny::icon("circle-question", `aria-hidden` = "true"),
          i18n_span("calc.question", lang))),
        bslib::card_body(
          shiny::tags$div(
            class = "dl-segmented",
            shiny::radioButtons(ns("mode"), i18n_span("calc.mode", lang),
                                choiceNames = mode_names, choiceValues = .dl_calc_modes,
                                selected = "between", inline = TRUE)
          ),
          shiny::conditionalPanel(
            "input.mode == 'le' || input.mode == 'gt'", ns = ns,
            shiny::textInput(ns("x"), label = math_tag("x"), value = init("x"), width = "100%")
          ),
          shiny::conditionalPanel(
            "input.mode == 'between'", ns = ns,
            shiny::tags$div(
              class = "dl-calc-pair",
              shiny::textInput(ns("a"), label = math_tag("a"), value = init("a")),
              shiny::textInput(ns("b"), label = math_tag("b"), value = init("b"))
            ),
            shiny::uiOutput(ns("endpoints"))
          ),
          shiny::conditionalPanel(
            "input.mode == 'quantile'", ns = ns,
            shiny::textInput(ns("p"), label = math_tag("p"), value = init("p"), width = "100%"),
            shiny::tags$small(class = "dl-muted dl-field-help", i18n_span("calc.help.p", lang))
          ),
          shiny::conditionalPanel(
            "input.mode == 'central'", ns = ns,
            shiny::textInput(ns("prob"), label = i18n_span("calc.prob_label", lang),
                             value = init("prob"), width = "100%"),
            shiny::tags$small(class = "dl-muted dl-field-help", i18n_span("calc.help.prob", lang))
          ),
          shiny::tags$p(class = "dl-muted dl-field-help dl-calc-decimal",
                        shiny::icon("keyboard", `aria-hidden` = "true"),
                        i18n_span("calc.help.decimal", lang))
        )
      ),
      bslib::card(
        class = "dl-calc-result",
        bslib::card_header(shiny::tags$span(
          class = "dl-card-title", shiny::icon("equals", `aria-hidden` = "true"),
          i18n_span("calc.result", lang))),
        bslib::card_body(shiny::tags$div(`aria-live` = "polite", shiny::uiOutput(ns("result"))))
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
      bslib::card(
        class = "dl-plot-card",
        bslib::card_header(shiny::textOutput(ns("density_title"), inline = TRUE)),
        plotly::plotlyOutput(ns("density"), height = "300px")
      ),
      bslib::card(
        class = "dl-plot-card",
        bslib::card_header(i18n_span("explorer.cdf", lang)),
        plotly::plotlyOutput(ns("cdf"), height = "300px")
      )
    )
  )
}

# `lang` may be fixed or reactive; `dark` a reactive logical; `select` a
# reactive navigation request list(tab, id, params) from other pages.
mod_calc_server <- function(id, lang, dark = shiny::reactive(FALSE),
                            select = shiny::reactive(NULL)) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    entry <- shiny::reactive(dl_entry(shiny::req(input$dist)))
    sliders <- param_sliders(input, output, session, entry, "params")
    pars <- sliders$pars
    is_discrete <- shiny::reactive(identical(entry()$type, "discrete"))
    fields <- c("x", "a", "b", "p", "prob")

    # Values of the numeric fields per distribution, restored on return.
    memory <- new.env(parent = emptyenv())
    # Values last sent to the fields, used until the browser reports them.
    shown <- shiny::reactiveVal(calc_defaults(dl_dist("normal")))
    current_id <- NULL
    current_lang <- NULL

    # Fields read with the marks of the language; NULL for unset fields.
    parsed <- shiny::reactive({
      lg <- lang_r()
      stats::setNames(lapply(fields, function(f) {
        v <- input[[f]]
        if (is.null(v)) NULL else parse_num(v, lg)
      }), fields)
    })
    # Numbers in the fields, without parsing attributes.
    values <- shiny::reactive({
      s <- shown()
      p <- parsed()
      stats::setNames(lapply(fields, function(f) {
        if (is.null(p[[f]])) s[[f]] else as.vector(p[[f]])
      }), fields)
    })
    # Fields used by each mode.
    mode_fields <- list(le = "x", gt = "x", between = c("a", "b"), quantile = "p",
                        central = "prob")
    # Notes on how the fields of the current mode were read: a mark taken as
    # a thousands separator, or a number beyond the double range.
    read_info <- shiny::reactive({
      p <- parsed()[mode_fields[[mode()]]]
      list(
        thousands = Filter(function(f) isTRUE(attr(p[[f]], "thousands")), names(p)),
        range = Filter(function(f) identical(attr(p[[f]], "reason"), "range"), names(p))
      )
    })

    send_fields <- function(vals, lg) {
      for (f in fields) {
        shiny::updateTextInput(session, f, value = format_input(vals[[f]], lg))
      }
      shown(vals)
    }

    # A new distribution keeps the fields of the one left behind and restores
    # its own, or starts from its quartiles.
    shiny::observeEvent(entry(), {
      new_id <- entry()$id
      if (!is.null(current_id) && !identical(current_id, new_id)) {
        # Fields left invalid keep the last values sent to them.
        left <- shiny::isolate(values())
        bad <- vapply(left, function(v) is.na(v), logical(1))
        left[bad] <- shiny::isolate(shown())[bad]
        memory[[current_id]] <- left
      }
      vals <- memory[[new_id]]
      if (is.null(vals)) {
        d <- tryCatch(do.call(dl_dist, c(list(new_id), sliders$get(new_id))),
                      error = function(err) dl_dist(new_id))
        vals <- calc_defaults(d)
      }
      current_id <<- new_id
      send_fields(vals, shiny::isolate(lang_r()))
    })

    # Choices are relabeled and the numbers rewritten with the decimal mark
    # of the new language, keeping every value.
    shiny::observeEvent(lang_r(), {
      lg <- lang_r()
      if (!is.null(current_lang) && !identical(current_lang, lg)) {
        vals <- stats::setNames(lapply(fields, function(f) {
          v <- shiny::isolate(input[[f]])
          if (is.null(v)) shiny::isolate(shown())[[f]] else parse_num(v, current_lang)
        }), fields)
        shiny::updateSelectInput(session, "dist", choices = dist_choices(lg),
                                 selected = shiny::isolate(input$dist))
        ok <- vapply(vals, function(v) !is.na(v), logical(1))
        vals[!ok] <- shiny::isolate(shown())[!ok]
        send_fields(vals, lg)
      }
      current_lang <<- lg
    })

    # Requests from other pages open a distribution with given parameters.
    shiny::observeEvent(select(), {
      r <- select()
      if (!identical(r$tab, "calculator") || !is_string(r$id) || !r$id %in% dl_ids()) return()
      if (length(r$params)) sliders$set(r$id, r$params)
      shiny::updateSelectInput(session, "dist", selected = r$id)
    })

    dist <- shiny::reactive({
      e <- entry()
      p <- pars()
      shiny::req(p, identical(p$id, e$id))
      dist_from_pars(p, lang_r())
    })
    dist_d <- shiny::debounce(dist, 150)
    dist_now <- shiny::reactive({
      d <- dist_d()
      shiny::req(identical(d$id, entry()$id))
      d
    })

    mode <- shiny::reactive({
      m <- input$mode
      if (is_string(m) && m %in% .dl_calc_modes) m else "between"
    })
    closed <- shiny::reactive(list(
      lower = identical(input$lower_end, "closed"),
      upper = !identical(input$upper_end, "open")
    ))

    # Result of the current question; invalid fields give a translated message.
    res <- shiny::reactive({
      d <- dist_now()
      lg <- lang_r()
      v <- values()
      m <- mode()
      shiny::validate(shiny::need(!length(read_info()$range), tr("calc.error.range", lg)))
      need_num <- function(...) {
        shiny::validate(shiny::need(all(!is.na(c(...))), tr("calc.error.number", lg)))
      }
      if (m %in% c("le", "gt")) need_num(v$x)
      if (m == "between") {
        need_num(v$a, v$b)
        shiny::validate(shiny::need(v$a <= v$b, tr("calc.error.order", lg)))
      }
      if (m == "quantile") {
        need_num(v$p)
        shiny::validate(shiny::need(v$p > 0 && v$p < 1, tr("calc.error.p", lg)))
      }
      if (m == "central") {
        need_num(v$prob)
        shiny::validate(shiny::need(v$prob > 0 && v$prob < 1, tr("calc.error.p", lg)))
      }
      cl <- closed()
      out <- tryCatch(
        calc_result(d, m, x = v$x, a = v$a, b = v$b, lower_closed = cl$lower,
                    upper_closed = cl$upper, p = v$p, prob = v$prob),
        error = function(err) conditionMessage(err))
      shiny::validate(shiny::need(is.list(out), out))
      out
    })

    output$header <- shiny::renderUI(explore_header(entry(), lang_r()))

    output$density_title <- shiny::renderText({
      tr(if (is_discrete()) "explorer.pmf" else "explorer.density", lang_r())
    })

    # Open or closed ends for discrete distributions; a note for continuous.
    output$endpoints <- shiny::renderUI({
      lg <- lang_r()
      if (!is_discrete()) {
        return(shiny::tags$p(class = "dl-calc-note", shiny::icon("circle-info", `aria-hidden` = "true"),
                             tr("calc.note.continuous_ends", lg)))
      }
      lower <- shiny::isolate(input$lower_end) %||% "open"
      upper <- shiny::isolate(input$upper_end) %||% "closed"
      shiny::tagList(
        shiny::tags$div(
          class = "dl-calc-pair dl-segmented",
          shiny::radioButtons(session$ns("lower_end"), tr("calc.lower_end", lg),
                              choiceNames = list(math_tag("a < X"), math_tag("a \\le X")),
                              choiceValues = c("open", "closed"), selected = lower, inline = TRUE),
          shiny::radioButtons(session$ns("upper_end"), tr("calc.upper_end", lg),
                              choiceNames = list(math_tag("X \\le b"), math_tag("X < b")),
                              choiceValues = c("closed", "open"), selected = upper, inline = TRUE)
        ),
        shiny::tags$p(class = "dl-calc-note", shiny::icon("circle-info", `aria-hidden` = "true"),
                      tr("calc.note.discrete_ends", lg))
      )
    })

    output$result <- shiny::renderUI({
      r <- res()
      lg <- lang_r()
      th <- read_info()$thousands
      read <- lapply(th, function(f) {
        tr("calc.note.thousands", lg, text = input[[f]],
           value = format_value(values()[[f]], lg, digits = 15))
      })
      calc_result_tag(r, dist_now(), lg, session$ns("code"), read = read)
    })

    plot_data <- shiny::reactive({
      res_curve <- tryCatch(calc_curve(dist_now(), res()),
                            error = function(err) conditionMessage(err))
      shiny::validate(shiny::need(is.data.frame(res_curve), res_curve))
      res_curve
    })

    output$density <- plotly::renderPlotly({
      calc_density_plot(plot_data(), res(), dist_now(), lang_r(), plot_theme(dark()))
    })
    output$cdf <- plotly::renderPlotly({
      calc_cdf_plot(plot_data(), res(), dist_now(), lang_r(), plot_theme(dark()))
    })

    list(entry = entry, dist = dist, result = res)
  })
}

# Plotting grid for a result: the usual range, widened (by at most its own
# width on each side) so the points of the question are visible.
calc_curve <- function(dist, res) {
  r <- curve_range(dist)
  width <- r[2] - r[1]
  pts <- c(res$region$lower, res$region$upper)
  pts <- pts[is.finite(pts)]
  lo <- max(min(c(r[1], pts)), r[1] - width)
  hi <- min(max(c(r[2], pts)), r[2] + width)
  if (identical(dist$entry$type, "discrete")) {
    s <- dl_support(dist)
    lo <- max(lo, s$lower)
    hi <- min(hi, s$upper)
  }
  curve_grid(dist, lo, hi, n = 501L)
}

# TRUE for values of x inside the region of a result.
in_region <- function(x, r) {
  above <- if (r$lower_closed) x >= r$lower else x > r$lower
  below <- if (r$upper_closed) x <= r$upper else x < r$upper
  above & below
}

# Shaded color of the region, by color mode.
region_colors <- function(th) {
  dark <- isTRUE(th$dark)
  list(fill = if (dark) "rgba(242, 163, 58, 0.45)" else "rgba(242, 163, 58, 0.50)",
       line = th$accent, bar = if (dark) "#F2A33A" else "#E08E1B",
       muted = if (dark) "rgba(169, 172, 242, 0.32)" else "rgba(43, 45, 110, 0.18)")
}

calc_density_plot <- function(d, res, dist, lang, th) {
  rc <- region_colors(th)
  r <- res$region
  inside <- in_region(d$x, r)
  if (identical(dist$entry$type, "discrete")) {
    p <- plotly::plot_ly(d, x = ~x, y = ~density, type = "bar",
                         marker = list(color = ifelse(inside, rc$bar, rc$muted)),
                         hovertemplate = "x = %{x}<br>P = %{y:.4f}<extra></extra>")
    ylab <- tr("plot.probability", lang)
  } else {
    p <- plotly::plot_ly(d, x = ~x, y = ~density, type = "scatter", mode = "lines",
                         line = list(color = th$line, width = 2.5),
                         hovertemplate = "x = %{x:.4f}<br>f(x) = %{y:.4f}<extra></extra>")
    # Region polygon with its exact ends, clipped to the plotted range.
    lo <- max(r$lower, min(d$x))
    hi <- min(r$upper, max(d$x))
    if (hi > lo) {
      xs <- sort(unique(c(lo, d$x[d$x > lo & d$x < hi], hi)))
      ys <- dl_density(dist, xs)
      ok <- is.finite(ys)
      p <- plotly::add_trace(p, x = xs[ok], y = ys[ok], type = "scatter", mode = "lines",
                             fill = "tozeroy", fillcolor = rc$fill,
                             line = list(color = rc$line, width = 1), hoverinfo = "skip",
                             inherit = FALSE)
    }
    ylab <- tr("plot.density", lang)
  }
  p <- plotly::layout(p, shapes = end_lines(res, dist, d, rc$line))
  plot_layout(p, tr("plot.x", lang), ylab, th, lang, ylim = density_ylim(d))
}

# Dashed vertical lines at the finite ends of the region inside the range;
# for discrete distributions they sit between bars, at the cut points + 1/2.
end_lines <- function(res, dist, d, color) {
  r <- res$region
  ends <- if (identical(dist$entry$type, "discrete")) {
    if (is.null(res$ends)) numeric(0) else res$ends + 0.5
  } else {
    c(r$lower, r$upper)
  }
  ends <- ends[is.finite(ends) & ends >= min(d$x) - 0.5 & ends <= max(d$x) + 0.5]
  lapply(unique(ends), function(v) {
    list(type = "line", xref = "x", yref = "paper", x0 = v, x1 = v, y0 = 0, y1 = 1,
         line = list(color = color, width = 1.5, dash = "dash"))
  })
}

calc_cdf_plot <- function(d, res, dist, lang, th) {
  rc <- region_colors(th)
  discrete <- identical(dist$entry$type, "discrete")
  p <- plotly::plot_ly(d, x = ~x, y = ~cdf, type = "scatter", mode = "lines",
                       line = list(color = th$line2, shape = if (discrete) "hv" else "linear",
                                   width = 2.5),
                       hovertemplate = "x = %{x:.4f}<br>F(x) = %{y:.4f}<extra></extra>")
  # Points where the CDF is read: the cut points of the event, or the
  # quantile at height p.
  cuts <- if (res$mode %in% c("quantile", "central")) res$q else res$ends
  cuts <- cuts[is.finite(cuts) & cuts >= min(d$x) & cuts <= max(d$x)]
  if (length(cuts)) {
    fx <- dl_cdf(dist, cuts)
    guides <- unlist(lapply(seq_along(cuts), function(i) list(
      list(type = "line", xref = "x", yref = "y", x0 = cuts[i], x1 = cuts[i], y0 = 0, y1 = fx[i],
           line = list(color = rc$line, width = 1.2, dash = "dot")),
      list(type = "line", xref = "x", yref = "y", x0 = min(d$x), x1 = cuts[i], y0 = fx[i], y1 = fx[i],
           line = list(color = rc$line, width = 1.2, dash = "dot"))
    )), recursive = FALSE)
    p <- plotly::add_trace(p, x = cuts, y = fx, type = "scatter", mode = "markers",
                           marker = list(color = rc$line, size = 9,
                                         line = list(color = th$hover_bg, width = 1.5)),
                           hovertemplate = "x = %{x}<br>F(x) = %{y:.4f}<extra></extra>",
                           inherit = FALSE)
    # A quantile is read from the height p across to the curve.
    if (res$mode == "quantile") {
      guides <- c(guides, list(list(
        type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1, y0 = res$p, y1 = res$p,
        line = list(color = th$muted, width = 1, dash = "dash"))))
    }
    p <- plotly::layout(p, shapes = guides)
  }
  plot_layout(p, tr("plot.x", lang), tr("plot.cdf", lang), th, lang, ylim = c(0, 1.02))
}

# Result card: the event in math, the probability (or quantile) in large
# type with its percentage, a sentence, the complement, notes and the R code.
# `read` holds notes on how typed numbers were read.
calc_result_tag <- function(res, dist, lang, code_id, read = list()) {
  discrete <- identical(dist$entry$type, "discrete")
  prob_mode <- res$mode %in% c("le", "gt", "between")
  big <- switch(res$mode,
    quantile = format_value(res$q, lang),
    central = paste0("[", format_value(res$q[1], lang), "; ", format_value(res$q[2], lang), "]"),
    format_prob(res$log_p, lang, empty = res$empty)
  )
  chip <- if (prob_mode) format_pct(res$log_p, lang) else if (res$mode == "central" && discrete) {
    tr("calc.coverage", lang, p = format_prob(res$log_p, lang))
  }
  notes <- read
  if (prob_mode) {
    notes <- c(notes, list(tr("calc.complement", lang,
                              p = format_prob(res$log_comp, lang, empty = res$comp_empty))))
    if (is.finite(res$log_p) && exp(res$log_p) < 1e-4) {
      notes <- c(notes, list(tr("calc.note.tiny", lang)))
    }
    if (outside_support(dist, res$region)) notes <- c(notes, list(tr("calc.note.outside", lang)))
  }
  if (res$mode == "quantile" && discrete) notes <- c(notes, list(tr("calc.note.quantile_discrete", lang)))
  code <- calc_code(dist, res)
  shiny::tags$div(
    class = "dl-calc-out",
    shiny::tags$div(class = "dl-calc-event", math_tag(calc_tex(res, lang), display = TRUE)),
    shiny::tags$div(
      class = "dl-calc-value",
      shiny::tags$span(class = "dl-calc-big", big),
      if (!is.null(chip)) shiny::tags$span(class = "badge dl-calc-pct", chip)
    ),
    shiny::tags$p(class = "dl-calc-sentence", calc_sentence(res, dist, lang)),
    if (length(notes)) {
      shiny::tags$ul(class = "dl-calc-notes", lapply(notes, shiny::tags$li))
    },
    shiny::tags$div(
      class = "dl-code",
      shiny::tags$div(
        class = "dl-code-head",
        shiny::tags$span(class = "dl-code-title", shiny::icon("code", `aria-hidden` = "true"),
                         tr("calc.code", lang)),
        shiny::tags$button(type = "button", class = "btn btn-sm dl-copy", `data-target` = code_id,
                           `data-done` = tr("calc.copied", lang),
                           shiny::icon("copy", `aria-hidden` = "true"),
                           shiny::tags$span(class = "dl-copy-label", tr("calc.copy", lang)))
      ),
      shiny::tags$pre(class = "dl-code-block", .noWS = "inside",
                      shiny::tags$code(id = code_id, .noWS = c("inside", "outside"), code))
    )
  )
}

# TRUE when a finite end of the region lies outside the support.
outside_support <- function(dist, r) {
  s <- dl_support(dist)
  ends <- c(r$lower, r$upper)
  ends <- ends[is.finite(ends)]
  any(ends < s$lower | ends > s$upper)
}
