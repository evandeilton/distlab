# Compare: two to four distributions overlaid, or one distribution with a
# parameter swept over a few values, with a summary table of moments.

.dl_compare_slots <- 4L

# Presets that teach a relationship or the role of a parameter. Overlay
# presets list (id, params) per slot; sweep presets give the distribution,
# the parameter, its values and the other parameters.
.dl_compare_presets <- list(
  binom_pois = list(mode = "overlay", slots = list(
    list("binomial", list(size = 50, prob = 0.06)),
    list("poisson", list(lambda = 3)))),
  t_normal = list(mode = "overlay", slots = list(
    list("normal", list(mean = 0, sd = 1)),
    list("student_t", list(df = 1)),
    list("student_t", list(df = 5)))),
  gamma_shape = list(mode = "sweep", id = "gamma", param = "shape",
                     values = c(0.5, 1, 2, 5), params = list(rate = 1)),
  beta_shapes = list(mode = "overlay", slots = list(
    list("beta", list(shape1 = 0.5, shape2 = 0.5)),
    list("beta", list(shape1 = 2, shape2 = 5)),
    list("beta", list(shape1 = 5, shape2 = 2)),
    list("beta", list(shape1 = 2, shape2 = 2)))),
  exp_weibull = list(mode = "overlay", slots = list(
    list("exponential", list(rate = 1)),
    list("weibull", list(shape = 1, scale = 1)),
    list("weibull", list(shape = 2, scale = 1)))),
  poisson_lambda = list(mode = "sweep", id = "poisson", param = "lambda",
                        values = c(1, 4, 10, 20), params = list())
)

# Distributions in the slots before any choice: the "t vs Normal" preset,
# then a Gamma in the fourth slot.
compare_initial_ids <- function() {
  ids <- vapply(.dl_compare_presets$t_normal$slots, `[[`, "", 1L)
  c(ids, "gamma")[seq_len(.dl_compare_slots)]
}

mod_compare_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  ids0 <- compare_initial_ids()
  n0 <- length(.dl_compare_presets$t_normal$slots)
  pal <- dl_palette(.dl_compare_slots)
  slot_card <- function(k) {
    shiny::tags$div(
      id = ns(paste0("slot", k)), class = "dl-slot",
      style = paste0("--dl-series: ", pal[k], ";"),
      hidden = if (k > n0) NA,
      shiny::tags$div(
        class = "dl-slot-head",
        shiny::tags$span(class = "dl-slot-swatch", `aria-hidden` = "true"),
        shiny::tags$span(class = "dl-slot-title",
                         shiny::tags$span(`data-i18n` = "compare.slot", `data-i18n-n` = k,
                                          tr("compare.slot", lang, n = k))),
        shiny::tags$button(
          type = "button", class = "btn btn-sm dl-slot-remove action-button",
          id = ns(paste0("remove", k)),
          `data-i18n-title` = "compare.remove", title = tr("compare.remove", lang),
          `data-i18n-aria-label` = "compare.remove", `aria-label` = tr("compare.remove", lang),
          shiny::icon("xmark", `aria-hidden` = "true")
        )
      ),
      shiny::selectInput(ns(paste0("s", k, "_dist")), i18n_span("explorer.distribution", lang),
                         choices = dist_choices(lang), selected = ids0[k], width = "100%"),
      shiny::uiOutput(ns(paste0("s", k, "_params")))
    )
  }
  preset_btn <- function(key) {
    shiny::tags$button(
      type = "button", class = "dl-goto dl-chip dl-preset", `data-dist` = key,
      `data-input` = ns("preset"), `aria-pressed` = if (key == "t_normal") "true" else "false",
      i18n_span(paste0("compare.preset.", key), lang)
    )
  }
  shiny::tags$div(
    class = "dl-compare",
    shiny::tags$div(
      class = "dl-comp-head",
      shiny::tags$h1(class = "dl-page-title", i18n_span("nav.compare", lang)),
      shiny::tags$p(class = "dl-muted dl-comp-sub", i18n_span("compare.subtitle", lang))
    ),
    shiny::tags$div(
      class = "dl-presets", role = "group",
      `data-i18n-aria-label` = "compare.presets", `aria-label` = tr("compare.presets", lang),
      shiny::tags$span(class = "dl-quick-label", i18n_span("compare.presets", lang)),
      lapply(names(.dl_compare_presets), preset_btn),
      shiny::tags$p(class = "dl-preset-hint", `aria-live` = "polite",
                    shiny::uiOutput(ns("preset_hint"), inline = TRUE))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8), xl = c(3, 9)), fill = FALSE,
      bslib::card(
        class = "dl-compare-controls",
        bslib::card_body(
          shiny::tags$div(
            class = "dl-segmented",
            shiny::radioButtons(
              ns("mode"), i18n_span("compare.mode", lang),
              choiceNames = list(i18n_span("compare.mode.overlay", lang),
                                 i18n_span("compare.mode.sweep", lang)),
              choiceValues = c("overlay", "sweep"), selected = "overlay", inline = TRUE
            )
          ),
          shiny::conditionalPanel(
            "input.mode == 'overlay'", ns = ns,
            shiny::tags$div(class = "dl-slots", lapply(seq_len(.dl_compare_slots), slot_card)),
            shiny::actionButton(ns("add"), i18n_span("compare.add", lang),
                                icon = shiny::icon("plus", `aria-hidden` = "true"),
                                class = "btn-sm btn-outline-primary dl-slot-add")
          ),
          shiny::conditionalPanel(
            "input.mode == 'sweep'", ns = ns,
            shiny::selectInput(ns("sw_dist"), i18n_span("explorer.distribution", lang),
                               choices = dist_choices(lang), selected = "gamma", width = "100%"),
            shiny::selectInput(ns("sw_param"), i18n_span("compare.sweep_param", lang),
                               choices = sweep_param_choices(dl_entry("gamma")),
                               selected = "shape", width = "100%"),
            shiny::textInput(ns("sw_values"), i18n_span("compare.sweep_values", lang),
                             value = format_list(sweep_defaults(dl_entry("gamma"), "shape"), lang),
                             width = "100%"),
            shiny::tags$small(class = "dl-muted dl-field-help", i18n_span("compare.sweep_help", lang)),
            shiny::tags$h2(class = "dl-sidebar-heading", i18n_span("compare.sweep_fixed", lang)),
            shiny::uiOutput(ns("sw_params"))
          )
        )
      ),
      shiny::tags$div(
        class = "dl-stack",
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, xl = c(6, 6)), fill = FALSE,
          bslib::card(
            class = "dl-plot-card",
            bslib::card_header(i18n_span("compare.density", lang)),
            plotly::plotlyOutput(ns("density"), height = "340px")
          ),
          bslib::card(
            class = "dl-plot-card",
            bslib::card_header(i18n_span("explorer.cdf", lang)),
            plotly::plotlyOutput(ns("cdf"), height = "340px")
          )
        ),
        shiny::uiOutput(ns("note")),
        bslib::card(
          bslib::card_header(shiny::tags$span(
            class = "dl-card-title", shiny::icon("table", `aria-hidden` = "true"),
            i18n_span("compare.summary", lang))),
          bslib::card_body(shiny::uiOutput(ns("summary")))
        )
      )
    )
  )
}

# Parameters that can be swept: every parameter of the entry, by R name.
sweep_param_choices <- function(entry) {
  nm <- vapply(entry$parameters, `[[`, "", "name")
  stats::setNames(nm, nm)
}

# Four values around the default of a parameter, inside its slider range
# and valid with the other parameters at `others` (the defaults unless
# given), so a sweep never starts with a domain or constraint error.
sweep_defaults <- function(entry, param, others = default_params(entry)) {
  par <- param_spec(entry, param)
  d <- par$default
  v <- if (isTRUE(par$integer)) d + c(-2, 0, 3, 8) * max(1, round(d / 4)) else
    signif(d * c(0.5, 1, 2, 4), 3)
  if (par$slider$min < 0 && !isTRUE(par$integer)) v <- d + c(-2, -1, 0, 2) * par$slider$step * 10
  valid <- function(v) {
    v <- v[v >= par$slider$min & v <= par$slider$max]
    ok <- vapply(v, function(x) {
      vals <- others
      vals[[param]] <- x
      !is.null(tryCatch(do.call(dl_dist, c(list(entry$id), vals)), error = function(e) NULL))
    }, logical(1))
    unique(round(v[ok], 10))
  }
  out <- valid(v)
  # Other parameters far from their defaults: step away from the current value.
  if (length(out) < 2L && is.numeric(others[[param]])) {
    out <- valid(others[[param]] + par$slider$step * c(-10, -5, 0, 5, 10, 20))
  }
  out
}

# Numbers as one text field: "; " between values, decimal mark of `lang`.
format_list <- function(v, lang) paste(vapply(v, format_input, "", lang = lang), collapse = "; ")

# Label of a series: name and parameter values, such as "Gamma(shape = 2,
# rate = 1)"; pt-BR separates values with "; " because of the decimal comma.
series_label <- function(dist, lang) {
  sep <- if (lang == "pt-BR") "; " else ", "
  vals <- vapply(dist$params, format_value, "", lang = lang)
  sprintf("%s(%s)", entry_text(dist$entry, "names", lang),
          paste(names(vals), vals, sep = " = ", collapse = sep))
}

# Colors of the series by color mode: the brand palette, lighter blue and
# vermillion on dark backgrounds.
series_colors <- function(n, dark = FALSE) {
  pal <- dl_palette()
  if (isTRUE(dark)) pal <- c("#3CC9C5", "#F2A33A", "#56B4E9", "#FF8A4C", "#E59CC8",
                             "#8FD0F5", "#A3A6CC")
  rep_len(pal, n)
}

.dl_dashes <- c("solid", "dash", "dot", "dashdot", "longdash")

# Support as text: whole numbers as a set, intervals with open or closed
# ends; pt-BR separates numbers with "; ".
support_text <- function(dist, lang) {
  s <- dl_support(dist)
  sep <- if (lang == "pt-BR") "; " else ", "
  v <- function(x) format_value(x, lang)
  if (identical(dist$entry$type, "discrete")) {
    if (is.infinite(s$upper)) {
      return(paste0("{", paste(v(s$lower), v(s$lower + 1), "\u2026", sep = sep), "}"))
    }
    pts <- if (s$upper - s$lower <= 3) vapply(seq(s$lower, s$upper), v, "") else
      c(v(s$lower), v(s$lower + 1), "\u2026", v(s$upper))
    return(paste0("{", paste(pts, collapse = sep), "}"))
  }
  open_lo <- s$lower_open || is.infinite(s$lower)
  open_hi <- s$upper_open || is.infinite(s$upper)
  paste0(if (open_lo) "(" else "[", v(s$lower), sep, v(s$upper), if (open_hi) ")" else "]")
}

# Shared range of several distributions: the union of their plotting ranges,
# with continuous ones limited to their medians plus or minus 6 IQRs so one
# heavy tail (Cauchy) does not squeeze the others. Errors when a discrete
# grid would be too long.
shared_range <- function(dists) {
  discrete <- vapply(dists, function(d) identical(d$entry$type, "discrete"), logical(1))
  r <- vapply(dists, function(d) {
    rg <- curve_range(d)
    if (identical(d$entry$type, "discrete")) return(rg)
    qq <- dl_quantile(d, c(0.25, 0.5, 0.75))
    iqr <- qq[3] - qq[1]
    if (is.finite(iqr) && iqr > 0) rg <- c(max(rg[1], qq[2] - 6 * iqr), min(rg[2], qq[2] + 6 * iqr))
    rg
  }, numeric(2))
  lo <- min(r[1, ])
  hi <- max(r[2, ])
  if (any(discrete) && hi - lo > 5000) {
    stop("The discrete range spans more than 5000 values.", call. = FALSE)
  }
  c(lo, hi)
}

# Curves of every series on the shared range.
compare_curves <- function(series) {
  rng <- shared_range(lapply(series, `[[`, "dist"))
  lapply(series, function(s) {
    s$curve <- curve_grid(s$dist, rng[1], rng[2], n = 401L)
    s
  })
}

# Y limit that keeps an unbounded density spike (Gamma or Beta with a shape
# below 1) from flattening the other curves; NULL when no cap is needed.
compare_ylim <- function(series) {
  info <- lapply(series, function(s) {
    d <- s$curve
    body <- d$density[d$cdf >= 0.01 & d$cdf <= 0.99]
    top <- if (nrow(d)) max(d$density) else 0
    cap <- if (length(body)) 1.1 * max(body) else top
    # A spike: the maximum sits at an end of the grid, far above the body.
    at_end <- nrow(d) > 0 && which.max(d$density) %in% c(1L, nrow(d))
    list(top = top, cap = cap, spike = at_end && top > 2 * cap)
  })
  spike <- vapply(info, `[[`, logical(1), "spike")
  if (!any(spike)) return(NULL)
  tops <- vapply(info, `[[`, numeric(1), "top")
  caps <- vapply(info, `[[`, numeric(1), "cap")
  cap <- if (all(spike)) max(caps) else 1.15 * max(tops[!spike])
  c(0, cap)
}

compare_density_plot <- function(series, lang, th) {
  p <- plotly::plot_ly()
  for (s in series) {
    d <- s$curve
    if (s$discrete) {
      p <- plotly::add_trace(p, x = d$x, y = d$density, type = "bar", name = s$label,
                             marker = list(color = s$color, opacity = 0.85,
                                           line = list(color = s$color, width = 1)),
                             hovertemplate = paste0(s$label, "<br>x = %{x}<br>P = %{y:.4f}<extra></extra>"))
    } else {
      p <- plotly::add_trace(p, x = d$x, y = d$density, type = "scatter", mode = "lines",
                             name = s$label,
                             line = list(color = s$color, width = 2.5, dash = s$dash),
                             hovertemplate = paste0(s$label, "<br>x = %{x:.4f}<br>f(x) = %{y:.4f}<extra></extra>"))
    }
  }
  ylab <- if (all(vapply(series, `[[`, logical(1), "discrete"))) tr("plot.probability", lang) else
    tr("plot.density", lang)
  p <- plot_layout(p, tr("plot.x", lang), ylab, th, lang, ylim = compare_ylim(series))
  compare_legend(plotly::layout(p, barmode = "group", bargap = 0.15), th)
}

compare_cdf_plot <- function(series, lang, th) {
  p <- plotly::plot_ly()
  for (s in series) {
    d <- s$curve
    args <- list(p, x = d$x, y = d$cdf, type = "scatter",
                 mode = if (s$discrete) "lines+markers" else "lines", name = s$label,
                 line = list(color = s$color, width = 2.2, dash = s$dash,
                             shape = if (s$discrete) "hv" else "linear"))
    # Discrete steps get a marker at each jump.
    if (s$discrete) args$marker <- list(color = s$color, size = 5)
    p <- do.call(plotly::add_trace, c(args, list(hovertemplate = paste0(s$label, "<br>x = %{x:.4f}<br>F(x) = %{y:.4f}<extra></extra>"))))
  }
  p <- plot_layout(p, tr("plot.x", lang), tr("plot.cdf", lang), th, lang, ylim = c(0, 1.02))
  compare_legend(p, th)
}

# Horizontal legend under the plot, in the colors of the mode.
compare_legend <- function(p, th) {
  plotly::layout(p, showlegend = TRUE,
                 legend = list(orientation = "h", x = 0, y = -0.22, yanchor = "top",
                               font = list(color = th$text, size = 11),
                               bgcolor = "rgba(0,0,0,0)"),
                 margin = list(l = 56, r = 12, t = 16, b = 40))
}

# Summary table: moments, median and support of each series.
compare_table <- function(series, lang) {
  cols <- c("mean", "variance", "sd", "skewness", "excess_kurtosis")
  th <- function(text) shiny::tags$th(scope = "col", text)
  rows <- lapply(series, function(s) {
    m <- tryCatch(dl_moments(s$dist), error = function(err) NULL)
    med <- tryCatch(dl_quantile(s$dist, 0.5), error = function(err) NA_real_)
    cells <- lapply(cols, function(k) {
      undefined <- !is.null(m) && is.na(m[[k]])
      shiny::tags$td(class = paste(c("dl-cell-num", if (undefined) "dl-cell-undefined"),
                                   collapse = " "),
                     if (is.null(m)) "\u2014" else format_moment(m[[k]], lang))
    })
    shiny::tags$tr(
      shiny::tags$th(scope = "row", class = "dl-series-name",
                     shiny::tags$span(class = "dl-series-key", `aria-hidden` = "true",
                                      style = paste0("--dl-series: ", s$color, ";"),
                                      `data-dash` = s$dash,
                                      `data-kind` = if (s$discrete) "bar" else "line"),
                     s$label),
      cells,
      shiny::tags$td(class = "dl-cell-num", format_num(med, lang)),
      shiny::tags$td(class = "dl-cell-support", support_text(s$dist, lang))
    )
  })
  shiny::tags$div(
    class = "table-responsive",
    shiny::tags$table(
      class = "table dl-table dl-compare-table",
      shiny::tags$caption(class = "visually-hidden", tr("compare.summary", lang)),
      shiny::tags$thead(shiny::tags$tr(
        th(tr("compare.col.dist", lang)),
        lapply(cols, function(k) th(tr(paste0("moments.", k), lang))),
        th(tr("formula.median", lang)), th(tr("compendium.support", lang))
      )),
      shiny::tags$tbody(rows)
    )
  )
}

# `lang` may be fixed or reactive; `dark` a reactive logical; `select` a
# reactive navigation request list(tab, id, params) from other pages.
mod_compare_server <- function(id, lang, dark = shiny::reactive(FALSE),
                               select = shiny::reactive(NULL)) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    K <- .dl_compare_slots
    ids0 <- compare_initial_ids()

    # One set of sliders per slot, plus one for the sweep.
    slot_entry <- lapply(seq_len(K), function(k) {
      force(k)
      shiny::reactive(dl_entry(shiny::req(input[[paste0("s", k, "_dist")]])))
    })
    slot_sliders <- lapply(seq_len(K), function(k) {
      param_sliders(input, output, session, slot_entry[[k]], paste0("s", k, "_params"),
                    prefix = paste0("s", k, "_"))
    })
    sw_entry <- shiny::reactive(dl_entry(shiny::req(input$sw_dist)))
    sw_sliders <- param_sliders(input, output, session, sw_entry, "sw_params", prefix = "sw_",
                                skip = shiny::reactive(input$sw_param))

    # Slots in use, in slot order; a slot keeps its color when others leave.
    active <- shiny::reactiveVal(seq_along(.dl_compare_presets$t_normal$slots))
    preset <- shiny::reactiveVal("t_normal")
    # Sweep choices waiting for the browser to switch distribution.
    pending <- new.env(parent = emptyenv())

    for (k in seq_along(.dl_compare_presets$t_normal$slots)) {
      s <- .dl_compare_presets$t_normal$slots[[k]]
      slot_sliders[[k]]$set(s[[1]], s[[2]])
    }

    show_slots <- function(act) {
      session$sendCustomMessage("dl-show", list(
        show = as.list(ns(paste0("slot", act))),
        hide = as.list(ns(paste0("slot", setdiff(seq_len(K), act))))
      ))
      session$sendCustomMessage("dl-show", list(
        show = if (length(act) < K) list(ns("add")) else list(),
        hide = if (length(act) >= K) list(ns("add")) else list()
      ))
      rm_show <- if (length(act) > 2L) ns(paste0("remove", act)) else character(0)
      session$sendCustomMessage("dl-show", list(
        show = as.list(rm_show),
        hide = as.list(setdiff(ns(paste0("remove", seq_len(K))), rm_show))
      ))
    }
    set_active <- function(act) {
      act <- sort(unique(act))
      active(act)
      show_slots(act)
    }
    shiny::observeEvent(TRUE, show_slots(shiny::isolate(active())), once = TRUE)

    # Put distribution `did` with `params` into slot k.
    fill_slot <- function(k, did, params = list()) {
      slot_sliders[[k]]$set(did, params)
      shiny::updateSelectInput(session, paste0("s", k, "_dist"), selected = did)
    }

    shiny::observeEvent(input$add, {
      act <- active()
      free <- setdiff(seq_len(K), act)
      if (length(free)) {
        set_active(c(act, free[1]))
        preset(NULL)
      }
    })
    for (k in seq_len(K)) local({
      kk <- k
      shiny::observeEvent(input[[paste0("remove", kk)]], {
        act <- active()
        if (length(act) > 2L && kk %in% act) {
          set_active(setdiff(act, kk))
          preset(NULL)
        }
      })
    })

    apply_preset <- function(key) {
      pr <- .dl_compare_presets[[key]]
      shiny::updateRadioButtons(session, "mode", selected = pr$mode)
      if (pr$mode == "overlay") {
        for (k in seq_along(pr$slots)) fill_slot(k, pr$slots[[k]][[1]], pr$slots[[k]][[2]])
        set_active(seq_along(pr$slots))
      } else {
        sw_sliders$set(pr$id, pr$params)
        # When the distribution changes, its observer must keep this choice.
        if (!identical(shiny::isolate(input$sw_dist), pr$id)) {
          pending$param <- pr$param
          pending$values <- pr$values
        }
        shiny::updateSelectInput(session, "sw_dist", selected = pr$id)
        apply_sweep_choice(pr$id, pr$param, pr$values)
      }
      preset(key)
      session$sendCustomMessage("dl-pressed", list(group = ns("preset"), value = key))
    }
    shiny::observeEvent(input$preset, {
      if (is_string(input$preset) && input$preset %in% names(.dl_compare_presets)) {
        apply_preset(input$preset)
      }
    })

    # Parameter and values of the sweep. A pending preset choice wins once.
    apply_sweep_choice <- function(did, param, values) {
      e <- dl_entry(did)
      shiny::updateSelectInput(session, "sw_param", choices = sweep_param_choices(e),
                               selected = param)
      shiny::updateTextInput(session, "sw_values", value = format_list(values, lang_r()))
      sweep_key <<- paste(did, param)
    }
    sweep_key <- "gamma shape"
    shiny::observeEvent(sw_entry(), {
      e <- sw_entry()
      param <- pending$param
      values <- pending$values
      if (is.null(param) || !param %in% sweep_param_choices(e)) {
        param <- shiny::isolate(input$sw_param)
        if (!is_string(param) || !param %in% sweep_param_choices(e)) param <- e$parameters[[1]]$name
        values <- sweep_defaults(e, param, sw_sliders$get(e$id))
      }
      pending$param <- NULL
      pending$values <- NULL
      if (!identical(sweep_key, paste(e$id, param))) apply_sweep_choice(e$id, param, values)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$sw_param, {
      e <- sw_entry()
      p <- input$sw_param
      if (is_string(p) && p %in% sweep_param_choices(e) && !identical(sweep_key, paste(e$id, p))) {
        apply_sweep_choice(e$id, p, sweep_defaults(e, p, sw_sliders$get(e$id)))
        preset(NULL)
      }
    }, ignoreInit = TRUE)

    # Other pages open a distribution here: slot 1 gets it (with the
    # parameters of the Explorer, when given) and slot 2 a related one.
    shiny::observeEvent(select(), {
      r <- select()
      if (!identical(r$tab, "compare") || !is_string(r$id) || !r$id %in% dl_ids()) return()
      shiny::updateRadioButtons(session, "mode", selected = "overlay")
      fill_slot(1L, r$id, r$params %||% list())
      rel <- related_edges(r$id)
      other <- if (length(rel)) rel[[1]]$other else r$id
      fill_slot(2L, other)
      set_active(1:2)
      preset(NULL)
      session$sendCustomMessage("dl-pressed", list(group = ns("preset"), value = ""))
    })

    # Relabel choices in the new language and rewrite the sweep values with
    # its decimal mark.
    last_lang <- NULL
    shiny::observeEvent(lang_r(), {
      lg <- lang_r()
      if (!is.null(last_lang) && !identical(last_lang, lg)) {
        for (k in seq_len(K)) {
          nm <- paste0("s", k, "_dist")
          shiny::updateSelectInput(session, nm, choices = dist_choices(lg),
                                   selected = shiny::isolate(input[[nm]]) %||% ids0[k])
        }
        shiny::updateSelectInput(session, "sw_dist", choices = dist_choices(lg),
                                 selected = shiny::isolate(input$sw_dist) %||% "gamma")
        v <- parse_num_list(shiny::isolate(input$sw_values), last_lang)
        if (length(v) && !anyNA(v)) {
          shiny::updateTextInput(session, "sw_values", value = format_list(v, lg))
        }
      }
      last_lang <<- lg
    })

    mode <- shiny::reactive(if (identical(input$mode, "sweep")) "sweep" else "overlay")

    # Series to draw: list(dist, label, color, dash, discrete).
    series <- shiny::reactive({
      lg <- lang_r()
      dk <- dark()
      if (mode() == "overlay") {
        act <- active()
        cols <- series_colors(K, dk)
        out <- lapply(seq_along(act), function(i) {
          k <- act[i]
          e <- slot_entry[[k]]()
          p <- slot_sliders[[k]]$pars()
          shiny::req(p, identical(p$id, e$id))
          d <- dist_from_pars(p, lg)
          list(dist = d, color = cols[k], dash = .dl_dashes[k])
        })
      } else {
        e <- sw_entry()
        p <- sw_sliders$pars()
        shiny::req(p, identical(p$id, e$id))
        param <- input$sw_param
        shiny::req(is_string(param), param %in% sweep_param_choices(e))
        v <- parse_num_list(input$sw_values, lg)
        shiny::validate(shiny::need(length(v) >= 2L && length(v) <= 5L && !anyNA(v),
                                    tr("compare.error.values", lg)))
        cols <- series_colors(length(v), dk)
        out <- lapply(seq_along(v), function(i) {
          vals <- p$vals
          vals[[param]] <- v[i]
          d <- tryCatch(do.call(dl_dist, c(list(e$id), vals)), error = function(err) NULL)
          shiny::validate(shiny::need(!is.null(d), tr("compare.error.value", lg,
                                                      value = format_value(v[i], lg))))
          list(dist = d, color = cols[i], dash = .dl_dashes[i])
        })
      }
      lapply(out, function(s) {
        s$label <- series_label(s$dist, lg)
        s$discrete <- identical(s$dist$entry$type, "discrete")
        s
      })
    })
    series_d <- shiny::debounce(series, 150)

    curves <- shiny::reactive({
      res <- tryCatch(compare_curves(series_d()), error = function(err) conditionMessage(err))
      shiny::validate(shiny::need(is.list(res), res))
      res
    })

    output$density <- plotly::renderPlotly(compare_density_plot(curves(), lang_r(), plot_theme(dark())))
    output$cdf <- plotly::renderPlotly(compare_cdf_plot(curves(), lang_r(), plot_theme(dark())))
    output$summary <- shiny::renderUI(compare_table(series_d(), lang_r()))

    # Mixed types share one axis: bars are probabilities, curves densities.
    output$note <- shiny::renderUI({
      kinds <- vapply(series_d(), `[[`, logical(1), "discrete")
      if (!(any(kinds) && !all(kinds))) return(NULL)
      shiny::tags$p(class = "dl-calc-note", shiny::icon("circle-info", `aria-hidden` = "true"),
                    tr("compare.note.mixed", lang_r()))
    })

    output$preset_hint <- shiny::renderUI({
      key <- preset()
      if (is.null(key)) return(tr("compare.preset_none", lang_r()))
      tr(paste0("compare.preset.", key, ".hint"), lang_r())
    })

    list(series = series, active = active, mode = mode, preset = preset)
  })
}
