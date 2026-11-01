# Explorer: one distribution at a time, with parameter sliders, density and
# CDF plots, moments, formula sheet, story and related distributions.

mod_explore_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  tab_panel <- function(tab) {
    bslib::nav_panel(
      title = i18n_span(paste0("formula.tab.", tab), lang), value = tab,
      shiny::uiOutput(ns(paste0("formulas_", tab)))
    )
  }
  bslib::layout_sidebar(
    class = "dl-explorer",
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("dist"), i18n_span("explorer.distribution", lang),
                         choices = dist_choices(lang), selected = "normal"),
      shiny::tags$h2(class = "dl-sidebar-heading",
                     i18n_span("explorer.parameters", lang)),
      shiny::uiOutput(ns("params")),
      shiny::tags$div(
        class = "dl-side-actions",
        shiny::actionButton(ns("to_calc"), i18n_span("explorer.to_calc", lang),
                            icon = shiny::icon("calculator", `aria-hidden` = "true"),
                            class = "btn-sm btn-outline-primary"),
        shiny::actionButton(ns("to_compare"), i18n_span("explorer.to_compare", lang),
                            icon = shiny::icon("layer-group", `aria-hidden` = "true"),
                            class = "btn-sm btn-outline-primary")
      )
    ),
    shiny::uiOutput(ns("header")),
    bslib::layout_columns(
      col_widths = c(6, 6), fill = FALSE,
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
    ),
    shiny::uiOutput(ns("moments")),
    bslib::layout_columns(
      col_widths = c(7, 5), fill = FALSE,
      bslib::card(
        class = "dl-formula-card", full_screen = TRUE,
        bslib::card_header(shiny::tags$span(
          class = "dl-card-title",
          shiny::icon("square-root-variable", `aria-hidden` = "true"),
          i18n_span("explorer.formulas", lang))),
        bslib::card_body(
          do.call(bslib::navset_underline, c(
            list(id = ns("formula_tab")),
            lapply(names(formula_tabs("discrete")), tab_panel)
          ))
        )
      ),
      shiny::tags$div(
        class = "dl-stack",
        bslib::card(
          bslib::card_header(shiny::tags$span(
            class = "dl-card-title",
            shiny::icon("book-open", `aria-hidden` = "true"),
            i18n_span("explorer.story", lang))),
          bslib::card_body(shiny::tags$p(
            class = "dl-story", shiny::textOutput(ns("story"), inline = TRUE)))
        ),
        bslib::card(
          class = "dl-related-card",
          bslib::card_header(shiny::tags$span(
            class = "dl-card-title",
            shiny::icon("diagram-project", `aria-hidden` = "true"),
            i18n_span("related.title", lang))),
          bslib::card_body(shiny::uiOutput(ns("related")))
        )
      )
    )
  )
}

# Distribution choices grouped by data type, labeled in `lang`.
dist_choices <- function(lang) {
  reg <- dl_registry()
  groups <- intersect(.dl_groups, unique(vapply(reg, `[[`, "", "group")))
  out <- lapply(groups, function(g) {
    ids <- dl_ids(group = g)
    labels <- vapply(ids, function(i) entry_text(reg[[i]], "names", lang), "")
    o <- order(labels, method = "radix")
    stats::setNames(ids[o], labels[o])
  })
  names(out) <- vapply(groups, function(g) tr(paste0("group.", g), lang), "")
  out
}

# `lang` may be a fixed language or a reactive; `dark` a reactive logical.
# `select` is a reactive navigation request from other pages (Home, Compendium):
# list(tab, id); requests for the Explorer open distribution `id`.
# Returns the reactives of the current entry, parameter values and
# distribution, and `route`, a request to open the Calculator or Compare.
mod_explore_server <- function(id, lang, dark = shiny::reactive(FALSE),
                               select = shiny::reactive(NULL)) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    entry <- shiny::reactive(dl_entry(shiny::req(input$dist)))
    sliders <- param_sliders(input, output, session, entry, "params")
    pars <- sliders$pars

    # Choices are relabeled in the new language, keeping the selection.
    shiny::observeEvent(lang_r(), {
      shiny::updateSelectInput(session, "dist", choices = dist_choices(lang_r()),
                               selected = shiny::isolate(input$dist))
    }, ignoreInit = TRUE)

    # Requests from other pages (Home search, chips) and related-card clicks.
    open_dist <- function(did) {
      if (is_string(did) && did %in% dl_ids()) {
        shiny::updateSelectInput(session, "dist", selected = did)
      }
    }
    shiny::observeEvent(select(), {
      r <- select()
      if (is.null(r$tab) || identical(r$tab, "explorer")) open_dist(r$id)
    })
    shiny::observeEvent(input$goto, open_dist(input$goto))

    # Links to the Calculator and Compare carry the open distribution and
    # its parameter values.
    route <- shiny::reactiveVal(NULL)
    count <- 0L
    send_to <- function(tab) {
      p <- pars()
      shiny::req(p, identical(p$id, entry()$id))
      count <<- count + 1L
      route(list(tab = tab, id = p$id, params = p$vals, n = count))
    }
    shiny::observeEvent(input$to_calc, send_to("calculator"))
    shiny::observeEvent(input$to_compare, send_to("compare"))

    # Current distribution, built from the resolved values.
    dist <- shiny::reactive({
      e <- entry()
      p <- pars()
      shiny::req(p, identical(p$id, e$id))
      dist_from_pars(p, lang_r())
    })
    # Slider drags fire many events; recompute only after a short pause.
    dist_d <- shiny::debounce(dist, 150)
    # The debounced value lags a distribution switch; wait until it matches
    # the selected entry so plots and moments never mix two distributions.
    dist_now <- shiny::reactive({
      d <- dist_d()
      shiny::req(identical(d$id, entry()$id))
      d
    })

    # Engine errors (e.g. no finite plotting range) become messages, not crashes.
    curve <- shiny::reactive({
      res <- tryCatch(dl_curve(dist_now()), error = function(err) conditionMessage(err))
      shiny::validate(shiny::need(is.data.frame(res), res))
      res
    })
    is_discrete <- shiny::reactive(identical(entry()$type, "discrete"))

    output$header <- shiny::renderUI(explore_header(entry(), lang_r()))

    output$density_title <- shiny::renderText({
      tr(if (is_discrete()) "explorer.pmf" else "explorer.density", lang_r())
    })

    output$density <- plotly::renderPlotly({
      d <- curve()
      lg <- lang_r()
      th <- plot_theme(dark())
      if (is_discrete()) {
        p <- plotly::plot_ly(d, x = ~x, y = ~density, type = "bar",
                             marker = list(color = th$fill_solid),
                             hovertemplate = "x = %{x}<br>P = %{y:.4f}<extra></extra>")
        ylab <- tr("plot.probability", lg)
      } else {
        p <- plotly::plot_ly(d, x = ~x, y = ~density, type = "scatter",
                             mode = "lines", fill = "tozeroy",
                             fillcolor = th$fill, line = list(color = th$line, width = 2.5),
                             hovertemplate = "x = %{x:.4f}<br>f(x) = %{y:.4f}<extra></extra>")
        ylab <- tr("plot.density", lg)
      }
      p <- mean_marker(p, dist_now(), th, lg)
      plot_layout(p, tr("plot.x", lg), ylab, th, lg, ylim = density_ylim(d))
    })

    output$cdf <- plotly::renderPlotly({
      d <- curve()
      lg <- lang_r()
      th <- plot_theme(dark())
      # Discrete CDFs are right-continuous step functions.
      shape <- if (is_discrete()) "hv" else "linear"
      p <- plotly::plot_ly(d, x = ~x, y = ~cdf, type = "scatter", mode = "lines",
                           line = list(color = th$line2, shape = shape, width = 2.5),
                           hovertemplate = "x = %{x:.4f}<br>F(x) = %{y:.4f}<extra></extra>")
      plot_layout(p, tr("plot.x", lg), tr("plot.cdf", lg), th, lg, ylim = c(0, 1.02))
    })

    output$moments <- shiny::renderUI({
      # Read the reactive outside tryCatch so req()/validate() still propagate.
      d <- dist_now()
      lg <- lang_r()
      # An engine error shows a dash; NA from the engine means "does not exist".
      m <- tryCatch(dl_moments(d), error = function(err) NULL)
      moment_boxes(m, lg)
    })

    for (tab in names(formula_tabs("discrete"))) local({
      tb <- tab
      output[[paste0("formulas_", tb)]] <- shiny::renderUI(
        formula_tab_content(entry(), tb, lang_r())
      )
    })

    output$story <- shiny::renderText(entry_text(entry(), "story", lang_r()))

    output$related <- shiny::renderUI(
      related_list_tag(entry()$id, lang_r(), ns("goto"))
    )

    list(entry = entry, params = pars, dist = dist, dist_now = dist_now,
         route = route)
  })
}

# Title of the open distribution with its type and data-type group.
explore_header <- function(entry, lang) {
  shiny::tags$div(
    class = "dl-dist-header",
    shiny::tags$h1(class = "dl-dist-name", entry_text(entry, "names", lang)),
    shiny::tags$span(class = "badge dl-badge-type", tr(paste0("type.", entry$type), lang)),
    shiny::tags$span(class = paste0("badge dl-badge-group dl-group-", entry$group),
                     tr(paste0("group.", entry$group), lang))
  )
}

# Moment tiles, each with an icon chip in a brand color.
moment_boxes <- function(m, lang) {
  spec <- list(
    mean = c("crosshairs", "indigo"),
    variance = c("arrows-left-right", "teal"),
    sd = c("ruler-horizontal", "amber"),
    skewness = c("scale-unbalanced", "indigo"),
    excess_kurtosis = c("mountain", "teal")
  )
  tiles <- lapply(names(spec), function(k) {
    value <- if (is.null(m)) "\u2014" else format_moment(m[[k]], lang)
    shiny::tags$div(
      class = paste0("dl-stat dl-stat-", spec[[k]][2]),
      shiny::tags$span(class = "dl-icon-chip",
                       shiny::icon(spec[[k]][1], `aria-hidden` = "true")),
      shiny::tags$div(
        shiny::tags$p(class = "dl-stat-title", tr(paste0("moments.", k), lang)),
        shiny::tags$p(class = paste("dl-stat-value",
                                    if (!is.null(m) && is.na(m[[k]])) "dl-stat-text"),
                      value)
      )
    )
  })
  shiny::tags$div(class = "dl-moments", role = "group", tiles)
}

# Plot colors for the light or dark color mode.
plot_theme <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(dark = TRUE, text = "#E6E7F5", muted = "#A3A6CC", grid = "rgba(230, 231, 245, 0.10)",
         zero = "rgba(230, 231, 245, 0.28)", line = "#3CC9C5",
         fill = "rgba(60, 201, 197, 0.22)", fill_solid = "#3CC9C5",
         line2 = "#A9ACF2", accent = "#F2A33A", hover_bg = "#1D1F40")
  } else {
    list(dark = FALSE, text = "#1B1C3A", muted = "#5B5E8A", grid = "rgba(27, 28, 58, 0.08)",
         zero = "rgba(27, 28, 58, 0.25)", line = "#0E7C7A",
         fill = "rgba(20, 163, 160, 0.20)", fill_solid = "#14A3A0",
         line2 = "#2B2D6E", accent = "#C77A12", hover_bg = "#FFFFFF")
  }
}

# Dashed vertical line at the mean, when it exists and is finite.
mean_marker <- function(p, d, th, lang) {
  mu <- tryCatch(dl_moments(d)[["mean"]], error = function(err) NA_real_)
  if (!is.finite(mu)) return(p)
  plotly::layout(p, shapes = list(list(
    type = "line", xref = "x", yref = "paper", x0 = mu, x1 = mu, y0 = 0, y1 = 1,
    line = list(color = th$accent, width = 1.5, dash = "dash")
  )), annotations = list(list(
    x = mu, y = 1, xref = "x", yref = "paper", yanchor = "bottom", xanchor = "left",
    text = tr("plot.mean", lang), showarrow = FALSE,
    font = list(color = th$accent, size = 11)
  )))
}

# Y-axis limits that keep an unbounded density spike (e.g. Gamma with shape < 1)
# from flattening the plot; NULL when no cap is needed.
density_ylim <- function(d) {
  body <- d$density[d$cdf >= 0.01 & d$cdf <= 0.99]
  if (!length(body)) return(NULL)
  cap <- 1.1 * max(body)
  if (max(d$density) > 2 * cap) c(0, cap) else NULL
}

# Shared plotly layout: transparent background, brand fonts, axis colors for
# the color mode, decimal mark of the language, no legend.
plot_layout <- function(p, xlab, ylab, th = plot_theme(), lang = "en-US", ylim = NULL) {
  axis <- function(title) {
    list(title = list(text = title, font = list(color = th$muted)),
         color = th$muted, gridcolor = th$grid, zerolinecolor = th$zero,
         linecolor = th$zero, tickfont = list(color = th$muted))
  }
  yaxis <- axis(ylab)
  if (!is.null(ylim)) yaxis$range <- ylim
  plotly::config(
    plotly::layout(
      p, xaxis = axis(xlab), yaxis = yaxis, showlegend = FALSE,
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Inter, system-ui, sans-serif", color = th$text, size = 12),
      separators = if (match_lang(lang) == "pt-BR") ",." else ".,",
      hoverlabel = list(bgcolor = th$hover_bg, font = list(color = th$text)),
      margin = list(l = 56, r = 12, t = 24, b = 44),
      modebar = list(bgcolor = "rgba(0,0,0,0)", color = th$muted, activecolor = th$line)
    ),
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "pan2d",
                               "zoomIn2d", "zoomOut2d", "hoverClosestCartesian",
                               "hoverCompareCartesian", "toggleSpikelines")
  )
}
