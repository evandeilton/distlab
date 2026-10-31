# Explorer: one distribution at a time, with parameter sliders, density and
# CDF plots, moments and the distribution story.

mod_explore_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  ids <- dl_ids()
  choices <- stats::setNames(
    ids, vapply(ids, function(i) entry_text(dl_entry(i), "names", lang), "")
  )
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::selectInput(ns("dist"), tr("explorer.distribution", lang),
                         choices = choices, selected = "normal"),
      shiny::h6(tr("explorer.parameters", lang)),
      shiny::uiOutput(ns("params"))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header(shiny::textOutput(ns("density_title"))),
                  plotly::plotlyOutput(ns("density"), height = "320px")),
      bslib::card(bslib::card_header(tr("explorer.cdf", lang)),
                  plotly::plotlyOutput(ns("cdf"), height = "320px"))
    ),
    shiny::uiOutput(ns("moments")),
    bslib::card(bslib::card_header(tr("explorer.story", lang)),
                bslib::card_body(shiny::textOutput(ns("story"))))
  )
}

mod_explore_server <- function(id, lang) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    entry <- shiny::reactive(dl_entry(shiny::req(input$dist)))

    # One slider per parameter, rebuilt when the distribution changes. Ids carry
    # the distribution id so stale values from another distribution are never read.
    output$params <- shiny::renderUI({
      e <- entry()
      lapply(e$parameters, function(par) {
        # Integer parameters have whole min, max and step, so their sliders
        # only produce whole numbers.
        shiny::sliderInput(
          ns(param_input_id(e$id, par$name)),
          label = shiny::code(par$name),
          min = par$slider$min, max = par$slider$max,
          value = par$default, step = par$slider$step
        )
      })
    })

    # Sliders must render even while the sidebar is collapsed (the default on
    # narrow screens); otherwise no parameter ever reaches the server.
    shiny::outputOptions(output, "params", suspendWhenHidden = FALSE)

    # Current distribution; waits until every slider of this entry exists.
    dist <- shiny::reactive({
      e <- entry()
      par_names <- vapply(e$parameters, `[[`, "", "name")
      vals <- lapply(param_input_id(e$id, par_names), function(k) input[[k]])
      shiny::req(all(!vapply(vals, is.null, logical(1))))
      names(vals) <- par_names
      res <- tryCatch(do.call(dl_dist, c(list(e$id), vals)),
                      error = function(err) conditionMessage(err))
      if (!inherits(res, "dl_dist")) {
        shiny::validate(tr("error.invalid_params", lang, message = res))
      }
      res
    })
    # Slider drags fire many events; recompute only after a short pause.
    dist_d <- shiny::debounce(dist, 150)

    # Engine errors (e.g. no finite plotting range) become messages, not crashes.
    curve <- shiny::reactive({
      res <- tryCatch(dl_curve(dist_d()), error = function(err) conditionMessage(err))
      shiny::validate(shiny::need(is.data.frame(res), res))
      res
    })
    is_discrete <- shiny::reactive(identical(entry()$type, "discrete"))

    output$density_title <- shiny::renderText({
      tr(if (is_discrete()) "explorer.pmf" else "explorer.density", lang)
    })

    output$density <- plotly::renderPlotly({
      d <- curve()
      if (is_discrete()) {
        p <- plotly::plot_ly(d, x = ~x, y = ~density, type = "bar",
                             marker = list(color = "#1f6f8b"))
        ylab <- tr("plot.probability", lang)
      } else {
        p <- plotly::plot_ly(d, x = ~x, y = ~density, type = "scatter",
                             mode = "lines", fill = "tozeroy",
                             line = list(color = "#1f6f8b"))
        ylab <- tr("plot.density", lang)
      }
      plot_layout(p, tr("plot.x", lang), ylab, ylim = density_ylim(d))
    })

    output$cdf <- plotly::renderPlotly({
      d <- curve()
      # Discrete CDFs are right-continuous step functions.
      shape <- if (is_discrete()) "hv" else "linear"
      p <- plotly::plot_ly(d, x = ~x, y = ~cdf, type = "scatter", mode = "lines",
                           line = list(color = "#1f6f8b", shape = shape))
      plot_layout(p, tr("plot.x", lang), tr("plot.cdf", lang))
    })

    output$moments <- shiny::renderUI({
      keys <- c("mean", "variance", "sd", "skewness", "excess_kurtosis")
      # Read the reactive outside tryCatch so req()/validate() still propagate.
      d <- dist_d()
      # An engine error shows a dash; NA from the engine means "does not exist".
      m <- tryCatch(dl_moments(d), error = function(err) NULL)
      boxes <- lapply(keys, function(k) {
        value <- if (is.null(m)) "\u2014" else format_moment(m[[k]], lang)
        bslib::value_box(title = tr(paste0("moments.", k), lang), value = value)
      })
      do.call(bslib::layout_column_wrap,
              c(list(width = "170px", fill = FALSE), boxes))
    })

    output$story <- shiny::renderText(entry_text(entry(), "story", lang))
  })
}

# Input id of a parameter slider, unique per distribution.
param_input_id <- function(dist_id, par_name) paste0("p_", dist_id, "_", par_name)

# Y-axis limits that keep an unbounded density spike (e.g. Gamma with shape < 1)
# from flattening the plot; NULL when no cap is needed.
density_ylim <- function(d) {
  body <- d$density[d$cdf >= 0.01 & d$cdf <= 0.99]
  if (!length(body)) return(NULL)
  cap <- 1.1 * max(body)
  if (max(d$density) > 2 * cap) c(0, cap) else NULL
}

# Shared plotly layout: axis titles, no legend, compact margins.
plot_layout <- function(p, xlab, ylab, ylim = NULL) {
  yaxis <- list(title = ylab)
  if (!is.null(ylim)) yaxis$range <- ylim
  plotly::config(
    plotly::layout(p, xaxis = list(title = xlab), yaxis = yaxis,
                   showlegend = FALSE, margin = list(l = 50, r = 10, t = 10, b = 40)),
    displaylogo = FALSE
  )
}
