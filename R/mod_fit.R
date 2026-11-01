# Fit lab: input and estimation are explicit actions. Changing the interface
# language, colour mode or diagnostic view never repeats an optimization.

mod_fit_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  choices <- function(keys, values = keys, prefix = "fit.") {
    stats::setNames(values, vapply(keys, function(k) tr(paste0(prefix, k), lang), ""))
  }
  panel <- function(key, ...) bslib::card(
    bslib::card_header(i18n_span(paste0("fit.", key), lang)), bslib::card_body(...)
  )
  plot_card <- function(key, output) bslib::card(
    class = "dl-plot-card", bslib::card_header(i18n_span(paste0("fit.", key), lang)),
    plotly::plotlyOutput(ns(output), height = "290px")
  )
  tab <- function(value, ...) bslib::nav_panel(
    i18n_span(paste0("fit.tab_", value), lang), value = value, ...)
  upload <- shiny::fileInput(ns("file"), i18n_span("fit.upload_label", lang),
    accept = c(".csv", ".tsv", ".txt"), buttonLabel = i18n_span("fit.browse", lang),
    placeholder = tr("fit.no_file", lang))
  upload <- htmltools::tagQuery(upload)$find("input.form-control")$
    addAttrs(`data-i18n-placeholder` = "fit.no_file")$allTags()
  bslib::layout_sidebar(
    class = "dl-explorer dl-fit",
    sidebar = bslib::sidebar(
      width = 320,
      shiny::tags$h2(class = "dl-sidebar-heading", i18n_span("fit.input", lang)),
      shiny::selectInput(ns("source"), i18n_span("fit.source", lang),
        choices = choices(c("paste", "upload", "example", "simulate")), selected = "example"),
      shiny::conditionalPanel("input.source == 'paste' || input.source == 'upload'", ns = ns,
        shiny::selectInput(ns("kind"), i18n_span("fit.kind", lang),
          choices = choices(c("raw", "frequency", "grouped")), selected = "raw"),
        shiny::selectInput(ns("decimal"), i18n_span("fit.decimal", lang),
          choices = choices(c("auto", "point", "comma"), c("auto", ".", ",")), selected = "auto")),
      shiny::conditionalPanel("input.source == 'paste'", ns = ns,
        shiny::textAreaInput(ns("text"), i18n_span("fit.paste_label", lang), rows = 7,
          value = "value\n1.2\n2.4\n0.8\n3.1\n1.7\n2.2")),
      shiny::conditionalPanel("input.source == 'upload'", ns = ns,
        upload),
      shiny::conditionalPanel("input.source == 'paste' || input.source == 'upload'", ns = ns,
        shiny::uiOutput(ns("columns")),
        shiny::tags$p(class = "dl-muted dl-field-help", i18n_span("fit.input_help", lang))),
      shiny::conditionalPanel("input.source == 'example'", ns = ns,
        shiny::selectInput(ns("example"), i18n_span("fit.example", lang),
          choices = choices(c("example_faithful", "example_iris", "example_counts"),
                            c("faithful", "iris", "counts")), selected = "faithful")),
      shiny::conditionalPanel("input.source == 'simulate'", ns = ns,
        shiny::selectInput(ns("sim_dist"), i18n_span("explorer.distribution", lang),
          choices = dist_choices(lang), selected = "normal"),
        shiny::uiOutput(ns("sim_params")),
        shiny::numericInput(ns("sim_n"), i18n_span("fit.sample_size", lang),
                            value = 300, min = 5, max = 100000, step = 1),
        shiny::numericInput(ns("seed"), i18n_span("sim.seed", lang),
                            value = 2026, min = 1, max = 2147483647, step = 1)),
      shiny::actionButton(ns("load"), i18n_span("fit.load", lang), icon = shiny::icon("arrow-right"),
                          class = "btn-outline-primary dl-fit-action"),
      shiny::tags$hr(),
      shiny::tags$h2(class = "dl-sidebar-heading", i18n_span("fit.candidates", lang)),
      shiny::uiOutput(ns("candidate_choices")),
      shiny::uiOutput(ns("fixed")),
      shiny::selectInput(ns("level"), i18n_span("fit.confidence", lang),
                         choices = c("90%" = .9, "95%" = .95, "99%" = .99), selected = "0.95"),
      shiny::actionButton(ns("run"), i18n_span("fit.run", lang), icon = shiny::icon("chart-line"),
                          class = "btn-primary dl-fit-action"),
      shiny::tags$p(class = "dl-muted dl-field-help", i18n_span("fit.explicit_help", lang))
    ),
    shiny::tags$div(class = "dl-comp-head",
      shiny::tags$h1(class = "dl-page-title", i18n_span("nav.fit", lang)),
      shiny::tags$p(class = "dl-muted dl-comp-sub", i18n_span("fit.subtitle", lang))),
    shiny::uiOutput(ns("status")),
    bslib::navset_underline(id = ns("tab"),
      tab("data",
        panel("descriptives", shiny::uiOutput(ns("descriptives"))),
        bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
          plot_card("histogram", "histogram"), plot_card("ecdf", "ecdf")),
        shiny::uiOutput(ns("preview_note")),
        panel("eligibility", shiny::tags$details(
          shiny::tags$summary(i18n_span("fit.show_candidates", lang)),
          shiny::uiOutput(ns("eligibility"))))),
      tab("results",
        panel("ranking", shiny::tags$p(class = "dl-muted", i18n_span("fit.ranking_help", lang)),
          shiny::uiOutput(ns("ranking")),
          shiny::downloadButton(ns("download_ranking"), i18n_span("fit.download_ranking", lang),
                                 class = "btn-sm btn-outline-primary")),
        panel("estimates",
          shiny::selectInput(ns("diagnostic_id"), i18n_span("fit.selected", lang), choices = character()),
          shiny::uiOutput(ns("estimates")), shiny::uiOutput(ns("fit_notes")),
          shiny::actionButton(ns("open_explorer"), i18n_span("fit.open_explorer", lang),
                               class = "btn-sm btn-outline-primary"),
          shiny::downloadButton(ns("download_estimates"), i18n_span("fit.download_estimates", lang),
                                 class = "btn-sm btn-outline-primary"))),
      tab("diagnostics",
        bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
          plot_card("density", "density"), plot_card("cdf", "cdf"),
          plot_card("qq", "qq"), plot_card("pp", "pp")),
        panel("gof", shiny::tags$p(class = "dl-muted", i18n_span("fit.gof_help", lang)),
          shiny::uiOutput(ns("gof")), shiny::uiOutput(ns("diagnostic_notes")),
          shiny::downloadButton(ns("download_gof"), i18n_span("fit.download_gof", lang),
                                 class = "btn-sm btn-outline-primary"))),
      tab("export",
        panel("reproduce", shiny::uiOutput(ns("code")),
          shiny::downloadButton(ns("download_code"), i18n_span("fit.download_code", lang),
                                 class = "btn-sm btn-outline-primary"),
          shiny::downloadButton(ns("download_data"), i18n_span("fit.download_data", lang),
                                 class = "btn-sm btn-outline-primary"))))
  )
}

# Accessible responsive tables shared by Fit cards; numeric data retain their
# full precision in downloads, independently of the on-screen number format.
fit_table_tag <- function(data, labels, lang, caption) {
  if (!nrow(data)) return(shiny::tags$p(class = "dl-muted", tr("fit.no_results", lang)))
  rows <- lapply(seq_len(nrow(data)), function(i) shiny::tags$tr(
    lapply(seq_along(data), function(j) {
      val <- data[[j]][i]
      numeric <- is.numeric(data[[j]])
      shiny::tags$td(class = if (numeric) "dl-cell-num",
        if (numeric) format_num(val, lang, digits = 5) else as.character(val))
    })
  ))
  shiny::tags$div(class = "table-responsive", shiny::tags$table(
    class = "table dl-table dl-compare-table dl-fit-table",
    shiny::tags$caption(class = "visually-hidden", caption),
    shiny::tags$thead(shiny::tags$tr(lapply(labels, function(s) shiny::tags$th(scope = "col", s)))),
    shiny::tags$tbody(rows)))
}

fit_data_table <- function(data) {
  if (data$type == "grouped") {
    data.frame(lower = data$lower, upper = data$upper, frequency = data$counts)
  } else data.frame(value = data$x, frequency = data$counts)
}

fit_error_tag <- function(key, lang, detail = NULL) shiny::tags$p(
  class = "dl-fit-notice", role = "status", tr(key, lang),
  if (!is.null(detail)) shiny::tags$span(class = "dl-muted", paste0(" ", detail)))

# Engine errors remain intact in its return object and CSV exports. The app
# maps their stable meanings to dictionary keys so technical English does not
# leak into a Portuguese session, including after a live language change.
fit_message_text <- function(message, lang) {
  if (!is_string(message) || !nzchar(message)) return("")
  own <- paste0("fit.", c("error_n", "error_empty", "error_column", "error_numeric",
                          "error_candidates", "error_fixed"))
  own <- c(own, "sim.error.seed")
  for (key in own) {
    if (message %in% c(tr(key, "en-US"), tr(key, "pt-BR"))) return(tr(key, lang))
  }
  patterns <- c(
    error_frequency = "Frequencies must|frequency must|frequency.*positive|At least one frequency",
    error_intervals = "Grouped intervals|Grouped data require",
    error_missing = "Missing data|No complete observations",
    error_finite = "Observations must be finite",
    error_numeric = "nonempty numeric vectors",
    error_empty = "Paste a nonempty|No data rows",
    error_size = "exceeds 10 MB",
    error_upload = "Upload path",
    error_confidence = "`level` must",
    error_seed = "`seed` must",
    failure_fixed_domain = "Fixed parameter|Parameter '.*outside its domain|Parameters of .*must satisfy",
    failure_fixed_required = "integer designs or grouped endpoints",
    failure_constant = "Constant data",
    failure_cauchy_ties = "Cauchy likelihood is unbounded",
    failure_negbin_zero = "All-zero data do not identify",
    failure_negbin_limit = "No finite Negative-binomial size MLE",
    failure_poisson_zero = "Poisson likelihood supremum",
    failure_exponential_zero = "All-zero data have no finite Exponential",
    failure_support = "strictly inside this family's support|Discrete distributions require integer",
    failure_likelihood = "No finite likelihood",
    failure_boundary = "No stable finite interior MLE",
    failure_grouped_information = "Grouped data do not identify a stable finite maximum",
    warning_student = "Student t uses",
    warning_endpoints = "Support endpoints are nonregular",
    warning_boundary = "Probability estimate is on the boundary",
    warning_laplace = "Laplace location has a nonsmooth",
    warning_information = "Observed information is singular",
    warning_optimizer = "Optimizer:",
    failure_settings = "Unknown fixed parameter|must be a named list|`ids` must|`fixed` contains"
  )
  for (key in names(patterns)) if (grepl(patterns[[key]], message)) return(tr(paste0("fit.", key), lang))
  tr("fit.failure_generic", lang)
}

mod_fit_server <- function(id, lang, dark = shiny::reactive(FALSE),
                           select = shiny::reactive(NULL)) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- shiny::reactiveVal(NULL)
    result <- shiny::reactiveVal(NULL)
    data_error <- shiny::reactiveVal(NULL)
    fit_error <- shiny::reactiveVal(NULL)
    route <- shiny::reactiveVal(NULL)
    serial <- shiny::reactiveVal(0L)
    fitted_serial <- shiny::reactiveVal(-1L)
    count <- 0L
    sim_entry <- shiny::reactive(dl_entry(input$sim_dist %||% "normal"))
    sliders <- param_sliders(input, output, session, sim_entry, "sim_params")

    # Parse only a table preview here. The explicit Load action freezes its
    # numeric columns, so typing or choosing another file cannot mutate a fit.
    table_in <- shiny::reactive({
      source <- input$source %||% "example"
      if (!source %in% c("paste", "upload")) return(NULL)
      tryCatch({
        if (source == "upload") {
          shiny::req(input$file$datapath)
          fit_read_table(path = input$file$datapath, decimal_mark = input$decimal %||% "auto")
        } else fit_read_table(text = input$text %||% "", decimal_mark = input$decimal %||% "auto")
      }, error = function(e) e)
    })
    output$columns <- shiny::renderUI({
      tb <- table_in()
      if (is.null(tb) || inherits(tb, "error")) return(NULL)
      choices <- names(tb)
      kind <- input$kind %||% "raw"
      field <- function(id, key, at) shiny::selectInput(ns(id), tr(paste0("fit.", key), lang_r()),
        choices = choices, selected = shiny::isolate(input[[id]]) %||% choices[min(at, length(choices))])
      if (kind == "grouped") shiny::tagList(field("lower_col", "lower", 1),
        field("upper_col", "upper", 2), field("freq_col", "frequency_column", 3)) else
        shiny::tagList(field("value_col", "value_column", 1),
          if (kind == "frequency") field("freq_col", "frequency_column", 2))
    })
    shiny::observeEvent(input$load, {
      data_error(NULL)
      got <- tryCatch({
        source <- input$source %||% "example"
        if (source == "example") {
          x <- switch(input$example %||% "faithful", faithful = datasets::faithful$waiting,
                      iris = datasets::iris$Sepal.Length,
                      counts = c(0, 1, 2, 3, 4, 5, 6, 7))
          if (identical(input$example, "counts")) {
            dl_fit_data(x, frequency = c(15, 32, 27, 18, 9, 5, 2, 1))
          } else dl_fit_data(x)
        } else if (source == "simulate") {
          n <- input$sim_n
          if (length(n) != 1L || !is.finite(n) || n < 5 || n > 100000 || n != round(n)) {
            stop(tr("fit.error_n", shiny::isolate(lang_r())), call. = FALSE)
          }
          seed <- valid_seed(input$seed)
          if (is.null(seed)) stop(tr("sim.error.seed", shiny::isolate(lang_r())), call. = FALSE)
          p <- sliders$pars()
          shiny::req(p, identical(p$id, sim_entry()$id))
          dist <- do.call(dl_dist, c(list(p$id), p$vals))
          dl_fit_data(with_seed(seed, dl_sample(dist, n)))
        } else {
          tb <- table_in()
          if (inherits(tb, "error")) stop(tb)
          if (is.null(tb) || !nrow(tb)) stop(tr("fit.error_empty", shiny::isolate(lang_r())))
          col <- function(id, at) {
            nm <- input[[id]] %||% names(tb)[min(at, ncol(tb))]
            if (!nm %in% names(tb)) stop(tr("fit.error_column", shiny::isolate(lang_r())))
            v <- tb[[nm]]
            if (!is.numeric(v)) stop(tr("fit.error_numeric", shiny::isolate(lang_r())))
            v
          }
          kind <- input$kind %||% "raw"
          if (kind == "grouped") dl_fit_data(lower = col("lower_col", 1),
            upper = col("upper_col", 2), frequency = col("freq_col", 3)) else
            dl_fit_data(col("value_col", 1),
              frequency = if (kind == "frequency") col("freq_col", 2) else NULL)
        }
      }, error = function(e) e)
      if (inherits(got, "error")) data_error(conditionMessage(got)) else {
        data(got)
        serial(serial() + 1L)
        fit_error(NULL)
        bslib::nav_select("tab", "data", session = session)
      }
    }, ignoreInit = TRUE)

    candidates <- shiny::reactive({ shiny::req(data()); dl_fit_candidates(data()) })
    selected_candidates <- shiny::reactive({
      cand <- candidates()
      intersect(input$candidates %||% character(), cand$id[cand$eligible])
    })
    output$candidate_choices <- shiny::renderUI({
      if (is.null(data())) return(shiny::tags$p(class = "dl-muted", tr("fit.load_first", lang_r())))
      cand <- candidates()
      ids <- cand$id[cand$eligible]
      labels <- vapply(ids, function(k) entry_text(dl_entry(k), "names", lang_r()), "")
      previous <- shiny::isolate(input$candidates)
      initial <- intersect(c("normal", "lognormal", "gamma", "weibull", "poisson", "negbinomial"), ids)
      shiny::checkboxGroupInput(ns("candidates"), label = NULL,
        choices = stats::setNames(ids, labels), selected = intersect(previous %||% initial, ids))
    })
    output$fixed <- shiny::renderUI({
      ids <- selected_candidates()
      d <- data()
      needs <- list(binomial = "size", betabinomial = "size", hypergeometric = c("m", "n", "k"))
      if (d$type == "grouped") needs <- c(needs, list(uniform = c("min", "max"),
                                                       discrete_uniform = c("min", "max")))
      selected <- intersect(ids, names(needs))
      if (!length(selected)) return(NULL)
      shiny::tagList(shiny::tags$p(class = "dl-muted dl-field-help", tr("fit.fixed_help", lang_r())),
        lapply(selected, function(id) shiny::tags$div(class = "dl-fit-fixed",
          shiny::tags$strong(entry_text(dl_entry(id), "names", lang_r())),
          lapply(needs[[id]], function(nm) {
            key <- paste0("fixed_", id, "_", nm)
            default <- default_params(dl_entry(id))[[nm]]
            if (nm == "size" && any(is.finite(d$x))) default <- max(default, ceiling(max(d$x[is.finite(d$x)])))
            shiny::numericInput(ns(key), label = nm, value = shiny::isolate(input[[key]]) %||% default,
                                step = if (id == "uniform") .1 else 1)
          }))))
    })
    shiny::observeEvent(input$run, {
      fit_error(NULL)
      got <- tryCatch({
        shiny::req(data())
        ids <- selected_candidates()
        if (!length(ids)) stop(tr("fit.error_candidates", shiny::isolate(lang_r())), call. = FALSE)
        fixed <- lapply(ids, function(id) {
          keys <- switch(id, binomial = "size", betabinomial = "size",
                         hypergeometric = c("m", "n", "k"),
                         if (data()$type == "grouped" && id %in% c("uniform", "discrete_uniform")) c("min", "max"))
          vals <- lapply(keys, function(nm) input[[paste0("fixed_", id, "_", nm)]])
          if (length(keys) && any(vapply(vals, is.null, logical(1)))) {
            stop(tr("fit.error_fixed", shiny::isolate(lang_r())), call. = FALSE)
          }
          stats::setNames(vals, keys)
        })
        names(fixed) <- ids
        level <- suppressWarnings(as.numeric(input$level %||% ".95"))
        shiny::withProgress(message = tr("fit.fitting", shiny::isolate(lang_r())), value = .25,
          dl_fit(data(), ids = ids, fixed = fixed, level = level))
      }, error = function(e) e)
      if (inherits(got, "error")) fit_error(conditionMessage(got)) else {
        result(got)
        fitted_serial(serial())
        bslib::nav_select("tab", "results", session = session)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(list(result(), lang_r()), {
      r <- result()
      if (is.null(r)) return()
      ids <- intersect(r$ranking$id, names(r$fits)) %||% character()
      preferred <- shiny::isolate(input$diagnostic_id)
      if (!length(ids)) preferred <- character() else
        if (!length(preferred) || !preferred %in% ids) preferred <- ids[1]
      shiny::updateSelectInput(session, "diagnostic_id",
        choices = stats::setNames(ids, vapply(ids, function(k) entry_text(dl_entry(k), "names", lang_r()), "")),
        selected = preferred)
    })
    selected_fit <- shiny::reactive({
      r <- result()
      shiny::req(r, length(r$fits))
      id <- input$diagnostic_id %||% names(r$fits)[1]
      shiny::req(id %in% names(r$fits))
      r$fits[[id]]
    })
    diagnostics <- shiny::reactive({
      r <- result()
      shiny::req(r, length(r$fits))
      dl_fit_diagnostics(r, id = selected_fit()$id)
    })

    output$status <- shiny::renderUI({
      lg <- lang_r()
      shiny::tagList(
        if (!is.null(data_error())) fit_error_tag("fit.error_input", lg, fit_message_text(data_error(), lg)),
        if (!is.null(fit_error())) fit_error_tag("fit.error_fit", lg, fit_message_text(fit_error(), lg)),
        if (is.null(data())) fit_error_tag("fit.welcome", lg),
        if (!is.null(result()) && serial() != fitted_serial()) fit_error_tag("fit.stale", lg))
    })
    output$descriptives <- shiny::renderUI({
      shiny::req(data())
      s <- dl_fit_describe(data())
      keys <- c("n", "removed", "unique", "ties", "zeros", "negatives", "integer", "mean", "sd",
                "variance", "min", "q1", "median", "q3", "max", "skewness", "kurtosis")
      keys <- intersect(keys, names(s))
      df <- data.frame(measure = vapply(keys, function(k) tr(paste0("fit.desc_", k), lang_r()), ""),
        value = vapply(keys, function(k) if (is.logical(s[[k]]) && !is.na(s[[k]])) {
          tr(if (s[[k]]) "fit.yes" else "fit.no", lang_r())
        } else format_num(as.numeric(s[[k]]), lang_r()), ""))
      shiny::tagList(
        if (isTRUE(s$approximate)) shiny::tags$p(class = "dl-muted", tr("fit.grouped_descriptives", lang_r())),
        shiny::tags$div(class = "dl-moments dl-fit-summary", lapply(
          c("n", "mean", "sd", "min", "median", "max"), function(k) {
            shiny::tags$div(class = "dl-stat dl-stat-teal", shiny::tags$div(
              shiny::tags$p(class = "dl-stat-title", tr(paste0("fit.desc_", k), lang_r())),
              shiny::tags$p(class = "dl-stat-value", format_num(s[[k]], lang_r()))))
          })),
        shiny::tags$details(shiny::tags$summary(tr("fit.show_descriptives", lang_r())),
          fit_table_tag(df, c(tr("fit.measure", lang_r()), tr("fit.value", lang_r())),
                        lang_r(), tr("fit.descriptives", lang_r()))))
    })
    output$eligibility <- shiny::renderUI({
      cand <- candidates()
      reason <- vapply(seq_len(nrow(cand)), function(i) {
        key <- cand$reason_key[i]
        translated <- tr(key, lang_r())
        if (!nzchar(key)) tr("fit.reason_ok", lang_r()) else if (identical(translated, key)) cand$reason[i] else translated
      }, "")
      df <- data.frame(distribution = vapply(cand$id, function(k) entry_text(dl_entry(k), "names", lang_r()), ""),
        eligible = vapply(cand$eligible, function(ok) tr(if (ok) "fit.yes" else "fit.no", lang_r()), ""), reason = reason)
      fit_table_tag(df, vapply(c("distribution", "eligible", "reason"), function(k) tr(paste0("fit.", k), lang_r()), ""),
                    lang_r(), tr("fit.eligibility", lang_r()))
    })
    output$ranking <- shiny::renderUI(fit_ranking_tag(result(), lang_r()))
    output$estimates <- shiny::renderUI({
      f <- selected_fit()
      df <- f$estimates
      df$fixed <- vapply(df$fixed, function(v) tr(if (v) "fit.yes" else "fit.no", lang_r()), "")
      fit_table_tag(df, vapply(names(df), function(k) tr(paste0("fit.col_", k), lang_r()), ""),
                    lang_r(), tr("fit.estimates", lang_r()))
    })
    output$fit_notes <- shiny::renderUI({
      f <- selected_fit()
      shiny::tagList(
        if (isTRUE(f$boundary)) fit_error_tag("fit.boundary", lang_r()),
        if (!identical(as.integer(f$convergence), 0L)) fit_error_tag("fit.nonconvergence", lang_r()),
        if (any(!is.finite(f$estimates$se) & !f$estimates$fixed)) fit_error_tag("fit.no_se", lang_r()),
        if (length(f$warnings)) shiny::tags$details(
          shiny::tags$summary(tr("fit.technical_notes", lang_r())),
          shiny::tags$ul(lapply(f$warnings, function(s) shiny::tags$li(fit_message_text(s, lang_r()))))))
    })
    output$code <- shiny::renderUI({ shiny::req(result()); code_tag(result()$code, ns("fit-code"), lang_r()) })
    shiny::observeEvent(input$open_explorer, {
      f <- selected_fit()
      count <<- count + 1L
      route(list(tab = "explorer", id = f$id, params = f$dist$params, nonce = count))
    })
    shiny::observeEvent(select(), {
      request <- select()
      if (!identical(request$tab, "fit") || !is_string(request$id) || !request$id %in% dl_ids()) return()
      shiny::updateSelectInput(session, "source", selected = "simulate")
      shiny::updateSelectInput(session, "sim_dist", selected = request$id)
      if (length(request$params)) sliders$set(request$id, request$params)
    })

    shiny::observeEvent(lang_r(), {
      lg <- lang_r()
      update <- function(id, keys, values = keys) shiny::updateSelectInput(session, id,
        choices = stats::setNames(values, vapply(keys, function(k) tr(paste0("fit.", k), lg), "")),
        selected = shiny::isolate(input[[id]]))
      update("source", c("paste", "upload", "example", "simulate"))
      update("kind", c("raw", "frequency", "grouped"))
      update("decimal", c("auto", "point", "comma"), c("auto", ".", ","))
      update("example", c("example_faithful", "example_iris", "example_counts"), c("faithful", "iris", "counts"))
      shiny::updateSelectInput(session, "sim_dist", choices = dist_choices(lg),
                               selected = shiny::isolate(input$sim_dist))
    }, ignoreInit = TRUE)

    # Plots and exports are registered below to keep data and fitting code
    # independent of rendering. Each view reads the already-fitted object.
    output$histogram <- plotly::renderPlotly(fit_preview_plot(data(), "histogram", lang_r(), plot_theme(dark())))
    output$ecdf <- plotly::renderPlotly(fit_preview_plot(data(), "ecdf", lang_r(), plot_theme(dark())))
    output$preview_note <- shiny::renderUI({
      d <- data()
      shiny::req(d)
      keys <- if (d$type == "grouped") {
        if (any(!is.finite(d$x))) "fit.note_open_bins"
      } else {
        rg <- fit_preview_range(d)
        if (any(d$x < rg[1] | d$x > rg[2])) "fit.note_clipped"
      }
      shiny::tagList(lapply(keys, function(k) shiny::tags$p(class = "dl-muted", tr(k, lang_r()))))
    })
    for (key in c("density", "cdf", "qq", "pp")) local({
      view <- key
      output[[view]] <- plotly::renderPlotly(fit_diagnostic_plot(diagnostics(), view, lang_r(), plot_theme(dark())))
    })
    output$gof <- shiny::renderUI({
      d <- diagnostics()
      df <- d$gof
      if ("label" %in% names(df)) df$label <- tr(
        if (selected_fit()$k > 0) "fit.gof_descriptive" else "fit.gof_fixed", lang_r())
      if ("test" %in% names(df)) df$test[df$test == "Chi-square"] <- tr("fit.chi_square", lang_r())
      fit_table_tag(df, vapply(names(df), function(k) tr(paste0("fit.gof_", k), lang_r()), ""),
                    lang_r(), tr("fit.gof", lang_r()))
    })
    output$diagnostic_notes <- shiny::renderUI({
      d <- diagnostics()
      shiny::tagList(lapply(unique(d$notes), function(k) {
          shiny::tags$p(class = "dl-muted", tr(k, lang_r()))
        }))
    })
    csv_download <- function(name, fn) shiny::downloadHandler(
      filename = function() paste0("distlab-", name, ".csv"),
      content = function(file) utils::write.csv(fn(), file, row.names = FALSE, na = ""))
    output$download_ranking <- csv_download("ranking", function() { shiny::req(result()); result()$ranking })
    output$download_estimates <- csv_download("estimates", function() selected_fit()$estimates)
    output$download_gof <- csv_download("gof", function() diagnostics()$gof)
    output$download_data <- csv_download("data", function() {
      d <- if (!is.null(result())) result()$data else data()
      shiny::req(d)
      fit_data_table(d)
    })
    output$download_code <- shiny::downloadHandler(filename = function() "distlab-fit.R",
      content = function(file) { shiny::req(result()); writeLines(result()$code, file, useBytes = TRUE) })
    list(data = shiny::reactive(data()), fit = shiny::reactive(result()), route = shiny::reactive(route()))
  })
}

# Weighted views do not expand a frequency table into n observations.
fit_preview_range <- function(data) {
  rg <- fit_weighted_quantile(data$x, data$counts, c(.005, .995))
  if (!all(is.finite(rg))) rg <- range(data$x)
  if (rg[1] == rg[2]) {
    radius <- max(1, abs(rg[1]) * .01)
    rg <- rg + c(-radius, radius)
  }
  rg
}

fit_preview_plot <- function(data, view, lang, th) {
  shiny::req(data)
  p <- plotly::plot_ly()
  if (view == "ecdf") {
    x <- if (data$type == "grouped") data$upper else data$x
    o <- order(x)
    y <- cumsum(data$counts[o]) / data$n
    p <- plotly::add_trace(p, x = x[o], y = y, type = "scatter", mode = "lines",
                           line = list(color = th$line, shape = "hv", width = 2))
    return(plot_layout(p, "x", tr("fit.empirical_cdf", lang), th, lang))
  }
  if (data$type == "grouped") {
    ok <- is.finite(data$lower) & is.finite(data$upper)
    left <- data$lower[ok]
    right <- data$upper[ok]
    mid <- (left + right) / 2
    height <- data$counts[ok] / data$n / (right - left)
    width <- right - left
  } else if (all(data$x == round(data$x)) && length(data$x) <= 100L) {
    mid <- data$x
    width <- rep(.85, length(mid))
    height <- data$counts / data$n
  } else {
    rg <- fit_preview_range(data)
    t <- seq(0, 1, length.out = min(61L, ceiling(log2(data$n)) + 2L))
    breaks <- unique((1 - t) * rg[1] + t * rg[2])
    if (length(breaks) < 2L) breaks <- range(data$x)
    index <- cut(data$x, breaks, include.lowest = TRUE, labels = FALSE)
    counts <- vapply(seq_len(length(breaks) - 1L), function(k) sum(data$counts[!is.na(index) & index == k]), 0)
    width <- diff(breaks)
    mid <- utils::head(breaks, -1L) / 2 + utils::tail(breaks, -1L) / 2
    height <- counts / data$n / width
  }
  p <- plotly::add_trace(p, x = mid, y = height, width = width, type = "bar",
    marker = list(color = th$fill, line = list(color = th$fill_solid, width = 1)))
  plotly::layout(plot_layout(p, "x", tr("fit.empirical", lang), th, lang), bargap = 0)
}

fit_diagnostic_plot <- function(diagnostics, view, lang, th) {
  d <- diagnostics[[view]]
  shiny::validate(shiny::need(!is.null(d) && nrow(d), tr("fit.view_unavailable", lang)))
  p <- plotly::plot_ly()
  if (view == "density") {
    empirical <- d[d$kind == "empirical", , drop = FALSE]
    model <- d[d$kind == "fitted", , drop = FALSE]
    if (nrow(empirical)) {
      p <- plotly::add_trace(p, x = empirical$x, y = empirical$empirical,
        width = if (isTRUE(diagnostics$discrete)) rep(.8, nrow(empirical)) else empirical$upper - empirical$lower,
        type = "bar", name = tr("fit.empirical", lang),
        marker = list(color = th$fill, line = list(color = th$fill_solid, width = 1)))
    }
    grouped <- "fit.note_grouped_diagnostics" %in% diagnostics$notes
    mass <- isTRUE(diagnostics$discrete) || grouped
    p <- plotly::add_trace(p, x = model$x, y = model$fitted, type = "scatter",
      mode = if (mass) "markers" else "lines", name = tr("fit.fitted", lang),
      line = if (!mass) list(color = th$line2, width = 2),
      marker = if (mass) list(color = th$line2, size = 6))
    ylim <- sim_ylim(empirical$empirical, data.frame(density = model$fitted,
      cdf = rep(.5, nrow(model))))
    return(compare_legend(plotly::layout(plot_layout(p, "x",
      tr(if (grouped) "fit.class_probability" else if (isTRUE(diagnostics$discrete))
        "plot.probability" else "plot.density", lang),
      th, lang, ylim = ylim), bargap = 0), th))
  }
  if (view == "cdf") {
    p <- plotly::add_trace(p, x = d$x, y = d$empirical, type = "scatter", mode = "lines",
      name = tr("fit.empirical", lang), line = list(color = th$line, width = 2, shape = "hv"))
    p <- plotly::add_trace(p, x = d$x, y = d$fitted, type = "scatter", mode = "lines",
      name = tr("fit.fitted", lang), line = list(color = th$line2, width = 2,
        shape = if (isTRUE(diagnostics$discrete)) "hv" else "linear"))
    return(compare_legend(plot_layout(p, "x", "F(x)", th, lang), th))
  }
  if (view == "qq") {
    ok <- is.finite(d$theoretical) & is.finite(d$lower) & is.finite(d$upper)
    env <- d[ok, , drop = FALSE]
    if (nrow(env)) p <- plotly::add_trace(p,
      x = c(env$theoretical, rev(env$theoretical)), y = c(env$lower, rev(env$upper)),
      type = "scatter", mode = "lines", fill = "toself", fillcolor = th$fill,
      line = list(color = "transparent"), hoverinfo = "skip", name = tr("fit.envelope", lang))
  }
  p <- plotly::add_trace(p, x = d$theoretical, y = d$observed, type = "scatter", mode = "markers",
    marker = list(color = th$line, size = 5), name = tr("fit.observed", lang))
  rg <- range(c(d$theoretical, d$observed), finite = TRUE)
  p <- plotly::add_trace(p, x = rg, y = rg, type = "scatter", mode = "lines",
                         line = list(color = th$line2, width = 1, dash = "dash"), hoverinfo = "skip")
  plot_layout(p, tr(if (view == "qq") "fit.theoretical_quantile" else "fit.theoretical_probability", lang),
    tr(if (view == "qq") "fit.observed_quantile" else "fit.observed_probability", lang), th, lang)
}

fit_ranking_tag <- function(result, lang) {
  if (is.null(result)) return(shiny::tags$p(class = "dl-muted", tr("fit.run_first", lang)))
  rank <- result$ranking
  cols <- intersect(c("id", "logLik", "k", "AIC", "BIC", "delta_AIC", "weight", "converged", "message"), names(rank))
  tables <- lapply(unique(rank$likelihood_group), function(group) {
    df <- rank[rank$likelihood_group == group, cols, drop = FALSE]
    df$id <- vapply(df$id, function(k) entry_text(dl_entry(k), "names", lang), "")
    df$converged <- vapply(df$converged, function(ok) tr(if (ok) "fit.yes" else "fit.no", lang), "")
    if ("message" %in% names(df)) df$message <- vapply(df$message, fit_message_text, "", lang = lang)
    shiny::tagList(shiny::tags$h3(class = "dl-sim-heading", tr(paste0("fit.group_", group), lang)),
      fit_table_tag(df, vapply(cols, function(k) tr(paste0("fit.col_", k), lang), ""), lang, tr("fit.ranking", lang)))
  })
  failures <- result$failures
  shiny::tagList(tables,
    if (length(failures) && NROW(failures)) shiny::tags$details(
      shiny::tags$summary(tr("fit.failures", lang)),
      shiny::tags$ul(lapply(names(failures), function(id) shiny::tags$li(
        shiny::tags$strong(paste0(entry_text(dl_entry(id), "names", lang), ": ")),
        fit_message_text(failures[[id]], lang))))))
}
