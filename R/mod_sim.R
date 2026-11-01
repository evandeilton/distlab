# Simulation lab: seeded draws of one distribution (sample check, law of
# large numbers, central limit theorem, sums and transformations,
# memorylessness) and one demo per relationship edge. Every tab shows the R
# code that reproduces its numbers.

.dl_sim_tabs <- c("sample", "lln", "clt", "transform", "relations", "memoryless")

# Choices for the number of draws, and the default.
.dl_sim_n <- c(100, 500, 1000, 5000, 10000, 50000, 100000)
.dl_sim_n_default <- 5000

# Sample sizes of the CLT frames and the choices of samples per size.
.dl_clt_sizes <- c(1, 2, 5, 10, 30, 100)
.dl_clt_reps <- c(500, 1000, 2000, 5000)

# Seeds accepted by the app: whole numbers from 1 to 2^31 - 1.
.dl_seed_max <- 2147483647

# Largest |a| and |b| of a + bX in the app.
.dl_ab_max <- 1000

mod_sim_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  tab <- function(value, icon, ...) {
    bslib::nav_panel(
      title = shiny::tags$span(class = "dl-nav-label", shiny::icon(icon, `aria-hidden` = "true"),
                               i18n_span(paste0("sim.tab.", value), lang)),
      value = value, ...
    )
  }
  heading <- function(key) shiny::tags$h3(class = "dl-sim-heading", i18n_span(key, lang))
  bslib::layout_sidebar(
    class = "dl-explorer dl-sim",
    sidebar = bslib::sidebar(
      width = 300,
      shiny::conditionalPanel(
        "input.tab != 'relations'", ns = ns,
        shiny::selectInput(ns("dist"), i18n_span("explorer.distribution", lang),
                           choices = dist_choices(lang), selected = "normal"),
        shiny::tags$h2(class = "dl-sidebar-heading", i18n_span("explorer.parameters", lang)),
        shiny::uiOutput(ns("params"))
      ),
      shiny::conditionalPanel(
        "input.tab == 'relations'", ns = ns,
        shiny::selectInput(ns("edge"), i18n_span("sim.relation", lang),
                           choices = edge_choices(lang), selected = "binomial__poisson__limit")
      ),
      shiny::tags$h2(class = "dl-sidebar-heading", i18n_span("sim.settings", lang)),
      shiny::tags$div(
        class = "dl-seed-row",
        shiny::numericInput(ns("seed"), i18n_span("sim.seed", lang), value = 2026, min = 1,
                            max = .dl_seed_max, step = 1),
        shiny::actionButton(
          ns("dice"), label = shiny::tags$span(class = "visually-hidden",
                                               i18n_span("sim.new_seed", lang)),
          icon = shiny::icon("dice", `aria-hidden` = "true"),
          class = "btn-sm btn-outline-primary dl-dice",
          title = tr("sim.new_seed", lang), `data-i18n-title` = "sim.new_seed"
        )
      ),
      shiny::selectInput(ns("n"), i18n_span("sim.draws", lang),
                         choices = count_choices(.dl_sim_n, lang),
                         selected = as.character(.dl_sim_n_default)),
      shiny::tags$p(class = "dl-muted dl-field-help dl-sim-help", i18n_span("sim.seed_help", lang))
    ),
    shiny::conditionalPanel("input.tab != 'relations'", ns = ns, shiny::uiOutput(ns("header"))),
    bslib::navset_card_underline(
      id = ns("tab"),
      tab("sample", "chart-column",
          shiny::tags$p(class = "dl-sim-intro", i18n_span("sim.sample.intro", lang)),
          bslib::layout_columns(
            col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
            shiny::tags$section(class = "dl-sim-block",
                                shiny::tags$h3(class = "dl-sim-heading",
                                               shiny::textOutput(ns("hist_title"), inline = TRUE)),
                                plotly::plotlyOutput(ns("sample_hist"), height = "300px")),
            shiny::tags$section(class = "dl-sim-block", heading("sim.ecdf"),
                                plotly::plotlyOutput(ns("sample_ecdf"), height = "300px"))
          ),
          shiny::uiOutput(ns("sample_note")),
          heading("sim.sample.check"),
          shiny::uiOutput(ns("mc_table")),
          shiny::uiOutput(ns("sample_code"))),
      tab("lln", "chart-line",
          shiny::tags$p(class = "dl-sim-intro", i18n_span("sim.lln.intro", lang)),
          shiny::tags$div(
            class = "dl-sim-controls",
            shiny::sliderInput(ns("paths"), i18n_span("sim.lln.paths", lang), min = 1, max = 5,
                               value = 3, step = 1, ticks = FALSE),
            bslib::input_switch(ns("lln_log"), i18n_span("sim.lln.log", lang), value = TRUE)
          ),
          plotly::plotlyOutput(ns("lln_plot"), height = "360px"),
          shiny::uiOutput(ns("lln_note")),
          shiny::uiOutput(ns("lln_code"))),
      tab("clt", "bell",
          shiny::tags$p(class = "dl-sim-intro", i18n_span("sim.clt.intro", lang)),
          shiny::tags$div(
            class = "dl-sim-controls",
            shiny::selectInput(ns("reps"), i18n_span("sim.clt.reps", lang),
                               choices = count_choices(.dl_clt_reps, lang), selected = "2000")
          ),
          plotly::plotlyOutput(ns("clt_plot"), height = "420px"),
          shiny::uiOutput(ns("clt_note")),
          shiny::uiOutput(ns("clt_table")),
          shiny::uiOutput(ns("clt_code"))),
      tab("transform", "wand-magic-sparkles",
          shiny::tags$p(class = "dl-sim-intro", i18n_span("sim.transform.intro", lang)),
          shiny::tags$div(
            class = "dl-sim-controls",
            shiny::selectInput(ns("op"), i18n_span("sim.transform.op", lang),
                               choices = transform_choices(lang), selected = "sum"),
            shiny::conditionalPanel(
              "['sum', 'mean', 'min', 'max'].indexOf(input.op) >= 0", ns = ns,
              shiny::sliderInput(ns("k"), i18n_span("sim.transform.k", lang), min = 1, max = 30,
                                 value = 3, step = 1, ticks = FALSE)
            ),
            shiny::conditionalPanel(
              "input.op == 'affine'", ns = ns,
              shiny::tags$div(
                class = "dl-calc-pair",
                shiny::numericInput(ns("a"), label = math_tag("a"), value = 0, step = 0.5,
                                    min = -.dl_ab_max, max = .dl_ab_max),
                shiny::numericInput(ns("b"), label = math_tag("b"), value = 2, step = 0.5,
                                    min = -.dl_ab_max, max = .dl_ab_max)
              )
            )
          ),
          shiny::tags$section(class = "dl-sim-block",
                              shiny::tags$h3(class = "dl-sim-heading",
                                             i18n_span("sim.transform.title", lang)),
                              plotly::plotlyOutput(ns("tf_plot"), height = "320px")),
          shiny::uiOutput(ns("tf_summary")),
          shiny::uiOutput(ns("tf_code"))),
      tab("relations", "diagram-project",
          shiny::uiOutput(ns("rel_head")),
          plotly::plotlyOutput(ns("rel_plot"), height = "420px"),
          shiny::uiOutput(ns("rel_note")),
          shiny::uiOutput(ns("rel_table")),
          shiny::uiOutput(ns("rel_code"))),
      tab("memoryless", "hourglass-half",
          shiny::tags$p(class = "dl-sim-intro", i18n_span("sim.mem.intro", lang)),
          shiny::uiOutput(ns("mem_controls")),
          shiny::conditionalPanel(
            "output.mem_ok", ns = ns,
            shiny::uiOutput(ns("mem_verdict")),
            bslib::layout_columns(
              col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
              shiny::tags$section(class = "dl-sim-block", heading("sim.mem.survival"),
                                  plotly::plotlyOutput(ns("mem_surv"), height = "300px")),
              shiny::tags$section(class = "dl-sim-block", heading("sim.mem.hist"),
                                  plotly::plotlyOutput(ns("mem_hist"), height = "300px"))
            ),
            shiny::uiOutput(ns("mem_note")),
            shiny::uiOutput(ns("mem_code"))
          ))
    )
  )
}

# Relationship edges grouped by relation type, labeled "Binomial -> Poisson"
# in `lang`; values are edge ids.
edge_choices <- function(lang) {
  reg <- dl_registry()
  rel <- relations_frame(reg)
  rel$id <- edge_ids(rel)
  name <- function(i) entry_text(reg[[i]], "names", lang)
  types <- intersect(.dl_relation_types, unique(rel$type))
  out <- lapply(types, function(t) {
    r <- rel[rel$type == t, , drop = FALSE]
    labels <- paste(vapply(r$from, name, ""), "\u2192", vapply(r$to, name, ""))
    o <- order(labels, method = "radix")
    stats::setNames(r$id[o], labels[o])
  })
  names(out) <- vapply(types, function(t) tr(paste0("related.type.", t), lang), "")
  out
}

# Choices of a count select: values as text, labels with the thousands
# separator of the language ("100.000" in pt-BR, "100,000" in en-US).
count_choices <- function(values, lang) {
  pt <- match_lang(lang) == "pt-BR"
  labels <- formatC(values, format = "d", big.mark = if (pt) "." else ",",
                    decimal.mark = if (pt) "," else ".")
  stats::setNames(format(values, scientific = FALSE, trim = TRUE), labels)
}

# Operations of the Transformations tab, labeled in `lang`.
transform_choices <- function(lang) {
  stats::setNames(.dl_transform_ops,
                  vapply(.dl_transform_ops, function(o) tr(paste0("sim.op.", o), lang), ""))
}

# Seed typed by the student, or NULL when it is not a whole number in
# 1..2^31 - 1.
valid_seed <- function(x) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x) && x == round(x) &&
    x >= 1 && x <= .dl_seed_max
  if (ok) as.integer(x)
}

# Translated text of an engine error: keyed errors have their own entry,
# others a general message (the English engine text is never shown).
sim_error_text <- function(err, lang) {
  key <- err$key
  if (is_string(key)) {
    txt <- tr(paste0("sim.error.", key), lang)
    if (!identical(txt, paste0("sim.error.", key))) return(txt)
  }
  tr("sim.error.engine", lang)
}

# Run an engine call keeping an error as the value, so results never depend
# on the language; sim_value() translates it when an output renders.
sim_run <- function(expr) tryCatch(expr, error = function(err) err)

# A result for rendering: an engine error becomes a translated validation
# message, never a crash.
sim_value <- function(res, lang) {
  if (inherits(res, "error")) shiny::validate(sim_error_text(res, lang))
  res
}

# `lang` may be fixed or reactive; `dark` a reactive logical; `select` a
# reactive navigation request list(tab, id, params, edge) from other pages:
# `id` (with `params`) opens a distribution, `edge` a relation demo.
# Returns a reactive request list(tab, id, params, n) that opens an end of a
# relation in the Explorer.
mod_sim_server <- function(id, lang, dark = shiny::reactive(FALSE),
                           select = shiny::reactive(NULL)) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    entry <- shiny::reactive(dl_entry(shiny::req(input$dist)))
    sliders <- param_sliders(input, output, session, entry, "params")
    pars <- sliders$pars
    route <- shiny::reactiveVal(NULL)
    count <- 0L

    # Choices are relabeled in the new language, keeping the selections.
    shiny::observeEvent(lang_r(), {
      lg <- lang_r()
      shiny::updateSelectInput(session, "dist", choices = dist_choices(lg),
                               selected = shiny::isolate(input$dist))
      shiny::updateSelectInput(session, "edge", choices = edge_choices(lg),
                               selected = shiny::isolate(input$edge))
      shiny::updateSelectInput(session, "op", choices = transform_choices(lg),
                               selected = shiny::isolate(input$op))
      shiny::updateSelectInput(session, "n", choices = count_choices(.dl_sim_n, lg),
                               selected = shiny::isolate(input$n))
      shiny::updateSelectInput(session, "reps", choices = count_choices(.dl_clt_reps, lg),
                               selected = shiny::isolate(input$reps))
    }, ignoreInit = TRUE)

    # Requests from other pages: a distribution (with the Explorer's values)
    # or a relation of the Compendium.
    shiny::observeEvent(select(), {
      r <- select()
      if (!identical(r$tab, "simulation")) return()
      if (is_string(r$edge) && r$edge %in% names(.dl_sim_recipes)) {
        shiny::updateSelectInput(session, "edge", selected = r$edge)
        bslib::nav_select("tab", "relations", session = session)
        return()
      }
      if (!is_string(r$id) || !r$id %in% dl_ids()) return()
      if (length(r$params)) sliders$set(r$id, r$params)
      shiny::updateSelectInput(session, "dist", selected = r$id)
      if (identical(shiny::isolate(input$tab), "relations")) {
        bslib::nav_select("tab", "sample", session = session)
      }
    })

    shiny::observeEvent(input$dice, {
      shiny::updateNumericInput(session, "seed", value = sample.int(.dl_seed_max, 1L))
    })
    shiny::observeEvent(input$to_exp, {
      shiny::updateSelectInput(session, "dist", selected = "exponential")
    })

    # Seed and number of draws; typing in the seed field is debounced.
    seed_in <- shiny::debounce(shiny::reactive(input$seed), 300)
    # The language is read only for the message, so a switch never re-runs
    # a simulation.
    seed <- shiny::reactive({
      s <- valid_seed(seed_in())
      if (is.null(s)) shiny::validate(tr("sim.error.seed", lang_r()))
      s
    })
    n_draws <- shiny::reactive({
      v <- suppressWarnings(as.numeric(input$n))
      if (length(v) == 1L && v %in% .dl_sim_n) v else .dl_sim_n_default
    })

    dist <- shiny::reactive({
      e <- entry()
      p <- pars()
      shiny::req(p, identical(p$id, e$id))
      d <- sim_run(do.call(dl_dist, c(list(p$id), p$vals)))
      if (inherits(d, "error")) {
        shiny::validate(tr("error.invalid_params", lang_r(), message = conditionMessage(d)))
      }
      d
    })
    # Slider drags fire many events; simulate only after a short pause.
    dist_d <- shiny::debounce(dist, 300)
    # The debounced value lags a distribution switch; wait until it matches.
    dist_now <- shiny::reactive({
      d <- dist_d()
      shiny::req(identical(d$id, entry()$id))
      d
    })
    is_discrete <- shiny::reactive(identical(entry()$type, "discrete"))
    # Each tab simulates only while it is open (the first tab until the
    # navset reports).
    tab_open <- function(name) shiny::req(identical(input$tab %||% "sample", name))

    output$header <- shiny::renderUI(explore_header(entry(), lang_r()))

    # Sample: histogram, ECDF and the Monte Carlo table.
    mc <- shiny::reactive({
      tab_open("sample")
      d <- dist_now()
      s <- seed()
      n <- n_draws()
      sim_run(mc_check_run(d, n, s))
    })
    mc_val <- shiny::reactive(sim_value(mc(), lang_r()))
    output$hist_title <- shiny::renderText({
      tr(if (is_discrete()) "sim.hist.pmf" else "sim.hist.density", lang_r())
    })
    sample_view <- shiny::reactive({
      d <- dist_now()
      res <- mc()
      if (inherits(res, "error")) return(res)
      sim_run(sim_view(res$sample, d, mode = if (is_discrete()) "law" else "none"))
    })
    output$sample_hist <- plotly::renderPlotly({
      mc_val()
      v <- sim_value(sample_view(), lang_r())
      sim_hist_plot(v, lang_r(), plot_theme(dark()), tr("sim.exact", lang_r()))
    })
    output$sample_ecdf <- plotly::renderPlotly({
      v <- sim_value(sample_view(), lang_r())
      sim_ecdf_plot(mc_val()$sample, dist_now(), v$range, lang_r(), plot_theme(dark()))
    })
    output$sample_note <- shiny::renderUI({
      v <- sample_view()
      if (is.list(v) && !inherits(v, "error")) outside_note(v$outside, lang_r())
    })
    output$mc_table <- shiny::renderUI(mc_table_tag(mc_val()$table, lang_r()))
    output$sample_code <- shiny::renderUI(code_tag(mc_val()$code, ns("sample-code"), lang_r()))

    # Law of large numbers.
    lln <- shiny::reactive({
      tab_open("lln")
      d <- dist_now()
      s <- seed()
      n <- n_draws()
      p <- suppressWarnings(as.numeric(input$paths))
      p <- if (length(p) == 1L && p %in% 1:5) p else 3
      sim_run(dl_lln(d, n, paths = p, seed = s))
    })
    lln_val <- shiny::reactive(sim_value(lln(), lang_r()))
    output$lln_plot <- plotly::renderPlotly({
      lln_plot(lln_val(), isTRUE(input$lln_log), lang_r(), plot_theme(dark()),
               lower = dl_support(dist_now())$lower)
    })
    output$lln_note <- shiny::renderUI(lln_note(lln_val(), lang_r()))
    output$lln_code <- shiny::renderUI(code_tag(lln_val()$code, ns("lln-code"), lang_r()))

    # Central limit theorem.
    clt <- shiny::reactive({
      tab_open("clt")
      d <- dist_now()
      s <- seed()
      r <- suppressWarnings(as.numeric(input$reps))
      r <- if (length(r) == 1L && r %in% .dl_clt_reps) r else 2000
      sim_run(dl_clt(d, sizes = .dl_clt_sizes, reps = r, seed = s))
    })
    clt_val <- shiny::reactive(sim_value(clt(), lang_r()))
    output$clt_plot <- plotly::renderPlotly({
      clt_plot(clt_val(), dist_now(), lang_r(), plot_theme(dark()))
    })
    output$clt_note <- shiny::renderUI({
      res <- clt_val()
      lg <- lang_r()
      notes <- if (res$standardized) {
        c(tr("sim.clt.note", lg), if (is_discrete()) tr("sim.clt.note.discrete", lg))
      } else if (is.na(res$mean)) {
        tr("sim.clt.note.no_mean", lg)
      } else if (is.infinite(res$mean)) {
        tr("sim.clt.note.infinite_mean", lg)
      } else {
        tr("sim.clt.note.infinite_var", lg)
      }
      note_list(notes)
    })
    output$clt_table <- shiny::renderUI({
      res <- clt_val()
      if (!res$standardized) return(NULL)
      clt_table_tag(res, lang_r())
    })
    output$clt_code <- shiny::renderUI(code_tag(clt_val()$code, ns("clt-code"), lang_r()))

    # Sums and transformations.
    op <- shiny::reactive({
      o <- input$op
      if (is_string(o) && o %in% .dl_transform_ops) o else "sum"
    })
    tf <- shiny::reactive({
      tab_open("transform")
      d <- dist_now()
      s <- seed()
      n <- n_draws()
      o <- op()
      k <- suppressWarnings(as.numeric(input$k))
      k <- if (length(k) == 1L && is.finite(k) && k >= 1 && k <= 30) round(k) else 3
      a <- if (o == "affine") input$a else 0
      b <- if (o == "affine") input$b else 1
      ok <- is.numeric(a) && is.numeric(b) && length(a) == 1L && length(b) == 1L &&
        is.finite(a) && is.finite(b) && abs(a) <= .dl_ab_max && abs(b) <= .dl_ab_max
      if (!ok) shiny::validate(tr("sim.error.ab", lang_r(), max = .dl_ab_max))
      if (o %in% .dl_transform_multi) n <- min(n, floor(1e7 / k))
      sim_run(dl_transform(d, o, k = k, n = n, seed = s, a = a, b = b))
    })
    tf_val <- shiny::reactive(sim_value(tf(), lang_r()))
    # The plot range may fail on its own (a very wide law); the law and its
    # moments are still shown.
    tf_view <- shiny::reactive({
      res <- tf()
      if (inherits(res, "error")) return(res)
      ref <- res$target %||% res$approx
      mode <- transform_bin_mode(dist_now(), res$op, res$a, res$b)
      sim_run(sim_view(res$sample, ref, mode = mode, total = length(res$sample) + res$dropped))
    })
    output$tf_plot <- plotly::renderPlotly({
      res <- tf_val()
      lg <- lang_r()
      v <- sim_value(tf_view(), lg)
      label <- if (!is.null(res$target)) tr("sim.exact", lg) else tr("sim.approx", lg)
      sim_hist_plot(v, lg, plot_theme(dark()), label, dashed = is.null(res$target), var = "y")
    })
    output$tf_summary <- shiny::renderUI({
      v <- tf_view()
      tf_summary_tag(tf_val(), dist_now(), if (!inherits(v, "error")) v, lang_r())
    })
    output$tf_code <- shiny::renderUI(code_tag(tf_val()$code, ns("tf-code"), lang_r()))

    # Relations.
    edge <- shiny::reactive({
      e <- input$edge
      if (is_string(e) && e %in% names(.dl_sim_recipes)) e else "binomial__poisson__limit"
    })
    demo <- shiny::reactive({
      tab_open("relations")
      e <- edge()
      # Limits are exact: no seed or number of draws.
      limit <- identical(.dl_sim_recipes[[e]]$kind, "limit")
      s <- if (!limit) seed()
      n <- if (!limit) n_draws() else .dl_sim_n_default
      sim_run(relation_demo_run(e, n = n, seed = s))
    })
    demo_val <- shiny::reactive(sim_value(demo(), lang_r()))
    output$rel_head <- shiny::renderUI({
      tab_open("relations")
      rel_head_tag(edge(), lang_r(), ns("open_end"))
    })
    output$rel_plot <- plotly::renderPlotly(rel_plot(demo_val(), lang_r(), plot_theme(dark())))
    output$rel_note <- shiny::renderUI(rel_note(demo_val(), lang_r()))
    output$rel_table <- shiny::renderUI(rel_table_tag(demo_val(), lang_r()))
    output$rel_code <- shiny::renderUI(code_tag(demo_val()$code, ns("rel-code"), lang_r()))
    # Either end opens in the Explorer with the parameters of the recipe.
    shiny::observeEvent(input$open_end, {
      e <- edge()
      r <- .dl_sim_recipes[[e]]
      rel <- dl_relations()
      rel <- rel[edge_ids(rel) == e, , drop = FALSE]
      did <- input$open_end
      if (!is_string(did) || !did %in% c(rel$from, rel$to)) return()
      count <<- count + 1L
      params <- if (identical(did, rel$from)) r$from else r$to
      route(list(tab = "explorer", id = did, params = params, n = count))
    })

    # Memorylessness.
    mem_ok <- shiny::reactive({
      s <- dl_support(dist_now())
      s$lower == 0 && s$upper == Inf
    })
    # Shows the plots of the tab only for supports [0, Inf); the empty text
    # is false for the conditionalPanel.
    output$mem_ok <- shiny::renderText(if (mem_ok()) "yes" else "")
    shiny::outputOptions(output, "mem_ok", suspendWhenHidden = FALSE)
    output$mem_controls <- shiny::renderUI({
      tab_open("memoryless")
      d <- dist_now()
      lg <- lang_r()
      if (!mem_ok()) {
        return(shiny::tags$div(
          class = "dl-calc-note dl-sim-unsupported",
          shiny::icon("circle-info", `aria-hidden` = "true"),
          tr("sim.mem.unsupported", lg), " ",
          shiny::actionButton(ns("to_exp"), tr("sim.mem.to_exp", lg),
                              class = "btn-sm btn-outline-primary")
        ))
      }
      sl <- mem_slider(d)
      old <- shiny::isolate(input$mem_s)
      value <- if (is.numeric(old) && length(old) == 1L && old >= sl$min && old <= sl$max) old else sl$value
      shiny::tags$div(
        class = "dl-sim-controls",
        shiny::sliderInput(ns("mem_s"), tr("sim.mem.s", lg), min = sl$min, max = sl$max,
                           value = value, step = sl$step, ticks = FALSE)
      )
    })
    mem <- shiny::reactive({
      tab_open("memoryless")
      shiny::req(mem_ok())
      d <- dist_now()
      sd <- seed()
      n <- n_draws()
      s <- shiny::req(input$mem_s)
      if (identical(d$entry$type, "discrete")) s <- round(s)
      sim_run(dl_memoryless(d, s, n = n, seed = sd))
    })
    mem_val <- shiny::reactive(sim_value(mem(), lang_r()))
    output$mem_verdict <- shiny::renderUI(mem_verdict_tag(mem_val(), dist_now(), n_draws(),
                                                          lang_r()))
    output$mem_note <- shiny::renderUI({
      if (is_discrete()) note_list(tr("sim.mem.note.discrete", lang_r()))
    })
    output$mem_code <- shiny::renderUI(code_tag(mem_val()$code, ns("mem-code"), lang_r()))
    output$mem_surv <- plotly::renderPlotly(mem_surv_plot(mem_val(), dist_now(), lang_r(),
                                                          plot_theme(dark())))
    output$mem_hist <- plotly::renderPlotly({
      res <- mem_val()
      d <- dist_now()
      v <- sim_value(sim_run(sim_view(res$residual, d, mode = if (is_discrete()) "law" else "none")),
                     lang_r())
      sim_hist_plot(v, lang_r(), plot_theme(dark()), tr("sim.mem.law_x", lang_r()), var = "t",
                    labels = list(prob = "P(X \u2212 s = t)", dens = "f(t)"))
    })

    list(dist = dist, tab = shiny::reactive(input$tab), seed = seed, mc = mc_val, lln = lln_val,
         clt = clt_val, tf = tf_val, demo = demo_val, mem = mem_val, route = route)
  })
}

# Range of the memorylessness slider: one step to the 80% point of X, with
# about 50 steps (whole steps for discrete laws), starting at the median.
# s = 0 is left out, since every law passes the check there.
mem_slider <- function(dist) {
  q <- dl_quantile(dist, c(0.5, 0.8))
  if (identical(dist$entry$type, "discrete")) {
    hi <- max(2, round(q[2]))
    return(list(min = 1, max = hi, step = 1, value = min(max(1, round(q[1])), hi)))
  }
  hi <- signif(q[2], 2)
  step <- signif(hi / 50, 1)
  list(min = step, max = hi, step = step, value = min(max(step, round(q[1] / step) * step), hi))
}

# TRUE when Y keeps whole-number values: a discrete X under a sum, an
# extreme, a square or a + bX with whole a and b.
transform_lattice <- function(dist, op, a, b) {
  identical(dist$entry$type, "discrete") &&
    (op %in% c("sum", "min", "max", "square") ||
       (op == "affine" && a == round(a) && b == round(b)))
}

# Bins of Y (see bin_step()): the lattice of a whole-number transform, a
# lattice found in the draws for other transforms of a discrete X (means,
# fractional a + bX), continuous bins for a continuous X.
transform_bin_mode <- function(dist, op, a, b) {
  if (!identical(dist$entry$type, "discrete")) return("none")
  if (transform_lattice(dist, op, a, b)) "law" else "auto"
}

# Everything a histogram needs: the draws binned on a plotting range, the
# reference law on the same range and the share of the finite draws outside
# it (non-finite draws are counted apart). `ref` is the law drawn over the
# bars (NULL for none); `mode` chooses lattice or continuous bins, see
# bin_step(). `unit` is TRUE for bars on a unit lattice, whose heights are
# probabilities.
sim_view <- function(x, ref, mode = "auto", total = length(x)) {
  x <- x[is.finite(x)]
  ref_discrete <- !is.null(ref) && identical(ref$entry$type, "discrete")
  rng <- if (!is.null(ref)) sim_range(ref, x) else sample_range(x)
  step <- bin_step(x, rng, mode, unit = ref_discrete)
  whole <- all(x == round(x))
  bins <- if (is.na(step)) {
    sim_bins(x, range = rng, total = total)
  } else if (step == 1 && whole) {
    rng <- c(floor(rng[1]), ceiling(rng[2]))
    sim_bins(x, discrete = TRUE, range = rng, total = total)
  } else {
    sim_bins(x, range = rng, total = total, step = step)
  }
  curve <- if (!is.null(ref)) law_curve(ref, rng)
  list(bins = bins, curve = curve, range = rng, step = step, unit = isTRUE(step == 1),
       ref_discrete = ref_discrete, outside = sum(x < rng[1] | x > rng[2]) / total)
}

# Y limit that keeps an unbounded density (Chi-square with 1 df, Beta with a
# shape below 1) from flattening the bars; NULL when no cap is needed.
sim_ylim <- function(bars, curve) {
  top_bar <- if (length(bars)) max(bars) else 0
  if (is.null(curve) || !nrow(curve)) return(NULL)
  body <- curve$density[curve$cdf >= 0.01 & curve$cdf <= 0.99]
  cap <- 1.15 * max(c(top_bar, body, 0))
  if (cap > 0 && max(curve$density) > 1.5 * cap) c(0, cap) else NULL
}

# Histogram (bars) with the reference law: a curve for continuous laws,
# points for mass functions (a line when they are too many to tell apart);
# `dashed` marks an approximation. `var` names the axis ("x" gives f(x) and
# P(X = x)); `labels` overrides the probability and density axis titles.
sim_hist_plot <- function(v, lang, th, ref_label, dashed = FALSE, var = "x", labels = NULL) {
  b <- v$bins
  y <- if (v$unit) b$proportion else b$density
  p <- plotly::plot_ly()
  p <- plotly::add_trace(
    p, x = b$mid, y = y, width = b$right - b$left, type = "bar", name = tr("sim.simulated", lang),
    marker = list(color = th$fill, line = list(color = th$fill_solid, width = 1)),
    hovertemplate = paste0(var, " = %{x:.4g}<br>%{y:.4f}<extra></extra>")
  )
  cv <- v$curve
  if (!is.null(cv) && nrow(cv)) {
    if (v$ref_discrete && nrow(cv) <= 300) {
      p <- plotly::add_trace(p, x = cv$x, y = cv$density, type = "scatter", mode = "markers",
                             name = ref_label,
                             marker = list(color = th$line2, size = 7,
                                           line = list(color = th$hover_bg, width = 1)),
                             hovertemplate = "x = %{x}<br>P = %{y:.4f}<extra></extra>")
    } else {
      p <- plotly::add_trace(p, x = cv$x, y = cv$density, type = "scatter", mode = "lines",
                             name = ref_label,
                             line = list(color = th$line2, width = 2.5,
                                         dash = if (dashed) "dash" else "solid"),
                             hovertemplate = "x = %{x:.3f}<br>f(x) = %{y:.4f}<extra></extra>")
    }
  }
  ylab <- if (v$unit) {
    labels$prob %||% sprintf("P(%s = %s)", toupper(var), var)
  } else {
    labels$dens %||% sprintf("f(%s)", var)
  }
  p <- plot_layout(p, var, ylab, th, lang, ylim = sim_ylim(y, cv))
  compare_legend(plotly::layout(p, bargap = 0), th)
}

# Empirical CDF of the draws against the exact CDF, on the plotting range.
sim_ecdf_plot <- function(x, dist, range, lang, th) {
  discrete <- identical(dist$entry$type, "discrete")
  grid <- law_curve(dist, range)
  xs <- sort(x)
  fn <- findInterval(grid$x, xs) / length(xs)
  grid$x <- signif(grid$x, 7)
  grid$cdf <- signif(grid$cdf, 7)
  shape <- if (discrete) "hv" else "linear"
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = grid$x, y = fn, type = "scatter", mode = "lines",
                         name = tr("sim.ecdf.empirical", lang),
                         line = list(color = th$line, width = 2.5, shape = "hv"),
                         hovertemplate = "x = %{x:.3f}<br>F\u0302(x) = %{y:.4f}<extra></extra>")
  p <- plotly::add_trace(p, x = grid$x, y = grid$cdf, type = "scatter", mode = "lines",
                         name = tr("sim.exact", lang),
                         line = list(color = th$line2, width = 2, dash = "dash", shape = shape),
                         hovertemplate = "x = %{x:.3f}<br>F(x) = %{y:.4f}<extra></extra>")
  p <- plot_layout(p, tr("plot.x", lang), tr("plot.cdf", lang), th, lang, ylim = c(0, 1.02))
  compare_legend(p, th)
}

# Note on the share of draws outside the plotted range, when there are any.
outside_note <- function(share, lang) {
  if (!is.finite(share) || share <= 0) return(NULL)
  note_list(tr("sim.note.outside", lang, pct = format_share(share, lang)))
}

# Share as a percentage with one decimal ("< 0.1%" for tiny shares).
format_share <- function(p, lang) {
  mark <- if (match_lang(lang) == "pt-BR") "," else "."
  if (p > 0 && p < 5e-4) return(paste0("< ", formatC(0.1, format = "f", digits = 1, decimal.mark = mark), "%"))
  paste0(formatC(100 * p, format = "f", digits = 1, decimal.mark = mark), "%")
}

# Notes with the info icon, as in the Calculator.
note_list <- function(notes) {
  notes <- unlist(notes)
  if (!length(notes)) return(NULL)
  shiny::tagList(lapply(notes, function(t) {
    shiny::tags$p(class = "dl-calc-note", shiny::icon("circle-info", `aria-hidden` = "true"), t)
  }))
}

# Interval as text: "[a, b]" in en-US, "[a; b]" in pt-BR.
interval_text <- function(lo, hi, lang) {
  sep <- if (match_lang(lang) == "pt-BR") "; " else ", "
  paste0("[", format_num(lo, lang), sep, format_num(hi, lang), "]")
}

# Table of the Monte Carlo check: exact value, estimate, standard error, 95%
# interval and whether it holds the exact value.
mc_table_tag <- function(tab, lang) {
  th <- function(text) shiny::tags$th(scope = "col", text)
  label <- function(i) {
    qn <- tab$quantity[i]
    if (startsWith(qn, "cdf_")) {
      return(paste0("P(X \u2264 ", format_num(tab$q[i], lang), ")"))
    }
    tr(paste0("moments.", qn), lang)
  }
  exact_text <- function(i) {
    v <- tab$exact[i]
    if (startsWith(tab$quantity[i], "cdf_")) format_num(v, lang) else format_moment(v, lang)
  }
  rows <- lapply(seq_len(nrow(tab)), function(i) {
    cov <- tab$covered[i]
    mark <- if (is.na(cov)) {
      shiny::tags$span(class = "dl-muted", `aria-label` = tr("sim.covered.na", lang), "\u2014")
    } else if (cov) {
      shiny::tags$span(class = "dl-mark-yes", role = "img", `aria-label` = tr("sim.covered.yes", lang),
                       "\u2713")
    } else {
      shiny::tags$span(class = "dl-mark-no", role = "img", `aria-label` = tr("sim.covered.no", lang),
                       "\u2717")
    }
    undefined <- is.na(tab$exact[i])
    shiny::tags$tr(
      shiny::tags$th(scope = "row", class = "dl-series-name", label(i)),
      shiny::tags$td(class = paste(c("dl-cell-num", if (undefined) "dl-cell-undefined"), collapse = " "),
                     exact_text(i)),
      shiny::tags$td(class = "dl-cell-num", format_num(tab$estimate[i], lang)),
      shiny::tags$td(class = "dl-cell-num", format_num(tab$se[i], lang)),
      shiny::tags$td(class = "dl-cell-num",
                     if (is.na(tab$lower[i])) "\u2014" else interval_text(tab$lower[i], tab$upper[i], lang)),
      shiny::tags$td(class = "dl-cell-mark", mark)
    )
  })
  shiny::tags$div(
    class = "table-responsive",
    shiny::tags$table(
      class = "table dl-table dl-compare-table dl-mc-table",
      shiny::tags$caption(class = "visually-hidden", tr("sim.sample.check", lang)),
      shiny::tags$thead(shiny::tags$tr(
        th(tr("sim.col.quantity", lang)), th(tr("sim.col.exact", lang)),
        th(tr("sim.col.estimate", lang)), th(tr("sim.col.se", lang)),
        th(tr("sim.col.interval", lang)), th(tr("sim.col.covered", lang))
      )),
      shiny::tags$tbody(rows)
    )
  )
}

# Running means of each path, thinned to about 2000 log-spaced points, with
# the exact mean and the band mu +/- 2 sigma / sqrt(k) when they exist.
lln_plot <- function(res, log_x, lang, th, lower = -Inf) {
  d <- res$paths
  n <- max(d$k)
  idx <- unique(c(round(exp(seq(0, log(n), length.out = 2000))), n))
  np <- max(d$path)
  cols <- series_colors(np, th$dark)
  p <- plotly::plot_ly()
  mu <- res$mean
  sigma <- res$sd
  if (is.finite(mu) && is.finite(sigma) && sigma > 0) {
    half <- signif(2 * sigma / sqrt(idx), 7)
    p <- plotly::add_trace(p, x = idx, y = mu + half, type = "scatter", mode = "lines",
                           line = list(color = th$muted, width = 1, dash = "dot"),
                           name = "\u03bc \u00b1 2\u03c3/\u221ak", hoverinfo = "skip", showlegend = TRUE)
    p <- plotly::add_trace(p, x = idx, y = mu - half, type = "scatter", mode = "lines",
                           line = list(color = th$muted, width = 1, dash = "dot"),
                           fill = "tonexty", fillcolor = alpha_hex(th$muted, 0.10),
                           name = "\u03bc \u00b1 2\u03c3/\u221ak", hoverinfo = "skip", showlegend = FALSE)
  }
  for (j in seq_len(np)) {
    # Seven significant digits are plenty on screen and halve the payload.
    m <- signif(d$mean[d$path == j][idx], 7)
    label <- tr("sim.lln.path", lang, n = j)
    p <- plotly::add_trace(p, x = idx, y = m, type = "scatter", mode = "lines", name = label,
                           line = list(color = cols[j], width = 2),
                           hovertemplate = paste0(label, "<br>k = %{x}<br>%{y:.4f}<extra></extra>"))
  }
  shapes <- if (is.finite(mu)) list(list(
    type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1, y0 = mu, y1 = mu,
    line = list(color = th$accent, width = 1.5, dash = "dash")))
  all_m <- d$mean[d$k %in% idx]
  ylim <- lln_ylim(all_m, d$mean[d$k >= max(1, n %/% 20)], mu, sigma, lower)
  p <- plot_layout(p, tr("sim.lln.xlab", lang), tr("sim.lln.ylab", lang), th, lang, ylim = ylim)
  p <- plotly::layout(p, shapes = shapes, xaxis = list(type = if (log_x) "log" else "linear"))
  compare_legend(p, th)
}

# Y range of the running means. With a finite sigma, the band from k = 1
# (mu +/- 2.2 sigma) and every path are shown. Without it, early running
# means can be huge, so the range follows the late ones (k >= n/20) unless
# the paths stay close to them anyway.
lln_ylim <- function(all_m, late, mu, sigma, lower = -Inf) {
  all_m <- all_m[is.finite(all_m)]
  if (!length(all_m)) return(NULL)
  if (is.finite(mu) && is.finite(sigma)) {
    # Centered on the mean, as wide as the farthest path (at most the band at
    # k = 1, mu +/- 2.2 sigma), and never below the support.
    half <- min(max(abs(range(all_m) - mu)), 2.2 * sigma)
    half <- 1.04 * max(half, 1e-8)
    return(c(max(mu - half, lower), mu + half))
  }
  if (!length(late) || !all(is.finite(late))) return(NULL)
  lo <- min(late, if (is.finite(mu)) mu)
  hi <- max(late, if (is.finite(mu)) mu)
  pad <- max(hi - lo, 1e-8) * 0.6
  if (min(all_m) < lo - 4 * pad || max(all_m) > hi + 4 * pad) c(lo - 4 * pad, hi + 4 * pad)
}

# Notes under the running means: the band, or why the paths do not settle.
lln_note <- function(res, lang) {
  mu <- res$mean
  sigma <- res$sd
  key <- if (is.na(mu)) {
    "sim.lln.note.no_mean"
  } else if (is.infinite(mu)) {
    "sim.lln.note.infinite_mean"
  } else if (!is.finite(sigma)) {
    "sim.lln.note.infinite_var"
  } else {
    "sim.lln.note.band"
  }
  note_list(tr(key, lang, mu = format_value(mu, lang)))
}

# Shared animation settings: short frames and transitions, a translated play
# button, and slider steps labeled with the frame labels.
sim_animate <- function(p, labels, lang, th) {
  p <- plotly::animation_opts(p, frame = 700, transition = 150, redraw = TRUE)
  p <- plotly::animation_slider(
    p, currentvalue = list(prefix = "", font = list(color = th$text, size = 13)),
    font = list(color = th$muted)
  )
  p <- plotly::animation_button(p, label = tr("sim.play", lang), font = list(color = th$text),
                                bgcolor = th$hover_bg, bordercolor = th$grid)
  steps <- p$x$layout$sliders[[1]]$steps
  for (i in seq_along(steps)) p$x$layout$sliders[[1]]$steps[[i]]$label <- labels[i]
  p
}

# Standardized sample means against N(0, 1), one frame per sample size; raw
# means on the range of the law itself when they cannot be standardized.
clt_plot <- function(res, dist, lang, th) {
  bins <- clt_bins(res, dist)
  breaks <- attr(bins, "breaks")
  sizes <- res$distance$size
  xlab <- tr(if (res$standardized) "sim.clt.xlab" else "sim.clt.xlab_raw", lang)
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, data = bins, x = ~mid, y = ~density, frame = ~frame, type = "bar",
                         width = ~width, name = tr("sim.simulated", lang),
                         marker = list(color = th$fill, line = list(color = th$fill_solid, width = 1)),
                         hovertemplate = "%{x:.2f}<br>%{y:.4f}<extra></extra>")
  top <- max(bins$density)
  if (res$standardized) {
    z <- seq(-4, 4, length.out = 401)
    p <- plotly::add_trace(p, x = z, y = stats::dnorm(z), type = "scatter", mode = "lines",
                           name = "N(0, 1)", inherit = FALSE,
                           line = list(color = th$line2, width = 2.5),
                           hovertemplate = "z = %{x:.2f}<br>\u03c6(z) = %{y:.4f}<extra></extra>")
    top <- max(top, stats::dnorm(0))
  }
  ylab <- tr("plot.density", lang)
  p <- plot_layout(p, xlab, ylab, th, lang, ylim = c(0, 1.08 * top))
  p <- compare_legend(plotly::layout(p, bargap = 0, xaxis = list(range = range(breaks))), th)
  # Legend above the plot, so the animation slider has the space below.
  p <- plotly::layout(p, legend = list(y = 1.02, yanchor = "bottom", x = 1, xanchor = "right"),
                      margin = list(l = 64, r = 12, t = 48, b = 40))
  sim_animate(p, paste("n =", sizes), lang, th)
}

# Bars of each CLT frame on one range: bins 0.2 wide on [-4, 4] for
# standardized means (an automatic grid for raw ones), or, for a discrete law,
# bars on the lattice of the means (spacing 1/(sigma sqrt(n)) for a unit
# lattice) when it is coarser than those bins. The breaks are kept as an
# attribute for the x range.
clt_bins <- function(res, dist) {
  m <- res$means
  sizes <- res$distance$size
  if (res$standardized) {
    breaks <- seq(-4, 4, by = 0.2)
  } else {
    breaks <- sim_breaks(m$value, sim_range(dist, m$value))
  }
  rng <- range(breaks)
  width <- diff(breaks)[1]
  discrete <- identical(dist$entry$type, "discrete")
  bins <- do.call(rbind, lapply(seq_along(sizes), function(i) {
    v <- m$value[m$size == sizes[i]]
    # Discrete means sit on a lattice; bars group whole lattice points into
    # about the continuous width, so no bar is a gap or a spike.
    step <- if (discrete) lattice_step(v) else NA_real_
    b <- if (is.na(step) || (rng[2] - rng[1]) / step > 1e6) {
      sim_bins(v, breaks = breaks, total = length(v))
    } else {
      sim_bins(v, range = rng, total = length(v), step = step,
               group = max(1, round(width / step)))
    }
    data.frame(frame = i, b, width = b$right - b$left)
  }))
  attr(bins, "breaks") <- breaks
  bins
}

# Distance to N(0, 1) per sample size, for screen readers too.
clt_table_tag <- function(res, lang) {
  d <- res$distance
  rows <- lapply(seq_len(nrow(d)), function(i) {
    shiny::tags$tr(shiny::tags$td(class = "dl-cell-num", format_num(d$size[i], lang)),
                   shiny::tags$td(class = "dl-cell-num", format_num(d$ks[i], lang)))
  })
  shiny::tags$div(
    class = "table-responsive dl-sim-table",
    shiny::tags$table(
      class = "table dl-table dl-compare-table",
      shiny::tags$caption(tr("sim.metric.clt", lang)),
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(scope = "col", tr("sim.clt.col.size", lang)),
        shiny::tags$th(scope = "col", tr("sim.clt.col.ks", lang))
      )),
      shiny::tags$tbody(rows)
    )
  )
}

# Summary of a transformation: the law named (exact or approximate), sample
# and reference moments, dropped draws and teaching notes.
tf_summary_tag <- function(res, dist, view, lang) {
  ref <- res$target %||% res$approx
  head <- if (!is.null(res$target)) {
    tr("sim.transform.exact", lang, law = series_label(res$target, lang))
  } else if (!is.null(res$approx)) {
    tr("sim.transform.approx", lang, law = series_label(res$approx, lang))
  } else {
    tr("sim.transform.none", lang)
  }
  y <- res$sample
  sample_m <- c(mean = if (length(y)) mean(y) else NA_real_,
                sd = if (length(y) > 1L) stats::sd(y) else NA_real_)
  ref_m <- if (!is.null(ref)) dl_moments(ref)[c("mean", "sd")]
  row <- function(key) {
    shiny::tags$tr(
      shiny::tags$th(scope = "row", class = "dl-series-name", tr(paste0("moments.", key), lang)),
      shiny::tags$td(class = "dl-cell-num", format_num(sample_m[[key]], lang)),
      if (!is.null(ref_m)) shiny::tags$td(class = "dl-cell-num", format_moment(ref_m[[key]], lang))
    )
  }
  notes <- c(
    if (res$dropped > 0) tr("sim.transform.dropped", lang, n = res$dropped),
    if (res$op == "mean" && dist$id == "cauchy") tr("sim.transform.note.cauchy_mean", lang),
    if (!is.null(view) && is.finite(view$outside) && view$outside > 0) {
      tr("sim.note.outside", lang, pct = format_share(view$outside, lang))
    }
  )
  shiny::tags$div(
    class = "dl-sim-summary",
    shiny::tags$p(class = "dl-sim-law", shiny::tags$strong(head)),
    shiny::tags$div(
      class = "table-responsive dl-sim-table",
      shiny::tags$table(
        class = "table dl-table dl-compare-table",
        shiny::tags$caption(class = "visually-hidden", tr("sim.transform.title", lang)),
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(scope = "col", ""),
          shiny::tags$th(scope = "col", tr("sim.simulated", lang)),
          if (!is.null(ref_m)) {
            shiny::tags$th(scope = "col", tr(if (!is.null(res$target)) "sim.exact" else "sim.approx", lang))
          }
        )),
        shiny::tags$tbody(row("mean"), row("sd"))
      )
    ),
    note_list(notes)
  )
}

# Heading of a relation: both ends, type, condition, formula, the recipe
# parameters and buttons that open either end in the Explorer.
rel_head_tag <- function(edge_id, lang, open_id, reg = dl_registry()) {
  rel <- relations_frame(reg)
  r <- rel[edge_ids(rel) == edge_id, , drop = FALSE]
  name <- function(i) entry_text(reg[[i]], "names", lang)
  cond <- if (lang == "pt-BR") r$condition_pt else r$condition_en
  shiny::tags$div(
    class = "dl-sim-rel-head",
    shiny::tags$h3(
      class = "dl-panel-title dl-rel-path",
      name(r$from),
      shiny::tags$span(class = "dl-rel-arrow", `aria-hidden` = "true", "\u2192"),
      shiny::tags$span(class = "visually-hidden", tr("related.to", lang)),
      name(r$to), " ", relation_badge(r$type, lang)
    ),
    shiny::tags$p(class = "dl-rel-cond", cond),
    shiny::tags$div(class = "dl-rel-math dl-panel-math", math_tag(r$latex)),
    shiny::tags$p(class = "dl-muted dl-sim-params",
                  tr("sim.rel.params", lang), " ", shiny::tags$code(.dl_sim_recipes[[edge_id]]$params)),
    shiny::tags$div(
      class = "dl-panel-actions",
      lapply(unique(c(r$from, r$to)), function(i) {
        dist_button(i, tr("compendium.open_name", lang, name = name(i)), open_id,
                    class = "btn btn-outline-primary btn-sm",
                    label = tr("related.open", lang, name = name(i)))
      })
    )
  )
}

# Animated plot of a relation demo: a histogram that builds up (draws), the
# sum and its exact law for each number of terms (sums), or the
# approximating law converging to the limit law (limits).
rel_plot <- function(res, lang, th) {
  nf <- nrow(res$frames)
  cv <- res$curves
  target <- cv[cv$series == "target", , drop = FALSE]
  approx <- cv[cv$series == "approx", , drop = FALSE]
  p <- plotly::plot_ly()
  target_name <- tr(if (res$kind == "limit") "sim.rel.series.target" else "sim.exact", lang)
  bar_marker <- list(color = th$fill, line = list(color = th$fill_solid, width = 1))
  tops <- numeric(0)
  if (res$kind != "limit") {
    breaks <- if (!res$discrete) {
      sim_breaks(res$samples$value[res$samples$frame == nf], res$range)
    }
    bins <- do.call(rbind, lapply(seq_len(nf), function(i) {
      v <- res$samples$value[res$samples$frame == i]
      data.frame(frame = i, sim_bins(v, discrete = res$discrete, breaks = breaks,
                                     range = res$range, total = length(v)))
    }))
    y <- if (res$discrete) bins$proportion else bins$density
    bins$y <- y
    tops <- c(tops, y)
    p <- plotly::add_trace(p, data = bins, x = ~mid, y = ~y, frame = ~frame, type = "bar",
                           width = if (res$discrete) 1 else diff(breaks)[1],
                           name = tr("sim.simulated", lang), marker = bar_marker,
                           hovertemplate = "x = %{x:.3f}<br>%{y:.4f}<extra></extra>")
  } else if (identical(dl_entry(res$edge$from)$type, "discrete")) {
    # Bars of a discrete approximating law; for Normal limits their width is
    # the spacing 1/sigma of the standardized points.
    width <- if (res$discrete) rep(1, nrow(approx)) else stats::ave(approx$x, approx$frame,
      FUN = function(z) if (length(z) > 1L) rep(min(diff(z)), length(z)) else rep(1, length(z)))
    approx$width <- width
    tops <- c(tops, approx$density)
    p <- plotly::add_trace(p, data = approx, x = ~x, y = ~density, frame = ~frame, type = "bar",
                           width = ~width, name = tr("sim.rel.series.approx", lang),
                           marker = bar_marker,
                           hovertemplate = "x = %{x:.3f}<br>%{y:.4f}<extra></extra>")
  } else {
    tops <- c(tops, approx$density)
    p <- plotly::add_trace(p, data = approx, x = ~x, y = ~density, frame = ~frame,
                           type = "scatter", mode = "lines", name = tr("sim.rel.series.approx", lang),
                           line = list(color = th$line, width = 2.5),
                           hovertemplate = "x = %{x:.3f}<br>%{y:.4f}<extra></extra>")
  }
  # The target is the same in every frame except for sums.
  framed <- res$kind == "sum"
  tg <- if (framed) target else target[target$frame == 1, , drop = FALSE]
  disc_target <- res$discrete
  body <- tg$density[tg$cdf >= 0.01 & tg$cdf <= 0.99]
  args <- list(p, data = tg, x = ~x, y = ~density, name = target_name, inherit = FALSE)
  if (framed) args$frame <- ~frame
  if (disc_target) {
    args <- c(args, list(type = "scatter", mode = "markers",
                         marker = list(color = th$line2, size = 7, line = list(color = th$hover_bg, width = 1)),
                         hovertemplate = "x = %{x}<br>P = %{y:.4f}<extra></extra>"))
  } else {
    args <- c(args, list(type = "scatter", mode = "lines",
                         line = list(color = th$line2, width = 2.5, dash = if (res$kind == "limit") "dash" else "solid"),
                         hovertemplate = "x = %{x:.3f}<br>%{y:.4f}<extra></extra>"))
  }
  p <- do.call(plotly::add_trace, args)
  top <- max(c(tops, body, 0))
  if (!length(body)) top <- max(c(tops, tg$density))
  ylab <- tr(if (res$discrete) "plot.probability" else "plot.density", lang)
  p <- plot_layout(p, tr("plot.x", lang), ylab, th, lang, ylim = c(0, 1.1 * top))
  p <- plotly::layout(p, bargap = 0, xaxis = list(range = res$range + c(-0.5, 0.5) * res$discrete))
  p <- compare_legend(p, th)
  p <- plotly::layout(p, legend = list(y = 1.02, yanchor = "bottom", x = 1, xanchor = "right"),
                      margin = list(l = 64, r = 12, t = 48, b = 40))
  sim_animate(p, res$frames$label, lang, th)
}

# How to read the relation demo, and the share of draws off the plot.
rel_note <- function(res, lang) {
  metric <- res$distance$metric[1]
  notes <- c(
    tr(paste0("sim.rel.note.", res$kind), lang),
    if (res$kind != "limit") tr(paste0("sim.rel.note.band_", metric), lang),
    if (res$kind != "limit" && any(res$frames$outside > 0)) {
      tr("sim.note.outside", lang, pct = format_share(max(res$frames$outside), lang))
    }
  )
  note_list(notes)
}

# Distance per frame (and the 95% noise level for KS distances).
rel_table_tag <- function(res, lang) {
  d <- res$distance
  band <- any(is.finite(d$band))
  rows <- lapply(seq_len(nrow(d)), function(i) {
    shiny::tags$tr(
      shiny::tags$th(scope = "row", class = "dl-series-name", res$frames$label[i]),
      shiny::tags$td(class = "dl-cell-num", format_num(d$value[i], lang)),
      if (band) shiny::tags$td(class = "dl-cell-num", format_num(d$band[i], lang))
    )
  })
  shiny::tags$div(
    class = "table-responsive dl-sim-table",
    shiny::tags$table(
      class = "table dl-table dl-compare-table",
      shiny::tags$caption(tr(paste0("sim.metric.", d$metric[1]), lang)),
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(scope = "col", tr("sim.rel.col.frame", lang)),
        shiny::tags$th(scope = "col", tr("sim.rel.col.distance", lang)),
        if (band) shiny::tags$th(scope = "col", tr(paste0("sim.rel.col.band_", d$metric[1]), lang))
      )),
      shiny::tags$tbody(rows)
    )
  )
}

# Verdict of the memorylessness tab, with the kept draws and the largest gap
# between the two survival curves.
mem_verdict_tag <- function(res, dist, n, lang) {
  gap <- max(abs(res$exact$survival - res$exact$residual))
  yes <- isTRUE(res$memoryless)
  # A special case (Gamma with shape 1, ...) names the memoryless law it is.
  special <- if (yes && !dist$id %in% c("exponential", "geometric")) {
    base <- if (dist$id == "negbinomial") "geometric" else "exponential"
    tr("sim.mem.verdict.special", lang, law = entry_text(dist$entry, "names", lang),
       base = entry_text(dl_entry(base), "names", lang))
  }
  shiny::tags$div(
    class = paste("dl-sim-verdict", if (yes) "dl-sim-verdict-yes" else "dl-sim-verdict-no"),
    role = "status",
    shiny::tags$p(class = "dl-sim-verdict-main",
                  shiny::icon(if (yes) "circle-check" else "circle-xmark", `aria-hidden` = "true"),
                  tr(if (yes) "sim.mem.verdict.yes" else "sim.mem.verdict.no", lang)),
    if (!is.null(special)) shiny::tags$p(special),
    shiny::tags$p(class = "dl-muted",
                  tr("sim.mem.kept", lang, kept = format(res$kept, scientific = FALSE),
                     n = format(n, scientific = FALSE)), " ",
                  tr("sim.mem.gap", lang, gap = format_gap(gap, lang)))
  )
}

# Gap between two exact curves: 0, "< 10^-12" for rounding residue, or the
# value with the decimal mark of the language.
format_gap <- function(g, lang) {
  if (g == 0) return("0")
  if (g < 1e-12) return(paste0("< 10", sup_digits("-12")))
  format_num(g, lang)
}

# Exact survival of X and of the residual after s.
mem_surv_plot <- function(res, dist, lang, th) {
  discrete <- identical(dist$entry$type, "discrete")
  e <- res$exact
  shape <- if (discrete) "hv" else "linear"
  mode <- if (discrete) "lines+markers" else "lines"
  lab_x <- if (discrete) "P(X \u2265 t)" else "P(X > t)"
  lab_r <- if (discrete) "P(X \u2265 s + t | X \u2265 s)" else "P(X > s + t | X > s)"
  curve <- function(p, y, name, color, dash) {
    args <- list(p, x = e$t, y = y, type = "scatter", mode = mode, name = name,
                 line = list(color = color, width = 2.5, dash = dash, shape = shape),
                 hovertemplate = "t = %{x:.3f}<br>%{y:.4f}<extra></extra>")
    # Discrete survival functions get a marker at each whole t.
    if (discrete) args$marker <- list(color = color, size = 5)
    do.call(plotly::add_trace, args)
  }
  p <- plotly::plot_ly()
  p <- curve(p, e$survival, lab_x, th$line2, "solid")
  p <- curve(p, e$residual, lab_r, th$accent, "dash")
  p <- plot_layout(p, "t", tr("sim.mem.ylab", lang), th, lang, ylim = c(0, 1.02))
  compare_legend(p, th)
}
