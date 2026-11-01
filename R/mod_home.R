# Home: search, quick-start actions and one card per data-type group.

# Icon of each data-type group.
.dl_group_icons <- c(binary = "toggle-on", counts = "list-ol", bounded = "percent",
                     positive = "hourglass-half", real_line = "arrows-left-right",
                     directional = "compass", multivariate = "table-cells",
                     mixed = "shapes")

mod_home_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "dl-home",
    shiny::tags$section(
      class = "dl-hero",
      shiny::tags$div(
        class = "dl-hero-text",
        shiny::tags$img(src = "distlab-www/logo-mark.svg", alt = "", class = "dl-hero-logo",
                        width = "64", height = "64"),
        shiny::tags$h1(class = "dl-hero-title", tr("app.title", lang)),
        shiny::tags$p(class = "dl-hero-tagline", i18n_span("app.tagline", lang))
      ),
      shiny::tags$div(
        class = "dl-hero-search",
        shiny::selectizeInput(
          ns("search"), i18n_span("home.search_label", lang),
          choices = search_choices(lang), selected = "", width = "100%",
          options = list(placeholder = tr("home.search_placeholder", lang))
        ),
        shiny::tags$div(
          class = "dl-quick", role = "group",
          `aria-labelledby` = ns("quick_label"),
          shiny::tags$span(id = ns("quick_label"), class = "dl-quick-label",
                           i18n_span("home.quick_start", lang)),
          shiny::actionButton(ns("go_normal"), i18n_span("home.explore_normal", lang),
                              icon = shiny::icon("wave-square", `aria-hidden` = "true"),
                              class = "btn-primary"),
          shiny::actionButton(ns("go_fit"), i18n_span("home.fit_data", lang),
                              icon = shiny::icon("chart-line", `aria-hidden` = "true"),
                              class = "btn-outline-primary")
        )
      )
    ),
    shiny::tags$h2(class = "dl-section-title", i18n_span("home.browse", lang)),
    shiny::uiOutput(ns("groups"))
  )
}

# Search choices: every distribution name in `lang`, sorted.
search_choices <- function(lang) {
  reg <- dl_registry()
  labels <- vapply(reg, function(e) entry_text(e, "names", lang), "")
  o <- order(labels, method = "radix")
  c(stats::setNames("", ""), stats::setNames(names(reg)[o], labels[o]))
}

# Data-type groups present in the registry, in their canonical order.
registry_groups <- function(reg = dl_registry()) {
  intersect(.dl_groups, unique(unlist(lapply(reg, entry_groups))))
}

# One card per group with its distributions as clickable chips and, when
# `compendium_id` is given, a link that opens the group in the Compendium.
group_cards <- function(lang, input_id, compendium_id = NULL, reg = dl_registry()) {
  cards <- lapply(registry_groups(reg), function(g) {
    ids <- group_members(g, reg)
    labels <- vapply(ids, function(i) entry_text(reg[[i]], "names", lang), "")
    o <- order(labels, method = "radix")
    bslib::card(
      class = paste0("dl-group-card dl-group-", g),
      bslib::card_body(
        shiny::tags$div(
          class = "dl-group-head",
          shiny::tags$span(class = "dl-icon-chip",
                           shiny::icon(.dl_group_icons[[g]], `aria-hidden` = "true")),
          shiny::tags$div(
            shiny::tags$h3(class = "dl-group-title", tr(paste0("group.", g), lang)),
            shiny::tags$p(class = "dl-group-count",
                          tr(if (length(ids) == 1L) "home.count_one" else "home.count",
                             lang, n = length(ids)))
          )
        ),
        shiny::tags$p(class = "dl-group-desc", tr(paste0("group.", g, ".desc"), lang)),
        shiny::tags$div(
          class = "dl-chips",
          lapply(o, function(k) {
            dist_button(ids[k], labels[k], input_id,
                        label = tr("related.open", lang, name = labels[k]))
          })
        ),
        if (!is.null(compendium_id)) {
          shiny::tags$div(
            class = "dl-group-more",
            dist_button(g, tr("home.in_compendium", lang), compendium_id,
                        class = "dl-link-btn",
                        label = tr("home.in_compendium_group", lang,
                                   group = tr(paste0("group.", g), lang)))
          )
        }
      )
    )
  })
  do.call(bslib::layout_column_wrap,
          c(list(width = "240px", fill = FALSE, class = "dl-groups"), cards))
}

# Returns a reactive with the latest navigation request: list(tab, id, group, n),
# where `n` increases so repeated requests for the same target still fire.
mod_home_server <- function(id, lang) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    route <- shiny::reactiveVal(NULL)
    count <- 0L
    go <- function(tab, did = NULL, group = NULL) {
      count <<- count + 1L
      route(list(tab = tab, id = did, group = group, n = count))
    }

    output$groups <- shiny::renderUI(
      group_cards(lang_r(), session$ns("pick"), session$ns("compendium"))
    )

    shiny::observeEvent(lang_r(), {
      shiny::updateSelectizeInput(
        session, "search", choices = search_choices(lang_r()), selected = "",
        options = list(placeholder = tr("home.search_placeholder", lang_r()))
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$search, {
      if (is_string(input$search) && input$search %in% dl_ids()) {
        go("explorer", input$search)
        shiny::updateSelectizeInput(session, "search", selected = "")
      }
    })
    shiny::observeEvent(input$pick, {
      if (is_string(input$pick) && input$pick %in% dl_ids()) go("explorer", input$pick)
    })
    shiny::observeEvent(input$compendium, {
      if (is_string(input$compendium) && input$compendium %in% .dl_groups) {
        go("compendium", group = input$compendium)
      }
    })
    shiny::observeEvent(input$go_normal, go("explorer", "normal"))
    shiny::observeEvent(input$go_fit, go("fit"))

    route
  })
}
