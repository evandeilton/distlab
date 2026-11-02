#' Run the distlab app
#'
#' Launches the 'shiny' dashboard. The language can also be switched at any
#' time from the navigation bar, keeping the open page, distribution and
#' parameter values.
#'
#' @param lang Initial interface language, one of [dl_languages()].
#' @param ... Further arguments passed to [shiny::shinyApp()], such as
#'   `options = list(port = 8080)`.
#'
#' @returns A `shiny.appobj`; printing it (as the console does) starts the app.
#'
#' @family app
#' @export
#' @examples
#' if (interactive()) {
#'   run_app(lang = "en-US")
#' }
run_app <- function(lang = "pt-BR", ...) {
  lang <- match_lang(lang)
  shiny::shinyApp(ui = app_ui(lang), server = app_server(lang), ...)
}

# Sections of the navbar: value, dictionary key and icon. Sections without a
# module yet show a header only.
.dl_sections <- list(
  list(value = "home", icon = "house"),
  list(value = "compendium", icon = "book-open"),
  list(value = "explorer", icon = "magnifying-glass-chart"),
  list(value = "calculator", icon = "calculator"),
  list(value = "compare", icon = "layer-group"),
  list(value = "simulation", icon = "shuffle"),
  list(value = "fit", icon = "chart-line"),
  list(value = "about", icon = "circle-info")
)

# Top-level page: branded navbar with one panel per app section, plus the
# language and color-mode toggles.
app_ui <- function(lang) {
  # Logo and favicon are served from here; registered with the UI so the app
  # also works when built without run_app().
  shiny::addResourcePath("distlab-www",
                         system.file("app", "www", package = "distlab"))
  panel_body <- function(s) {
    switch(s$value,
           home = mod_home_ui("home", lang),
           compendium = mod_compendium_ui("compendium", lang),
           explorer = mod_explore_ui("explore", lang),
           calculator = mod_calc_ui("calc", lang),
           compare = mod_compare_ui("compare", lang),
           simulation = mod_sim_ui("sim", lang),
           fit = mod_fit_ui("fit", lang),
           about = about_panel(lang),
           upcoming_panel(s, lang))
  }
  panels <- lapply(.dl_sections, function(s) {
    bslib::nav_panel(
      title = shiny::tags$span(class = "dl-nav-label",
                               shiny::icon(s$icon, `aria-hidden` = "true"),
                               i18n_span(paste0("nav.", s$value), lang)),
      value = s$value,
      panel_body(s)
    )
  })
  page <- do.call(bslib::page_navbar, c(
    list(
      id = "nav",
      title = shiny::tags$span(
        class = "dl-brand",
        shiny::tags$img(src = "distlab-www/logo-mark.svg", alt = "", width = "30",
                        height = "30", class = "dl-brand-mark"),
        shiny::tags$span(class = "dl-brand-name", "distlab")
      ),
      window_title = "distlab",
      lang = lang,
      theme = dl_theme(),
      fillable = FALSE,
      navbar_options = bslib::navbar_options(theme = "dark", class = "dl-navbar"),
      header = dl_head()
    ),
    panels,
    list(
      bslib::nav_spacer(),
      bslib::nav_item(lang_toggle(lang)),
      bslib::nav_item(dark_toggle(lang))
    )
  ))
  page
}

# Two-button language switch; distlab.js sends the choice as input$lang.
lang_toggle <- function(lang) {
  btn <- function(code, short, key) {
    shiny::tags$button(
      type = "button", class = "btn dl-lang-btn", `data-lang` = code,
      `aria-pressed` = if (code == lang) "true" else "false",
      `data-i18n-title` = key, title = tr(key, lang),
      `data-i18n-aria-label` = key, `aria-label` = tr(key, lang),
      short
    )
  }
  shiny::tags$div(
    class = "btn-group dl-lang-toggle", role = "group", id = "dl_lang",
    `data-initial` = lang,
    `data-i18n-aria-label` = "nav.language", `aria-label` = tr("nav.language", lang),
    btn("pt-BR", "PT", "nav.lang_pt"),
    btn("en-US", "EN", "nav.lang_en")
  )
}

# Color-mode switch. It starts from the operating system preference; the
# wrapper carries translated labels that distlab.js copies onto the button.
dark_toggle <- function(lang) {
  shiny::tags$span(
    class = "dl-dark-toggle",
    `data-i18n-label-dark` = "nav.to_dark", `data-label-dark` = tr("nav.to_dark", lang),
    `data-i18n-label-light` = "nav.to_light", `data-label-light` = tr("nav.to_light", lang),
    bslib::input_dark_mode(id = "dark_mode")
  )
}

# Header of a section that ships in a later release.
upcoming_panel <- function(s, lang) {
  shiny::tags$section(
    class = "dl-upcoming",
    shiny::tags$span(class = "dl-icon-chip dl-icon-chip-lg",
                     shiny::icon(s$icon, `aria-hidden` = "true")),
    shiny::tags$div(
      shiny::tags$h1(class = "dl-page-title", i18n_span(paste0("nav.", s$value), lang)),
      shiny::tags$p(class = "dl-upcoming-note", i18n_span("upcoming.note", lang))
    )
  )
}

about_panel <- function(lang) {
  version <- as.character(utils::packageVersion("distlab"))
  shiny::tags$section(
    class = "dl-about",
    bslib::card(
      bslib::card_body(
        shiny::tags$div(
          class = "dl-about-head",
          shiny::tags$img(src = "distlab-www/logo-mark.svg", alt = "", width = "56",
                          height = "56"),
          shiny::tags$div(
            shiny::tags$h1(class = "dl-page-title", "distlab"),
            shiny::tags$p(class = "dl-muted",
                          shiny::tags$span(`data-i18n` = "about.version",
                                           `data-i18n-version` = version,
                                           tr("about.version", lang, version = version)))
          )
        ),
        shiny::tags$p(i18n_span("about.text", lang)),
        shiny::tags$p(i18n_span("about.details", lang)),
        shiny::tags$p(class = "dl-muted", i18n_span("about.license", lang))
      )
    )
  )
}

app_server <- function(lang) {
  initial <- match_lang(lang)
  function(input, output, session) {
    # Current language: the navbar toggle, or the initial one until it reports.
    lang_r <- shiny::reactive(coerce_lang(input$lang, initial))
    dark <- shiny::reactive(identical(input$dark_mode, "dark"))

    # One navigation request at a time, from any page: list(tab, id, params,
    # group, edge).
    route <- shiny::reactiveVal(NULL)
    home <- mod_home_server("home", lang_r)
    compendium <- mod_compendium_server("compendium", lang_r, dark = dark, request = route)
    explore <- mod_explore_server("explore", lang_r, dark = dark, select = route)
    calc <- mod_calc_server("calc", lang_r, dark = dark, select = route)
    compare <- mod_compare_server("compare", lang_r, dark = dark, select = route)
    sim <- mod_sim_server("sim", lang_r, dark = dark, select = route)
    fit <- mod_fit_server("fit", lang_r, dark = dark, select = route)
    shiny::observeEvent(home(), route(c(home(), from = "home")))
    shiny::observeEvent(compendium(), route(c(compendium(), from = "compendium")))
    shiny::observeEvent(explore$route(), route(c(explore$route(), from = "explorer")))
    shiny::observeEvent(sim$route(), route(c(sim$route(), from = "simulation")))
    shiny::observeEvent(fit$route(), route(c(fit$route(), from = "fit")))

    # Static texts are re-translated in the browser from the dictionary.
    shiny::observeEvent(lang_r(), {
      session$sendCustomMessage("dl-lang", list(lang = lang_r(),
                                                dict = dl_dictionary(lang_r())))
    }, ignoreInit = TRUE)

    # Navigation requested by Home (search, chips, quick start), by the
    # Compendium, by the Explorer (open the Calculator, Compare or the
    # Simulation) or by the Simulation (open an end of a relation).
    shiny::observeEvent(route(), {
      bslib::nav_select("nav", route()$tab, session = session)
    })
  }
}
