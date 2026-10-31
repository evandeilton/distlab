#' Run the distlab app
#'
#' Launches the 'shiny' dashboard.
#'
#' @param lang Interface language, one of [dl_languages()].
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
  shiny::addResourcePath("distlab-www",
                         system.file("app", "www", package = "distlab"))
  shiny::shinyApp(ui = app_ui(lang), server = app_server(lang), ...)
}

# Top-level page: navbar with one panel per app section.
app_ui <- function(lang) {
  bslib::page_navbar(
    title = tr("app.title", lang),
    theme = dl_theme(),
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "distlab-www/distlab.css")
    ),
    bslib::nav_panel(tr("nav.explorer", lang), mod_explore_ui("explore", lang)),
    bslib::nav_panel(
      tr("nav.about", lang),
      bslib::card(bslib::card_body(shiny::p(tr("about.text", lang))))
    ),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode(id = "dark_mode"))
  )
}

app_server <- function(lang) {
  function(input, output, session) {
    mod_explore_server("explore", lang)
  }
}

# Provisional theme; colors and fonts move to _brand.yml once the brand is set.
dl_theme <- function() {
  bslib::bs_theme(version = 5, primary = "#1f6f8b", secondary = "#5c6b73")
}
