# Brand, theme and front-end assets of the app.

# Parsed inst/brand/_brand.yml, cached.
dl_brand <- function() {
  if (is.null(.dl_env$brand)) {
    path <- system.file("brand", "_brand.yml", package = "distlab")
    .dl_env$brand <- yaml::read_yaml(path)
  }
  .dl_env$brand
}

# Hex value of a brand color, resolving palette names (e.g. "indigo").
brand_color <- function(name) {
  col <- dl_brand()$color
  value <- col[[name]] %||% col$palette[[name]] %||% name
  col$palette[[value]] %||% value
}

# Colorblind-safe categorical palette for plots with several series.
dl_palette <- function(n = NULL) {
  pal <- unlist(dl_brand()$defaults$distlab$categorical)
  if (is.null(n)) pal else rep_len(pal, n)
}

# Bootstrap 5.3 theme built from the brand file. Font names only: the font
# files are vendored and declared in fonts.css, so nothing is downloaded.
dl_theme <- function() {
  b <- dl_brand()
  dark <- b$defaults$distlab$dark
  theme <- bslib::bs_theme(
    version = 5,
    primary = brand_color("primary"),
    secondary = brand_color("secondary"),
    warning = brand_color("warning"),
    info = brand_color("teal"),
    bg = brand_color("background"),
    fg = brand_color("foreground"),
    base_font = bslib::font_collection(b$typography$base, "system-ui",
                                       "-apple-system", "Segoe UI", "sans-serif"),
    heading_font = bslib::font_collection(b$typography$headings$family,
                                          b$typography$base, "sans-serif"),
    code_font = bslib::font_collection(b$typography$monospace, "ui-monospace",
                                       "SFMono-Regular", "monospace"),
    "headings-font-weight" = b$typography$headings$weight,
    "border-radius" = "0.6rem",
    "border-radius-lg" = "0.9rem",
    "card-border-radius" = "0.9rem",
    "link-decoration" = "none",
    "focus-ring-color" = "rgba(20, 163, 160, 0.45)"
  )
  # Dark color mode: Bootstrap reads these *-dark variables.
  bslib::bs_add_variables(
    theme,
    "body-bg-dark" = brand_color(dark$background),
    "body-color-dark" = "#E6E7F5",
    "body-secondary-bg-dark" = brand_color(dark$card),
    "body-tertiary-bg-dark" = "#181A38",
    "border-color-dark" = "#2E3160",
    "link-color-dark" = "#A9ACF2",
    "link-hover-color-dark" = "#C9CBF7",
    "headings-color-dark" = "#F1F2FA"
  )
}

# Head tags and dependencies shared by every page: favicon, fonts, KaTeX,
# the app stylesheet and the client-side script.
dl_head <- function() {
  shiny::tagList(
    shiny::tags$link(rel = "icon", type = "image/svg+xml",
                     href = "distlab-www/favicon.svg"),
    shiny::tags$meta(name = "color-scheme", content = "light dark"),
    katex_dependency(),
    distlab_dependency()
  )
}

# Vendored KaTeX (MIT), rendered client-side by distlab.js.
katex_dependency <- function() {
  htmltools::htmlDependency(
    name = "katex", version = "0.16.47",
    src = c(file = system.file("app", "www", "katex", package = "distlab")),
    script = "katex.min.js", stylesheet = "katex.min.css",
    all_files = TRUE
  )
}

# App fonts, styles and the script that renders math, switches language and
# routes chip clicks.
distlab_dependency <- function() {
  htmltools::htmlDependency(
    name = "distlab", version = as.character(utils::packageVersion("distlab")),
    src = c(file = system.file("app", "www", package = "distlab")),
    script = "distlab.js", stylesheet = c("fonts.css", "distlab.css"),
    all_files = TRUE
  )
}
