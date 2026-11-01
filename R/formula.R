# Formula sheet: HTML built from the `latex` field of a registry entry. Math
# goes into `.dl-math` elements as TeX source; distlab.js renders it with
# KaTeX in the browser.

# Left-hand side of each formula spec.
.dl_formula_lhs <- c(
  quantile = "F^{-1}(u)",
  mean = "\\operatorname{E}(X)",
  variance = "\\operatorname{Var}(X)",
  skewness = "\\gamma_1",
  excess_kurtosis = "\\gamma_2",
  raw_moment = "\\operatorname{E}\\left(X^{j}\\right)",
  mgf = "M_X(t)",
  cf = "\\varphi_X(t)",
  pgf = "G_X(z)",
  median = "\\operatorname{Med}(X)",
  mode = "\\operatorname{Mo}(X)",
  entropy = "H(X)"
)

# Formula keys shown in each tab of the sheet.
formula_tabs <- function(type) {
  list(
    functions = c("pdf", "cdf", "quantile"),
    moments = c("mean", "variance", "skewness", "excess_kurtosis", "raw_moment"),
    generating = c("mgf", "cf", if (identical(type, "discrete")) "pgf"),
    summaries = c("median", "mode", "entropy")
  )
}

# Inline or display math element; TeX is kept as text and rendered client-side.
math_tag <- function(tex, display = FALSE, class = NULL) {
  shiny::tags$span(class = paste(c("dl-math", class), collapse = " "),
                   `data-display` = if (display) "true" else "false", tex)
}

# Decimal comma inside TeX for pt-BR ("0{,}5").
tex_decimal <- function(tex, lang) {
  if (match_lang(lang) == "pt-BR") gsub("([0-9])\\.([0-9])", "\\1{,}\\2", tex) else tex
}

# Normalize a formula spec to a list of pieces.
formula_pieces <- function(spec) {
  if (is.null(spec)) return(list())
  if (is.list(spec) && !is.null(names(spec))) list(spec) else spec
}

# One piece of a formula: the math (or a translated status) plus its condition.
formula_piece_tag <- function(piece, lhs, lang) {
  expr <- piece[["expr"]]
  status <- piece[["status"]]
  body <- if (identical(status, "infinite")) {
    math_tag(tex_decimal(paste(lhs, "= \\infty"), lang), display = TRUE)
  } else if (identical(status, "undefined")) {
    shiny::tags$span(class = "dl-formula-status",
                     math_tag(lhs), " ",
                     shiny::tags$span(class = "badge dl-badge-muted",
                                      tr("formula.undefined", lang)))
  } else if (is.null(expr)) {
    shiny::tags$span(class = "dl-formula-status",
                     math_tag(lhs), " ",
                     shiny::tags$span(class = "badge dl-badge-muted",
                                      tr("formula.no_closed_form", lang)))
  } else {
    tex <- if (is.null(lhs)) expr else paste(lhs, "=", expr)
    math_tag(tex_decimal(tex, lang), display = TRUE)
  }
  cond <- piece[["condition"]]
  shiny::tags$div(
    class = "dl-piece",
    shiny::tags$div(class = "dl-piece-math", body),
    if (!is.null(cond)) {
      shiny::tags$div(class = "dl-piece-cond",
                      shiny::tags$span(class = "dl-cond-word", tr("formula.if", lang)),
                      " ", math_tag(tex_decimal(cond, lang)))
    }
  )
}

# One row of the sheet: translated name and every piece of the formula.
formula_row <- function(entry, key, lang) {
  spec <- entry$latex[[key]]
  if (key %in% c("pdf", "cdf")) {
    pieces <- list(list(expr = spec))
    lhs <- NULL
  } else {
    pieces <- formula_pieces(spec)
    lhs <- .dl_formula_lhs[[key]]
  }
  if (!length(pieces)) return(NULL)
  label_key <- if (key == "pdf" && identical(entry$type, "discrete")) "pmf" else key
  shiny::tags$div(
    class = "dl-formula",
    shiny::tags$div(class = "dl-formula-label", tr(paste0("formula.", label_key), lang)),
    shiny::tags$div(class = "dl-formula-body",
                    lapply(pieces, formula_piece_tag, lhs = lhs, lang = lang))
  )
}

# All rows of one tab of the sheet.
formula_tab_content <- function(entry, tab, lang) {
  keys <- formula_tabs(entry$type)[[tab]]
  rows <- lapply(keys, function(k) formula_row(entry, k, lang))
  shiny::tags$div(class = "dl-formula-list", Filter(Negate(is.null), rows))
}
