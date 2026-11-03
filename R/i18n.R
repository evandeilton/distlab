#' Supported interface languages
#'
#' @returns A character vector of language tags.
#'
#' @family app
#' @export
#' @examples
#' dl_languages()
dl_languages <- function() .dl_langs

# Load (and cache) the dictionary for one language.
dl_dictionary <- function(lang) {
  lang <- match_lang(lang)
  key <- paste0("dict_", lang)
  if (is.null(.dl_env[[key]])) {
    path <- system.file("i18n", paste0(lang, ".yml"), package = "distlab")
    .dl_env[[key]] <- yaml::read_yaml(path)
  }
  .dl_env[[key]]
}

# Translate a key; `{name}` placeholders are filled from `...`.
# A missing key returns the key itself so gaps are visible, never an error.
tr <- function(key, lang, ...) {
  text <- dl_dictionary(lang)[[key]]
  if (is.null(text)) return(key)
  vals <- list(...)
  for (nm in names(vals)) {
    text <- gsub(paste0("{", nm, "}"), paste(format(vals[[nm]]), collapse = ", "),
                 text, fixed = TRUE)
  }
  text
}

# Text of a registry field (names, story) in the requested language.
entry_text <- function(entry, field, lang) {
  entry[[field]][[match_lang(lang)]]
}

match_lang <- function(lang) {
  if (!is_string(lang) || !lang %in% .dl_langs) {
    stop("`lang` must be one of: ", paste(.dl_langs, collapse = ", "), ".",
         call. = FALSE)
  }
  lang
}

# Numbers for display, with the decimal mark of the language: up to
# `digits` significant digits and never more than 4 decimal places. A nonzero
# value below 0.0001, where 4 decimal places keep no significant digit, shows
# as a power of ten with 2 significant digits, such as 1.3 x 10^-40 written
# with the times sign and superscripts. NA and infinite values show as a dash.
format_num <- function(x, lang, digits = 4) {
  mark <- if (match_lang(lang) == "pt-BR") "," else "."
  out <- rep("\u2014", length(x))
  ok <- is.finite(x)
  tiny <- ok & x != 0 & abs(x) < 1e-4
  # Decimal places the significant digits need; beyond 4, round at 4.
  wide <- ok & !tiny & x != 0 & digits - 1 - floor(log10(abs(x))) > 4
  plain <- ok & !tiny & !wide
  out[plain] <- trimws(formatC(x[plain] + 0, digits = digits, format = "fg",
                               decimal.mark = mark, big.mark = ""))
  out[wide] <- drop_zeros(formatC(x[wide], digits = 4, format = "f", decimal.mark = mark),
                          mark)
  out[tiny] <- format_power(x[tiny], mark)
  out
}

# Fixed-point text without trailing zeros after the decimal mark.
drop_zeros <- function(s, mark) {
  has <- grepl(mark, s, fixed = TRUE)
  s[has] <- sub(paste0("\\", mark, "$"), "", sub("0+$", "", s[has]))
  s
}

# Sign, mantissa rounded to `digits` significant digits and exponent of
# nonzero finite numbers; the mantissa is taken from log10, so subnormal
# values keep their size.
power_parts <- function(x, digits = 2) {
  l <- log10(abs(x))
  e <- floor(l)
  m <- round(10^(l - e), digits - 1)
  # Rounding up to 10 moves to the next power.
  up <- m >= 10
  m[up] <- m[up] / 10
  e[up] <- e[up] + 1
  list(negative = x < 0, mantissa = m, exponent = e)
}

# Powers of ten in plain text, in the style of the Calculator.
format_power <- function(x, mark, digits = 2) {
  p <- power_parts(x, digits)
  paste0(ifelse(p$negative, "-", ""),
         formatC(p$mantissa, digits = digits - 1, format = "f", decimal.mark = mark),
         " \u00d7 10", sup_digits(formatC(p$exponent, format = "d")))
}

# Format a moment for display: an infinite moment shows as the infinity sign,
# one that does not exist (NA) as translated text.
format_moment <- function(x, lang) {
  if (is.na(x)) return(tr("moments.undefined", lang))
  if (is.infinite(x)) return(if (x > 0) "\u221e" else "-\u221e")
  format_num(x, lang)
}

# Text element that the client re-translates on a language switch: the
# span carries its dictionary key, and distlab.js replaces its text.
i18n_span <- function(key, lang, ...) {
  shiny::tags$span(`data-i18n` = key, tr(key, lang), ...)
}

# Language from a client input, falling back to `default` when unset or unknown.
coerce_lang <- function(x, default) {
  if (is_string(x) && x %in% .dl_langs) x else default
}
