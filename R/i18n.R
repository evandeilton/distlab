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

# Format numbers with the decimal mark of the language.
format_num <- function(x, lang, digits = 4) {
  mark <- if (match_lang(lang) == "pt-BR") "," else "."
  out <- trimws(formatC(x, digits = digits, format = "fg", decimal.mark = mark,
                        big.mark = ""))
  ifelse(is.finite(x), out, "\u2014")
}
