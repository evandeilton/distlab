# Helpers for the study report tests.

# Reports built once per test run and reused: default simulations and
# relation demos, two calculator questions.
.report_cache <- new.env(parent = emptyenv())
cached_report <- function(id, lang, params = list()) {
  key <- paste(id, lang, paste(names(params), unlist(params), collapse = ","), sep = "|")
  if (is.null(.report_cache[[key]])) {
    d <- do.call(dl_dist, c(list(id), params))
    .report_cache[[key]] <- dl_report(
      d, lang = lang,
      calculations = list(list(mode = "le", x = 2), list(mode = "quantile", p = 0.9))
    )
  }
  .report_cache[[key]]
}

# Code blocks of the appendix: kind and R code, with HTML entities decoded.
report_code_blocks <- function(html) {
  m <- regmatches(html, gregexpr('<code id="code-text-[0-9]+" data-block="[a-z_]+">[\\s\\S]*?</code>',
                                 html, perl = TRUE))[[1]]
  kind <- sub('^<code id="code-text-[0-9]+" data-block="([a-z_]+)">[\\s\\S]*$', "\\1", m, perl = TRUE)
  code <- sub('^<code[^>]*>([\\s\\S]*)</code>$', "\\1", m, perl = TRUE)
  code <- gsub("&lt;", "<", code, fixed = TRUE)
  code <- gsub("&gt;", ">", code, fixed = TRUE)
  code <- gsub("&#39;", "'", code, fixed = TRUE)
  code <- gsub("&quot;", "\"", code, fixed = TRUE)
  code <- gsub("&amp;", "&", code, fixed = TRUE)
  data.frame(kind = kind, code = code, stringsAsFactors = FALSE)
}

# Evaluate a block in a fresh environment, keeping the caller's random
# stream; the environment holds the value of the last expression as `.value`.
eval_block <- function(code) {
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", old, envir = genv) else
    if (exists(".Random.seed", envir = genv, inherits = FALSE)) rm(".Random.seed", envir = genv))
  env <- new.env(parent = genv)
  env$.value <- eval(parse(text = code), envir = env)
  env
}

# TRUE when `text` appears as the whole content of an element (">text<").
shown <- function(html, text) grepl(paste0(">", text, "<"), html, fixed = TRUE)
