# Self-contained HTML pages: every script and stylesheet of the page is
# inlined, and the files that stylesheets point to (fonts) become data URIs,
# so one file opens offline without a folder of assets.

# Dependencies left out of reports: icons of shared helpers then render as
# empty elements, hidden by the report stylesheet.
.dl_report_skip_deps <- "font-awesome"

# Media types of the files a stylesheet may point to.
.dl_mime <- c(woff2 = "font/woff2", woff = "font/woff", ttf = "font/ttf", otf = "font/otf",
              eot = "application/vnd.ms-fontobject", svg = "image/svg+xml",
              png = "image/png", gif = "image/gif", jpg = "image/jpeg", jpeg = "image/jpeg")

# Fonts, KaTeX, the report stylesheet and the script that typesets math and
# copies code (distlab.js works without Shiny).
report_dependencies <- function() {
  www <- system.file("app", "www", package = "distlab")
  list(
    katex_dependency(),
    htmltools::htmlDependency(
      name = "distlab-report", version = as.character(utils::packageVersion("distlab")),
      src = c(file = www), script = "distlab.js", stylesheet = c("fonts.css", "report.css")
    )
  )
}

# Text of a file as UTF-8.
read_text <- function(path) {
  out <- readChar(path, file.size(path), useBytes = TRUE)
  Encoding(out) <- "UTF-8"
  out
}

# Base64 on one line: a line break would end a CSS string.
base64_line <- function(raw) gsub("[\r\n]", "", jsonlite::base64_enc(raw))

# Base64 data URI of a file.
data_uri <- function(path, type = NULL) {
  ext <- tolower(sub("^.*\\.", "", basename(path)))
  type <- type %||% unname(.dl_mime[ext])
  if (is.null(type) || is.na(type)) type <- "application/octet-stream"
  paste0("data:", type, ";base64,", base64_line(readBin(path, "raw", file.size(path))))
}

# Base64 data URI of text, for download links.
text_data_uri <- function(text, type) {
  paste0("data:", type, ";charset=utf-8;base64,", base64_line(charToRaw(enc2utf8(text))))
}

# Directory that holds the files of a dependency.
dependency_dir <- function(dep) {
  dir <- dep$src$file
  if (is.null(dir)) {
    stop("The dependency '", dep$name, "' has no local files to inline.", call. = FALSE)
  }
  if (!is.null(dep$package)) dir <- system.file(dir, package = dep$package)
  dir
}

# Stylesheet text with each local url(...) replaced by a data URI; remote
# references are an error, since the page must open offline.
inline_css <- function(path) {
  css <- read_text(path)
  base <- dirname(path)
  pattern <- "url\\(\\s*(['\"]?)([^'\")]+)\\1\\s*\\)"
  m <- gregexpr(pattern, css, perl = TRUE)
  hits <- regmatches(css, m)[[1]]
  if (!length(hits)) return(css)
  regmatches(css, m) <- list(vapply(hits, function(u) {
    target <- sub(pattern, "\\2", u, perl = TRUE)
    if (grepl("^(data:|#)", target)) return(u)
    if (grepl("^([a-z]+:)?//", target, ignore.case = TRUE)) {
      stop("The stylesheet ", basename(path), " refers to a remote file: ", target, call. = FALSE)
    }
    # Query strings and fragments ("font.woff2?v=1#iefix") are not part of the path.
    file <- file.path(base, sub("[?#].*$", "", target))
    if (!file.exists(file)) {
      stop("The stylesheet ", basename(path), " refers to a missing file: ", target, call. = FALSE)
    }
    paste0("url(\"", data_uri(file), "\")")
  }, "", USE.NAMES = FALSE))
  css
}

# Script text that cannot close or comment out its <script> element.
safe_script <- function(js) {
  js <- gsub("</(script)", "<\\\\/\\1", js, ignore.case = TRUE, perl = TRUE)
  gsub("<!--", "<\\!--", js, fixed = TRUE)
}

# Inline <style>, <script> and <meta> tags of one dependency, each marked
# with its name and version.
inline_dependency <- function(dep) {
  dir <- dependency_dir(dep)
  mark <- sprintf(" data-dependency=\"%s@%s\"", dep$name, dep$version)
  file_of <- function(x, field) if (is.list(x)) x[[field]] else x
  meta <- vapply(names(dep$meta), function(nm) {
    sprintf("<meta name=\"%s\" content=\"%s\">", htmltools::htmlEscape(nm, TRUE),
            htmltools::htmlEscape(dep$meta[[nm]], TRUE))
  }, "")
  css <- vapply(as.list(dep$stylesheet), function(s) {
    paste0("<style", mark, ">\n", inline_css(file.path(dir, file_of(s, "href"))), "\n</style>")
  }, "")
  js <- vapply(as.list(dep$script), function(s) {
    paste0("<script", mark, ">\n", safe_script(read_text(file.path(dir, file_of(s, "src")))),
           "\n</script>")
  }, "")
  paste(c(meta, css, js, dep$head), collapse = "\n")
}

# Whole page: rendered body tags with every dependency inlined in the head.
report_page <- function(body, lang, title) {
  rendered <- htmltools::renderTags(body)
  deps <- htmltools::resolveDependencies(c(report_dependencies(), rendered$dependencies))
  deps <- Filter(function(d) !d$name %in% .dl_report_skip_deps, deps)
  head <- vapply(deps, inline_dependency, "")
  enc2utf8(paste0(
    "<!DOCTYPE html>\n<html lang=\"", lang, "\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<meta name=\"color-scheme\" content=\"light\">\n",
    "<title>", htmltools::htmlEscape(title), "</title>\n",
    paste(head, collapse = "\n"), "\n", rendered$head, "\n</head>\n<body>\n",
    rendered$html, "\n</body>\n</html>\n"
  ))
}

# Serializer for report plots: 15 significant digits, so narrow grids far
# from zero keep distinct points, without the 17-digit noise of plotly's
# default.
report_json <- function(x, ...) {
  jsonlite::toJSON(x, digits = NA, auto_unbox = TRUE, force = TRUE, null = "null",
                   na = "null", ...)
}

# A plotly figure for the page: built now, without the copy of the data that
# plotly keeps for the R side, with a fixed element id and height.
report_widget <- function(p, id, height = 340) {
  b <- plotly::plotly_build(p)
  b$x$attrs <- NULL
  b$x$visdat <- NULL
  b$x$cur_data <- NULL
  attr(b$x, "TOJSON_FUNC") <- report_json
  b$elementId <- id
  b$width <- "100%"
  b$height <- height
  b
}
