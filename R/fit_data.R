#' Prepare observations for distribution fitting
#'
#' Raw observations and value/frequency tables are stored without expanding
#' repeated observations. Grouped observations refer to intervals `(lower,
#' upper]`; their midpoints are used only for approximate descriptives.
#'
#' @param x Numeric observations or values in a frequency table.
#' @param frequency Nonnegative whole-number frequencies.
#' @param lower,upper Numeric class boundaries for grouped observations.
#'   Infinite outer boundaries are allowed. Classes must be ordered and must
#'   not overlap; gaps are allowed and are not conditioned away in fitting.
#'   A class with a positive frequency needs both boundaries: use `-Inf` or
#'   `Inf` for an open class, not `NA`.
#' @param na.rm Remove incomplete rows. For raw data `removed` counts missing
#'   observations; for tables it counts incomplete rows, and
#'   `removed_frequency` adds up the known frequencies of those rows.
#'
#' @returns A `dl_fit_data` list with `type`, `x`, `counts`, `lower`, `upper`,
#'   `n`, `removed` and `removed_frequency`. Counts and total sample size
#'   cannot exceed `2^53 - 1`.
#' @export
#' @examples
#' dl_fit_data(c(1, 2, 2, NA))
#' dl_fit_data(lower = c(0, 1), upper = c(1, 2), frequency = c(3, 5))
dl_fit_data <- function(x = NULL, frequency = NULL, lower = NULL, upper = NULL,
                        na.rm = TRUE) {
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }
  grouped <- !is.null(lower) || !is.null(upper)
  type <- if (grouped) "grouped" else if (is.null(frequency)) "raw" else "frequency"
  if (grouped && (!is.null(x) || is.null(lower) || is.null(upper) ||
                  is.null(frequency))) {
    stop("Grouped data require `lower`, `upper` and `frequency`, without `x`.",
         call. = FALSE)
  }
  values <- if (grouped) list(lower = lower, upper = upper, frequency = frequency) else
    c(list(x = x), if (!is.null(frequency)) list(frequency = frequency))
  numeric_vector <- function(v) is.numeric(v) && is.null(dim(v)) && !is.object(v)
  if (!all(vapply(values, numeric_vector, logical(1))) ||
      !length(values[[1]]) || length(unique(lengths(values))) != 1L) {
    stop("Data must be nonempty numeric vectors of equal length.", call. = FALSE)
  }
  # A missing class limit would silently drop its observations; open classes
  # are written with infinite limits instead.
  if (grouped && any((is.na(lower) | is.na(upper)) & !is.na(frequency) & frequency > 0)) {
    stop("Grouped classes with a positive frequency need both limits; use -Inf or Inf for an open class.",
         call. = FALSE)
  }
  # Validate every known frequency, including those of rows removed below.
  if (type != "raw") {
    known <- frequency[!is.na(frequency)]
    if (any(!is.finite(known) | known < 0 | known != floor(known)) ||
        sum(known) > 2^53 - 1) {
      stop("Frequencies must be finite nonnegative whole numbers with total < 2^53.",
           call. = FALSE)
    }
  }
  missing <- Reduce(`|`, lapply(values, is.na))
  if (any(missing) && !na.rm) stop("Missing data; use `na.rm = TRUE`.", call. = FALSE)
  removed <- sum(missing)
  removed_frequency <- if (type == "raw") removed else
    sum(frequency[missing & !is.na(frequency)])
  values <- lapply(values, function(v) v[!missing])
  if (!length(values[[1]])) stop("No complete observations remain.", call. = FALSE)
  counts <- if (type != "raw") values$frequency else rep(1, length(values$x))
  if (sum(counts) == 0) stop("At least one frequency must be positive.", call. = FALSE)
  if (grouped) {
    lower <- values$lower
    upper <- values$upper
    if (any(lower >= upper) || any(lower == Inf | upper == -Inf) ||
        (length(lower) > 1L && any(lower[-1] < upper[-length(upper)]))) {
      stop("Grouped intervals must satisfy lower < upper, be ordered and not overlap.",
           call. = FALSE)
    }
    x <- lower / 2 + upper / 2
    x[!is.finite(x)] <- NA_real_
  } else {
    x <- values$x
    if (any(!is.finite(x))) stop("Observations must be finite (no Inf).", call. = FALSE)
    # Aggregate duplicates before any numerical work, even for raw data.
    ord <- order(x)
    x <- x[ord]
    counts <- counts[ord]
    group <- cumsum(c(TRUE, diff(x) != 0))
    counts <- as.numeric(rowsum(counts, group, reorder = FALSE))
    x <- x[!duplicated(x)]
    positive <- counts > 0
    x <- x[positive]
    counts <- counts[positive]
    lower <- upper <- NULL
  }
  structure(list(type = type, x = x, counts = as.numeric(counts), lower = lower,
                 upper = upper, n = sum(counts), removed = removed,
                 removed_frequency = as.numeric(removed_frequency)),
            class = "dl_fit_data")
}

# Type-7 sample quantiles with compressed integer frequencies.
fit_weighted_quantile <- function(x, counts, p) {
  n <- sum(counts)
  cumulative <- cumsum(counts)
  at <- function(rank) x[findInterval(rank - 1, cumulative) + 1L]
  h <- (n - 1) * p + 1
  lo <- floor(h)
  frac <- h - lo
  a <- at(lo)
  b <- at(pmin(lo + 1, n))
  (1 - frac) * a + frac * b
}

#' Describe observations prepared for fitting
#'
#' Frequency tables have exact sample descriptives. Grouped tables use
#' midpoints and are explicitly approximate; moments and quantiles are
#' unavailable when a positive-frequency class has an infinite boundary.
#'
#' @param data A [dl_fit_data()] object or raw numeric vector.
#' @returns A named list. `removed` and `removed_frequency` repeat the
#'   counts of incomplete entries from [dl_fit_data()]. Variance is the sample
#'   variance; skewness and excess `kurtosis` use population central moments.
#'   For grouped tables, zeros, negatives, ties and the integer flag are
#'   unavailable.
#' @export
#' @examples
#' dl_fit_describe(dl_fit_data(c(1, 2, 2, 4)))
dl_fit_describe <- function(data) {
  if (!inherits(data, "dl_fit_data")) data <- dl_fit_data(data)
  keep <- data$counts > 0
  x <- data$x[keep]
  w <- data$counts[keep]
  n <- data$n
  grouped <- data$type == "grouped"
  out <- list(n = n, removed = data$removed,
              removed_frequency = data$removed_frequency %||% data$removed,
              unique = if (grouped) NA_real_ else length(x),
              ties = if (grouped) NA_real_ else n - length(x),
              zeros = if (grouped) NA_real_ else sum(w[x == 0]),
              negatives = if (grouped) NA_real_ else sum(w[x < 0]),
              integer = if (grouped) NA else all(x == floor(x)),
              mean = NA_real_, variance = NA_real_, sd = NA_real_, min = NA_real_,
              q1 = NA_real_, median = NA_real_, q3 = NA_real_, max = NA_real_,
              skewness = NA_real_, kurtosis = NA_real_, approximate = grouped)
  if (anyNA(x)) return(out)
  mean <- sum(x * (w / n))
  centered <- x - mean
  variance <- if (n > 1) sum((w / (n - 1)) * centered^2) else NA_real_
  m2 <- sum((w / n) * centered^2)
  q <- fit_weighted_quantile(x, w, c(.25, .5, .75))
  out[c("mean", "variance", "sd", "min", "q1", "median", "q3", "max")] <-
    list(mean, variance, sqrt(variance), min(x), q[1], q[2], q[3], max(x))
  if (is.finite(m2) && m2 > 0) {
    z <- centered / sqrt(m2)
    out$skewness <- sum((w / n) * z^3)
    out$kurtosis <- sum((w / n) * z^4) - 3
  }
  out
}

# Largest upload or pasted table, in bytes; the default request limit of 'shiny'.
fit_input_limit <- 5 * 1024^2

# Read uploaded CSV/TSV or pasted data. fread detects semicolon delimiters
# together with decimal commas; explicit decimal_mark is useful for ambiguous
# one-column text. No shell commands or remote URLs are accepted.
fit_read_table <- function(text = NULL, path = NULL, decimal_mark = "auto") {
  if (is.null(text) == is.null(path)) {
    stop("Supply exactly one of `text` or `path`.", call. = FALSE)
  }
  if (!is_string(decimal_mark) || !decimal_mark %in% c("auto", ".", ",")) {
    stop("`decimal_mark` must be 'auto', '.' or ','.", call. = FALSE)
  }
  if (!is.null(path)) {
    if (!is_string(path) || !file.exists(path) || dir.exists(path)) {
      stop("Upload path does not identify a readable file.", call. = FALSE)
    }
    if (file.info(path)$size > fit_input_limit) stop("Input exceeds 5 MB.", call. = FALSE)
    text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(trimws(text))) {
    stop("Paste a nonempty numeric table.", call. = FALSE)
  }
  if (nchar(text, type = "bytes") > fit_input_limit) stop("Input exceeds 5 MB.", call. = FALSE)
  # A byte order mark from spreadsheet exports is not part of the header.
  text <- sub("^\ufeff", "", text)
  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  filled <- which(nzchar(lines))
  lines <- lines[min(filled):max(filled)]
  marks <- if (decimal_mark == "auto") c(",", ".") else decimal_mark
  single <- fit_single_column(lines, marks)
  if (!is.null(single)) return(single)
  if (length(lines) == 1L) {
    across <- fit_one_line(lines, decimal_mark)
    if (!is.null(across)) return(across)
  }
  # fread recovers from ragged rows by inventing or dropping columns; here a
  # ragged table is an input error, never a silently different table. The
  # warning is recorded and fread is left to finish its own cleanup.
  ragged <- FALSE
  out <- withCallingHandlers(
    data.table::fread(text = text, dec = decimal_mark, data.table = FALSE,
                      na.strings = c("", "NA", "NaN"), check.names = TRUE,
                      showProgress = FALSE),
    warning = function(w) {
      if (grepl("column names but the data has|Stopped early|Discarded single-line footer",
                conditionMessage(w))) {
        ragged <<- TRUE
        invokeRestart("muffleWarning")
      }
    })
  if (ragged) {
    stop("Rows have different numbers of fields; check the delimiter and decimal mark.",
         call. = FALSE)
  }
  if (!nrow(out) || !ncol(out)) stop("No data rows found.", call. = FALSE)
  # Text columns may still be numbers with thousands separators.
  for (j in seq_along(out)) {
    if (!is.character(out[[j]])) next
    for (mark in fit_mark_order(out[[j]], decimal_mark)) {
      parsed <- fit_parse_numbers(out[[j]], mark)
      if (!is.null(parsed)) {
        out[[j]] <- parsed
        break
      }
    }
  }
  out
}

# Decimal marks to try, in order. In automatic mode a comma anywhere makes the
# decimal comma the first reading, as fread does for semicolon tables.
fit_mark_order <- function(tokens, decimal_mark) {
  if (decimal_mark != "auto") return(decimal_mark)
  if (any(grepl(",", tokens, fixed = TRUE))) c(",", ".") else c(".", ",")
}

# Numbers written with decimal mark `mark`. The other mark may separate
# thousands in well-formed groups of three ("1.234,5" or "1,234.5"). NULL
# unless every token is a number, a missing value or an infinite value.
fit_parse_numbers <- function(tokens, mark) {
  s <- trimws(tokens)
  missing <- is.na(s) | s %in% c("", "NA", "NaN")
  infinite <- s %in% c("Inf", "+Inf", "-Inf")
  group <- if (mark == ",") "." else ","
  g <- if (mark == ",") "\\." else ","
  d <- if (mark == ",") "," else "\\."
  int <- paste0("([0-9]{1,3}(", g, "[0-9]{3})+|[0-9]+)")
  pattern <- paste0("^[+-]?(", int, "(", d, "[0-9]+)?|", d, "[0-9]+)([eE][+-]?[0-9]+)?$")
  if (!all(missing | infinite | grepl(pattern, s, perl = TRUE))) return(NULL)
  clean <- gsub(group, "", s[!missing], fixed = TRUE)
  if (mark == ",") clean <- chartr(",", ".", clean)
  out <- rep(NA_real_, length(s))
  out[!missing] <- as.numeric(clean)
  out
}

# A newline-only column of decimal commas has no field delimiter, and fread
# would take its commas for one. Read it directly, with an optional header.
fit_single_column <- function(lines, marks) {
  if (any(grepl("[;\t]", lines)) || !any(grepl(",", lines, fixed = TRUE))) return(NULL)
  header <- !grepl(",", lines[1], fixed = TRUE) &&
    all(vapply(marks, function(m) is.null(fit_parse_numbers(lines[1], m)), logical(1)))
  body <- if (header) lines[-1] else lines
  if (!length(body) || !any(grepl(",", body, fixed = TRUE))) return(NULL)
  for (mark in marks) {
    values <- fit_parse_numbers(body, mark)
    if (!is.null(values)) {
      name <- if (header) make.names(gsub('^"|"$', "", lines[1])) else "V1"
      return(stats::setNames(data.frame(values), name))
    }
  }
  NULL
}

# One line of numbers is a single sample written across rather than down.
# Delimiters are tried in order; a comma never separates decimal-comma values.
fit_one_line <- function(line, decimal_mark) {
  for (sep in c(";", "\t", ",", " ")) {
    tokens <- if (sep == " ") strsplit(line, "[[:space:]]+")[[1]] else
      strsplit(line, sep, fixed = TRUE)[[1]]
    if (length(tokens) < 2L) next
    for (mark in setdiff(fit_mark_order(tokens, decimal_mark), if (sep == ",") ",")) {
      values <- fit_parse_numbers(tokens, mark)
      if (!is.null(values)) return(data.frame(V1 = values))
    }
  }
  NULL
}

#' Suggest fitting candidates from observation support
#'
#' Eligibility checks support only, and does not guarantee existence of an
#' MLE. Some eligible families require fixed structural parameters in
#' [dl_fit()]. Grouped classes are checked for possible support overlap,
#' without pretending that their midpoints are exact observations.
#'
#' @param data A [dl_fit_data()] object or raw numeric vector.
#' @returns A data frame with `id`, `type`, `eligible`, `reason` and
#'   `reason_key` (a stable translation key).
#' @export
#' @examples
#' dl_fit_candidates(c(0, 1, 2, 3))
dl_fit_candidates <- function(data) {
  if (!inherits(data, "dl_fit_data")) data <- dl_fit_data(data)
  cat <- dl_catalog()
  out <- cat[, c("id", "type")]
  out$eligible <- TRUE
  out$reason <- out$reason_key <- ""
  grouped <- data$type == "grouped"
  positive <- data$counts > 0
  x <- data$x[positive]
  lo <- data$lower[positive]
  hi <- data$upper[positive]
  for (i in seq_len(nrow(out))) {
    id <- out$id[i]
    key <- reason <- ""
    if (out$type[i] == "discrete" &&
        if (grouped) any(floor(hi) <= floor(lo)) else any(x != floor(x))) {
      key <- if (grouped) "interval_integer" else "integer"
      reason <- "Discrete candidates require whole-number observations or a whole number in each class."
    } else if (id == "bernoulli" &&
               if (grouped) any(!((lo < 0 & hi >= 0) | (lo < 1 & hi >= 1))) else
                 any(!x %in% c(0, 1))) {
      key <- "binary"
      reason <- "Bernoulli data must be 0 or 1."
    } else if (id %in% c("poisson", "geometric", "negbinomial", "binomial",
                         "betabinomial", "hypergeometric") &&
               if (grouped) any(hi < 0) else any(x < 0)) {
      key <- "nonnegative"
      reason <- "This family requires nonnegative observations."
    } else if (id == "beta" &&
               if (grouped) any(hi <= 0 | lo >= 1) else any(x <= 0 | x >= 1)) {
      key <- "unit"
      reason <- "Beta fitting requires observations strictly between 0 and 1."
    } else if (id %in% c("exponential", "gamma", "chisq", "weibull", "lognormal", "f") &&
               if (grouped) any(hi <= 0) else
                 any(if (id == "exponential") x < 0 else x <= 0)) {
      key <- if (id == "exponential") "nonnegative" else "positive"
      reason <- if (id == "exponential") "This family requires nonnegative observations." else
        "This family requires strictly positive observations."
    }
    if (nzchar(key)) out$eligible[i] <- FALSE
    if (!nzchar(key) && id %in% c("binomial", "betabinomial", "hypergeometric")) {
      key <- "fixed_required"
      reason <- if (id == "hypergeometric") "Fix m, n and k before fitting." else
        "Fix the number of trials (size) before fitting."
    }
    if (!nzchar(key) && grouped && id %in% c("uniform", "discrete_uniform")) {
      key <- "grouped_fixed"
      reason <- "Grouped uniform models require fixed min and max."
    }
    out$reason[i] <- reason
    out$reason_key[i] <- if (nzchar(key)) paste0("fit.reason_", key) else ""
  }
  out
}
