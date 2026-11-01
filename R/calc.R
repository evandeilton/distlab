# Probability calculator: results, R code and number formatting, without
# shiny, so every piece can be tested on its own.

.dl_calc_modes <- c("le", "gt", "between", "quantile", "central")

# Number typed by the student, read with the marks of the language: the
# decimal mark is "," in pt-BR and "." in en-US, the other mark separates
# thousands. With both marks, the last one must be the decimal mark
# ("1.234,5" in pt-BR, "1,234.5" in en-US). A lone thousands mark followed by
# groups of exactly three digits (lead group 1-9, up to three digits) is a
# thousands separator ("1.234" is 1234 in pt-BR) and the result carries the
# attribute `thousands = TRUE`; any other lone mark is taken as a decimal
# mark. A Unicode minus and "Inf" or the infinity sign are accepted. NA when
# the text is not a number; NA with attribute `reason = "range"` when it
# overflows the double range ("1e400").
parse_num <- function(x, lang) {
  if (is.numeric(x) && length(x) == 1L) return(as.numeric(x))
  if (!is_string(x)) return(NA_real_)
  s <- gsub("\\s+", "", x)
  s <- gsub("\u2212", "-", s, fixed = TRUE)
  s <- sub("^\\+", "", s)
  low <- tolower(s)
  if (low %in% c("inf", "\u221e")) return(Inf)
  if (low %in% c("-inf", "-\u221e")) return(-Inf)
  pt <- match_lang(lang) == "pt-BR"
  dec <- if (pt) "," else "."
  sep <- if (pt) "." else ","
  # Regex-quoted marks.
  qdec <- paste0("\\", dec)
  qsep <- paste0("\\", sep)
  exp_part <- "([eE][-+]?[0-9]+)?"
  grouped <- paste0("-?[1-9][0-9]{0,2}(", qsep, "[0-9]{3})+")
  has_dec <- grepl(dec, s, fixed = TRUE)
  has_sep <- grepl(sep, s, fixed = TRUE)
  thousands <- FALSE
  if (has_dec && has_sep) {
    # Thousands groups first, then one decimal mark at the end.
    if (!grepl(paste0("^", grouped, qdec, "[0-9]+", exp_part, "$"), s)) return(NA_real_)
    s <- gsub(sep, "", s, fixed = TRUE)
  } else if (has_sep) {
    if (grepl(paste0("^", grouped, "$"), s)) {
      s <- gsub(sep, "", s, fixed = TRUE)
      thousands <- TRUE
    } else {
      s <- sub(sep, dec, s, fixed = TRUE)
    }
  }
  s <- gsub(dec, ".", s, fixed = TRUE)
  if (!grepl("^-?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", s)) {
    return(NA_real_)
  }
  v <- as.numeric(s)
  if (!is.finite(v)) return(structure(NA_real_, reason = "range"))
  if (thousands) attr(v, "thousands") <- TRUE
  v
}

# List of numbers separated by semicolons or spaces (and commas in en-US,
# where the comma is not the decimal mark). NA marks entries that are not
# numbers.
parse_num_list <- function(x, lang) {
  if (!is_string(x)) return(numeric(0))
  sep <- if (match_lang(lang) == "pt-BR") "[;[:space:]]+" else "[;,[:space:]]+"
  parts <- strsplit(trimws(x), sep)[[1]]
  parts <- parts[nzchar(parts)]
  vapply(parts, function(p) as.vector(parse_num(p, lang)), numeric(1), USE.NAMES = FALSE)
}

# Value for a text input or a sentence: up to `digits` significant digits
# with the decimal mark of the language; infinite values as the sign.
format_value <- function(x, lang, digits = 6) {
  if (is.infinite(x)) return(if (x > 0) "\u221e" else "-\u221e")
  format_num(x, lang, digits = digits)
}

# Value for a text input: 15 significant digits so a typed value survives a
# language or distribution switch, and "Inf", so the text parses back.
format_input <- function(x, lang) {
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  format_num(x, lang, digits = 15)
}

# Digits as Unicode superscripts, for powers of ten in plain text.
sup_digits <- function(s) {
  chartr("-0123456789",
         "\u207b\u2070\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079", s)
}

# Probability for display from its natural log. Values below 1e-4 use
# scientific notation built from the log, so tails beyond the double range
# (below about 1e-308) still show their size instead of 0. `empty` is TRUE
# when the event has no support point, so a probability of 0 is exact.
format_prob <- function(lp, lang, empty = TRUE, digits = 4) {
  mark <- if (match_lang(lang) == "pt-BR") "," else "."
  if (is.na(lp)) return("\u2014")
  if (lp == -Inf) {
    return(if (empty) "0" else paste0("< 10", sup_digits("-308")))
  }
  p <- exp(lp)
  if (p >= 1e-4) {
    # Enough digits to tell a probability close to 1 from 1 itself.
    d <- if (p < 1) min(12, max(digits, ceiling(-log10(1 - p)) + 2)) else digits
    return(trimws(formatC(p, digits = d, format = "fg", decimal.mark = mark)))
  }
  l10 <- lp / log(10)
  e <- floor(l10)
  m <- round(10^(l10 - e), digits - 1)
  if (m >= 10) {
    m <- m / 10
    e <- e + 1
  }
  mant <- formatC(m, digits = digits - 1, format = "f", decimal.mark = mark)
  paste0(mant, " \u00d7 10", sup_digits(format(e, scientific = FALSE)))
}

# Probability as a percentage with two decimals; tiny values as "< 0.01%".
format_pct <- function(lp, lang) {
  mark <- if (match_lang(lang) == "pt-BR") "," else "."
  if (is.na(lp)) return("\u2014")
  p <- exp(lp)
  if (p == 0 && lp == -Inf) return("0%")
  if (p < 5e-5) return(paste0("< ", formatC(0.01, format = "f", digits = 2,
                                            decimal.mark = mark), "%"))
  if (p < 1 && p > 1 - 5e-5) {
    return(paste0("> ", formatC(99.99, format = "f", digits = 2,
                                decimal.mark = mark), "%"))
  }
  paste0(formatC(100 * p, format = "f", digits = 2, decimal.mark = mark), "%")
}

# Number as R source: up to 15 significant digits, Inf as is.
code_num <- function(x) {
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  trimws(formatC(x, digits = 15, format = "g"))
}

# Number inside TeX, with the decimal comma of pt-BR.
tex_num <- function(x, lang) {
  if (is.infinite(x)) return(if (x > 0) "\\infty" else "-\\infty")
  tex_decimal(format_num(x, "en-US", digits = 6), lang)
}

# Name and arguments of the R function of kind "p" or "q" for code shown to
# the student: base or 'extraDistr' functions as they are; the Bernoulli as
# a Binomial with one trial; NULL for functions internal to distlab.
code_call <- function(dist, kind) {
  fun <- dist$entry$functions[[kind]]
  args <- dist$params
  if (identical(dist$id, "bernoulli")) {
    fun <- paste0(kind, "binom")
    args <- c(list(size = 1), args)
  } else if (startsWith(fun, "distlab::")) {
    return(NULL)
  }
  fun <- sub("^stats::", "", fun)
  arg_text <- paste(names(args), vapply(args, code_num, ""), sep = " = ",
                    collapse = ", ")
  list(fun = fun, args = arg_text)
}

# One call of the p or q function at `v`, optionally on the upper tail.
code_expr <- function(cc, v, upper = FALSE, log_p = FALSE) {
  paste0(cc$fun, "(", code_num(v), ", ", cc$args,
         if (upper) ", lower.tail = FALSE", if (log_p) ", log.p = TRUE", ")")
}

# The distribution as engine code, for entries without a public R function.
code_dist <- function(dist) {
  args <- dist$params
  arg_text <- paste(names(args), vapply(args, code_num, ""), sep = " = ",
                    collapse = ", ")
  sprintf("d <- distlab::dl_dist(\"%s\", %s)", dist$id, arg_text)
}

# Math-only text of an inequality, for comments in generated code.
ineq_text <- function(lower, upper, lower_closed, upper_closed) {
  l <- if (lower == -Inf) "" else paste(code_num(lower), if (lower_closed) "<=" else "<", "")
  u <- if (upper == Inf) "" else paste("", if (upper_closed) "<=" else "<", code_num(upper))
  if (lower == -Inf && upper == Inf) return("P(-Inf < X < Inf)")
  paste0("P(", l, "X", u, ")")
}

# R code that reproduces a calculator result `res` (from calc_result()).
# Differences of CDF values are written on the tail the engine uses.
calc_code <- function(dist, res) {
  cc_p <- code_call(dist, "p")
  cc_q <- code_call(dist, "q")
  if (res$mode %in% c("quantile", "central")) {
    if (is.null(cc_q)) {
      call <- if (res$mode == "quantile") sprintf("distlab::dl_quantile(d, %s)", code_num(res$p)) else
        sprintf("distlab::dl_interval(d, %s)[c(\"lower\", \"upper\")]", code_num(res$prob))
      return(paste(code_dist(dist), call, sep = "\n"))
    }
    if (res$mode == "quantile") return(code_expr(cc_q, res$p))
    tail <- (1 - res$prob) / 2
    return(sprintf("c(%s,\n  %s)", code_expr(cc_q, tail), code_expr(cc_q, tail, upper = TRUE)))
  }
  r <- res$region
  if (is.null(cc_p)) {
    call <- sprintf("distlab::dl_prob(d, lower = %s, upper = %s, lower_closed = %s, upper_closed = %s)",
                    code_num(r$lower), code_num(r$upper), r$lower_closed, r$upper_closed)
    return(paste(code_dist(dist), call, sep = "\n"))
  }
  ends <- res$ends
  if (is.null(ends)) return("0")
  lo <- ends[1]
  hi <- ends[2]
  expr <- if (lo == -Inf && hi == Inf) {
    "1"
  } else if (lo == -Inf) {
    code_expr(cc_p, hi)
  } else if (prob_upper_side(dist, lo, hi)) {
    if (hi == Inf) code_expr(cc_p, lo, upper = TRUE) else
      paste(code_expr(cc_p, lo, upper = TRUE), "-", code_expr(cc_p, hi, upper = TRUE))
  } else {
    paste(code_expr(cc_p, hi), "-", code_expr(cc_p, lo))
  }
  lines <- character(0)
  # Discrete ends become whole-number cut points; the comment shows how.
  if (identical(dist$entry$type, "discrete") && res$mode == "between") {
    lines <- c(lines, sprintf("# %s = %s", ineq_text(r$lower, r$upper, r$lower_closed, r$upper_closed),
                              cut_text(lo, hi)))
  }
  lines <- c(lines, expr)
  # A probability too small for a double is still available on the log
  # scale: the log.p form for one tail, the engine for a difference.
  single <- !grepl(" - ", expr, fixed = TRUE) && expr != "1"
  if (is.finite(res$log_p) && exp(res$log_p) < 1e-300) {
    if (single) {
      lp_expr <- paste0(substr(expr, 1L, nchar(expr) - 1L), ", log.p = TRUE)")
      lines <- c(lines, paste0(lp_expr, " / log(10)  # log10"))
    } else {
      lines <- c(lines, code_dist(dist), sprintf(
        "distlab::dl_prob(d, lower = %s, upper = %s, lower_closed = %s, upper_closed = %s, log = TRUE) / log(10)  # log10",
        code_num(r$lower), code_num(r$upper), r$lower_closed, r$upper_closed))
    }
  }
  paste(lines, collapse = "\n")
}

# P(X <= hi) - P(X <= lo) as text for code comments.
cut_text <- function(lo, hi) {
  if (lo == -Inf) return(sprintf("P(X <= %s)", code_num(hi)))
  if (hi == Inf) return(sprintf("P(X > %s)", code_num(lo)))
  sprintf("P(X <= %s) - P(X <= %s)", code_num(hi), code_num(lo))
}

# log of P(X in the complement of the region), summed over its two parts.
complement_log <- function(dist, r) {
  parts <- c(
    if (r$lower > -Inf) dl_prob(dist, -Inf, r$lower, upper_closed = !r$lower_closed, log = TRUE),
    if (r$upper < Inf) dl_prob(dist, r$upper, Inf, lower_closed = !r$upper_closed, log = TRUE)
  )
  if (!length(parts)) return(-Inf)
  m <- max(parts)
  if (m == -Inf) return(-Inf)
  m + log(sum(exp(parts - m)))
}

# TRUE when no support point lies outside the region, so the complement is
# exactly 0 (not an underflow).
complement_empty <- function(dist, r) {
  part_empty <- function(lower, upper, lc, uc) {
    pr <- list(lower = lower, upper = upper, lower_closed = lc, upper_closed = uc)
    region_empty(dist, pr, prob_ends(dist, lower, upper, lc, uc))
  }
  (r$lower == -Inf || part_empty(-Inf, r$lower, FALSE, !r$lower_closed)) &&
    (r$upper == Inf || part_empty(r$upper, Inf, !r$upper_closed, FALSE))
}

# TRUE when the region holds no support point, so its probability is 0
# exactly (not an underflow).
region_empty <- function(dist, r, ends) {
  if (is.null(ends)) return(TRUE)
  s <- dl_support(dist)
  if (identical(dist$entry$type, "discrete")) {
    return(max(ends[1] + 1, s$lower) > min(ends[2], s$upper))
  }
  max(r$lower, s$lower) >= min(r$upper, s$upper)
}

# Result of one calculation. `mode` is one of .dl_calc_modes; x, a, b, p and
# prob are numbers, and the closed flags apply to the ends of "between".
# Returns a list with the mode, the region of X that is shaded, the CDF cut
# points, the log probability and its complement, and the quantile values.
calc_result <- function(dist, mode, x = NA, a = NA, b = NA, lower_closed = FALSE,
                        upper_closed = TRUE, p = NA, prob = NA) {
  mode <- match.arg(mode, .dl_calc_modes)
  discrete <- identical(dist$entry$type, "discrete")
  region <- function(lower, upper, lc, uc) {
    list(lower = lower, upper = upper, lower_closed = lc, upper_closed = uc)
  }
  out <- list(mode = mode, p = p, prob = prob)
  if (mode == "le") {
    out$region <- region(-Inf, x, FALSE, TRUE)
  } else if (mode == "gt") {
    out$region <- region(x, Inf, FALSE, FALSE)
  } else if (mode == "between") {
    if (!discrete) {
      lower_closed <- FALSE
      upper_closed <- TRUE
    }
    out$region <- region(a, b, lower_closed, upper_closed)
  } else if (mode == "quantile") {
    out$q <- dl_quantile(dist, p)
    out$region <- region(-Inf, out$q, FALSE, TRUE)
  } else {
    iv <- dl_interval(dist, prob)
    out$q <- unname(iv[c("lower", "upper")])
    out$region <- region(iv[["lower"]], iv[["upper"]], TRUE, TRUE)
  }
  r <- out$region
  out$ends <- prob_ends(dist, r$lower, r$upper, r$lower_closed, r$upper_closed)
  out$log_p <- dl_prob(dist, r$lower, r$upper, r$lower_closed, r$upper_closed, log = TRUE)
  out$log_comp <- complement_log(dist, r)
  out$comp_empty <- complement_empty(dist, r)
  out$empty <- region_empty(dist, r, out$ends)
  out
}

# Default inputs for a distribution: x at the median, a and b at the
# quartiles (rounded to three significant digits, or whole numbers for
# discrete entries), p = 0.9 and a central probability of 0.95.
calc_defaults <- function(dist) {
  discrete <- identical(dist$entry$type, "discrete")
  nice <- function(v) {
    v <- if (discrete) round(v) else signif(v, 3)
    v + 0
  }
  q <- vapply(dl_quantile(dist, c(0.25, 0.5, 0.75)), nice, numeric(1))
  a <- q[1]
  # A discrete interval (a, b] needs room for at least one point.
  if (discrete && a >= q[3]) a <- q[3] - 1
  list(x = q[2], a = a, b = q[3], p = 0.9, prob = 0.95)
}

# TeX of the event or quantile shown in the result card.
calc_tex <- function(res, lang) {
  r <- res$region
  n <- function(v) tex_num(v, lang)
  switch(res$mode,
    le = sprintf("P(X \\le %s)", n(r$upper)),
    gt = sprintf("P(X > %s)", n(r$lower)),
    between = sprintf("P(%s %s X %s %s)", n(r$lower), if (r$lower_closed) "\\le" else "<",
                      if (r$upper_closed) "\\le" else "<", n(r$upper)),
    quantile = sprintf("Q(%s) = %s", n(res$p), n(res$q)),
    central = sprintf("P(%s \\le X \\le %s)", n(r$lower), n(r$upper))
  )
}

# Sentence that states the result in the language of the interface.
calc_sentence <- function(res, dist, lang) {
  r <- res$region
  v <- function(x) format_value(x, lang)
  pr <- format_prob(res$log_p, lang, empty = res$empty)
  pct <- format_pct(res$log_p, lang)
  discrete <- identical(dist$entry$type, "discrete")
  switch(res$mode,
    le = tr("calc.sentence.le", lang, x = v(r$upper), p = pr, pct = pct),
    gt = tr("calc.sentence.gt", lang, x = v(r$lower), p = pr, pct = pct),
    between = if (discrete) {
      tr("calc.sentence.between_discrete", lang, a = v(r$lower), b = v(r$upper),
         event = event_text(r, lang), p = pr, pct = pct)
    } else {
      tr("calc.sentence.between", lang, a = v(r$lower), b = v(r$upper), p = pr, pct = pct)
    },
    quantile = if (discrete) {
      tr("calc.sentence.quantile_discrete", lang, p = v(res$p), x = v(res$q),
         fx = format_prob(log(dl_cdf(dist, res$q)), lang))
    } else {
      tr("calc.sentence.quantile", lang, p = v(res$p), x = v(res$q))
    },
    central = if (discrete) {
      tr("calc.sentence.central_discrete", lang, prob = v(res$prob), a = v(r$lower),
         b = v(r$upper), cov = pr)
    } else {
      tr("calc.sentence.central", lang, prob = v(res$prob), a = v(r$lower),
         b = v(r$upper), tail = v((1 - res$prob) / 2))
    }
  )
}

# Plain-text event such as "2 <= X < 5", with the signs of the language.
event_text <- function(r, lang) {
  op <- function(closed) if (closed) "\u2264" else "<"
  paste(format_value(r$lower, lang), op(r$lower_closed), "X", op(r$upper_closed),
        format_value(r$upper, lang))
}
