# Helpers that turn the math-only LaTeX conditions of registry formulas into R
# predicates of the parameters, so tests can check them at parameter values.

# Sentinel for "\ldots" inside a set such as \{1, 3, 5, \ldots\}.
.latex_dots <- list("dots")

# Membership predicate for a LaTeX set: a finite list, or an arithmetic
# progression given by its first two terms, "\ldots" and an optional last term.
.latex_set <- function(...) {
  vals <- list(...)
  tol <- 1e-9
  dots <- which(vapply(vals, identical, logical(1), .latex_dots))
  if (!length(dots)) {
    v <- unlist(vals)
    return(function(x) any(abs(x - v) < tol))
  }
  first <- vals[[1]]
  step <- vals[[2]] - first
  last <- if (dots < length(vals)) vals[[length(vals)]] else Inf
  function(x) {
    k <- (x - first) / step
    abs(k - round(k)) < tol && k > -tol && x <= last + tol
  }
}

# Predicate for the integers.
.latex_integers <- function(x) abs(x - round(x)) < 1e-9 * max(1, abs(x))

# Split a string at commas that are outside parentheses.
split_top_commas <- function(s) {
  ch <- strsplit(s, "")[[1]]
  depth <- cumsum(ch == "(") - cumsum(ch == ")")
  cut <- which(ch == "," & depth == 0)
  starts <- c(1, cut + 1)
  ends <- c(cut - 1, length(ch))
  trimws(substring(s, starts, ends))
}

# Translate a LaTeX condition into an R expression string of the parameter
# names, or NULL when it involves other symbols (u, t, z, j, ...).
latex_condition_r <- function(cond, entry) {
  s <- cond
  s <- gsub("\\mathbb{Z}", ".latex_integers", s, fixed = TRUE)
  s <- gsub("\\ldots", ".latex_dots", s, fixed = TRUE)
  s <- gsub("\\\\log\\s*([0-9]+)", "log(\\1)", s)
  s <- gsub("\\\\log(?![A-Za-z])", "log", s, perl = TRUE)
  s <- gsub("\\\\notin(?![A-Za-z])", " %NOTIN% ", s, perl = TRUE)
  s <- gsub("\\\\in(?![A-Za-z])", " %IN% ", s, perl = TRUE)
  s <- gsub("\\\\le(?![A-Za-z])", " <= ", s, perl = TRUE)
  s <- gsub("\\\\ge(?![A-Za-z])", " >= ", s, perl = TRUE)
  s <- gsub("\\\\neq?(?![A-Za-z])", " != ", s, perl = TRUE)
  # Innermost fractions first, until none is left.
  repeat {
    new <- gsub("\\\\frac\\{([^{}]*)\\}\\{([^{}]*)\\}", "((\\1)/(\\2))", s)
    if (identical(new, s)) break
    s <- new
  }
  s <- gsub("\\{", ".latex_set(", s, fixed = TRUE)
  s <- gsub("\\}", ")", s, fixed = TRUE)
  # Parameter symbols, longest first, matched as whole tokens.
  syms <- vapply(entry$parameters, `[[`, character(1), "latex")
  nms <- vapply(entry$parameters, `[[`, character(1), "name")
  ord <- order(-nchar(syms))
  for (i in ord) {
    pat <- gsub("([\\\\^$.|?*+()\\[\\]{}])", "\\\\\\1", syms[i], perl = TRUE)
    pat <- paste0("(?<![A-Za-z\\\\._])", pat, "(?![A-Za-z0-9_])")
    s <- gsub(pat, paste0(" ", nms[i], " "), s, perl = TRUE)
  }
  # Implicit products: "(n + 1) p", "(r - 1)(1 - p)", "n p".
  s <- gsub("\\)\\s*\\(", ") * (", s)
  s <- gsub("([A-Za-z0-9_)])\\s+([A-Za-z(])", "\\1 * \\2", s, perl = TRUE)
  s <- gsub("(?<![<>!=])=(?!=)", "==", s, perl = TRUE)
  ops <- "(<=|>=|!=|==|<|>|%IN%|%NOTIN%)"
  clauses <- vapply(split_top_commas(s), function(cl) {
    parts <- trimws(strsplit(cl, ops, perl = TRUE)[[1]])
    op <- regmatches(cl, gregexpr(ops, cl, perl = TRUE))[[1]]
    if (length(parts) != length(op) + 1L || !length(op)) return(NA_character_)
    # Operands in parentheses: %IN% binds tighter than arithmetic in R.
    paste0("((", parts[-length(parts)], ") ", op, " (", parts[-1], "))",
           collapse = " & ")
  }, character(1))
  if (anyNA(clauses)) return(NULL)
  out <- paste(clauses, collapse = " & ")
  expr <- tryCatch(str2lang(out), error = function(e) NULL)
  if (is.null(expr)) return(NULL)
  known <- c(nms, "log", "sqrt", ".latex_set", ".latex_dots", ".latex_integers",
             "%IN%", "%NOTIN%", "(", "&", "+", "-", "*", "/", "^",
             "<", "<=", ">", ">=", "==", "!=")
  if (!all(all.names(expr) %in% known)) return(NULL)
  out
}

# Evaluate a translated condition at a named list of parameter values.
latex_condition_eval <- function(r_cond, params) {
  env <- list2env(c(params, list(
    .latex_set = .latex_set, .latex_dots = .latex_dots,
    .latex_integers = .latex_integers,
    `%IN%` = function(x, set) set(x),
    `%NOTIN%` = function(x, set) !set(x)
  )), parent = baseenv())
  isTRUE(eval(str2lang(r_cond), env))
}

# Pieces of a formula spec as a list, whether it holds one piece or several.
formula_pieces <- function(x) if (!is.null(names(x))) list(x) else x
