# Helpers that turn the math-only LaTeX conditions of registry formulas into R
# predicates of the parameters and of the formula variable (u, t, z or j).

# Variable of each formula key; keys not listed depend on the parameters only.
.latex_vars <- c(quantile = "u", mgf = "t", cf = "t", pgf = "z", raw_moment = "j")

# Sample values of each variable, on both sides of every threshold in use.
.latex_var_grid <- list(
  u = c(0, 0.25, 0.5, 0.75, 1),
  t = c(-3, -1, -0.5, 0, 0.3, 0.6, 1, 1.5, 2, 2 * pi, 5),
  z = c(-3, -1.5, -0.5, 0, 0.5, 1, 1.5, 3),
  j = c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 3, 4, 5, 7)
)

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
  last <- if (dots < length(vals)) vals[[length(vals)]] else sign(step) * Inf
  function(x) {
    k <- (x - first) / step
    abs(k - round(k)) < tol && k > -tol && (x - last) * sign(step) <= tol
  }
}

# Predicate for the integers.
.latex_integers <- function(x) abs(x - round(x)) < 1e-9 * max(1, abs(x))

# Split a string at commas that are outside parentheses.
split_top_commas <- function(s) {
  ch <- strsplit(s, "")[[1]]
  depth <- cumsum(ch == "(") - cumsum(ch == ")")
  cut <- which(ch == "," & depth == 0)
  trimws(substring(s, c(1, cut + 1), c(cut - 1, length(ch))))
}

# Translate a LaTeX condition into an R expression string of the parameter
# names and the given variables, or NULL when it uses other symbols.
latex_condition_r <- function(cond, entry, vars = character()) {
  s <- cond
  s <- gsub("\\mathbb{Z}", ".latex_integers", s, fixed = TRUE)
  s <- gsub("\\ldots", ".latex_dots", s, fixed = TRUE)
  s <- gsub("\\\\log\\s*([0-9]+)", "log(\\1)", s)
  s <- gsub("\\\\log(?![A-Za-z])", "log", s, perl = TRUE)
  s <- gsub("\\\\pi(?![A-Za-z])", " pi ", s, perl = TRUE)
  s <- gsub("\\|([^|]+)\\|", " abs(\\1) ", s)
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
  for (i in order(-nchar(syms))) {
    pat <- gsub("([\\\\^$.|?*+()\\[\\]{}])", "\\\\\\1", syms[i], perl = TRUE)
    pat <- paste0("(?<![A-Za-z\\\\._])", pat, "(?![A-Za-z0-9_])")
    s <- gsub(pat, paste0(" ", nms[i], " "), s, perl = TRUE)
  }
  # Implicit products: "(n + 1) p", "(r - 1)(1 - p)", "n p", "2 pi".
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
  known <- c(nms, vars, "log", "sqrt", "abs", "pi", ".latex_set", ".latex_dots",
             ".latex_integers", "%IN%", "%NOTIN%", "(", "&", "+", "-", "*",
             "/", "^", "<", "<=", ">", ">=", "==", "!=")
  if (!all(all.names(expr) %in% known)) return(NULL)
  out
}

# Evaluate a translated condition at a named list of parameter (and
# variable) values.
latex_condition_eval <- function(r_cond, values) {
  env <- list2env(c(values, list(
    .latex_set = .latex_set, .latex_dots = .latex_dots,
    .latex_integers = .latex_integers,
    `%IN%` = function(x, set) set(x),
    `%NOTIN%` = function(x, set) !set(x)
  )), parent = baseenv())
  isTRUE(eval(str2lang(r_cond), env))
}

# Pieces of a formula spec as a list, whether it holds one piece or several.
formula_pieces <- function(x) if (!is.null(names(x))) list(x) else x

# Translated conditions of a key's pieces ("TRUE" for a piece without one).
piece_conditions <- function(entry, key) {
  var <- .latex_vars[key]
  lapply(formula_pieces(entry$latex[[key]]), function(pc) {
    if (is.null(pc$condition)) "TRUE" else
      latex_condition_r(pc$condition, entry, if (!is.na(var)) var)
  })
}

# Grid of values (parameter rows crossed with the key's variable) as a list
# of named lists.
key_grid <- function(params, key) {
  rows <- lapply(seq_len(nrow(params)), function(i) as.list(params[i, , drop = FALSE]))
  var <- .latex_vars[key]
  if (is.na(var)) return(rows)
  out <- list()
  for (r in rows) for (v in .latex_var_grid[[var]]) {
    out[[length(out) + 1L]] <- c(r, stats::setNames(list(v), var))
  }
  out
}

# Index of the piece whose condition holds at `values` (NA unless exactly one).
which_piece <- function(conds, values) {
  hits <- vapply(conds, latex_condition_eval, logical(1), values = values)
  if (sum(hits) == 1L) which(hits) else NA_integer_
}
