# Registry schema of multivariate entries: reference cards with vector and
# matrix parameters, described in LaTeX, and teaching `example` values used by
# the card illustrations.

# Formula specs of a multivariate entry, besides the joint `pdf` string. All
# are required (an `expr` may be null when there is no closed form).
.dl_mv_latex_keys <- c("mean", "covariance", "marginal", "mgf", "cf")

# Extra LaTeX macros for vectors and matrices, allowed in multivariate entries
# only (all supported by KaTeX).
.dl_mv_latex_macros <- c("boldsymbol", "top")

# Functions allowed in multivariate constraints, which are checked at the
# example values.
.dl_mv_ops <- c("sum", "abs", "all", "length", "nrow", "ncol", "diag", "t", "det")

# Check one multivariate entry; returns a character vector of problems.
validate_mv_entry <- function(entry) {
  req <- c("id", "names", "dimension", "type", "group", "subgroup", "tier", "support",
           "parameters", "latex", "story", "references")
  miss <- setdiff(req, names(entry))
  if (length(miss)) return(paste("missing field:", miss))
  macros <- c(.dl_latex_macros, .dl_mv_latex_macros)
  p <- character()
  if (!is_string(entry$id) || !grepl("^[a-z][a-z0-9_]*$", entry$id)) {
    p <- c(p, "id must be lower snake case")
  }
  for (field in c("names", "story")) {
    if (!has_langs(entry[[field]])) {
      p <- c(p, sprintf("%s must have text for %s", field,
                        paste(.dl_langs, collapse = " and ")))
    }
  }
  if (!is_string(entry$subgroup)) p <- c(p, "subgroup must be a string")
  if (!length(entry$references) ||
      !all(vapply(entry$references, is_string, logical(1)))) {
    p <- c(p, "references must be a non-empty list of strings")
  }
  if (!is_string(entry$type) || !entry$type %in% .dl_types) p <- c(p, "unknown type")
  if (!identical(entry$group, "multivariate")) {
    p <- c(p, "multivariate entries belong to the multivariate group")
  }
  if (!is.numeric(entry$tier) || length(entry$tier) != 1L || !entry$tier %in% 1:3) {
    p <- c(p, "tier must be 1, 2 or 3")
  }
  if (!is.list(entry$support) || !is_string(entry$support$latex)) {
    p <- c(p, "support needs a latex string")
  } else {
    p <- c(p, latex_string_problems(entry$support$latex, "support", macros = macros))
  }

  pars <- entry$parameters
  if (!length(pars)) p <- c(p, "at least one parameter is required")
  for (par in pars) p <- c(p, validate_mv_parameter(par, macros))
  par_names <- vapply(pars, function(x) format(x$name), character(1))
  if (anyDuplicated(par_names)) p <- c(p, "duplicated parameter names")
  pars_ok <- !length(p)
  values <- if (pars_ok) mv_examples(entry)
  for (cond in entry$constraints) {
    if (!is_bound(cond, par_names, logical = TRUE, extra = .dl_mv_ops)) {
      p <- c(p, sprintf("constraint '%s' is not a valid expression of the parameters",
                        format(cond)))
    } else if (pars_ok && !isTRUE(eval_bound(cond, values))) {
      p <- c(p, sprintf("constraint '%s' fails at the example values", cond))
    }
  }

  # R functions are optional (a card may have none); those given must exist
  # and take the parameters by name.
  fns <- entry$functions
  if (!is.null(fns) && (!is.list(fns) || !all(names(fns) %in% c("d", "r")))) {
    p <- c(p, "functions of a multivariate entry may only be d and r")
  } else {
    for (fn in names(fns)) {
      f <- resolve_fun(fns[[fn]], error = FALSE)
      if (is.null(f)) {
        p <- c(p, sprintf("function '%s' is missing or cannot be resolved", fn))
      } else if (length(bad <- setdiff(par_names, names(formals(f))))) {
        p <- c(p, sprintf("function '%s' has no argument(s): %s", fn,
                          paste(bad, collapse = ", ")))
      }
    }
  }
  p <- c(p, validate_mv_latex(entry$latex, macros))
  for (rel in entry$relationships) p <- c(p, validate_relationship(rel))
  p <- c(p, validate_graph(entry$graph))
  if (!is.null(entry$also_in) &&
      (!is.character(entry$also_in) || !all(entry$also_in %in% .dl_groups) ||
       entry$group %in% entry$also_in)) {
    p <- c(p, "also_in must list other known groups")
  }
  p
}

# TRUE when `x` has a non-empty string for every language.
has_langs <- function(x) {
  is.list(x) && all(.dl_langs %in% names(x)) &&
    all(vapply(x[.dl_langs], is_string, logical(1)))
}

# One parameter of a multivariate entry: name, LaTeX symbol (with optional
# `elements`), bilingual description and a finite example (scalar, vector or matrix) inside the
# optional elementwise lower bound and integer flag.
validate_mv_parameter <- function(par, macros) {
  label <- format(par$name %||% "<unnamed>")
  if (!is_string(par$name) || !is_string(par$latex)) {
    return(sprintf("parameter %s needs name and latex", label))
  }
  p <- latex_string_problems(par$latex, sprintf("parameter %s", label), macros = macros)
  # Optional coordinates of the symbol, such as "(p_1, \\ldots, p_k)".
  if (!is.null(par$elements)) {
    p <- c(p, if (is_string(par$elements)) {
      latex_string_problems(par$elements, sprintf("parameter %s elements", label),
                            macros = macros)
    } else {
      sprintf("parameter %s elements must be a LaTeX string", label)
    })
  }
  if (!has_langs(par$description)) {
    p <- c(p, sprintf("parameter %s needs a description for %s", label,
                      paste(.dl_langs, collapse = " and ")))
  }
  v <- tryCatch(mv_value(par$example), error = function(e) NULL)
  if (!is.numeric(v) || !length(v) || !all(is.finite(v))) {
    return(c(p, sprintf("parameter %s needs a finite numeric example", label)))
  }
  if (!is.null(par$lower)) {
    if (!is.numeric(par$lower) || length(par$lower) != 1L || is.na(par$lower)) {
      return(c(p, sprintf("parameter %s needs a numeric lower bound", label)))
    }
    above <- if (isTRUE(par$lower_open)) v > par$lower else v >= par$lower
    if (!all(above)) p <- c(p, sprintf("example of %s is below its lower bound", label))
  }
  if (isTRUE(par$integer) && any(v != round(v))) {
    p <- c(p, sprintf("example of integer parameter %s must be whole", label))
  }
  p
}

# YAML value as an R number, vector or matrix (a list of rows).
mv_value <- function(x) {
  if (is.list(x)) {
    rows <- lapply(x, function(r) as.numeric(unlist(r)))
    if (length(unique(lengths(rows))) != 1L) stop("ragged matrix")
    return(do.call(rbind, rows))
  }
  as.numeric(x)
}

# Example values of every parameter, named.
mv_examples <- function(entry) {
  stats::setNames(lapply(entry$parameters, function(par) mv_value(par$example)),
                  vapply(entry$parameters, `[[`, "", "name"))
}

# The `latex` field of a multivariate entry: a joint `pdf` string and the
# formula specs in .dl_mv_latex_keys.
validate_mv_latex <- function(latex, macros) {
  if (!is.list(latex) || !is_string(latex[["pdf"]])) return("latex needs a pdf string")
  p <- latex_string_problems(latex[["pdf"]], "latex pdf", macros = macros)
  extra <- setdiff(names(latex), c("pdf", .dl_mv_latex_keys))
  if (length(extra)) p <- c(p, paste("unknown latex key:", extra))
  miss <- setdiff(.dl_mv_latex_keys, names(latex))
  if (length(miss)) p <- c(p, paste("missing latex key:", miss))
  for (key in intersect(names(latex), .dl_mv_latex_keys)) {
    p <- c(p, validate_formula(latex[[key]], key, macros = macros))
  }
  p
}
