# Cache for the parsed registry, filled on first use.
.dl_env <- new.env(parent = emptyenv())

# Allowed values for categorical registry fields.
.dl_types <- c("discrete", "continuous")
.dl_groups <- c("binary", "counts", "bounded", "positive", "real_line",
                "directional", "multivariate", "mixed")
.dl_langs <- c("pt-BR", "en-US")

#' Distribution registry
#'
#' Reads the registry entries shipped in `inst/registry/`, one YAML file per
#' distribution. Each entry holds the support, parameters, R functions,
#' formulas, story and references used by the engine and the app.
#'
#' The `latex` field holds the density and distribution function as strings,
#' plus formula specs for the quantile, mean, variance, skewness, excess
#' kurtosis, raw moments, generating functions (`mgf`, `cf`, and `pgf` for
#' discrete entries), median, mode and entropy. Each spec is one piece or a
#' list of pieces; a piece has an `expr` (`NULL` when there is no closed form)
#' or a `status` (`"infinite"` or `"undefined"`), and an optional math-only
#' `condition`.
#'
#' @param refresh Logical. If `TRUE`, re-read the YAML files instead of using
#'   the cached copy.
#'
#' @returns A named list of registry entries, keyed by distribution id.
#'
#' @seealso [dl_ids()], [dl_dist()]
#' @family registry
#' @export
#' @examples
#' reg <- dl_registry()
#' names(reg)
#' reg$gamma$parameters[[1]]$name
dl_registry <- function(refresh = FALSE) {
  if (isTRUE(refresh) || is.null(.dl_env$registry)) {
    dir <- system.file("registry", package = "distlab")
    files <- sort(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE))
    entries <- lapply(files, read_registry_entry)
    names(entries) <- vapply(entries, `[[`, character(1), "id")
    problems <- validate_relationship_targets(entries)
    if (length(problems)) {
      stop("Invalid registry relationships:\n- ",
           paste(problems, collapse = "\n- "), call. = FALSE)
    }
    .dl_env$registry <- entries
  }
  .dl_env$registry
}

#' List distribution ids
#'
#' @param type Optional character vector of types (`"discrete"`,
#'   `"continuous"`) to keep.
#' @param group Optional character vector of data-type groups to keep, such
#'   as `"counts"`, `"positive"` or `"real_line"`.
#'
#' @returns A character vector of distribution ids.
#'
#' @family registry
#' @export
#' @examples
#' dl_ids()
#' dl_ids(type = "discrete")
dl_ids <- function(type = NULL, group = NULL) {
  reg <- dl_registry()
  keep <- rep(TRUE, length(reg))
  if (!is.null(type)) {
    keep <- keep & vapply(reg, function(e) e$type %in% type, logical(1))
  }
  if (!is.null(group)) {
    keep <- keep & vapply(reg, function(e) e$group %in% group, logical(1))
  }
  names(reg)[keep]
}

# Look up one entry by id, with a clear error for unknown ids.
dl_entry <- function(id) {
  reg <- dl_registry()
  if (!is.character(id) || length(id) != 1L || !id %in% names(reg)) {
    stop("Unknown distribution id: ", paste(format(id), collapse = ", "),
         ". See dl_ids().", call. = FALSE)
  }
  reg[[id]]
}

# Parse one YAML file and stop if the entry fails validation.
read_registry_entry <- function(path) {
  entry <- yaml::read_yaml(path)
  problems <- validate_entry(entry)
  expected_id <- sub("\\.ya?ml$", "", basename(path))
  if (!identical(entry$id, expected_id)) {
    problems <- c(problems, sprintf("id '%s' does not match file name",
                                    format(entry$id)))
  }
  if (length(problems)) {
    stop("Invalid registry entry ", basename(path), ":\n- ",
         paste(problems, collapse = "\n- "), call. = FALSE)
  }
  entry
}

# Check one registry entry; returns a character vector of problems (empty if valid).
validate_entry <- function(entry) {
  p <- character()
  req <- c("id", "names", "type", "group", "subgroup", "tier", "support",
           "parameters", "functions", "latex", "story", "references")
  miss <- setdiff(req, names(entry))
  if (length(miss)) {
    return(paste("missing field:", miss))
  }
  if (!is_string(entry$id) || !grepl("^[a-z][a-z0-9_]*$", entry$id)) {
    p <- c(p, "id must be lower snake case")
  }
  for (field in c("names", "story")) {
    if (!all(.dl_langs %in% names(entry[[field]])) ||
        !all(vapply(entry[[field]][.dl_langs], is_string, logical(1)))) {
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
  if (!is_string(entry$group) || !entry$group %in% .dl_groups) p <- c(p, "unknown group")
  if (!is.numeric(entry$tier) || length(entry$tier) != 1L || !entry$tier %in% 1:3) {
    p <- c(p, "tier must be 1, 2 or 3")
  }

  pars <- entry$parameters
  if (!length(pars)) p <- c(p, "at least one parameter is required")
  for (par in pars) {
    p <- c(p, validate_parameter(par))
  }
  par_names <- vapply(pars, function(x) format(x$name), character(1))
  if (anyDuplicated(par_names)) p <- c(p, "duplicated parameter names")
  # Bounds and constraints are evaluated at the defaults, so only when those are valid.
  pars_ok <- !length(p)

  s <- entry$support
  defaults <- stats::setNames(lapply(pars, `[[`, "default"), par_names)
  bounds_ok <- TRUE
  for (side in c("lower", "upper")) {
    if (!is_bound(s[[side]], par_names)) {
      bounds_ok <- FALSE
      p <- c(p, sprintf("support %s must be a number or an expression of the parameters",
                        side))
    }
  }
  if (pars_ok && bounds_ok) {
    lo <- eval_bound(s$lower, defaults)
    hi <- eval_bound(s$upper, defaults)
    if (!(lo < hi)) p <- c(p, "support needs lower < upper at the default parameters")
  }
  for (cond in entry$constraints) {
    if (!is_bound(cond, par_names, logical = TRUE)) {
      p <- c(p, sprintf("constraint '%s' is not a valid expression of the parameters",
                        format(cond)))
    } else if (pars_ok && !isTRUE(eval_bound(cond, defaults))) {
      p <- c(p, sprintf("constraint '%s' fails at the default parameters", cond))
    }
  }

  for (fn in c("d", "p", "q", "r")) {
    f <- resolve_fun(entry$functions[[fn]], error = FALSE)
    if (is.null(f)) {
      p <- c(p, sprintf("function '%s' is missing or cannot be resolved", fn))
    } else if (length(bad <- setdiff(par_names, names(formals(f))))) {
      # Parameters are passed by name, so each must be an argument of the function.
      p <- c(p, sprintf("function '%s' has no argument(s): %s", fn,
                        paste(bad, collapse = ", ")))
    }
  }
  p <- c(p, validate_latex(entry$latex, entry$type))
  for (rel in entry$relationships) {
    p <- c(p, validate_relationship(rel))
  }
  p
}

# Allowed relationship types between registry entries.
.dl_relation_types <- c("special_case", "transformation", "limit", "sum")

# Check one relationship spec: target id, type, bilingual condition and LaTeX.
validate_relationship <- function(rel) {
  label <- format(rel$to %||% "<no target>")
  p <- character()
  if (!is_string(rel$to)) p <- c(p, "relationship needs a target id `to`")
  if (!is_string(rel$type) || !rel$type %in% .dl_relation_types) {
    p <- c(p, sprintf("relationship to %s has an unknown type", label))
  }
  if (!all(.dl_langs %in% names(rel$condition)) ||
      !all(vapply(rel$condition[.dl_langs], is_string, logical(1)))) {
    p <- c(p, sprintf("relationship to %s needs condition text for %s", label,
                      paste(.dl_langs, collapse = " and ")))
  }
  if (!is_string(rel$latex)) {
    p <- c(p, sprintf("relationship to %s needs a latex string", label))
  }
  p
}

# Keys of the `latex` field. pdf and cdf are plain strings; the others are
# formula specs (see validate_formula()).
.dl_latex_formulas <- c("quantile", "mean", "variance", "skewness",
                        "excess_kurtosis", "raw_moment", "mgf", "cf", "pgf",
                        "median", "mode", "entropy")
.dl_latex_required <- c("quantile", "mean", "variance", "skewness",
                        "excess_kurtosis", "mgf", "cf", "median", "mode",
                        "entropy")
# Status of a quantity with no finite value: an integral that diverges to
# +Inf ("infinite") or one that has no value at all ("undefined").
.dl_formula_status <- c("infinite", "undefined")

# LaTeX macros allowed in registry formulas, all supported by KaTeX.
.dl_latex_macros <- c(
  # Greek letters.
  "alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", "zeta", "eta",
  "theta", "vartheta", "iota", "kappa", "lambda", "mu", "nu", "xi", "pi",
  "rho", "sigma", "tau", "upsilon", "phi", "varphi", "chi", "psi", "omega",
  "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon", "Phi",
  "Psi", "Omega",
  # Named functions.
  "log", "ln", "exp", "sin", "cos", "tan", "sinh", "cosh", "tanh", "arcsin",
  "arccos", "arctan", "max", "min", "sup", "inf", "lim", "det",
  # Operators, relations and symbols.
  "frac", "dfrac", "tfrac", "sqrt", "binom", "cdot", "times", "pm", "mp",
  "le", "ge", "leq", "geq", "ne", "neq", "lt", "gt", "approx", "sim", "in",
  "notin", "infty", "partial", "sum", "prod", "int", "ldots", "cdots", "dots",
  "to", "rightarrow", "Rightarrow", "mid", "prime",
  # Delimiters.
  "left", "right", "lfloor", "rfloor", "lceil", "rceil", "langle", "rangle",
  "lvert", "rvert", "vert",
  # Spacing, text and fonts.
  "quad", "qquad", "text", "mathrm", "mathbb", "mathbf", "mathcal",
  "operatorname", "begin", "end"
)

# Problems with one LaTeX string: math delimiters, unbalanced braces or
# \left/\right, macros outside the whitelist and, for conditions, words.
latex_string_problems <- function(s, label, condition = FALSE) {
  p <- character()
  # A line break "\\" is not an escape; drop it before looking at escapes.
  t <- gsub("\\\\\\\\", " ", s)
  if (grepl("$", t, fixed = TRUE) || grepl("\\\\[][()]", t)) {
    p <- c(p, sprintf("%s must not use $, \\(, \\), \\[ or \\] delimiters", label))
  }
  ch <- strsplit(gsub("\\\\[{}]", " ", t), "")[[1]]
  depth <- cumsum(ch == "{") - cumsum(ch == "}")
  if (length(depth) && (any(depth < 0) || depth[length(depth)] != 0)) {
    p <- c(p, sprintf("%s has unbalanced braces", label))
  }
  count <- function(pat) lengths(regmatches(t, gregexpr(pat, t, perl = TRUE)))
  if (count("\\\\left(?![A-Za-z])") != count("\\\\right(?![A-Za-z])")) {
    p <- c(p, sprintf("%s has unbalanced \\left and \\right", label))
  }
  macros <- sub("^\\\\", "", regmatches(t, gregexpr("\\\\[A-Za-z]+", t))[[1]])
  if (length(bad <- setdiff(macros, .dl_latex_macros))) {
    p <- c(p, sprintf("%s uses unsupported macro(s): %s", label,
                      paste0("\\", unique(bad), collapse = ", ")))
  }
  # Conditions are language neutral: no letter runs outside macros.
  if (condition && grepl("[A-Za-z]{2,}", gsub("\\\\[A-Za-z]+", " ", t))) {
    p <- c(p, sprintf("%s condition must be math only, without words", label))
  }
  p
}

# Check the `latex` field: pdf and cdf strings plus formula specs. The keys
# in .dl_latex_required must be present (expr may be null), and pgf too for
# discrete entries.
validate_latex <- function(latex, type) {
  p <- character()
  if (!is.list(latex) || !is_string(latex[["pdf"]]) || !is_string(latex[["cdf"]])) {
    return("latex needs pdf and cdf strings")
  }
  p <- c(p, latex_string_problems(latex[["pdf"]], "latex pdf"),
         latex_string_problems(latex[["cdf"]], "latex cdf"))
  extra <- setdiff(names(latex), c("pdf", "cdf", .dl_latex_formulas))
  if (length(extra)) {
    p <- c(p, paste("unknown latex key:", extra))
  }
  discrete <- identical(type, "discrete")
  req <- c(.dl_latex_required, if (discrete) "pgf")
  miss <- setdiff(req, names(latex))
  if (length(miss)) {
    p <- c(p, paste("missing latex key:", miss))
  }
  if (!discrete && "pgf" %in% names(latex)) {
    p <- c(p, "latex pgf is only for discrete entries")
  }
  for (key in intersect(names(latex), .dl_latex_formulas)) {
    p <- c(p, validate_formula(latex[[key]], key))
  }
  p
}

# Check one formula spec: a single piece or a non-empty list of pieces. A
# piece has either `expr` (a LaTeX string, or null when there is no closed
# form) or `status` ("infinite" or "undefined"), and an optional math-only
# `condition` string. Every piece of a list needs its own, distinct condition.
validate_formula <- function(x, key) {
  is_piece <- function(y) is.list(y) && !is.null(names(y))
  pieces <- if (is_piece(x)) list(x) else x
  if (!is.list(pieces) || !length(pieces) ||
      !all(vapply(pieces, is_piece, logical(1)))) {
    return(sprintf("latex %s must be a formula or a list of formulas", key))
  }
  p <- character()
  conds <- character()
  for (i in seq_along(pieces)) {
    pc <- pieces[[i]]
    label <- if (length(pieces) > 1L) sprintf("latex %s[%d]", key, i) else
      sprintf("latex %s", key)
    nm <- names(pc)
    if (length(bad <- setdiff(nm, c("expr", "status", "condition")))) {
      p <- c(p, sprintf("%s has unknown field(s): %s", label,
                        paste(bad, collapse = ", ")))
    }
    has_expr <- "expr" %in% nm
    has_status <- "status" %in% nm
    if (has_expr == has_status) {
      p <- c(p, sprintf("%s needs exactly one of expr and status", label))
    }
    if (has_expr && !is.null(pc[["expr"]])) {
      if (is_string(pc[["expr"]])) {
        p <- c(p, latex_string_problems(pc[["expr"]], label))
      } else {
        p <- c(p, sprintf("%s expr must be a string or null", label))
      }
    }
    if (has_status && !(is_string(pc[["status"]]) &&
                        pc[["status"]] %in% .dl_formula_status)) {
      p <- c(p, sprintf("%s status must be one of: %s", label,
                        paste(.dl_formula_status, collapse = ", ")))
    }
    if ("condition" %in% nm) {
      if (is_string(pc[["condition"]])) {
        p <- c(p, latex_string_problems(pc[["condition"]], label, condition = TRUE))
        conds <- c(conds, gsub("\\s+", "", pc[["condition"]]))
      } else {
        p <- c(p, sprintf("%s condition must be a string", label))
      }
    }
    if (length(pieces) > 1L && !"condition" %in% nm) {
      p <- c(p, sprintf("%s needs a condition", label))
    }
  }
  if (anyDuplicated(conds)) {
    p <- c(p, sprintf("latex %s has duplicated conditions", key))
  }
  p
}

# Every relationship must point to an id present in the registry.
validate_relationship_targets <- function(entries) {
  ids <- names(entries)
  p <- character()
  for (e in entries) {
    for (rel in e$relationships) {
      if (is_string(rel$to) && !rel$to %in% ids) {
        p <- c(p, sprintf("%s: relationship target '%s' is not in the registry",
                          e$id, rel$to))
      }
    }
  }
  p
}

# Functions and operators allowed in support bounds and parameter constraints.
.dl_bound_ops <- c("+", "-", "*", "/", "(", "min", "max")
.dl_logical_ops <- c("<", "<=", ">", ">=", "==", "!=", "&", "|", "!")

# TRUE when `x` is a number or a string holding a simple expression whose
# symbols are parameter names, numbers or the allowed operators.
is_bound <- function(x, par_names, logical = FALSE) {
  if (!logical && is.numeric(x) && length(x) == 1L && !is.na(x)) return(TRUE)
  if (!is_string(x)) return(FALSE)
  expr <- tryCatch(str2lang(x), error = function(e) NULL)
  if (is.null(expr)) return(FALSE)
  ok <- c(par_names, .dl_bound_ops, if (logical) .dl_logical_ops)
  all(all.names(expr) %in% ok)
}

# Value of a support bound or constraint at the given parameter values.
eval_bound <- function(x, params) {
  if (is.numeric(x)) return(x)
  env <- list2env(params, parent = baseenv())
  eval(str2lang(x), env)
}

# Check one parameter spec: domain, default and slider must be consistent.
validate_parameter <- function(par) {
  p <- character()
  label <- format(par$name %||% "<unnamed>")
  if (!is_string(par$name) || !is_string(par$latex)) {
    return(sprintf("parameter %s needs name and latex", label))
  }
  if (!is.numeric(par$lower) || !is.numeric(par$upper) ||
      !(par$lower < par$upper)) {
    return(sprintf("parameter %s needs numeric lower < upper", label))
  }
  if (!in_domain(par$default, par)) {
    p <- c(p, sprintf("default of %s is outside its domain", label))
  }
  sl <- par$slider
  if (is.null(sl) || !is.numeric(c(sl$min, sl$max, sl$step)) ||
      length(c(sl$min, sl$max, sl$step)) != 3L || !(sl$min < sl$max) ||
      !(sl$step > 0)) {
    p <- c(p, sprintf("slider of %s needs numeric min < max and step > 0", label))
  } else if (!in_domain(sl$min, par) || !in_domain(sl$max, par)) {
    p <- c(p, sprintf("slider range of %s leaves the domain", label))
  } else if (isTRUE(par$integer) &&
             any(c(sl$min, sl$max, sl$step) != round(c(sl$min, sl$max, sl$step)))) {
    p <- c(p, sprintf("slider of integer parameter %s needs whole min, max and step",
                      label))
  }
  p
}

# TRUE when a scalar value lies inside a parameter domain (open/closed ends,
# integer flag). Parameter values must be finite even when a bound is infinite.
in_domain <- function(value, par) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) return(FALSE)
  lower_ok <- if (isTRUE(par$lower_open)) value > par$lower else value >= par$lower
  upper_ok <- if (isTRUE(par$upper_open)) value < par$upper else value <= par$upper
  int_ok <- !isTRUE(par$integer) || value == round(value)
  lower_ok && upper_ok && int_ok
}

# Turn "pkg::fun" into the function object; NULL (or an error) when unavailable.
# "distlab::fun" may also name an internal helper of this package.
resolve_fun <- function(spec, error = TRUE) {
  parts <- if (is_string(spec)) strsplit(spec, "::", fixed = TRUE)[[1]]
  fun <- if (length(parts) == 2L && identical(parts[1], "distlab")) {
    f <- get0(parts[2], envir = asNamespace("distlab"), inherits = FALSE)
    if (is.function(f)) f
  } else if (length(parts) == 2L && requireNamespace(parts[1], quietly = TRUE)) {
    tryCatch(getExportedValue(parts[1], parts[2]), error = function(e) NULL)
  }
  if (is.null(fun) && error) {
    stop("Cannot find function ", format(spec), ".", call. = FALSE)
  }
  fun
}

is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

`%||%` <- function(x, y) if (is.null(x)) y else x
