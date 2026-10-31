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
           "parameters", "functions", "latex", "story")
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
  if (!is_string(entry$type) || !entry$type %in% .dl_types) p <- c(p, "unknown type")
  if (!is_string(entry$group) || !entry$group %in% .dl_groups) p <- c(p, "unknown group")
  if (!is.numeric(entry$tier) || length(entry$tier) != 1L || !entry$tier %in% 1:3) {
    p <- c(p, "tier must be 1, 2 or 3")
  }

  s <- entry$support
  if (!is.numeric(s$lower) || !is.numeric(s$upper) || !(s$lower < s$upper)) {
    p <- c(p, "support needs numeric lower < upper")
  }

  pars <- entry$parameters
  if (!length(pars)) p <- c(p, "at least one parameter is required")
  for (par in pars) {
    p <- c(p, validate_parameter(par))
  }
  par_names <- vapply(pars, function(x) format(x$name), character(1))
  if (anyDuplicated(par_names)) p <- c(p, "duplicated parameter names")

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
  if (!is_string(entry$latex$pdf) || !is_string(entry$latex$cdf)) {
    p <- c(p, "latex needs pdf and cdf strings")
  }
  p
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
resolve_fun <- function(spec, error = TRUE) {
  parts <- if (is_string(spec)) strsplit(spec, "::", fixed = TRUE)[[1]]
  fun <- if (length(parts) == 2L && requireNamespace(parts[1], quietly = TRUE)) {
    tryCatch(getExportedValue(parts[1], parts[2]), error = function(e) NULL)
  }
  if (is.null(fun) && error) {
    stop("Cannot find function ", format(spec), ".", call. = FALSE)
  }
  fun
}

is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

`%||%` <- function(x, y) if (is.null(x)) y else x
