#' Relationships between distributions
#'
#' Lists the relationship edges recorded in the registry: special cases,
#' transformations, sums and limits that lead from one distribution to
#' another.
#'
#' @param id Optional distribution id (see [dl_ids()]). When given, only the
#'   edges that start or end at `id` are returned.
#'
#' @returns A data frame with one row per edge and the columns `from` and
#'   `to` (distribution ids), `type` (`"special_case"`, `"transformation"`,
#'   `"sum"` or `"limit"`), `condition_en` and `condition_pt` (the condition
#'   in English and Portuguese), `latex` (the relation as a LaTeX string) and
#'   `label` (a short plain-text label, possibly empty).
#'
#' @seealso [dl_catalog()]
#' @family registry
#' @export
#' @examples
#' rel <- dl_relations()
#' table(rel$type)
#' dl_relations("poisson")[, c("from", "to", "type")]
dl_relations <- function(id = NULL) {
  reg <- dl_registry()
  if (!is.null(id)) id <- dl_entry(id)$id
  relations_frame(reg, id)
}

# Edge table of a registry list; `id` keeps the edges touching one entry.
relations_frame <- function(reg, id = NULL) {
  rows <- list()
  for (e in reg) {
    for (rel in e$relationships) {
      rows[[length(rows) + 1L]] <- data.frame(
        from = e$id, to = rel$to, type = rel$type,
        condition_en = rel$condition[["en-US"]],
        condition_pt = rel$condition[["pt-BR"]],
        latex = rel$latex, label = rel$label %||% "",
        stringsAsFactors = FALSE
      )
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(from = character(), to = character(), type = character(),
               condition_en = character(), condition_pt = character(),
               latex = character(), label = character(), stringsAsFactors = FALSE)
  if (!is.null(id)) out <- out[out$from == id | out$to == id, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Catalog of distributions
#'
#' One row per registry entry, with its names, classification and counts of
#' parameters and relationships.
#'
#' @returns A data frame with the columns `id`, `name_en`, `name_pt`,
#'   `group` (data type, such as `"counts"`), `subgroup` (family), `type`
#'   (`"discrete"` or `"continuous"`), `tier` (1 = core catalog), `n_params`
#'   and `n_relations` (edges that start or end at the entry, see
#'   [dl_relations()]).
#'
#' @seealso [dl_relations()], [dl_registry()]
#' @family registry
#' @export
#' @examples
#' cat <- dl_catalog()
#' head(cat)
#' cat[cat$group == "counts", c("id", "name_en", "n_relations")]
dl_catalog <- function() catalog_frame(dl_registry())

# Catalog table of a registry list.
catalog_frame <- function(reg) {
  rel <- relations_frame(reg)
  n_rel <- vapply(names(reg), function(i) sum(rel$from == i | rel$to == i), integer(1))
  data.frame(
    id = names(reg),
    name_en = vapply(reg, function(e) e$names[["en-US"]], ""),
    name_pt = vapply(reg, function(e) e$names[["pt-BR"]], ""),
    group = vapply(reg, `[[`, "", "group"),
    subgroup = vapply(reg, `[[`, "", "subgroup"),
    type = vapply(reg, `[[`, "", "type"),
    tier = vapply(reg, function(e) as.integer(e$tier), integer(1)),
    n_params = vapply(reg, function(e) length(e$parameters), integer(1)),
    n_relations = unname(n_rel),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# Groups an entry is listed under: its own group, then any `also_in` groups.
entry_groups <- function(entry) unique(c(entry$group, entry$also_in))

# TeX of a support bound: numbers, infinities, or an expression of the
# parameters with each name replaced by its symbol.
bound_tex <- function(x, entry) {
  if (is.numeric(x)) {
    if (is.infinite(x)) return(if (x > 0) "\\infty" else "-\\infty")
    return(format(x))
  }
  syms <- stats::setNames(vapply(entry$parameters, `[[`, "", "latex"),
                          vapply(entry$parameters, `[[`, "", "name"))
  # Function calls first, so a parameter named like a function (Uniform
  # `max`) is still replaced by its symbol.
  t <- gsub("\\b(max|min)\\(", "@\\1(", x, perl = TRUE)
  for (nm in names(syms)) {
    t <- gsub(paste0("(?<![@A-Za-z_.])", nm, "(?![A-Za-z0-9_.(])"),
              paste0("{", gsub("\\\\", "\\\\\\\\", syms[[nm]]), "}"), t, perl = TRUE)
  }
  t <- gsub("@(max|min)", "\\\\\\1", t)
  gsub("*", " \\cdot ", t, fixed = TRUE)
}

# Support as TeX: a set of whole numbers for discrete entries, an interval
# with open or closed ends for continuous ones.
support_tex <- function(entry) {
  s <- entry$support
  lo <- bound_tex(s$lower, entry)
  hi <- bound_tex(s$upper, entry)
  if (identical(entry$type, "discrete")) {
    if (is.numeric(s$upper) && is.infinite(s$upper)) {
      if (is.numeric(s$lower)) {
        return(sprintf("\\{%s, %s, %s, \\ldots\\}", lo, format(s$lower + 1),
                       format(s$lower + 2)))
      }
      return(sprintf("\\{%s, %s + 1, \\ldots\\}", lo, lo))
    }
    if (is.numeric(s$lower) && is.numeric(s$upper) && s$upper - s$lower <= 1) {
      return(sprintf("\\{%s, %s\\}", lo, hi))
    }
    return(sprintf("\\{%s, \\ldots, %s\\}", lo, hi))
  }
  closed_lo <- !isTRUE(s$lower_open) && !(is.numeric(s$lower) && is.infinite(s$lower))
  closed_hi <- !isTRUE(s$upper_open) && !(is.numeric(s$upper) && is.infinite(s$upper))
  sprintf("%s%s, %s%s", if (closed_lo) "[" else "(", lo, hi, if (closed_hi) "]" else ")")
}

# Parameter symbols of an entry as one TeX string.
params_tex <- function(entry) {
  paste(vapply(entry$parameters, `[[`, "", "latex"), collapse = ",\\ ")
}

# Name of a family subgroup in `lang`; unknown subgroups fall back to the id.
subgroup_label <- function(subgroup, lang) {
  key <- paste0("subgroup.", subgroup)
  out <- tr(key, lang)
  if (identical(out, key)) gsub("_", " ", subgroup) else out
}

# First sentences of a text, at most about `max` characters.
text_excerpt <- function(text, max = 240L) {
  text <- trimws(gsub("\\s+", " ", text))
  if (nchar(text) <= max) return(text)
  cut <- substr(text, 1L, max)
  ends <- gregexpr("[.!?](\\s|$)", cut)[[1]]
  if (ends[1] > 0) return(substr(cut, 1L, ends[length(ends)]))
  paste0(sub("\\s+\\S*$", "", cut), "\u2026")
}
