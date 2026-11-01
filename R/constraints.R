# Linked parameter sliders: registry constraints such as "min < max" or
# "k <= m + n" are kept true by moving slider values and ranges.

# Split an inequality constraint into sides, written as `lhs < rhs` or
# `lhs <= rhs`. NULL for constraints of any other form.
parse_constraint <- function(cond) {
  e <- tryCatch(str2lang(cond), error = function(err) NULL)
  if (!is.call(e) || length(e) != 3L) return(NULL)
  op <- as.character(e[[1]])
  if (!op %in% c("<", "<=", ">", ">=")) return(NULL)
  lhs <- e[[2]]
  rhs <- e[[3]]
  if (op %in% c(">", ">=")) {
    tmp <- lhs
    lhs <- rhs
    rhs <- tmp
  }
  list(lhs = lhs, rhs = rhs, strict = op %in% c("<", ">"))
}

# Name of the parameter when an expression side is a single symbol, else NULL.
single_param <- function(side, par_names) {
  if (is.symbol(side) && as.character(side) %in% par_names) as.character(side)
}

# Parameter spec by name.
param_spec <- function(entry, name) {
  for (par in entry$parameters) if (identical(par$name, name)) return(par)
  NULL
}

# TRUE when two slider values are equal up to rounding noise; the client may
# send integers or doubles for the same value.
same_value <- function(a, b) {
  a <- suppressWarnings(as.numeric(a))
  b <- suppressWarnings(as.numeric(b))
  length(a) == length(b) && all(is.finite(a) & is.finite(b)) &&
    all(abs(a - b) <= 1e-9 * pmax(1, abs(a), abs(b)))
}

# Gap needed by a strict inequality: one slider step of the moved parameter.
constraint_gap <- function(con, par) if (con$strict) par$slider$step else 0

# Round to the slider grid so floating noise never breaks equality checks.
snap_value <- function(x, par) {
  step <- par$slider$step
  round(round((x - par$slider$min) / step) * step + par$slider$min, 10)
}

# TRUE when the value lies on the slider range and in the parameter domain.
slider_ok <- function(x, par) {
  is.finite(x) && x >= par$slider$min - 1e-9 && x <= par$slider$max + 1e-9 &&
    in_domain(x, par)
}

# Bring parameter values back inside every constraint. `prev` holds the
# previous values: a parameter the user did not just change is moved first,
# so dragging `min` past `max` pushes `max` along. When that parameter is
# already at its slider end, both stop there (max at its slider maximum,
# min one step below, and the mirror case at the minimum). A side that is a sum (as in "k <= m + n")
# is never moved; the single parameter on the other side is clamped instead.
# Values that cannot be repaired are left as they are, so the domain check
# reports them.
resolve_constraints <- function(entry, vals, prev = NULL) {
  par_names <- vapply(entry$parameters, `[[`, "", "name")
  cons <- Filter(Negate(is.null), lapply(entry$constraints, parse_constraint))
  if (!length(cons)) return(vals)
  changed <- if (is.null(prev)) par_names else
    par_names[!mapply(same_value, vals[par_names], prev[par_names])]
  for (pass in 1:3) {
    stable <- TRUE
    for (con in cons) {
      env <- list2env(vals, parent = baseenv())
      lhs <- eval(con$lhs, env)
      rhs <- eval(con$rhs, env)
      ok <- if (con$strict) lhs < rhs else lhs <= rhs
      if (isTRUE(ok) || anyNA(c(lhs, rhs))) next
      # Candidate moves: raise a single-parameter rhs or lower a single lhs.
      cand <- list()
      r <- single_param(con$rhs, par_names)
      l <- single_param(con$lhs, par_names)
      if (!is.null(r)) {
        pr <- param_spec(entry, r)
        cand[[length(cand) + 1L]] <- list(name = r, par = pr,
                                          value = lhs + constraint_gap(con, pr))
      }
      if (!is.null(l)) {
        pl <- param_spec(entry, l)
        cand[[length(cand) + 1L]] <- list(name = l, par = pl,
                                          value = rhs - constraint_gap(con, pl))
      }
      # Prefer the parameter the user left alone.
      unchanged <- vapply(cand, function(cd) !cd$name %in% changed, logical(1))
      try_move <- function(cands) {
        for (cd in cands) {
          v <- snap_value(cd$value, cd$par)
          if (slider_ok(v, cd$par)) {
            vals[[cd$name]] <<- v
            return(TRUE)
          }
        }
        FALSE
      }
      moved <- try_move(cand[unchanged])
      # Both sides are single parameters and the untouched one is at its
      # slider end: park it there and put the changed one a gap away.
      if (!moved && any(unchanged) && !is.null(l) && !is.null(r)) {
        if (identical(cand[unchanged][[1]]$name, r)) {
          vr <- snap_value(pr$slider$max, pr)
          vl <- snap_value(pr$slider$max - constraint_gap(con, pl), pl)
        } else {
          vl <- snap_value(pl$slider$min, pl)
          vr <- snap_value(pl$slider$min + constraint_gap(con, pr), pr)
        }
        if (slider_ok(vr, pr) && slider_ok(vl, pl)) {
          vals[[r]] <- vr
          vals[[l]] <- vl
          moved <- TRUE
        }
      }
      if (!moved) moved <- try_move(cand[!unchanged])
      if (moved) stable <- FALSE
    }
    if (stable) break
  }
  vals
}

# Slider ranges given the current values: a parameter bounded by a sum of
# other parameters (k <= m + n) gets that bound as its slider limit.
slider_limits <- function(entry, vals) {
  par_names <- vapply(entry$parameters, `[[`, "", "name")
  lim <- lapply(entry$parameters, function(par) c(par$slider$min, par$slider$max))
  names(lim) <- par_names
  env <- list2env(vals, parent = baseenv())
  for (cond in entry$constraints) {
    con <- parse_constraint(cond)
    if (is.null(con)) next
    l <- single_param(con$lhs, par_names)
    r <- single_param(con$rhs, par_names)
    if (!is.null(l) && is.null(r)) {
      par <- param_spec(entry, l)
      hi <- eval(con$rhs, env) - constraint_gap(con, par)
      if (is.finite(hi) && hi > lim[[l]][1]) lim[[l]][2] <- min(lim[[l]][2], hi)
    }
    if (!is.null(r) && is.null(l)) {
      par <- param_spec(entry, r)
      lo <- eval(con$lhs, env) + constraint_gap(con, par)
      if (is.finite(lo) && lo < lim[[r]][2]) lim[[r]][1] <- max(lim[[r]][1], lo)
    }
  }
  lim
}
