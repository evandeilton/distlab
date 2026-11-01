# Parameter sliders with linked constraints, shared by the Explorer, the
# Calculator and Compare.

# Called inside a module server. `entry` is a reactive registry entry and
# `output_id` the uiOutput that holds the sliders; `prefix` keeps slider ids
# apart when one module shows several sets (Compare); `skip` is a reactive
# parameter name whose slider is kept but hidden. Values are remembered
# per distribution and restored when the student comes back.
# Returns list(pars, set, get): `pars` is a reactive list(id, vals) with
# resolved values; `set(id, vals)` stores values for a distribution and moves
# the sliders when that distribution is open; `get(id)` reads stored values.
param_sliders <- function(input, output, session, entry, output_id, prefix = "",
                          skip = shiny::reactive(NULL)) {
  ns <- session$ns
  memory <- new.env(parent = emptyenv())
  # Resolved parameter values of the current entry: list(id, vals).
  pars <- shiny::reactiveVal(NULL)
  # Slider ranges already sent to the client, per input id.
  sent_limits <- new.env(parent = emptyenv())
  slider_id <- function(did, nm) paste0(prefix, param_input_id(did, nm))

  # One slider per parameter, rebuilt when the distribution changes. Ids carry
  # the distribution id so stale values from another distribution are never read.
  output[[output_id]] <- shiny::renderUI({
    e <- entry()
    vals <- shiny::isolate(memory[[e$id]]) %||% default_params(e)
    lim <- slider_limits(e, vals)
    lapply(e$parameters, function(par) {
      input_id <- slider_id(e$id, par$name)
      sent_limits[[input_id]] <- lim[[par$name]]
      # Integer parameters have whole min, max and step, so their sliders
      # only produce whole numbers.
      slider <- shiny::sliderInput(
        ns(input_id),
        label = param_label(par),
        min = lim[[par$name]][1], max = lim[[par$name]][2],
        value = vals[[par$name]], step = par$slider$step, ticks = FALSE
      )
      if (par$name %in% skip()) shiny::tags$div(class = "dl-param-hidden", slider) else slider
    })
  })

  # Sliders must render even while hidden (a collapsed sidebar, an inactive
  # slot); otherwise no parameter ever reaches the server.
  shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)

  # Raw slider values; waits until every slider of this entry exists.
  raw <- shiny::reactive({
    e <- entry()
    par_names <- vapply(e$parameters, `[[`, "", "name")
    vals <- lapply(slider_id(e$id, par_names), function(k) input[[k]])
    shiny::req(all(!vapply(vals, is.null, logical(1))))
    names(vals) <- par_names
    list(id = e$id, vals = vals)
  })

  # Send values and ranges to the sliders of entry `e`, skipping those that
  # already show them.
  push <- function(e, fixed, current) {
    lim <- slider_limits(e, fixed)
    for (nm in names(fixed)) {
      input_id <- slider_id(e$id, nm)
      new_lim <- if (!same_value(sent_limits[[input_id]], lim[[nm]])) lim[[nm]]
      if (!same_value(fixed[[nm]], current[[nm]]) || !is.null(new_lim)) {
        sent_limits[[input_id]] <- lim[[nm]]
        shiny::updateSliderInput(session, input_id, value = fixed[[nm]],
                                 min = new_lim[1], max = new_lim[2])
      }
    }
  }

  # Keep registry constraints true by moving the other slider (min < max)
  # or clamping a value and its range (k <= m + n), instead of erroring.
  shiny::observe({
    r <- raw()
    e <- dl_entry(r$id)
    prev <- shiny::isolate(pars())
    prev_vals <- if (!is.null(prev) && identical(prev$id, r$id)) prev$vals
    fixed <- resolve_constraints(e, r$vals, prev_vals)
    push(e, fixed, r$vals)
    memory[[r$id]] <- fixed
    pars(list(id = r$id, vals = fixed))
  })

  set <- function(did, vals) {
    e <- dl_entry(did)
    keep <- intersect(names(vals), names(default_params(e)))
    full <- utils::modifyList(memory[[did]] %||% default_params(e), vals[keep])
    full <- resolve_constraints(e, full)
    memory[[did]] <- full
    current <- tryCatch(shiny::isolate(entry()$id), error = function(err) NULL)
    if (identical(current, did)) push(e, full, list())
    invisible(full)
  }

  # Values stored for a distribution, or its defaults.
  get <- function(did) memory[[did]] %||% default_params(dl_entry(did))

  list(pars = shiny::reactive(pars()), set = set, get = get)
}

# Registry defaults of an entry as a named list.
default_params <- function(entry) {
  stats::setNames(lapply(entry$parameters, `[[`, "default"),
                  vapply(entry$parameters, `[[`, "", "name"))
}

# Input id of a parameter slider, unique per distribution.
param_input_id <- function(dist_id, par_name) paste0("p_", dist_id, "_", par_name)

# Slider label: the math symbol of the parameter and its R argument name.
param_label <- function(par) {
  shiny::tags$span(class = "dl-param-label",
                   math_tag(par$latex, class = "dl-param-symbol"),
                   shiny::tags$code(par$name))
}

# Build a dl_dist from resolved slider values; a translated validation
# message when the values are outside the domain.
dist_from_pars <- function(p, lang) {
  res <- tryCatch(do.call(dl_dist, c(list(p$id), p$vals)),
                  error = function(err) conditionMessage(err))
  if (!inherits(res, "dl_dist")) {
    shiny::validate(tr("error.invalid_params", lang, message = res))
  }
  res
}
