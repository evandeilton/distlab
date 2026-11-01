# Relationship edges around one distribution, read from the registry.

# Every edge that touches `id`: those listed in its own entry ("out") and
# those listed in other entries that point to it ("in"). Each edge is a list
# with from, to, other (the id at the far end), direction, type, condition
# (by language) and latex.
related_edges <- function(id, reg = dl_registry()) {
  edge <- function(from, rel, direction) {
    list(from = from, to = rel$to, direction = direction,
         other = if (direction == "out") rel$to else from,
         type = rel$type, condition = rel$condition, latex = rel$latex)
  }
  out <- lapply(reg[[id]]$relationships, edge, from = id, direction = "out")
  inc <- list()
  for (e in reg) {
    if (identical(e$id, id)) next
    for (rel in e$relationships) {
      if (identical(rel$to, id)) inc[[length(inc) + 1L]] <- edge(e$id, rel, "in")
    }
  }
  c(out, inc)
}

# Card body listing the edges; each far-end name is a button that opens that
# distribution through the `input_id` input.
related_list_tag <- function(id, lang, input_id, reg = dl_registry()) {
  edges <- related_edges(id, reg)
  if (!length(edges)) {
    return(shiny::tags$p(class = "dl-empty", tr("related.none", lang)))
  }
  name <- function(i) entry_text(reg[[i]], "names", lang)
  # The open distribution is plain text; the far end is a button.
  end_tag <- function(i) {
    if (identical(i, id)) return(shiny::tags$span(class = "dl-rel-self", name(i)))
    dist_button(i, name(i), input_id, label = tr("related.open", lang, name = name(i)),
                class = "dl-chip dl-chip-sm")
  }
  items <- lapply(edges, function(ed) {
    shiny::tags$li(
      class = "dl-rel",
      shiny::tags$div(
        class = "dl-rel-head",
        shiny::tags$span(class = paste0("badge dl-rel-type dl-rel-", ed$type),
                         tr(paste0("related.type.", ed$type), lang)),
        shiny::tags$span(
          class = "dl-rel-path",
          end_tag(ed$from),
          shiny::tags$span(class = "dl-rel-arrow", `aria-hidden` = "true", "\u2192"),
          shiny::tags$span(class = "visually-hidden", tr("related.to", lang)),
          end_tag(ed$to)
        )
      ),
      shiny::tags$p(class = "dl-rel-cond", ed$condition[[lang]]),
      shiny::tags$div(class = "dl-rel-math", math_tag(ed$latex))
    )
  })
  shiny::tags$ul(class = "dl-rel-list", items)
}

# Button that sends a distribution id to a Shiny input (handled in distlab.js).
dist_button <- function(dist_id, text, input_id, label = NULL, class = "dl-chip") {
  shiny::tags$button(
    type = "button", class = paste("dl-goto", class),
    `data-dist` = dist_id, `data-input` = input_id,
    `aria-label` = label, text
  )
}
