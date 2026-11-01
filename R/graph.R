# Data of the relationship map: node and edge tables in the format of
# 'visNetwork', built from the registry for a language, a color mode and the
# active filters.

# Group colors on the map, for the light and dark color modes. `line` is the
# border (>= 3:1 on the surface); the fill is a tint of it. The legend in
# distlab.css uses the same values.
.dl_graph_colors <- list(
  light = list(
    surface = "#FFFFFF", ink = "#1B1C3A", label = "#2E3160",
    group = c(binary = "#A8467F", counts = "#0E7C7A", bounded = "#B06A00",
              positive = "#0067A5", real_line = "#C24E00", directional = "#2A7FB8",
              multivariate = "#5B5E8A", mixed = "#2B2D6E"),
    relation = c(special_case = "#3E418F", transformation = "#0E7C7A",
                 sum = "#6E4BB0", limit = "#B06A00")
  ),
  dark = list(
    surface = "#1D1F40", ink = "#F1F2FA", label = "#D5D7F7",
    group = c(binary = "#E59CC8", counts = "#3CC9C5", bounded = "#F2A33A",
              positive = "#6CB8F0", real_line = "#FF9A5C", directional = "#8FD0F5",
              multivariate = "#A3A6CC", mixed = "#A9ACF2"),
    relation = c(special_case = "#A9ACF2", transformation = "#3CC9C5",
                 sum = "#C9A6F2", limit = "#F2A33A")
  )
)

# Edges drawn dashed: limits are approximations, the others exact relations.
.dl_dashed_relations <- "limit"

# Colors for one mode.
graph_palette <- function(dark = FALSE) {
  .dl_graph_colors[[if (isTRUE(dark)) "dark" else "light"]]
}

# Mix two hex colors: `w` of `a` and 1 - w of `b`.
mix_hex <- function(a, b, w) {
  ca <- grDevices::col2rgb(a)
  cb <- grDevices::col2rgb(rep_len(b, length(a)))
  m <- round(w * ca + (1 - w) * cb)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

# Hex color with an alpha channel, as an rgba() string.
alpha_hex <- function(col, alpha) {
  v <- grDevices::col2rgb(col)
  sprintf("rgba(%d, %d, %d, %s)", v[1, ], v[2, ], v[3, ], format(alpha))
}

# Map positions of every entry. Entries with a `graph` field keep it; the
# others are placed on rows below the map, in id order, so a new entry is
# always visible and never covers an existing node.
graph_layout <- function(reg = dl_registry()) {
  ids <- names(reg)
  has <- vapply(reg, function(e) !is.null(e$graph) && !length(validate_graph(e$graph)),
                logical(1))
  xy <- lapply(reg, function(e) if (!is.null(e$graph)) as.numeric(unlist(e$graph$xy)))
  x <- ifelse(has, vapply(xy, function(v) v[1] %||% NA_real_, numeric(1)), NA_real_)
  y <- ifelse(has, vapply(xy, function(v) if (length(v) > 1) v[2] else NA_real_,
                          numeric(1)), NA_real_)
  free <- which(!has)
  if (length(free)) {
    x0 <- if (any(has)) min(x[has]) else 0
    y0 <- if (any(has)) max(y[has]) + 180 else 0
    per_row <- 6L
    k <- seq_along(free) - 1L
    x[free] <- x0 + (k %% per_row) * 240
    y[free] <- y0 + (k %/% per_row) * 140
  }
  data.frame(id = ids, x = unname(x), y = unname(y), stringsAsFactors = FALSE)
}

# Edge ids, unique and stable: from, to and type (with a counter when a pair
# has two edges of the same type).
edge_ids <- function(rel) {
  make.unique(paste(rel$from, rel$to, rel$type, sep = "__"), sep = "__")
}

# Is each entry active under the filters? An entry matches a group through
# its own group or any `also_in` group. Inactive nodes are dimmed, not
# removed, so the layout never changes. Named by id.
node_active <- function(reg, groups, types) {
  vapply(reg, function(e) any(entry_groups(e) %in% groups) && e$type %in% types,
         logical(1))
}

# Node table. `groups` and `types` are the active filters.
graph_nodes <- function(lang, dark = FALSE, groups = .dl_groups, types = .dl_types,
                        reg = dl_registry()) {
  pal <- graph_palette(dark)
  cat <- catalog_frame(reg)
  pos <- graph_layout(reg)
  name <- vapply(reg, function(e) entry_text(e, "names", lang), "")
  line <- unname(pal$group[cat$group])
  fill <- mix_hex(line, pal$surface, if (isTRUE(dark)) 0.30 else 0.14)
  fill_hi <- mix_hex(line, pal$surface, if (isTRUE(dark)) 0.50 else 0.30)
  active <- unname(node_active(reg, groups, types))
  esc <- htmltools::htmlEscape
  title <- sprintf("<strong>%s</strong><br>%s \u00b7 %s", esc(name),
                   esc(vapply(cat$group, function(g) tr(paste0("group.", g), lang), "")),
                   esc(vapply(cat$type, function(t) tr(paste0("type.", t), lang), "")))
  data.frame(
    # Ellipses fit their text tightly; en spaces keep the label off the border.
    id = cat$id, label = ifelse(cat$type == "discrete", unname(name),
                                paste0("\u2002", unname(name), "\u2002")),
    title = title,
    x = pos$x, y = pos$y,
    shape = ifelse(cat$type == "discrete", "box", "ellipse"),
    borderWidth = 2, borderWidthSelected = 4,
    color.background = fill, color.border = line,
    color.highlight.background = fill_hi, color.highlight.border = line,
    color.hover.background = fill_hi, color.hover.border = line,
    font.color = ifelse(active, pal$ink, alpha_hex(pal$ink, 0.35)),
    opacity = ifelse(active, 1, 0.25),
    dl_group = cat$group, dl_type = cat$type, dl_active = active,
    stringsAsFactors = FALSE
  )
}

# Edge table. `relations` are the visible relation types; edges touching a
# dimmed node are dimmed too.
graph_edges <- function(lang, dark = FALSE, groups = .dl_groups, types = .dl_types,
                        relations = .dl_relation_types, reg = dl_registry()) {
  pal <- graph_palette(dark)
  rel <- relations_frame(reg)
  if (!nrow(rel)) return(data.frame())
  active <- node_active(reg, groups, types)
  on <- active[rel$from] & active[rel$to]
  name <- vapply(reg, function(e) entry_text(e, "names", lang), "")
  cond <- if (lang == "pt-BR") rel$condition_pt else rel$condition_en
  type_label <- vapply(rel$type, function(t) tr(paste0("related.type.", t), lang), "")
  esc <- htmltools::htmlEscape
  title <- sprintf("<strong>%s \u2192 %s</strong><br>%s<br>%s", esc(name[rel$from]),
                   esc(name[rel$to]), esc(type_label), esc(cond))
  # Two edges in opposite directions are curved apart; the others are straight.
  pair <- paste(rel$from, rel$to) %in% paste(rel$to, rel$from) & rel$from != rel$to
  col <- unname(pal$relation[rel$type])
  data.frame(
    id = edge_ids(rel), from = rel$from, to = rel$to,
    label = ifelse(nzchar(rel$label), rel$label, unname(type_label)),
    title = title, arrows = "to",
    dashes = rel$type %in% .dl_dashed_relations,
    width = 2, selectionWidth = 2,
    color.color = ifelse(on, col, alpha_hex(col, 0.18)),
    color.highlight = col, color.hover = col,
    font.color = ifelse(on, pal$label, alpha_hex(pal$label, 0.30)),
    font.strokeColor = pal$surface,
    smooth.enabled = pair, smooth.type = "curvedCW", smooth.roundness = 0.32,
    hidden = !rel$type %in% relations,
    dl_type = rel$type, dl_active = unname(on),
    stringsAsFactors = FALSE
  )
}

# The map widget, with a fixed layout (no physics) and click events sent to
# the Shiny inputs `click_id` (list(kind, id)) and `dblclick_id` (node id).
graph_widget <- function(nodes, edges, click_id, dblclick_id, dark = FALSE) {
  pal <- graph_palette(dark)
  click <- sprintf(paste0(
    "function(p){if(!window.Shiny)return;var v={kind:'none',id:null};",
    "if(p.nodes.length){v={kind:'node',id:p.nodes[0]};}",
    "else if(p.edges.length){v={kind:'edge',id:p.edges[0]};}",
    "window.Shiny.setInputValue('%s',v,{priority:'event'});}"), click_id)
  dblclick <- sprintf(paste0(
    "function(p){if(window.Shiny&&p.nodes.length){",
    "window.Shiny.setInputValue('%s',p.nodes[0],{priority:'event'});}}"), dblclick_id)
  tooltip <- paste(
    "position: fixed; visibility: hidden; padding: 0.45rem 0.65rem;",
    "max-width: 320px; white-space: normal; font-family: Inter, system-ui, sans-serif;",
    "font-size: 0.82rem; line-height: 1.4; color: var(--dl-ink);",
    "background: var(--dl-surface); border: 1px solid var(--dl-border);",
    "border-radius: 0.5rem; box-shadow: var(--dl-shadow); z-index: 10;"
  )
  w <- visNetwork::visNetwork(nodes, edges, width = "100%", height = "100%")
  w <- visNetwork::visNodes(
    w, font = list(face = "Inter, system-ui, sans-serif", size = 22),
    margin = list(top = 10, bottom = 10, left = 14, right = 14),
    shapeProperties = list(borderRadius = 3), physics = FALSE, fixed = TRUE,
    labelHighlightBold = FALSE
  )
  w <- visNetwork::visEdges(
    w, font = list(face = "Inter, system-ui, sans-serif", size = 20, strokeWidth = 6,
                   align = "horizontal", color = pal$label, strokeColor = pal$surface),
    arrows = list(to = list(enabled = TRUE, scaleFactor = 0.7)),
    selfReference = list(size = 42, angle = pi / 2, renderBehindTheNode = TRUE),
    physics = FALSE, labelHighlightBold = FALSE, hoverWidth = 0.5
  )
  w <- visNetwork::visPhysics(w, enabled = FALSE)
  w <- visNetwork::visInteraction(
    w, dragNodes = FALSE, hover = TRUE, tooltipDelay = 250, tooltipStyle = tooltip,
    keyboard = list(enabled = TRUE, bindToWindow = FALSE), zoomSpeed = 0.6,
    selectConnectedEdges = TRUE
  )
  w <- visNetwork::visOptions(w, autoResize = TRUE)
  visNetwork::visEvents(w, click = click, doubleClick = dblclick)
}
