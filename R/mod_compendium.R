# Compendium: every distribution at once, as a relationship map, a list of
# relations, cards by data type and a sortable table. Filters by group, type
# and relation type apply to every view.

mod_compendium_ui <- function(id, lang) {
  ns <- shiny::NS(id)
  reg <- dl_registry()
  view_title <- function(key, icon) {
    shiny::tags$span(class = "dl-nav-label", shiny::icon(icon, `aria-hidden` = "true"),
                     i18n_span(key, lang))
  }
  shiny::tags$div(
    class = "dl-compendium",
    shiny::tags$div(
      class = "dl-comp-head",
      shiny::tags$h1(class = "dl-page-title", i18n_span("nav.compendium", lang)),
      shiny::tags$p(class = "dl-muted dl-comp-sub", i18n_span("compendium.subtitle", lang))
    ),
    compendium_toolbar(ns, lang, reg),
    bslib::navset_underline(
      id = ns("view"),
      bslib::nav_panel(
        title = view_title("compendium.view.graph", "diagram-project"), value = "graph",
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4), xl = c(9, 3)), fill = FALSE,
          bslib::card(
            class = "dl-map-card", full_screen = TRUE,
            bslib::card_body(
              class = "dl-map-body", padding = 0,
              shiny::tags$div(
                class = "dl-map", role = "img",
                `data-i18n-aria-label` = "compendium.map_label",
                `aria-label` = tr("compendium.map_label", lang),
                visNetwork::visNetworkOutput(ns("graph"), height = "100%")
              )
            ),
            bslib::card_footer(graph_legend(lang, reg))
          ),
          bslib::card(
            class = "dl-comp-panel",
            bslib::card_header(shiny::tags$span(
              class = "dl-card-title", shiny::icon("circle-info", `aria-hidden` = "true"),
              i18n_span("compendium.details", lang))),
            bslib::card_body(
              shiny::tags$div(`aria-live` = "polite", shiny::uiOutput(ns("panel")))
            )
          )
        )
      ),
      bslib::nav_panel(
        title = view_title("compendium.view.relations", "list"), value = "relations",
        shiny::tags$div(
          class = "dl-view-head",
          shiny::tags$p(class = "dl-muted", i18n_span("compendium.relations.intro", lang)),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-primary dl-print",
                             shiny::icon("print", `aria-hidden` = "true"),
                             i18n_span("compendium.print", lang))
        ),
        shiny::uiOutput(ns("relations"))
      ),
      bslib::nav_panel(
        title = view_title("compendium.view.bytype", "shapes"), value = "bytype",
        shiny::uiOutput(ns("bytype"))
      ),
      bslib::nav_panel(
        title = view_title("compendium.view.table", "table"), value = "table",
        shiny::tags$div(
          class = "dl-view-head dl-table-tools",
          shiny::tags$label(`for` = ns("table_filter"), class = "visually-hidden",
                            i18n_span("compendium.table.filter", lang)),
          shiny::tags$input(
            id = ns("table_filter"), type = "search", class = "form-control dl-table-filter",
            autocomplete = "off", `data-target` = ns("catalog"),
            `data-i18n-placeholder` = "compendium.table.filter_placeholder",
            placeholder = tr("compendium.table.filter_placeholder", lang)
          )
        ),
        shiny::tags$div(id = ns("catalog"), class = "dl-table-wrap",
                        shiny::uiOutput(ns("table")))
      )
    )
  )
}

# Search box, filter menus and the reset button shared by every view.
compendium_toolbar <- function(ns, lang, reg) {
  groups <- registry_groups(reg)
  swatch_label <- function(cls, key) {
    shiny::tags$span(class = "dl-opt",
                     shiny::tags$span(class = cls, `aria-hidden` = "true"),
                     i18n_span(key, lang))
  }
  menu <- function(key, icon, input) {
    shiny::tags$div(
      class = "dropdown dl-filter",
      shiny::tags$button(
        type = "button", class = "btn btn-sm dl-filter-btn dropdown-toggle",
        `data-bs-toggle` = "dropdown", `data-bs-auto-close` = "outside",
        `aria-expanded` = "false",
        shiny::icon(icon, `aria-hidden` = "true"), i18n_span(key, lang)
      ),
      shiny::tags$div(class = "dropdown-menu dl-filter-menu", input)
    )
  }
  shiny::tags$div(
    class = "dl-comp-toolbar",
    shiny::tags$div(
      class = "dl-comp-search",
      shiny::selectizeInput(
        ns("search"), i18n_span("compendium.search_label", lang),
        choices = search_choices(lang), selected = "", width = "100%",
        options = list(placeholder = tr("compendium.search_placeholder", lang))
      )
    ),
    shiny::tags$div(
      class = "dl-comp-filters", role = "group",
      `data-i18n-aria-label` = "compendium.filters",
      `aria-label` = tr("compendium.filters", lang),
      menu("compendium.filter.groups", "layer-group", shiny::checkboxGroupInput(
        ns("groups"), i18n_span("compendium.filter.groups", lang),
        choiceNames = lapply(groups, function(g) {
          swatch_label(paste0("dl-swatch dl-swatch-", g), paste0("group.", g))
        }),
        choiceValues = groups, selected = groups
      )),
      menu("compendium.filter.types", "shapes", shiny::checkboxGroupInput(
        ns("types"), i18n_span("compendium.filter.types", lang),
        choiceNames = lapply(.dl_types, function(t) {
          swatch_label(paste0("dl-shape dl-shape-", t), paste0("type.", t))
        }),
        choiceValues = .dl_types, selected = .dl_types
      )),
      menu("compendium.filter.relations", "arrow-right-long", shiny::checkboxGroupInput(
        ns("rel_types"), i18n_span("compendium.filter.relations", lang),
        choiceNames = lapply(.dl_relation_types, function(t) {
          swatch_label(paste0("dl-line dl-line-", t), paste0("related.type.", t))
        }),
        choiceValues = .dl_relation_types, selected = .dl_relation_types
      )),
      shiny::actionButton(ns("reset"), i18n_span("compendium.reset", lang),
                          icon = shiny::icon("rotate-left", `aria-hidden` = "true"),
                          class = "btn-sm btn-outline-primary dl-reset"),
      shiny::uiOutput(ns("filter_note"), inline = TRUE)
    )
  )
}

# Legend of the map: group colors, node shapes and edge styles.
graph_legend <- function(lang, reg = dl_registry()) {
  item <- function(cls, key) {
    shiny::tags$li(shiny::tags$span(class = cls, `aria-hidden` = "true"),
                   i18n_span(key, lang))
  }
  shiny::tags$div(
    class = "dl-legend",
    shiny::tags$ul(
      class = "dl-legend-list", `data-i18n-aria-label` = "compendium.legend.groups",
      `aria-label` = tr("compendium.legend.groups", lang),
      lapply(registry_groups(reg), function(g) {
        item(paste0("dl-swatch dl-swatch-", g), paste0("group.", g))
      })
    ),
    shiny::tags$ul(
      class = "dl-legend-list", `data-i18n-aria-label` = "compendium.legend.styles",
      `aria-label` = tr("compendium.legend.styles", lang),
      item("dl-shape dl-shape-discrete", "type.discrete"),
      item("dl-shape dl-shape-continuous", "type.continuous"),
      lapply(.dl_relation_types, function(t) {
        item(paste0("dl-line dl-line-", t), paste0("related.type.", t))
      })
    )
  )
}

# `lang` may be fixed or reactive; `dark` a reactive logical; `request` a
# reactive navigation request from other pages (list(tab, group, ...)).
# Returns a reactive with the latest navigation request: list(tab, id, n),
# where tab is "explorer", "calculator" or "compare".
mod_compendium_server <- function(id, lang, dark = shiny::reactive(FALSE),
                                  request = shiny::reactive(NULL)) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reg <- dl_registry()
    rel <- relations_frame(reg)
    rel$id <- edge_ids(rel)
    groups_all <- registry_groups(reg)
    route <- shiny::reactiveVal(NULL)
    count <- 0L
    go <- function(tab, did = NULL) {
      count <<- count + 1L
      route(list(tab = tab, id = did, n = count))
    }
    # Current selection on the map: NULL or list(kind = "node" | "edge", id).
    sel <- shiny::reactiveVal(NULL)

    # A checkbox group with nothing checked reports NULL, as does one that has
    # not reported yet; only the second means "everything".
    seen <- new.env(parent = emptyenv())
    filter_value <- function(name, all) {
      v <- input[[name]]
      if (!is.null(v)) seen[[name]] <- TRUE
      if (is.null(v) && !isTRUE(seen[[name]])) all else as.character(v)
    }
    filters <- shiny::reactive(list(
      groups = filter_value("groups", groups_all),
      types = filter_value("types", .dl_types),
      relations = filter_value("rel_types", .dl_relation_types)
    ))

    proxy <- visNetwork::visNetworkProxy(ns("graph"), session = session)
    graph_data <- function(lg, dk, f) {
      list(nodes = graph_nodes(lg, dk, f$groups, f$types, reg = reg),
           edges = graph_edges(lg, dk, f$groups, f$types, f$relations, reg = reg))
    }
    # Drawn once; language, color mode and filters then update it in place,
    # so the view (zoom, pan, selection) is kept.
    output$graph <- visNetwork::renderVisNetwork({
      lg <- shiny::isolate(lang_r())
      dk <- shiny::isolate(dark())
      d <- graph_data(lg, dk, shiny::isolate(filters()))
      graph_widget(d$nodes, d$edges, ns("click"), ns("dblclick"), dark = dk)
    })
    shiny::observeEvent(list(lang_r(), dark(), filters()), {
      d <- graph_data(lang_r(), dark(), filters())
      visNetwork::visUpdateNodes(proxy, d$nodes)
      visNetwork::visUpdateEdges(proxy, d$edges)
    }, ignoreInit = TRUE)

    valid_node <- function(x) is_string(x) && x %in% names(reg)
    valid_edge <- function(x) is_string(x) && x %in% rel$id

    # Selection made outside the map is mirrored on it.
    # The map tab may be hidden or not drawn yet, so distlab.js waits for a
    # visible map before selecting and focusing.
    map_select <- function(kind, target, ends) {
      bslib::nav_select("view", "graph", session = session)
      session$sendCustomMessage("dl-map-select", list(
        map = ns("graph"), kind = kind, id = target, ends = as.list(ends)
      ))
    }
    select_node <- function(did) {
      sel(list(kind = "node", id = did))
      map_select("node", did, did)
    }
    select_edge <- function(eid) {
      r <- rel[rel$id == eid, ]
      # A hidden relation type is switched back on so the edge can be seen.
      f <- filters()
      if (!r$type %in% f$relations) {
        shiny::updateCheckboxGroupInput(session, "rel_types",
                                        selected = union(f$relations, r$type))
      }
      sel(list(kind = "edge", id = eid))
      map_select("edge", eid, unique(c(r$from, r$to)))
    }

    shiny::observeEvent(input$click, {
      k <- input$click$kind
      i <- input$click$id
      if (identical(k, "node") && valid_node(i)) {
        sel(list(kind = "node", id = i))
      } else if (identical(k, "edge") && valid_edge(i)) {
        sel(list(kind = "edge", id = i))
      } else {
        sel(NULL)
      }
    })
    shiny::observeEvent(input$dblclick, {
      if (valid_node(input$dblclick)) go("explorer", input$dblclick)
    })
    shiny::observeEvent(input$open, {
      if (valid_node(input$open)) go("explorer", input$open)
    })
    shiny::observeEvent(input$calc, {
      if (valid_node(input$calc)) go("calculator", input$calc)
    })
    shiny::observeEvent(input$compare, {
      if (valid_node(input$compare)) go("compare", input$compare)
    })
    shiny::observeEvent(input$pick_node, {
      if (valid_node(input$pick_node)) select_node(input$pick_node)
    })
    shiny::observeEvent(input$pick_edge, {
      if (valid_edge(input$pick_edge)) select_edge(input$pick_edge)
    })
    shiny::observeEvent(input$search, {
      if (valid_node(input$search)) {
        select_node(input$search)
        shiny::updateSelectizeInput(session, "search", selected = "")
      }
    })
    shiny::observeEvent(input$reset, {
      sel(NULL)
      shiny::updateCheckboxGroupInput(session, "groups", selected = groups_all)
      shiny::updateCheckboxGroupInput(session, "types", selected = .dl_types)
      shiny::updateCheckboxGroupInput(session, "rel_types", selected = .dl_relation_types)
      visNetwork::visUnselectAll(proxy)
      visNetwork::visFit(proxy, animation = list(duration = 500))
    })

    # Deep links from other pages: a data-type group opens the cards view
    # with that group only.
    shiny::observeEvent(request(), {
      r <- request()
      if (identical(r$tab, "compendium") && is_string(r$group) && r$group %in% groups_all) {
        shiny::updateCheckboxGroupInput(session, "groups", selected = r$group)
        shiny::updateCheckboxGroupInput(session, "types", selected = .dl_types)
        bslib::nav_select("view", "bytype", session = session)
      }
    })

    shiny::observeEvent(lang_r(), {
      shiny::updateSelectizeInput(
        session, "search", choices = search_choices(lang_r()), selected = "",
        options = list(placeholder = tr("compendium.search_placeholder", lang_r()))
      )
    }, ignoreInit = TRUE)

    # Note shown while any filter hides part of the catalog.
    output$filter_note <- shiny::renderUI({
      f <- filters()
      narrowed <- !setequal(f$groups, groups_all) || !setequal(f$types, .dl_types) ||
        !setequal(f$relations, .dl_relation_types)
      if (!narrowed) return(NULL)
      shiny::tags$span(class = "badge dl-filter-note", role = "status",
                       shiny::icon("filter", `aria-hidden` = "true"),
                       tr("compendium.filtered", lang_r()))
    })

    output$panel <- shiny::renderUI({
      s <- sel()
      lg <- lang_r()
      if (is.null(s)) return(empty_panel(lg, reg, rel))
      if (s$kind == "node") {
        node_panel(reg[[s$id]], lg, ns("open"), ns("pick_edge"), rel = rel,
                   calc_id = ns("calc"), compare_id = ns("compare"))
      } else {
        edge_panel(rel[rel$id == s$id, , drop = FALSE], lg, ns("open"), reg = reg)
      }
    })

    output$relations <- shiny::renderUI({
      f <- filters()
      relations_table(lang_r(), ns("open"), ns("pick_edge"), f$groups, f$types,
                      f$relations, reg = reg)
    })
    output$bytype <- shiny::renderUI({
      f <- filters()
      bytype_tag(lang_r(), ns("open"), f$groups, f$types, reg = reg)
    })
    output$table <- shiny::renderUI({
      f <- filters()
      catalog_table(lang_r(), ns("open"), f$groups, f$types, reg = reg)
    })

    route
  })
}

# Side panel before any selection: catalog size and how to use the map.
empty_panel <- function(lang, reg, rel) {
  stat <- function(n, key, icon) {
    shiny::tags$div(class = "dl-panel-stat",
                    shiny::tags$span(class = "dl-icon-chip", shiny::icon(icon, `aria-hidden` = "true")),
                    shiny::tags$span(shiny::tags$strong(n), " ", tr(key, lang)))
  }
  shiny::tags$div(
    class = "dl-panel-empty",
    shiny::tags$div(class = "dl-panel-stats",
                    stat(length(reg), "compendium.stat_dists", "shapes"),
                    stat(nrow(rel), "compendium.stat_relations", "diagram-project")),
    shiny::tags$p(tr("compendium.details_empty", lang)),
    shiny::tags$p(class = "dl-muted dl-panel-hint", tr("compendium.legend.hint", lang))
  )
}

# Badge of a relation type.
relation_badge <- function(type, lang) {
  shiny::tags$span(class = paste0("badge dl-rel-type dl-rel-", type),
                   tr(paste0("related.type.", type), lang))
}

# Badges of an entry: data-type group, discrete or continuous, and tier.
entry_badges <- function(entry, lang, group = TRUE) {
  shiny::tags$div(
    class = "dl-badges",
    if (group) shiny::tags$span(class = paste0("badge dl-badge-group dl-group-", entry$group),
                                tr(paste0("group.", entry$group), lang)),
    shiny::tags$span(class = paste0("badge dl-badge-type dl-type-", entry$type),
                     tr(paste0("type.", entry$type), lang)),
    shiny::tags$span(class = "badge dl-badge-tier", tr(paste0("tier.", entry$tier), lang))
  )
}

# Side panel of a selected distribution: name, badges, support, parameters,
# story excerpt, its relations (each selects the edge) and buttons that open
# it in the Explorer, the Calculator and Compare.
node_panel <- function(entry, lang, open_id, edge_id, rel = NULL, calc_id = NULL,
                       compare_id = NULL) {
  reg <- dl_registry()
  if (is.null(rel)) {
    rel <- relations_frame(reg)
    rel$id <- edge_ids(rel)
  }
  mine <- rel[rel$from == entry$id | rel$to == entry$id, , drop = FALSE]
  name <- function(i) entry_text(reg[[i]], "names", lang)
  shiny::tags$div(
    class = "dl-panel-node",
    shiny::tags$h2(class = "dl-panel-title", entry_text(entry, "names", lang)),
    entry_badges(entry, lang),
    shiny::tags$dl(
      class = "dl-panel-facts",
      shiny::tags$dt(tr("compendium.support", lang)),
      shiny::tags$dd(math_tag(paste("x \\in", support_tex(entry)))),
      shiny::tags$dt(tr("compendium.parameters", lang)),
      shiny::tags$dd(shiny::tags$ul(
        class = "dl-param-list",
        lapply(entry$parameters, function(p) {
          shiny::tags$li(math_tag(p$latex), " ", shiny::tags$code(p$name))
        })
      ))
    ),
    shiny::tags$p(class = "dl-panel-story", text_excerpt(entry_text(entry, "story", lang))),
    shiny::tags$h3(class = "dl-panel-sub",
                   tr(if (nrow(mine) == 1L) "compendium.relations_one" else
                     "compendium.relations_n", lang, n = nrow(mine))),
    if (nrow(mine)) {
      shiny::tags$ul(
        class = "dl-panel-rels",
        lapply(seq_len(nrow(mine)), function(k) {
          r <- mine[k, ]
          text <- paste(name(r$from), "\u2192", name(r$to))
          shiny::tags$li(
            relation_badge(r$type, lang), " ",
            dist_button(r$id, text, edge_id, class = "dl-link-btn",
                        label = tr("compendium.show_relation", lang, name = text))
          )
        })
      )
    },
    shiny::tags$div(
      class = "dl-panel-actions",
      dist_button(entry$id, tr("compendium.open", lang), open_id,
                  class = "btn btn-primary btn-sm",
                  label = tr("related.open", lang, name = entry_text(entry, "names", lang))),
      if (!is.null(calc_id)) {
        dist_button(entry$id, tr("compendium.calc", lang), calc_id,
                    class = "btn btn-outline-primary btn-sm",
                    label = tr("compendium.calc_label", lang, name = entry_text(entry, "names", lang)))
      },
      if (!is.null(compare_id)) {
        dist_button(entry$id, tr("compendium.compare", lang), compare_id,
                    class = "btn btn-outline-primary btn-sm",
                    label = tr("compendium.compare_label", lang, name = entry_text(entry, "names", lang)))
      }
    )
  )
}

# Side panel of a selected edge: path, type, condition, formula (KaTeX) and
# buttons that open either end in the Explorer.
edge_panel <- function(r, lang, open_id, reg = dl_registry()) {
  name <- function(i) entry_text(reg[[i]], "names", lang)
  cond <- if (lang == "pt-BR") r$condition_pt else r$condition_en
  ends <- unique(c(r$from, r$to))
  shiny::tags$div(
    class = "dl-panel-edge",
    shiny::tags$h2(
      class = "dl-panel-title dl-rel-path",
      name(r$from),
      shiny::tags$span(class = "dl-rel-arrow", `aria-hidden` = "true", "\u2192"),
      shiny::tags$span(class = "visually-hidden", tr("related.to", lang)),
      name(r$to)
    ),
    shiny::tags$div(class = "dl-badges", relation_badge(r$type, lang)),
    shiny::tags$p(class = "dl-rel-cond", cond),
    shiny::tags$div(class = "dl-rel-math dl-panel-math", math_tag(r$latex)),
    shiny::tags$div(
      class = "dl-panel-actions",
      lapply(ends, function(i) {
        dist_button(i, tr("compendium.open_name", lang, name = name(i)), open_id,
                    class = "btn btn-outline-primary btn-sm",
                    label = tr("related.open", lang, name = name(i)))
      })
    )
  )
}

# Accessible list of every relation shown under the filters: a table with
# both ends (open in the Explorer), type, condition, formula and a button
# that shows the edge on the map.
relations_table <- function(lang, open_id, edge_id, groups = .dl_groups,
                            types = .dl_types, relations = .dl_relation_types,
                            reg = dl_registry()) {
  rel <- relations_frame(reg)
  rel$id <- edge_ids(rel)
  # Same rule as the map: an edge counts when both ends are active.
  active <- node_active(reg, groups, types)
  keep <- active[rel$from] & active[rel$to] & rel$type %in% relations
  rel <- rel[keep, , drop = FALSE]
  if (!nrow(rel)) return(shiny::tags$p(class = "dl-empty", tr("compendium.empty", lang)))
  name <- function(i) entry_text(reg[[i]], "names", lang)
  end_btn <- function(i) {
    dist_button(i, name(i), open_id, class = "dl-link-btn",
                label = tr("related.open", lang, name = name(i)))
  }
  th <- function(key) shiny::tags$th(scope = "col", tr(key, lang))
  rows <- lapply(seq_len(nrow(rel)), function(k) {
    r <- rel[k, ]
    shiny::tags$tr(
      shiny::tags$td(end_btn(r$from)),
      shiny::tags$td(relation_badge(r$type, lang)),
      shiny::tags$td(end_btn(r$to)),
      shiny::tags$td(class = "dl-cell-cond",
                     if (lang == "pt-BR") r$condition_pt else r$condition_en),
      shiny::tags$td(class = "dl-cell-math", math_tag(r$latex)),
      shiny::tags$td(class = "dl-cell-action",
                     dist_button(r$id, tr("compendium.show_on_map", lang), edge_id,
                                 class = "btn btn-sm btn-outline-primary",
                                 label = tr("compendium.show_relation", lang,
                                            name = paste(name(r$from), "\u2192", name(r$to)))))
    )
  })
  shiny::tags$div(
    class = "table-responsive dl-table-card",
    shiny::tags$table(
      class = "table dl-table dl-rel-table",
      shiny::tags$caption(class = "visually-hidden", tr("compendium.view.relations", lang)),
      shiny::tags$thead(shiny::tags$tr(
        th("compendium.col.from"), th("compendium.col.type"), th("compendium.col.to"),
        th("compendium.col.condition"), th("compendium.col.formula"),
        shiny::tags$th(scope = "col", shiny::tags$span(class = "visually-hidden",
                                                      tr("compendium.col.action", lang)))
      )),
      shiny::tags$tbody(rows)
    )
  )
}

# Entries listed under a group: those of the group and those cross-listed
# there through `also_in`.
group_members <- function(g, reg = dl_registry()) {
  names(reg)[vapply(reg, function(e) g %in% entry_groups(e), logical(1))]
}

# Cards by data type: one section per group, one block per family subgroup,
# one card per distribution.
bytype_tag <- function(lang, open_id, groups = .dl_groups, types = .dl_types,
                       reg = dl_registry()) {
  cat <- catalog_frame(reg)
  label <- stats::setNames(vapply(reg, function(e) entry_text(e, "names", lang), ""),
                           names(reg))
  sections <- lapply(intersect(registry_groups(reg), groups), function(g) {
    ids <- group_members(g, reg)
    ids <- ids[cat$type[match(ids, cat$id)] %in% types]
    if (!length(ids)) return(NULL)
    subs <- unique(vapply(reg[ids], `[[`, "", "subgroup"))
    sub_names <- vapply(subs, subgroup_label, "", lang = lang)
    subs <- subs[order(sub_names, method = "radix")]
    blocks <- lapply(subs, function(sg) {
      members <- ids[vapply(reg[ids], function(e) identical(e$subgroup, sg), logical(1))]
      members <- members[order(label[members], method = "radix")]
      shiny::tags$div(
        class = "dl-family",
        shiny::tags$h3(class = "dl-family-title", subgroup_label(sg, lang)),
        shiny::tags$div(class = "dl-dist-cards",
                        lapply(members, function(i) {
                          dist_card(reg[[i]], lang, open_id, cat$n_relations[cat$id == i],
                                    cross = !identical(reg[[i]]$group, g))
                        }))
      )
    })
    shiny::tags$section(
      class = paste0("dl-type-section dl-group-", g),
      `aria-labelledby` = paste0(open_id, "-", g),
      shiny::tags$div(
        class = "dl-group-head",
        shiny::tags$span(class = "dl-icon-chip",
                         shiny::icon(.dl_group_icons[[g]], `aria-hidden` = "true")),
        shiny::tags$div(
          shiny::tags$h2(class = "dl-group-title", id = paste0(open_id, "-", g),
                         tr(paste0("group.", g), lang)),
          shiny::tags$p(class = "dl-group-count",
                        tr(paste0("group.", g, ".desc"), lang))
        )
      ),
      shiny::tags$div(class = "dl-families", blocks)
    )
  })
  sections <- Filter(Negate(is.null), sections)
  if (!length(sections)) return(shiny::tags$p(class = "dl-empty", tr("compendium.empty", lang)))
  shiny::tags$div(class = "dl-bytype", sections)
}

# One distribution card: the name opens the Explorer (the whole card is
# clickable), with support, parameter symbols, badges and relation count.
dist_card <- function(entry, lang, open_id, n_rel, cross = FALSE) {
  name <- entry_text(entry, "names", lang)
  shiny::tags$article(
    class = paste("dl-dist-card", paste0("dl-group-", entry$group),
                  if (cross) "dl-cross"),
    shiny::tags$h4(class = "dl-dist-card-title",
                   dist_button(entry$id, name, open_id, class = "dl-card-link",
                               label = tr("related.open", lang, name = name))),
    shiny::tags$div(class = "dl-dist-card-math",
                    math_tag(paste("x \\in", support_tex(entry)))),
    shiny::tags$div(class = "dl-dist-card-params",
                    shiny::tags$span(class = "dl-muted", tr("compendium.parameters", lang)),
                    " ", math_tag(params_tex(entry))),
    shiny::tags$div(
      class = "dl-dist-card-foot",
      entry_badges(entry, lang, group = cross),
      shiny::tags$span(class = "dl-rel-count",
                       shiny::icon("diagram-project", `aria-hidden` = "true"),
                       tr(if (n_rel == 1L) "compendium.relations_one" else
                         "compendium.relations_n", lang, n = n_rel))
    )
  )
}

# R functions of an entry, as code chips.
functions_tag <- function(entry) {
  shiny::tags$span(class = "dl-fun-list",
                   lapply(c("d", "p", "q", "r"), function(k) {
                     shiny::tags$code(entry$functions[[k]])
                   }))
}

# Sortable, searchable table of every distribution under the filters.
# Sorting and the text filter run in the browser (distlab.js); every cell
# carries a sort key and every row its search text.
catalog_table <- function(lang, open_id, groups = .dl_groups, types = .dl_types,
                          reg = dl_registry()) {
  cat <- catalog_frame(reg)
  cat <- cat[node_active(reg, groups, types)[cat$id], , drop = FALSE]
  if (!nrow(cat)) return(shiny::tags$p(class = "dl-empty", tr("compendium.empty", lang)))
  name <- vapply(cat$id, function(i) entry_text(reg[[i]], "names", lang), "")
  cat <- cat[order(name, method = "radix"), , drop = FALSE]
  name <- vapply(cat$id, function(i) entry_text(reg[[i]], "names", lang), "")
  plain_th <- function(key) shiny::tags$th(scope = "col", tr(key, lang))
  sort_th <- function(key, col, numeric = FALSE) {
    shiny::tags$th(
      scope = "col", `aria-sort` = if (col == 0L) "ascending" else "none",
      shiny::tags$button(type = "button", class = "dl-sort", `data-col` = col,
                         `data-numeric` = if (numeric) "true" else "false",
                         tr(key, lang),
                         shiny::tags$span(class = "dl-sort-icon", `aria-hidden` = "true"))
    )
  }
  rows <- lapply(seq_len(nrow(cat)), function(k) {
    e <- reg[[cat$id[k]]]
    grp <- tr(paste0("group.", e$group), lang)
    fam <- subgroup_label(e$subgroup, lang)
    typ <- tr(paste0("type.", e$type), lang)
    search <- paste(e$names[["en-US"]], e$names[["pt-BR"]], e$id, grp, fam, typ,
                    paste(unlist(e$functions), collapse = " "))
    shiny::tags$tr(
      `data-search` = search,
      shiny::tags$td(`data-sort` = name[k],
                     dist_button(e$id, name[k], open_id, class = "dl-link-btn",
                                 label = tr("related.open", lang, name = name[k]))),
      shiny::tags$td(`data-sort` = grp, grp),
      shiny::tags$td(`data-sort` = fam, fam),
      shiny::tags$td(`data-sort` = typ, typ),
      shiny::tags$td(class = "dl-cell-math", `data-sort` = support_tex(e),
                     math_tag(support_tex(e))),
      shiny::tags$td(class = "dl-cell-math", `data-sort` = cat$n_params[k],
                     math_tag(params_tex(e))),
      shiny::tags$td(`data-sort` = e$functions$d, functions_tag(e)),
      shiny::tags$td(class = "dl-cell-num", `data-sort` = cat$n_relations[k],
                     cat$n_relations[k])
    )
  })
  shiny::tags$div(
    class = "table-responsive dl-table-card",
    shiny::tags$table(
      class = "table dl-table dl-catalog-table dl-sortable",
      shiny::tags$caption(class = "visually-hidden", tr("compendium.view.table", lang)),
      shiny::tags$thead(shiny::tags$tr(
        sort_th("compendium.col.name", 0L), sort_th("compendium.col.group", 1L),
        sort_th("compendium.col.family", 2L), sort_th("compendium.col.type", 3L),
        plain_th("compendium.col.support"), sort_th("compendium.col.parameters", 5L, TRUE),
        sort_th("compendium.col.functions", 6L), sort_th("compendium.col.relations", 7L, TRUE)
      )),
      shiny::tags$tbody(rows)
    ),
    shiny::tags$p(class = "dl-empty dl-table-empty", hidden = NA,
                  tr("compendium.table.no_match", lang))
  )
}
