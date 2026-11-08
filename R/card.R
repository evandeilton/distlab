# Reference card of a multivariate entry: story, support, parameters, formula
# sheet, a static illustration at the registry example values and the related
# distributions. Shown in a dialog over the open page.

# Formula rows of a card, with their label key and left-hand side (NULL when
# the registry expression is already a full statement).
.dl_mv_formula_rows <- list(
  mean = list(label = "formula.mean_vector", lhs = "\\operatorname{E}(X_i)"),
  covariance = list(label = "formula.covariance", lhs = "\\operatorname{Cov}(X_i, X_j)"),
  marginal = list(label = "formula.marginal", lhs = NULL),
  mgf = list(label = "formula.mgf", lhs = "M_{\\mathbf{X}}(\\mathbf{t})"),
  cf = list(label = "formula.cf", lhs = "\\varphi_{\\mathbf{X}}(\\mathbf{t})")
)

# Seed and size of the samples drawn for the illustrations, fixed so a card
# always shows the same picture.
.dl_mv_seed <- 20261107L
.dl_mv_draws <- 300L

# Dialog that holds the card; title and body are outputs, so they follow the
# language while the dialog is open. The title names the dialog.
card_modal <- function(ns, lang) {
  m <- shiny::modalDialog(
    shiny::uiOutput(ns("card")),
    title = shiny::textOutput(ns("card_title"), inline = TRUE),
    footer = shiny::modalButton(i18n_span("card.close", lang)),
    size = "xl", easyClose = TRUE
  )
  htmltools::tagAppendAttributes(m, role = "dialog", `aria-modal` = "true",
                                 `aria-labelledby` = ns("card_title"))
}

# Card dialog of the app. `request` is the navigation request of every page:
# list(card = id) opens the card of a multivariate entry over the open page,
# without switching tabs. Links inside the card open other cards, or a
# univariate law in the Explorer; returns a reactive with that request,
# list(tab = "explorer", id, n).
mod_card_server <- function(id, lang, request = shiny::reactive(NULL)) {
  lang_r <- if (shiny::is.reactive(lang)) lang else shiny::reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mv_ids <- dl_ids(dimension = "multivariate")
    # `card` is list(id, n); the counter re-renders a card opened again.
    card <- shiny::reactiveVal(NULL)
    route <- shiny::reactiveVal(NULL)
    count <- 0L
    show_card <- function(did) {
      count <<- count + 1L
      card(list(id = did, n = count))
      shiny::showModal(card_modal(ns, shiny::isolate(lang_r())))
    }
    shiny::observeEvent(request(), {
      r <- request()
      if (is_string(r$card) && r$card %in% mv_ids) show_card(r$card)
    })
    shiny::observeEvent(input$open, {
      did <- input$open
      if (is_string(did) && did %in% mv_ids) {
        show_card(did)
      } else if (is_string(did) && did %in% dl_ids()) {
        card(NULL)
        shiny::removeModal()
        count <<- count + 1L
        route(list(tab = "explorer", id = did, n = count))
      }
    })
    output$card_title <- shiny::renderText({
      shiny::req(card())
      entry_text(dl_entry(card()$id, multivariate = TRUE), "names", lang_r())
    })
    output$card <- shiny::renderUI({
      shiny::req(card())
      mv_card_tag(dl_entry(card()$id, multivariate = TRUE), lang_r(), ns("open"))
    })
    route
  })
}

# Body of the card. Links to other distributions go to `open_id`.
mv_card_tag <- function(entry, lang, open_id) {
  heading <- function(key, icon) {
    shiny::tags$h3(class = "dl-mv-sub", shiny::icon(icon, `aria-hidden` = "true"),
                   tr(key, lang))
  }
  shiny::tags$div(
    class = "dl-mv-card",
    shiny::tags$div(
      class = "dl-mv-head",
      entry_badges(entry, lang),
      shiny::tags$p(class = "dl-mv-note",
                    shiny::tags$span(class = "badge dl-badge-muted", tr("card.badge", lang)),
                    " ", tr("card.note", lang))
    ),
    shiny::tags$div(
      class = "dl-mv-grid",
      shiny::tags$section(
        class = "dl-mv-about",
        heading("card.story", "book-open"),
        shiny::tags$p(class = "dl-story", entry_text(entry, "story", lang)),
        mv_facts_tag(entry, lang)
      ),
      shiny::tags$section(
        class = "dl-mv-figure",
        heading("card.illustration", "chart-simple"),
        mv_figure_tag(entry, lang)
      )
    ),
    shiny::tags$section(
      class = "dl-mv-formulas",
      heading("card.formulas", "square-root-variable"),
      mv_formula_sheet(entry, lang)
    ),
    shiny::tags$section(
      class = "dl-mv-related",
      heading("related.title", "diagram-project"),
      related_list_tag(entry$id, lang, open_id)
    )
  )
}

# Support, parameters (symbol, R name and meaning) and R functions, when the
# entry has any.
mv_facts_tag <- function(entry, lang) {
  pars <- lapply(entry$parameters, function(p) {
    tex <- if (is.null(p$elements)) p$latex else paste(p$latex, "=", p$elements)
    shiny::tags$li(math_tag(tex), " ", shiny::tags$code(p$name),
                   shiny::tags$span(class = "dl-mv-desc", entry_text(p, "description", lang)))
  })
  shiny::tags$dl(
    class = "dl-panel-facts dl-mv-facts",
    shiny::tags$dt(tr("compendium.support", lang)),
    shiny::tags$dd(math_tag(support_math(entry))),
    shiny::tags$dt(tr("compendium.parameters", lang)),
    shiny::tags$dd(shiny::tags$ul(class = "dl-param-list", pars)),
    if (length(entry$functions)) {
      shiny::tagList(
        shiny::tags$dt(tr("card.functions", lang)),
        shiny::tags$dd(functions_tag(entry),
                       if (!is.null(entry$functions_note)) {
                         shiny::tags$span(class = "dl-mv-desc",
                                          entry_text(entry, "functions_note", lang))
                       })
      )
    }
  )
}

# Formula sheet: joint mass or density function, then the specs of
# .dl_mv_formula_rows, one row each.
mv_formula_sheet <- function(entry, lang) {
  pdf_key <- if (identical(entry$type, "discrete")) "formula.pmf" else "formula.pdf"
  row <- function(label, pieces, lhs) {
    shiny::tags$div(
      class = "dl-formula",
      shiny::tags$div(class = "dl-formula-label", tr(label, lang)),
      shiny::tags$div(class = "dl-formula-body",
                      lapply(pieces, formula_piece_tag, lhs = lhs, lang = lang))
    )
  }
  rows <- c(
    list(row(pdf_key, list(list(expr = entry$latex$pdf)), NULL)),
    lapply(names(.dl_mv_formula_rows), function(k) {
      spec <- .dl_mv_formula_rows[[k]]
      row(spec$label, formula_pieces(entry$latex[[k]]), spec$lhs)
    })
  )
  shiny::tags$div(class = "dl-formula-list", rows)
}

# Vector as text, "(0.2, 0.5, 0.3)" or "(0,2; 0,5; 0,3)", with 3 significant
# digits.
format_vector <- function(x, lang) {
  sep <- if (match_lang(lang) == "pt-BR") "; " else ", "
  paste0("(", paste(format_value(x, lang), collapse = sep), ")")
}

# Numbers behind the illustration of an entry, at its example values; NULL
# when the entry has none (or examples of an unsupported size).
# * categorical: probability of each category;
# * multinomial: expected counts n p_i and their standard deviations;
# * dirichlet (k = 3): draws on the simplex and the mean;
# * mvnormal (k = 2): draws and the 50% and 90% probability ellipses.
mv_illustration_data <- function(entry) {
  tryCatch(mv_illustration_values(entry), error = function(e) NULL)
}

# Body of mv_illustration_data(); may fail on unusable example values.
mv_illustration_values <- function(entry) {
  v <- mv_examples(entry)
  switch(
    entry$id,
    categorical = list(x = seq_along(v$prob), y = v$prob),
    multinomial = list(x = seq_along(v$prob), y = v$size * v$prob,
                       sd = sqrt(v$size * v$prob * (1 - v$prob))),
    dirichlet = if (length(v$alpha) == 3L) {
      a <- v$alpha
      n <- .dl_mv_draws
      # Normalized independent Gamma(alpha_i, 1) draws are Dirichlet(alpha).
      g <- with_seed(.dl_mv_seed,
                     matrix(stats::rgamma(n * 3L, shape = rep(a, each = n)), nrow = n))
      list(draws = g / rowSums(g), mean = a / sum(a))
    },
    mvnormal = if (length(v$mean) == 2L) {
      mu <- v$mean
      # Cholesky factor: X = mu + L Z with L L' = Sigma (chol() reads only
      # the upper triangle, so symmetry is checked first).
      if (!isSymmetric(v$sigma)) stop("sigma must be symmetric")
      L <- t(chol(v$sigma))
      z <- with_seed(.dl_mv_seed, matrix(stats::rnorm(2L * .dl_mv_draws), nrow = 2L))
      draws <- t(mu + L %*% z)
      theta <- seq(0, 2 * pi, length.out = 73)
      # The quadratic form (x - mu)' Sigma^-1 (x - mu) is chi-square with 2 df,
      # so the ellipse of radius sqrt(qchisq(p, 2)) holds probability p.
      levels <- c(0.5, 0.9)
      ellipses <- lapply(levels, function(p) {
        r <- sqrt(stats::qchisq(p, 2))
        t(mu + L %*% rbind(r * cos(theta), r * sin(theta)))
      })
      list(draws = draws, ellipses = ellipses, levels = levels, mean = mu,
           sigma = v$sigma)
    }
  )
}

# Figure of the card: the SVG and a caption that states the example values.
mv_figure_tag <- function(entry, lang) {
  d <- mv_illustration_data(entry)
  if (is.null(d)) return(shiny::tags$p(class = "dl-empty", tr("card.no_illustration", lang)))
  v <- mv_examples(entry)
  cap_id <- paste0("dl-mv-cap-", entry$id)
  caption <- switch(
    entry$id,
    categorical = tr("card.illus.categorical", lang, p = format_vector(v$prob, lang)),
    multinomial = tr("card.illus.multinomial", lang, n = format_value(v$size, lang),
                     p = format_vector(v$prob, lang)),
    dirichlet = tr("card.illus.dirichlet", lang, draws = .dl_mv_draws,
                   alpha = format_vector(v$alpha, lang)),
    mvnormal = tr("card.illus.mvnormal", lang, draws = .dl_mv_draws,
                  mu = format_vector(v$mean, lang),
                  v1 = format_value(v$sigma[1, 1], lang), v2 = format_value(v$sigma[2, 2], lang),
                  rho = format_value(v$sigma[1, 2] / sqrt(v$sigma[1, 1] * v$sigma[2, 2]), lang))
  )
  svg <- switch(entry$id,
                categorical = , multinomial = svg_bars(d, lang),
                dirichlet = svg_simplex(d),
                mvnormal = svg_scatter(d, lang))
  shiny::tags$figure(
    class = "dl-mv-fig",
    shiny::HTML(sprintf(
      '<svg class="dl-mv-plot" viewBox="0 0 320 240" role="img" aria-labelledby="%s" focusable="false">%s</svg>',
      cap_id, svg)),
    shiny::tags$figcaption(id = cap_id, class = "dl-mv-caption", caption)
  )
}

# SVG text element, escaped.
svg_text <- function(x, y, label, class = "dl-mv-text", anchor = "middle") {
  sprintf('<text class="%s" x="%.1f" y="%.1f" text-anchor="%s">%s</text>', class, x, y,
          anchor, htmltools::htmlEscape(label))
}

# Polyline path through the rows of a two-column matrix of pixel positions.
svg_path <- function(px, py, class, close = FALSE) {
  sprintf('<path class="%s" d="M%s%s"/>', class,
          paste(sprintf("%.1f,%.1f", px, py), collapse = " L"), if (close) " Z" else "")
}

# Bars at categories x with heights y, value labels and, when `sd` is given,
# a bar of one standard deviation on each side. Plot box: 320 x 240.
svg_bars <- function(d, lang) {
  left <- 44; right <- 308; top <- 16; bottom <- 214
  ticks <- pretty(c(0, max(d$y + (d$sd %||% 0))))
  ymax <- max(ticks)
  sy <- function(v) bottom - v / ymax * (bottom - top)
  slot <- (right - left) / length(d$x)
  cx <- left + (seq_along(d$x) - 0.5) * slot
  w <- slot * 0.56
  grid <- paste0(
    sprintf('<line class="dl-mv-gridline" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
            left, sy(ticks), right, sy(ticks)), collapse = "")
  ylab <- paste(vapply(seq_along(ticks), function(i) {
    svg_text(left - 6, sy(ticks[i]) + 4, format_num(ticks[i], lang), anchor = "end")
  }, ""), collapse = "")
  bars <- paste(sprintf('<rect class="dl-mv-bar" x="%.1f" y="%.1f" width="%.1f" height="%.1f"/>',
                        cx - w / 2, sy(d$y), w, bottom - sy(d$y)), collapse = "")
  whisk <- if (!is.null(d$sd)) {
    lo <- pmax(d$y - d$sd, 0)
    hi <- d$y + d$sd
    paste0(
      sprintf('<line class="dl-mv-whisker" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
              cx, sy(lo), cx, sy(hi)),
      sprintf('<line class="dl-mv-whisker" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
              cx - 6, sy(hi), cx + 6, sy(hi)),
      sprintf('<line class="dl-mv-whisker" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
              cx - 6, sy(lo), cx + 6, sy(lo)), collapse = "")
  } else ""
  # Value labels sit above the bar, or above the whisker when there is one.
  label_y <- sy(d$y + (d$sd %||% 0)) - 6
  vals <- paste(vapply(seq_along(d$y), function(i) {
    svg_text(cx[i], label_y[i], format_num(d$y[i], lang), class = "dl-mv-text dl-mv-value")
  }, ""), collapse = "")
  xlab <- paste(vapply(seq_along(d$x), function(i) {
    svg_text(cx[i], bottom + 16, as.character(d$x[i]))
  }, ""), collapse = "")
  axis <- sprintf('<line class="dl-mv-axis" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
                  left, bottom, right, bottom)
  paste0(grid, ylab, bars, whisk, axis, vals, xlab)
}

# Draws of a 3-part Dirichlet on the triangle of proportions: vertex i is
# the point where x_i = 1.
svg_simplex <- function(d) {
  # Equilateral triangle, 200 px wide.
  vx <- c(60, 260, 160)
  vy <- c(200, 200, 200 - 100 * sqrt(3))
  px <- d$draws %*% vx
  py <- d$draws %*% vy
  tri <- svg_path(vx, vy, "dl-mv-axis dl-mv-frame", close = TRUE)
  pts <- paste(sprintf('<circle class="dl-mv-point" cx="%.1f" cy="%.1f" r="2"/>', px, py),
               collapse = "")
  mean <- sprintf('<circle class="dl-mv-mean" cx="%.1f" cy="%.1f" r="5"/>',
                  sum(d$mean * vx), sum(d$mean * vy))
  labs <- paste0(svg_text(vx[1] - 4, vy[1] + 18, "x\u2081 = 1"),
                 svg_text(vx[2] + 4, vy[2] + 18, "x\u2082 = 1"),
                 svg_text(vx[3], vy[3] - 8, "x\u2083 = 1"))
  paste0(tri, pts, mean, labs)
}

# Scatter of bivariate draws with the probability ellipses, on a square
# window around the mean that holds both ellipses.
svg_scatter <- function(d, lang) {
  left <- 44; right <- 304; top <- 12; bottom <- 206
  # The window holds every draw and both ellipses.
  half <- ceiling(max(abs(sweep(rbind(d$draws, do.call(rbind, d$ellipses)), 2, d$mean))))
  lim1 <- d$mean[1] + c(-half, half)
  lim2 <- d$mean[2] + c(-half, half)
  # Equal scales on both axes keep the ellipses true to shape.
  side <- min(right - left, bottom - top)
  x0 <- left + (right - left - side) / 2
  sx <- function(v) x0 + (v - lim1[1]) / diff(lim1) * side
  sy <- function(v) bottom - (v - lim2[1]) / diff(lim2) * side
  pts <- paste(sprintf('<circle class="dl-mv-point" cx="%.1f" cy="%.1f" r="2"/>',
                       sx(d$draws[, 1]), sy(d$draws[, 2])), collapse = "")
  ell <- paste(vapply(seq_along(d$ellipses), function(i) {
    e <- d$ellipses[[i]]
    svg_path(sx(e[, 1]), sy(e[, 2]), paste0("dl-mv-ellipse dl-mv-ellipse-", i), close = TRUE)
  }, ""), collapse = "")
  t1 <- pretty(lim1)
  t1 <- t1[t1 >= lim1[1] & t1 <= lim1[2]]
  t2 <- pretty(lim2)
  t2 <- t2[t2 >= lim2[1] & t2 <= lim2[2]]
  frame <- sprintf('<rect class="dl-mv-axis dl-mv-frame" x="%.1f" y="%.1f" width="%.1f" height="%.1f"/>',
                   x0, bottom - side, side, side)
  grid <- paste0(
    paste(sprintf('<line class="dl-mv-gridline" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
                  sx(t1), bottom - side, sx(t1), bottom), collapse = ""),
    paste(sprintf('<line class="dl-mv-gridline" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
                  x0, sy(t2), x0 + side, sy(t2)), collapse = ""))
  labs <- paste0(
    paste(vapply(t1, function(v) svg_text(sx(v), bottom + 14, format_num(v, lang)), ""),
          collapse = ""),
    paste(vapply(t2, function(v) svg_text(x0 - 6, sy(v) + 4, format_num(v, lang),
                                          anchor = "end"), ""), collapse = ""),
    svg_text(x0 + side / 2, 236, "x\u2081", class = "dl-mv-text dl-mv-axis-title"),
    svg_text(x0 - 30, bottom - side / 2, "x\u2082", class = "dl-mv-text dl-mv-axis-title"))
  paste0(grid, frame, pts, ell, labs)
}
