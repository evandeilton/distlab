test_that("every node is positioned once, without NA or overlaps", {
  pos <- distlab:::graph_layout()
  expect_identical(pos$id, dl_ids())
  expect_false(anyNA(pos))
  d <- as.matrix(stats::dist(pos[, c("x", "y")]))
  diag(d) <- Inf
  expect_gt(min(d), 120)
})

test_that("entries without a position get one below the map", {
  reg <- dl_registry()
  reg$gamma$graph <- NULL
  reg$beta$graph <- NULL
  pos <- distlab:::graph_layout(reg)
  expect_false(anyNA(pos))
  placed <- pos$id %in% c("gamma", "beta")
  expect_true(all(pos$y[placed] > max(pos$y[!placed])))
  expect_false(anyDuplicated(paste(pos$x, pos$y)) > 0)
})

test_that("graph edges match the registry, one each, with styles by type", {
  rel <- dl_relations()
  e <- distlab:::graph_edges("en-US")
  expect_identical(nrow(e), nrow(rel))
  expect_identical(paste(e$from, e$to, e$dl_type), paste(rel$from, rel$to, rel$type))
  expect_false(anyDuplicated(e$id) > 0)
  expect_identical(e$dashes, rel$type == "limit")
  expect_true(all(e$arrows == "to"))
  pal <- distlab:::graph_palette(FALSE)$relation
  expect_identical(e$color.color, unname(pal[rel$type]))
  expect_false(anyNA(e))
  # Opposite pairs are curved, single edges straight.
  expect_true(e$smooth.enabled[e$from == "gamma" & e$to == "exponential"])
  expect_false(e$smooth.enabled[e$from == "normal" & e$to == "cauchy"])
})

test_that("graph nodes carry names, shapes, colors and escaped tooltips", {
  n <- distlab:::graph_nodes("pt-BR")
  expect_identical(n$id, dl_ids())
  expect_false(anyNA(n))
  expect_match(n$label[n$id == "gamma"], "Gama")
  expect_identical(n$shape[n$id == "poisson"], "box")
  expect_identical(n$shape[n$id == "normal"], "ellipse")
  reg <- dl_registry()
  reg$gamma$names[["en-US"]] <- "<img src=x onerror=alert(1)>"
  n <- distlab:::graph_nodes("en-US", reg = reg)
  expect_no_match(n$title, "<img", fixed = TRUE)
  expect_match(n$title[n$id == "gamma"], "&lt;img", fixed = TRUE)
  e <- distlab:::graph_edges("en-US", reg = reg)
  expect_no_match(e$title, "<img", fixed = TRUE)
})

test_that("filters dim nodes and edges and hide relation types", {
  n <- distlab:::graph_nodes("en-US", groups = "counts")
  expect_identical(n$opacity == 1, n$dl_group == "counts")
  e <- distlab:::graph_edges("en-US", groups = "counts", relations = "limit")
  expect_identical(e$hidden, e$dl_type != "limit")
  on <- e$from %in% dl_ids(group = "counts") & e$to %in% dl_ids(group = "counts")
  expect_identical(e$dl_active, on)
  n <- distlab:::graph_nodes("en-US", types = "continuous")
  expect_true(all(n$opacity[n$dl_type == "discrete"] < 1))
})

test_that("dark mode switches the palette", {
  light <- distlab:::graph_nodes("en-US", dark = FALSE)
  dark <- distlab:::graph_nodes("en-US", dark = TRUE)
  expect_false(any(light$color.border == dark$color.border))
  expect_identical(unique(dark$font.color), "#F1F2FA")
  expect_identical(unique(distlab:::graph_edges("en-US", dark = TRUE)$font.strokeColor),
                   "#1D1F40")
})

test_that("the stylesheet uses the map colors of R/graph.R", {
  css <- paste(readLines(system.file("app", "www", "distlab.css", package = "distlab")),
               collapse = "\n")
  for (mode in c("light", "dark")) {
    pal <- distlab:::graph_palette(mode == "dark")
    for (col in c(pal$group, pal$relation)) expect_match(css, col, fixed = TRUE, info = col)
  }
})

test_that("map text colors reach 4.5:1 on their backgrounds", {
  lum <- function(h) {
    v <- grDevices::col2rgb(h)[, 1] / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  ratio <- function(a, b) {
    l <- sort(c(lum(a), lum(b)), decreasing = TRUE)
    (l[1] + 0.05) / (l[2] + 0.05)
  }
  for (dk in c(FALSE, TRUE)) {
    pal <- distlab:::graph_palette(dk)
    expect_gte(ratio(pal$label, pal$surface), 4.5)
    n <- distlab:::graph_nodes("en-US", dark = dk)
    for (k in seq_len(nrow(n))) {
      expect_gte(ratio(n$font.color[k], n$color.highlight.background[k]), 4.5)
    }
  }
})

test_that("the widget has a fixed layout and click events", {
  w <- distlab:::graph_widget(distlab:::graph_nodes("en-US"),
                              distlab:::graph_edges("en-US"), "c-click", "c-dbl")
  expect_s3_class(w, "visNetwork")
  expect_false(w$x$options$physics$enabled)
  expect_false(w$x$options$interaction$dragNodes)
  expect_match(as.character(w$x$events$click), "c-click", fixed = TRUE)
  expect_match(as.character(w$x$events$doubleClick), "c-dbl", fixed = TRUE)
})
