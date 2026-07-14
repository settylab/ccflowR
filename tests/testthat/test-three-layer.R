# Tests for 3-layer Sankey plots (receptor_col = NULL)
# When receptor_col = NULL, the plot renders sender -> ligand -> receiver
# instead of the default sender -> ligand -> receptor -> receiver.

# =============================================================================
# SECTION 1: Construction & Basic Rendering
# =============================================================================

test_that("SankeyPlot creates 3-layer plot with receptor_col = NULL", {
  fix <- fixture_small_data()
  plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)

  expect_s3_class(plot, "SankeyPlot")
  expect_s3_class(plot, "R6")
})

test_that("3-layer plot renders to valid plotly object", {
  fix <- fixture_small_data()
  plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot works with data lacking receptor column entirely", {
  data <- data.frame(
    sender = c("CellA", "CellA", "CellB"),
    receiver = c("CellB", "CellC", "CellC"),
    ligand = c("LIG1", "LIG2", "LIG1"),
    stringsAsFactors = FALSE
  )
  colors <- c(CellA = "#E41A1C", CellB = "#377EB8", CellC = "#4DAF4A")

  plot <- SankeyPlot$new(data, colors, receptor_col = NULL)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot ignores receptor column when present in data but receptor_col = NULL", {
  fix <- fixture_small_data()
  # fix$data has a receptor column, but we set receptor_col = NULL
  plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot has correct node count (no receptor nodes)", {
  data <- data.frame(
    sender = c("A", "A", "B"),
    receiver = c("B", "C", "C"),
    ligand = c("L1", "L2", "L1"),
    stringsAsFactors = FALSE
  )
  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")

  plot <- SankeyPlot$new(data, colors, receptor_col = NULL)
  fig <- plot$render()

  # Nodes: 2 senders (A, B) + 2 ligands (L1, L2) + 2 receivers (B, C) = 6
  node_labels <- fig$x$attrs[[1]]$node$label
  expect_length(node_labels, 6)
})

test_that("3-layer plot has correct link count (2 per row, not 3)", {
  data <- data.frame(
    sender = c("A", "B"),
    receiver = c("B", "C"),
    ligand = c("L1", "L2"),
    stringsAsFactors = FALSE
  )
  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")

  plot <- SankeyPlot$new(data, colors, receptor_col = NULL)
  fig <- plot$render()

  # 2 rows * 2 links per row = 4 links
  link_sources <- fig$x$attrs[[1]]$link$source
  expect_length(link_sources, 4)
})


# =============================================================================
# SECTION 2: S3 API Layer (sankey_plot + sankey_flow)
# =============================================================================

test_that("sankey_plot() accepts receptor_col = NULL", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL)

  expect_s3_class(p, "sankey")
})

test_that("sankey_plot(receptor_col = NULL) renders correctly", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL)
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("sankey_flow() accepts receptor_col = NULL", {
  fix <- fixture_small_data()

  expect_no_error({
    # Capture output since sankey_flow calls show()
    invisible(capture.output(
      sankey_flow(fix$data, fix$colors, receptor_col = NULL)
    ))
  })
})


# =============================================================================
# SECTION 3: Summary
# =============================================================================

test_that("summary reports has_receptor = FALSE for 3-layer plot", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL)
  s <- summary(p)

  expect_false(s$has_receptor)
  expect_null(s$n_receptors)
})

test_that("summary reports has_receptor = TRUE for standard 4-layer plot", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors)
  s <- summary(p)

  expect_true(s$has_receptor)
  expect_true(s$n_receptors > 0)
})


# =============================================================================
# SECTION 4: Flow Method
# =============================================================================

test_that("3-layer plot forces uniform flow method", {
  ex <- example_communication()
  # flow_conserving should fall back to uniform when no receptor
  plot <- SankeyPlot$new(ex$data, ex$colors, receptor_col = NULL,
                          flow_method = "flow_conserving")

  s <- plot$summary()
  expect_s3_class(plot, "SankeyPlot")
  expect_equal(s$flow_method, "uniform")
})

test_that("flow_method('flow_conserving') quietly falls back in 3-layer mode", {
  fix <- fixture_small_data()
  plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)

  expect_silent(plot$flow_method("flow_conserving"))
  expect_equal(plot$summary()$flow_method, "uniform")
})


# =============================================================================
# SECTION 5: Coloring Strategies
# =============================================================================

test_that("3-layer plot works with all color-by strategies", {
  fix <- fixture_small_data()

  for (strategy in c("sender", "receiver", "ligand")) {
    plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)
    plot$colors(color_by = strategy)
    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("3-layer plot works with fixed color string", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_colors(by = "steelblue")
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot works with condition coloring", {
  mn <- fixture_multinichenet_data()
  plot <- SankeyPlot$new(mn$data, mn$colors, receptor_col = NULL,
                          group_col = "group")
  plot$colors(color_by = "condition")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})


# =============================================================================
# SECTION 6: Highlighting
# =============================================================================

test_that("3-layer highlight works with senders", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_highlight(senders = "CellA")
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer highlight works with ligands", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_highlight(ligands = "LIG1")
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer highlight works with receivers", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_highlight(receivers = "CellC")
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer highlight warns when receptor criteria specified", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL)

  expect_warning(
    p + sankey_highlight(receptors = "REC1"),
    "receptor layer is not displayed"
  )
})

test_that("3-layer highlight works with combined sender + ligand criteria", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_highlight(senders = "CellA", ligands = "LIG1")
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("clear_highlight works in 3-layer mode", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_highlight(senders = "CellA") +
    sankey_clear_highlight()
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})


# =============================================================================
# SECTION 7: Node Ordering
# =============================================================================

test_that("3-layer custom node order works for senders and receivers", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_order(
      senders = c("CellB", "CellA"),
      receivers = c("CellC", "CellB")
    )
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer custom node order works for ligands", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_order(ligands = c("LIG2", "LIG1"))
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer node_order warns when receptors specified", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL)

  expect_warning(
    p + sankey_order(receptors = c("REC1", "REC2")),
    "receptor layer is not displayed"
  )
})

test_that("3-layer custom order produces explicit x/y positions", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_order(senders = c("CellA", "CellB"))
  fig <- sankey_render(p)

  # When custom order is set, x positions should be present
  node_x <- fig$x$attrs[[1]]$node$x
  expect_false(is.null(node_x))

  # Should have 3 distinct x values (sender, ligand, receiver)
  expect_equal(length(unique(node_x)), 3)
})


# =============================================================================
# SECTION 8: Interactive Dropdowns
# =============================================================================

test_that("3-layer interactive mode renders without error", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_interactive()
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer interactive mode has 3 dropdown menus (no receptor)", {
  fix <- fixture_small_data()
  plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)
  plot$interactive(TRUE)
  fig <- plot$render()

  # Extract updatemenus from the layout
  layout_data <- fig$x$layoutAttrs
  has_updatemenus <- any(sapply(layout_data, function(x) !is.null(x$updatemenus)))
  expect_true(has_updatemenus)

  # Find the updatemenus
  for (la in layout_data) {
    if (!is.null(la$updatemenus)) {
      expect_length(la$updatemenus, 3)  # sender, ligand, receiver (no receptor)
      break
    }
  }
})


# =============================================================================
# SECTION 9: Node Color Modes
# =============================================================================

test_that("3-layer plot works with cell_type node coloring", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_node_colors("cell_type")
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})


# =============================================================================
# SECTION 10: Method Chaining (R6)
# =============================================================================

test_that("3-layer R6 method chaining works", {
  fix <- fixture_small_data()
  plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)

  result <- plot$
    colors(color_by = "sender")$
    alpha(0.5)$
    nodes(thickness = 25, padding = 35)$
    layout(width = 800, height = 600)$
    add_title("3-Layer Test")

  expect_s3_class(result, "SankeyPlot")

  fig <- result$render()
  expect_s3_class(fig, "plotly")
})


# =============================================================================
# SECTION 11: Full + Operator Pipeline
# =============================================================================

test_that("3-layer plot works with full customization pipeline", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_colors(by = "sender") +
    sankey_alpha(0.7) +
    sankey_title("Three Layer Sankey") +
    sankey_nodes(thickness = 20, padding = 30) +
    sankey_labels(size = 14) +
    sankey_layout(width = 800, height = 600)

  fig <- sankey_render(p)
  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot works with highlight + interactive", {
  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL) +
    sankey_highlight(senders = "CellA") +
    sankey_interactive()

  fig <- sankey_render(p)
  expect_s3_class(fig, "plotly")
})


# =============================================================================
# SECTION 12: Edge Cases
# =============================================================================

test_that("3-layer plot works with single row data", {
  data <- data.frame(
    sender = "A",
    receiver = "B",
    ligand = "L1",
    stringsAsFactors = FALSE
  )
  colors <- c(A = "#FF0000", B = "#0000FF")

  plot <- SankeyPlot$new(data, colors, receptor_col = NULL)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot works with example_communication data", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors, receptor_col = NULL)
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot works with multinichenet data", {
  mn <- example_multinichenet()
  p <- sankey_plot(mn$data, mn$colors, receptor_col = NULL, group_col = "group")
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot works with unicode data", {
  fix <- fixture_unicode_data()
  plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("3-layer plot works with special character data", {
  fix <- fixture_special_chars_data()
  plot <- SankeyPlot$new(fix$data, fix$colors, receptor_col = NULL)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("default receptor_col='receptor' still produces 4-layer plot", {
  fix <- fixture_small_data()
  plot <- SankeyPlot$new(fix$data, fix$colors)
  fig <- plot$render()

  # Should have receptor nodes (R: prefix)
  node_labels <- fig$x$attrs[[1]]$node$label
  # 4-layer: 3 links per row
  link_sources <- fig$x$attrs[[1]]$link$source
  expect_length(link_sources, nrow(fix$data) * 3)
})


# =============================================================================
# SECTION 13: Save/Export
# =============================================================================

test_that("3-layer plot saves to HTML", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  fix <- fixture_small_data()
  p <- sankey_plot(fix$data, fix$colors, receptor_col = NULL)

  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(sankey_save(p, tmp))
  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 0)
})
