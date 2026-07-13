# Core SankeyPlot Tests for ccflowR
# Tests for basic functionality, method chaining, and rendering
# Flow-conserving edge weight tests are in test-sankey-weights.R

# =============================================================================
# SECTION 1: Basic Instantiation Tests
# =============================================================================

test_that("SankeyPlot creates valid plot", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  expect_s3_class(plot, "SankeyPlot")
  expect_s3_class(plot, "BasePlot")
  expect_s3_class(plot, "R6")
})

test_that("SankeyPlot validates required columns", {
  bad_data <- data.frame(x = 1, y = 2)

  expect_error(
    SankeyPlot$new(bad_data),
    "Missing required columns"
  )
})

test_that("SankeyPlot works with minimal required columns", {
  minimal_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("B", "C"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2")
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")

  expect_no_error({
    plot <- SankeyPlot$new(minimal_data, colors)
  })

  expect_s3_class(plot, "SankeyPlot")
})

test_that("SankeyPlot auto-assigns colors for missing cell types", {
  data <- data.frame(
    sender = c("A", "B", "C"),
    receiver = c("B", "C", "D"),
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3")
  )

  # Only provide colors for some cell types
  colors <- c(A = "#FF0000")

  expect_no_error({
    plot <- SankeyPlot$new(data, colors)
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot works with NULL colors", {
  ex <- example_communication()

  expect_no_error({
    plot <- SankeyPlot$new(ex$data, cell_colors = NULL)
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 2: Method Chaining Tests
# =============================================================================

test_that("SankeyPlot method chaining works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  # All methods should return self for chaining
  result <- plot$
    colors(color_by = "sender")$
    alpha(0.5)$
    nodes(thickness = 25, padding = 35)$
    layout(width = 800, height = 600)$
    add_title("Test Title")

  expect_s3_class(result, "SankeyPlot")
})

test_that("all chainable methods return SankeyPlot", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  expect_s3_class(plot$colors(color_by = "sender"), "SankeyPlot")
  expect_s3_class(plot$alpha(0.5), "SankeyPlot")
  expect_s3_class(plot$nodes(thickness = 25), "SankeyPlot")
  expect_s3_class(plot$cell_colors(list(Macrophage = "#000000")), "SankeyPlot")
  expect_s3_class(plot$group_colors(list(G1 = "#111111")), "SankeyPlot")
  expect_s3_class(plot$legend(show = TRUE), "SankeyPlot")
  expect_s3_class(plot$orientation("h"), "SankeyPlot")
  expect_s3_class(plot$label_style(size = 14), "SankeyPlot")
  expect_s3_class(plot$interactive(FALSE), "SankeyPlot")
})

# =============================================================================
# SECTION 3: Rendering Tests
# =============================================================================

test_that("SankeyPlot render returns plotly object", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot show returns plotly object", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  expect_no_error({
    fig <- plot$show()
  })

  expect_s3_class(fig, "plotly")
})

test_that("render can be called multiple times", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  fig1 <- plot$render()
  fig2 <- plot$render()

  expect_s3_class(fig1, "plotly")
  expect_s3_class(fig2, "plotly")
})

# =============================================================================
# SECTION 4: Coloring Strategy Tests
# =============================================================================

test_that("SankeyPlot color_by='sender' works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$colors(color_by = "sender")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot color_by='condition' works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$colors(color_by = "condition")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot color_by='ligand' works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$colors(color_by = "ligand")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot color_by='condition' works with group data", {
  ex <- example_multinichenet()
  plot <- SankeyPlot$new(ex$data, ex$colors, group_col = "group")$
    colors(color_by = "condition")$
    group_colors(ex$group_colors)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot alpha setting works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$alpha(0.3)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("backward compat: 'binary' maps to 'condition' with deprecation warning", {
  ex <- example_communication()
  expect_warning(
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      colors(color_by = "binary"),
    "deprecated"
  )
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 5: Node Configuration Tests
# =============================================================================

test_that("SankeyPlot node ordering works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  plot$node_order(
    senders = c("T_cell", "Macrophage"),
    receivers = c("B_cell", "Fibroblast")
  )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot nodes() sets thickness and padding", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$
    nodes(thickness = 30, padding = 50)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot orientation works", {
  ex <- example_communication()

  # Horizontal
  plot_h <- SankeyPlot$new(ex$data, ex$colors)$orientation("h")
  fig_h <- plot_h$render()
  expect_s3_class(fig_h, "plotly")

  # Vertical
  plot_v <- SankeyPlot$new(ex$data, ex$colors)$orientation("v")
  fig_v <- plot_v$render()
  expect_s3_class(fig_v, "plotly")
})

# =============================================================================
# SECTION 6: Label Style Tests
# =============================================================================

test_that("SankeyPlot label_style sets font size", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$label_style(size = 20)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot label_style sets position", {
  ex <- example_communication()

  for (pos in c("left", "right", "top", "bottom", "inside")) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$label_style(position = pos)
    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("SankeyPlot label_style rejects invalid position", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  expect_error(
    plot$label_style(position = "invalid"),
    "position must be one of"
  )
})

# =============================================================================
# SECTION 7: Highlight Pathway Tests
# =============================================================================

test_that("SankeyPlot highlight_pathway works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  plot$highlight_pathway(
    senders = "Macrophage",
    dim_alpha = 0.1
  )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot highlight_pathway with multiple criteria", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  plot$highlight_pathway(
    senders = c("Macrophage", "T_cell"),
    ligands = "TGFB1",
    receivers = "Fibroblast",
    dim_alpha = 0.05
  )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot clear_highlight works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  plot$highlight_pathway(senders = "Macrophage")
  plot$clear_highlight()

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 8: Legend and Title Tests
# =============================================================================

test_that("SankeyPlot legend settings work", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$
    legend(show = TRUE, title = "Cell Types")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot add_title works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$add_title("My Sankey Plot")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 9: Interactive Features Tests
# =============================================================================

test_that("SankeyPlot interactive filter dropdowns work", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$interactive(TRUE)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot interactive can be toggled", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  plot$interactive(TRUE)
  fig1 <- plot$render()
  expect_s3_class(fig1, "plotly")

  plot$interactive(FALSE)
  fig2 <- plot$render()
  expect_s3_class(fig2, "plotly")
})

# =============================================================================
# SECTION 10: Summary Tests
# =============================================================================

test_that("SankeyPlot summary returns correct info", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  summary <- plot$summary()

  expect_type(summary, "list")
  expect_true("n_interactions" %in% names(summary))
  expect_true("n_senders" %in% names(summary))
  expect_true("n_receivers" %in% names(summary))
  expect_true("n_ligands" %in% names(summary))
  expect_true("n_receptors" %in% names(summary))
  expect_true("color_by" %in% names(summary))
  expect_true("alpha" %in% names(summary))
  expect_equal(summary$n_interactions, nrow(ex$data))
})

# =============================================================================
# SECTION 11: Convenience Function Tests
# =============================================================================

test_that("sankey_flow convenience function works", {
  ex <- example_communication()

  expect_no_error({
    fig <- sankey_flow(ex$data, ex$colors, title = "Test")
  })

  expect_s3_class(fig, "plotly")
})

test_that("sankey_flow accepts all customization parameters", {
  ex <- example_communication()

  expect_no_error({
    fig <- sankey_flow(
      ex$data, ex$colors,
      color_by = "sender",
      title = "Custom Title",
      width = 900,
      height = 700,
      alpha = 0.8,
      label_size = 18,
      label_position = "top",
      interactive = TRUE
    )
  })

  expect_s3_class(fig, "plotly")
})

test_that("sankey_flow works without colors", {
  ex <- example_communication()

  expect_no_error({
    fig <- sankey_flow(ex$data, cell_colors = NULL)
  })

  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 12: Layout and Dimensions Tests
# =============================================================================

test_that("SankeyPlot layout sets dimensions", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors, width = 1000, height = 800)

  summary <- plot$summary()
  expect_equal(summary$dimensions[["width"]], 1000)
  expect_equal(summary$dimensions[["height"]], 800)
})

test_that("SankeyPlot layout method works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$layout(width = 1200, height = 900)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 14: Custom Column Names Tests
# =============================================================================

test_that("SankeyPlot works with standard column names", {
  # Note: Custom column name support is limited - use standard names
  custom_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("B", "C"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    activity = c(0.8, 0.6)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")

  expect_no_error({
    plot <- SankeyPlot$new(
      custom_data, colors,
      auto_prepare = FALSE
    )
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 15: Edge Cases Tests
# =============================================================================

test_that("SankeyPlot handles single interaction", {
  single_data <- data.frame(
    sender = "A",
    receiver = "B",
    ligand = "L1",
    receptor = "R1"
  )

  colors <- c(A = "#FF0000", B = "#00FF00")

  expect_no_error({
    plot <- SankeyPlot$new(single_data, colors)
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot handles self-loops (sender = receiver)", {
  self_loop_data <- data.frame(
    sender = c("A", "A"),
    receiver = c("A", "B"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2")
  )

  colors <- c(A = "#FF0000", B = "#00FF00")

  expect_no_error({
    plot <- SankeyPlot$new(self_loop_data, colors)
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot handles duplicate interactions", {
  dup_data <- data.frame(
    sender = c("A", "A", "A"),
    receiver = c("B", "B", "B"),
    ligand = c("L1", "L1", "L1"),
    receptor = c("R1", "R1", "R1"),
    activity = c(0.5, 0.6, 0.7)
  )

  colors <- c(A = "#FF0000", B = "#00FF00")

  expect_no_error({
    plot <- SankeyPlot$new(dup_data, colors)
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot handles special characters in names", {
  special_data <- data.frame(
    sender = c("Cell-Type_1", "Cell.Type.2"),
    receiver = c("Cell (3)", "Cell [4]"),
    ligand = c("L1/L2", "L3-complex"),
    receptor = c("R1+R2", "R3*")
  )

  colors <- c(`Cell-Type_1` = "#FF0000", Cell.Type.2 = "#00FF00",
              `Cell (3)` = "#0000FF", `Cell [4]` = "#FFFF00")

  expect_no_error({
    plot <- SankeyPlot$new(special_data, colors)
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})
