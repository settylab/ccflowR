# Snapshot Tests for ccflowR
# Visual regression testing for Sankey plots
#
# Note: For plotly figures, we use testthat's JSON snapshot testing
# since vdiffr is designed for ggplot2/base R graphics.
# This captures the plotly figure specification for regression testing.

# Helper function to extract comparable plotly structure
# Removes volatile fields like random IDs while keeping structure
extract_plot_structure <- function(fig) {
  # Get the plotly data structure
  fig_data <- plotly::plotly_build(fig)$x

  # Extract key structural elements
  structure <- list(
    # Sankey trace structure
    trace_type = fig_data$data[[1]]$type,
    node_count = length(fig_data$data[[1]]$node$label),
    node_labels = fig_data$data[[1]]$node$label,
    link_count = length(fig_data$data[[1]]$link$source),
    # Layout structure (remove margins which can vary)
    layout_title = fig_data$layout$title$text,
    layout_font_family = fig_data$layout$font$family,
    layout_font_size = fig_data$layout$font$size
  )

  structure
}

# ============================================================================
# Snapshot tests for coloring strategies
# ============================================================================

test_that("snapshot: sender coloring strategy structure", {
  # Use fixed seed for reproducibility

  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$colors(color_by = "sender")
  fig <- plot$render()

  structure <- extract_plot_structure(fig)

  expect_equal(structure$trace_type, "sankey")
  expect_snapshot(structure)
})

test_that("snapshot: ligand coloring strategy structure", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$colors(color_by = "ligand")
  fig <- plot$render()

  structure <- extract_plot_structure(fig)

  expect_equal(structure$trace_type, "sankey")
  expect_snapshot(structure)
})

test_that("snapshot: group coloring strategy structure", {
  ex <- example_multinichenet()

  plot <- SankeyPlot$new(
    ex$data,
    ex$colors,
    group_colors = ex$group_colors,
    group_col = "group"
  )
  plot$colors(color_by = "condition")
  fig <- plot$render()

  structure <- extract_plot_structure(fig)

  expect_equal(structure$trace_type, "sankey")
  expect_snapshot(structure)
})

test_that("snapshot: condition coloring strategy structure", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$colors(color_by = "condition")
  fig <- plot$render()

  structure <- extract_plot_structure(fig)

  expect_equal(structure$trace_type, "sankey")
  expect_snapshot(structure)
})

# ============================================================================
# Snapshot tests for node/link colors
# ============================================================================

test_that("snapshot: node colors are consistent", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Extract node colors
  fig_data <- plotly::plotly_build(fig)$x
  node_colors <- fig_data$data[[1]]$node$color

  expect_snapshot(node_colors)
})

test_that("snapshot: link colors with sender strategy", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$colors(color_by = "sender")
  fig <- plot$render()

  # Extract link colors
  fig_data <- plotly::plotly_build(fig)$x
  link_colors <- fig_data$data[[1]]$link$color

  # Just check count and that colors are present
  expect_true(length(link_colors) > 0)
  expect_snapshot(length(link_colors))
})

# ============================================================================
# Snapshot tests for combined configurations
# ============================================================================

test_that("snapshot: custom margins with title", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$font("Arial", 16)$
    margins(left = 100, right = 100, top = 120, bottom = 80)$
    add_title("Presentation Slide")
  fig <- plot$render()

  structure <- extract_plot_structure(fig)
  expect_snapshot(structure)
})
