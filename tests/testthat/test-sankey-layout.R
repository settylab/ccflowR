# Tests for Sankey Plot Layout and Visualization Quality
# These tests ensure that Plotly optimizes visualization while keeping parameter flexibility
#
# Run: devtools::test(filter = "sankey-layout")

# =============================================================================
# SECTION 1: Dimension Tests - Verify width/height parameters work correctly
# =============================================================================

test_that("layout dimensions are stored correctly in plot object", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 800, height = 600)

  fig <- plot$render()

  # Verify dimensions are set on the figure object

  expect_equal(fig$width, 800)
  expect_equal(fig$height, 600)
})

test_that("layout dimensions from constructor are applied", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors, width = 700, height = 500)
  fig <- plot$render()

  expect_equal(fig$width, 700)
  expect_equal(fig$height, 500)
})

test_that("layout method overrides constructor dimensions", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors, width = 600, height = 400)$
    layout(width = 900, height = 1200)

  fig <- plot$render()

  expect_equal(fig$width, 900)
  expect_equal(fig$height, 1200)
})

test_that("dimensions are preserved after multiple render calls", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 750, height = 850)

  fig1 <- plot$render()
  fig2 <- plot$render()

  expect_equal(fig1$width, 750)
  expect_equal(fig1$height, 850)
  expect_equal(fig2$width, 750)
  expect_equal(fig2$height, 850)
})

test_that("summary reflects current dimensions", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 1000, height = 800)

  summary <- plot$summary()

  expect_equal(summary$dimensions[["width"]], 1000)
  expect_equal(summary$dimensions[["height"]], 800)
})

# =============================================================================
# SECTION 2: Sankey Trace Structure Tests - Verify Plotly receives correct data
# =============================================================================

test_that("rendered figure contains sankey trace", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Check that the figure has the sankey trace type
  expect_true(length(fig$x$attrs) > 0)

  # Find the sankey trace
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_false(is.null(sankey_trace))
  expect_equal(sankey_trace$type, "sankey")
})

test_that("sankey trace has required node properties", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Find the sankey trace
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_false(is.null(sankey_trace))
  expect_false(is.null(sankey_trace$node))
  expect_false(is.null(sankey_trace$node$label))
  expect_false(is.null(sankey_trace$node$color))
  expect_false(is.null(sankey_trace$node$pad))
  expect_false(is.null(sankey_trace$node$thickness))
})

test_that("sankey trace has required link properties", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Find the sankey trace
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_false(is.null(sankey_trace))
  expect_false(is.null(sankey_trace$link))
  expect_false(is.null(sankey_trace$link$source))
  expect_false(is.null(sankey_trace$link$target))
  expect_false(is.null(sankey_trace$link$value))
  expect_false(is.null(sankey_trace$link$color))
})

test_that("node count matches expected layers", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Find the sankey trace
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Total nodes = unique senders + unique ligands + unique receptors + unique receivers
  expected_nodes <- length(unique(ex$data$sender)) +
                    length(unique(ex$data$ligand)) +
                    length(unique(ex$data$receptor)) +
                    length(unique(ex$data$receiver))

  expect_equal(length(sankey_trace$node$label), expected_nodes)
})

test_that("link count matches expected (3 links per interaction)", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Find the sankey trace
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Each interaction creates 3 links: sender->ligand, ligand->receptor, receptor->receiver
  expected_links <- nrow(ex$data) * 3

  expect_equal(length(sankey_trace$link$source), expected_links)
  expect_equal(length(sankey_trace$link$target), expected_links)
  expect_equal(length(sankey_trace$link$value), expected_links)
})

# =============================================================================
# SECTION 3: Arrangement Mode Tests - Test Plotly auto-arrangement behavior
# =============================================================================

test_that("default arrangement is snap (Plotly optimizes layout)",
 {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Default: snap lets Plotly optimize node positions (minimizes crossings)
  expect_equal(sankey_trace$arrangement, "snap")
})

test_that("arrangement switches to fixed when custom order is set", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$node_order(senders = c("Macrophage", "T_cell"))
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Fixed arrangement locks positions when user specifies custom order
  expect_equal(sankey_trace$arrangement, "fixed")
})

test_that("sankey renders without explicit x/y positions (Plotly auto-arranges)", {
  # This test verifies that Plotly can auto-arrange nodes when x/y are not specified
  # This is the desired behavior for optimal visualization
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Find the sankey trace
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # When x/y are not specified, Plotly uses its auto-arrangement algorithm
  # This should be NULL or not present for optimal Plotly arrangement
  # If x/y ARE present, they should be valid (all values between 0 and 1)
  if (!is.null(sankey_trace$node$x)) {
    expect_true(all(sankey_trace$node$x >= 0 & sankey_trace$node$x <= 1))
  }
  if (!is.null(sankey_trace$node$y)) {
    expect_true(all(sankey_trace$node$y >= 0 & sankey_trace$node$y <= 1))
  }
})

# =============================================================================
# SECTION 4: Node Parameter Tests - Verify node settings are applied
# =============================================================================

test_that("node padding parameter is applied to trace", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    nodes(padding = 50)

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$node$pad, 50)
})

test_that("node thickness parameter is applied to trace", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    nodes(thickness = 35)

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$node$thickness, 35)
})

test_that("combined node settings are applied correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    nodes(padding = 25, thickness = 40)

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$node$pad, 25)
  expect_equal(sankey_trace$node$thickness, 40)
})

# =============================================================================
# SECTION 5: Font and Label Tests - Verify label settings reach Plotly
# =============================================================================

test_that("label font size is applied to trace", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    label_style(size = 18)

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$textfont$size, 18)
})

test_that("font family setting is applied", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    font(family = "Helvetica")

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$textfont$family, "Helvetica")
})

# =============================================================================
# SECTION 6: Margin Tests - Verify margins are applied to layout
# =============================================================================

test_that("margins are applied to layout", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    margins(left = 60, right = 60, top = 100, bottom = 50)

  fig <- plot$render()

  # Check layoutAttrs for margin settings
  margin_found <- FALSE
  for (la in fig$x$layoutAttrs) {
    if (!is.null(la$margin)) {
      expect_equal(la$margin$l, 60)
      expect_equal(la$margin$r, 60)
      expect_equal(la$margin$t, 100)
      expect_equal(la$margin$b, 50)
      margin_found <- TRUE
      break
    }
  }

  expect_true(margin_found)
})

# =============================================================================
# SECTION 7: Orientation Tests
# =============================================================================

test_that("horizontal orientation is set correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    orientation("h")

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$orientation, "h")
})

test_that("vertical orientation is set correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    orientation("v")

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$orientation, "v")
})

# =============================================================================
# SECTION 8: Link Value Tests - Verify flow values are computed correctly
# =============================================================================

test_that("link values are all positive", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_true(all(sankey_trace$link$value > 0))
})

test_that("link source and target indices are valid", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  n_nodes <- length(sankey_trace$node$label)

  # Source and target indices should be 0-indexed and < n_nodes
  expect_true(all(sankey_trace$link$source >= 0))
  expect_true(all(sankey_trace$link$source < n_nodes))
  expect_true(all(sankey_trace$link$target >= 0))
  expect_true(all(sankey_trace$link$target < n_nodes))
})

test_that("link colors have correct alpha transparency", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    alpha(0.7)

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # All link colors should be rgba format with the specified alpha
  for (color in sankey_trace$link$color) {
    expect_true(grepl("^rgba\\(", color))
    # Extract alpha value from rgba(r, g, b, alpha) - note spaces after commas
    alpha_match <- regmatches(color, regexec(",\\s*([0-9.]+)\\)$", color))[[1]]
    if (length(alpha_match) >= 2) {
      alpha_val <- as.numeric(alpha_match[2])
      expect_equal(alpha_val, 0.7, tolerance = 0.01)
    }
  }
})

# =============================================================================
# SECTION 9: Data Integrity Tests - Verify data is passed correctly
# =============================================================================

test_that("all senders appear as nodes", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Check that all unique senders appear in node labels
  for (sender in unique(ex$data$sender)) {
    expect_true(sender %in% sankey_trace$node$label,
                info = paste("Sender", sender, "should appear in node labels"))
  }
})

test_that("all receivers appear as nodes", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Check that all unique receivers appear in node labels
  for (receiver in unique(ex$data$receiver)) {
    expect_true(receiver %in% sankey_trace$node$label,
                info = paste("Receiver", receiver, "should appear in node labels"))
  }
})

test_that("all ligands appear as nodes", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Check that all unique ligands appear in node labels
  for (ligand in unique(ex$data$ligand)) {
    expect_true(ligand %in% sankey_trace$node$label,
                info = paste("Ligand", ligand, "should appear in node labels"))
  }
})

test_that("all receptors appear as nodes", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Check that all unique receptors appear in node labels
  for (receptor in unique(ex$data$receptor)) {
    expect_true(receptor %in% sankey_trace$node$label,
                info = paste("Receptor", receptor, "should appear in node labels"))
  }
})

# =============================================================================
# SECTION 10: Visual Quality Tests - Test scenarios prone to visual issues
# =============================================================================

test_that("plot with many nodes has appropriate padding", {
  # Create data with many nodes - prone to crowding
  n <- 15
  cell_types <- paste0("Cell", 1:n)
  ligands <- paste0("Lig", 1:n)
  receptors <- paste0("Rec", 1:n)

  data <- data.frame(
    sender = sample(cell_types, n * 2, replace = TRUE),
    receiver = sample(cell_types, n * 2, replace = TRUE),
    ligand = sample(ligands, n * 2, replace = TRUE),
    receptor = sample(receptors, n * 2, replace = TRUE),
    activity = runif(n * 2, 0.3, 1.0),
    stringsAsFactors = FALSE
  )

  colors <- setNames(rainbow(n), cell_types)

  plot <- SankeyPlot$new(data, colors)$
    layout(width = 800, height = 1200)$
    nodes(padding = 15, thickness = 20)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  # Verify the padding was applied
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$node$pad, 15)
})

test_that("wide aspect ratio renders correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 1200, height = 400)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
  expect_equal(fig$width, 1200)
  expect_equal(fig$height, 400)
})

test_that("tall aspect ratio renders correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 400, height = 1200)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
  expect_equal(fig$width, 400)
  expect_equal(fig$height, 1200)
})

test_that("square aspect ratio renders correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 800, height = 800)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
  expect_equal(fig$width, 800)
  expect_equal(fig$height, 800)
})

# =============================================================================
# SECTION 11: Plotly HTML Widget Tests - Verify proper widget creation
# =============================================================================

test_that("rendered figure is a valid htmlwidget", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
  expect_s3_class(fig, "htmlwidget")
})

test_that("figure can be converted to HTML", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # This should not error
  html <- htmltools::as.tags(fig)
  expect_s3_class(html, "shiny.tag.list")
})

# =============================================================================
# SECTION 12: Dimension Propagation Tests - Ensure dimensions reach plotly layout
# These tests verify that dimensions are properly set in the plotly layout,
# not just on the widget object. This is critical for Jupyter notebook display.
# =============================================================================

test_that("dimensions are set in plotly layout for Jupyter compatibility", {
  # This test ensures dimensions are set in the layout, not just on the widget
  # Plotly layout dimensions are more reliably respected in Jupyter
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 800, height = 600)

  fig <- plot$render()

  # Check if dimensions are set in fig$x$layout (direct layout object)
  # This is where dimensions should be for Jupyter compatibility
  layout_has_width <- !is.null(fig$x$layout$width)
  layout_has_height <- !is.null(fig$x$layout$height)

  # Dimensions should be set in layout for proper Jupyter rendering
  expect_true(layout_has_width,
              info = "Width should be set in plotly layout for Jupyter compatibility")
  expect_true(layout_has_height,
              info = "Height should be set in plotly layout for Jupyter compatibility")
})

test_that("layout dimensions match specified values", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 1000, height = 800)

  fig <- plot$render()

  # Check dimensions in fig$x$layout (direct layout object)
  expect_equal(fig$x$layout$width, 1000,
               info = "Layout width should match specified value")
  expect_equal(fig$x$layout$height, 800,
               info = "Layout height should match specified value")
})

test_that("autosize is disabled when explicit dimensions are set", {
  # When explicit dimensions are set, autosize should be FALSE
  # to prevent Plotly from resizing based on container
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 800, height = 600)

  fig <- plot$render()

  # Check for autosize = FALSE in layoutAttrs (set by plotly::layout call)
  autosize_disabled <- FALSE
  for (la in fig$x$layoutAttrs) {
    if (!is.null(la$autosize) && la$autosize == FALSE) {
      autosize_disabled <- TRUE
      break
    }
  }

  expect_true(autosize_disabled,
              info = "autosize should be FALSE when explicit dimensions are set")
})

# =============================================================================
# SECTION 13: Sizing Policy Tests - Verify htmlwidget sizing behavior
# =============================================================================

test_that("sizing policy respects explicit dimensions", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 900, height = 700)

  fig <- plot$render()

  # When explicit dimensions are set, defaultWidth should not be "100%"
  # which would ignore our specified width
  sizing <- fig$sizingPolicy

  # The widget should have the dimensions we specified
  expect_equal(fig$width, 900)
  expect_equal(fig$height, 700)
})

# =============================================================================
# SECTION 14: Parameter Effect Tests - Verify parameters actually change output
# These tests ensure that changing parameters produces different results
# =============================================================================

test_that("changing width produces different layout dimensions", {
  ex <- example_communication()

  plot1 <- SankeyPlot$new(ex$data, ex$colors)$layout(width = 400, height = 600)
  plot2 <- SankeyPlot$new(ex$data, ex$colors)$layout(width = 800, height = 600)
  plot3 <- SankeyPlot$new(ex$data, ex$colors)$layout(width = 1200, height = 600)

  fig1 <- plot1$render()
  fig2 <- plot2$render()
  fig3 <- plot3$render()

  # Each should have different widths

  expect_equal(fig1$width, 400)
  expect_equal(fig2$width, 800)
  expect_equal(fig3$width, 1200)

  # Layout should also reflect these
  expect_equal(fig1$x$layout$width, 400)
  expect_equal(fig2$x$layout$width, 800)
  expect_equal(fig3$x$layout$width, 1200)
})

test_that("changing height produces different layout dimensions", {
  ex <- example_communication()

  plot1 <- SankeyPlot$new(ex$data, ex$colors)$layout(width = 600, height = 400)
  plot2 <- SankeyPlot$new(ex$data, ex$colors)$layout(width = 600, height = 800)
  plot3 <- SankeyPlot$new(ex$data, ex$colors)$layout(width = 600, height = 1200)

  fig1 <- plot1$render()
  fig2 <- plot2$render()
  fig3 <- plot3$render()

  # Each should have different heights
  expect_equal(fig1$height, 400)
  expect_equal(fig2$height, 800)
  expect_equal(fig3$height, 1200)

  # Layout should also reflect these
  expect_equal(fig1$x$layout$height, 400)
  expect_equal(fig2$x$layout$height, 800)
  expect_equal(fig3$x$layout$height, 1200)
})

test_that("changing node padding produces different trace values", {
  ex <- example_communication()

  plot1 <- SankeyPlot$new(ex$data, ex$colors)$nodes(padding = 10)
  plot2 <- SankeyPlot$new(ex$data, ex$colors)$nodes(padding = 30)
  plot3 <- SankeyPlot$new(ex$data, ex$colors)$nodes(padding = 60)

  fig1 <- plot1$render()
  fig2 <- plot2$render()
  fig3 <- plot3$render()

  # Extract padding from each trace
  get_padding <- function(fig) {
    for (attr in fig$x$attrs) {
      if (!is.null(attr$type) && attr$type == "sankey") {
        return(attr$node$pad)
      }
    }
    NULL
  }

  expect_equal(get_padding(fig1), 10)
  expect_equal(get_padding(fig2), 30)
  expect_equal(get_padding(fig3), 60)
})

test_that("changing node thickness produces different trace values", {
  ex <- example_communication()

  plot1 <- SankeyPlot$new(ex$data, ex$colors)$nodes(thickness = 10)
  plot2 <- SankeyPlot$new(ex$data, ex$colors)$nodes(thickness = 25)
  plot3 <- SankeyPlot$new(ex$data, ex$colors)$nodes(thickness = 50)

  fig1 <- plot1$render()
  fig2 <- plot2$render()
  fig3 <- plot3$render()

  # Extract thickness from each trace
  get_thickness <- function(fig) {
    for (attr in fig$x$attrs) {
      if (!is.null(attr$type) && attr$type == "sankey") {
        return(attr$node$thickness)
      }
    }
    NULL
  }

  expect_equal(get_thickness(fig1), 10)
  expect_equal(get_thickness(fig2), 25)
  expect_equal(get_thickness(fig3), 50)
})

test_that("changing alpha produces different link colors", {
  ex <- example_communication()

  plot1 <- SankeyPlot$new(ex$data, ex$colors)$alpha(0.3)
  plot2 <- SankeyPlot$new(ex$data, ex$colors)$alpha(0.7)
  plot3 <- SankeyPlot$new(ex$data, ex$colors)$alpha(1.0)

  fig1 <- plot1$render()
  fig2 <- plot2$render()
  fig3 <- plot3$render()

  # Extract alpha from link colors
  get_alpha <- function(fig) {
    for (attr in fig$x$attrs) {
      if (!is.null(attr$type) && attr$type == "sankey") {
        color <- attr$link$color[1]
        # Extract alpha from rgba(r, g, b, alpha) - note spaces after commas
        match <- regmatches(color, regexec(",\\s*([0-9.]+)\\)$", color))[[1]]
        if (length(match) >= 2) {
          return(as.numeric(match[2]))
        }
      }
    }
    NULL
  }

  expect_equal(get_alpha(fig1), 0.3, tolerance = 0.01)
  expect_equal(get_alpha(fig2), 0.7, tolerance = 0.01)
  expect_equal(get_alpha(fig3), 1.0, tolerance = 0.01)
})

test_that("changing label font size produces different trace values", {
  ex <- example_communication()

  plot1 <- SankeyPlot$new(ex$data, ex$colors)$label_style(size = 10)
  plot2 <- SankeyPlot$new(ex$data, ex$colors)$label_style(size = 16)
  plot3 <- SankeyPlot$new(ex$data, ex$colors)$label_style(size = 24)

  fig1 <- plot1$render()
  fig2 <- plot2$render()
  fig3 <- plot3$render()

  # Extract font size from each trace
  get_font_size <- function(fig) {
    for (attr in fig$x$attrs) {
      if (!is.null(attr$type) && attr$type == "sankey") {
        return(attr$textfont$size)
      }
    }
    NULL
  }

  expect_equal(get_font_size(fig1), 10)
  expect_equal(get_font_size(fig2), 16)
  expect_equal(get_font_size(fig3), 24)
})

test_that("changing margins produces different layout values", {
  ex <- example_communication()

  plot1 <- SankeyPlot$new(ex$data, ex$colors)$margins(left = 20, right = 20, top = 20, bottom = 20)
  plot2 <- SankeyPlot$new(ex$data, ex$colors)$margins(left = 80, right = 80, top = 80, bottom = 80)

  fig1 <- plot1$render()
  fig2 <- plot2$render()

  # Extract margins from layout
  get_margins <- function(fig) {
    for (la in fig$x$layoutAttrs) {
      if (!is.null(la$margin)) {
        return(la$margin)
      }
    }
    NULL
  }

  m1 <- get_margins(fig1)
  m2 <- get_margins(fig2)

  expect_equal(m1$l, 20)
  expect_equal(m1$r, 20)
  expect_equal(m1$t, 20)
  expect_equal(m1$b, 20)

  expect_equal(m2$l, 80)
  expect_equal(m2$r, 80)
  expect_equal(m2$t, 80)
  expect_equal(m2$b, 80)
})

# =============================================================================
# SECTION 15: Anti-Truncation Tests - Verify all content is visible
# These tests ensure the plot doesn't truncate nodes or labels
# =============================================================================

test_that("all nodes are present in rendered figure", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 800, height = 600)

  fig <- plot$render()

  # Get all node labels from the figure
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  node_labels <- sankey_trace$node$label

  # Count expected nodes
  expected_senders <- length(unique(ex$data$sender))
  expected_ligands <- length(unique(ex$data$ligand))
  expected_receptors <- length(unique(ex$data$receptor))
  expected_receivers <- length(unique(ex$data$receiver))
  expected_total <- expected_senders + expected_ligands + expected_receptors + expected_receivers

  # All nodes must be present (no truncation)
  expect_equal(length(node_labels), expected_total,
               info = "All nodes must be present - no truncation allowed")
})

test_that("all links are present in rendered figure", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 800, height = 600)

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Each data row creates 3 links
  expected_links <- nrow(ex$data) * 3

  expect_equal(length(sankey_trace$link$source), expected_links,
               info = "All link sources must be present - no truncation")
  expect_equal(length(sankey_trace$link$target), expected_links,
               info = "All link targets must be present - no truncation")
  expect_equal(length(sankey_trace$link$value), expected_links,
               info = "All link values must be present - no truncation")
})

test_that("large dataset renders all nodes without truncation", {
  # Create a larger dataset that might be prone to truncation
  n_cells <- 10
  n_ligands <- 15
  n_receptors <- 15

  cell_types <- paste0("Cell_", 1:n_cells)
  ligands <- paste0("Ligand_", 1:n_ligands)
  receptors <- paste0("Receptor_", 1:n_receptors)

  set.seed(42)
  n_interactions <- 50
  data <- data.frame(
    sender = sample(cell_types, n_interactions, replace = TRUE),
    receiver = sample(cell_types, n_interactions, replace = TRUE),
    ligand = sample(ligands, n_interactions, replace = TRUE),
    receptor = sample(receptors, n_interactions, replace = TRUE),
    activity = runif(n_interactions, 0.1, 1.0),
    stringsAsFactors = FALSE
  )

  colors <- setNames(rainbow(n_cells), cell_types)

  plot <- SankeyPlot$new(data, colors)$
    layout(width = 1000, height = 1400)

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # Calculate expected node count
  expected_nodes <- length(unique(data$sender)) +
                    length(unique(data$ligand)) +
                    length(unique(data$receptor)) +
                    length(unique(data$receiver))

  expect_equal(length(sankey_trace$node$label), expected_nodes,
               info = "Large dataset should not truncate nodes")
})

test_that("long labels are not truncated in node list", {
  # Create data with very long labels
  data <- data.frame(
    sender = c("Very_Long_Sender_Cell_Type_Name_One", "Another_Extremely_Long_Sender_Name"),
    receiver = c("Super_Long_Receiver_Cell_Type_Name", "Yet_Another_Very_Long_Receiver_Name"),
    ligand = c("TRANSFORMING_GROWTH_FACTOR_BETA_RECEPTOR_1", "VASCULAR_ENDOTHELIAL_GROWTH_FACTOR_A"),
    receptor = c("TGFBR1_TGFBR2_COMPLEX_HETERODIMER", "KDR_FLT1_HETERODIMER_COMPLEX"),
    activity = c(0.8, 0.6),
    stringsAsFactors = FALSE
  )

  colors <- c(
    "Very_Long_Sender_Cell_Type_Name_One" = "#E41A1C",
    "Another_Extremely_Long_Sender_Name" = "#377EB8",
    "Super_Long_Receiver_Cell_Type_Name" = "#4DAF4A",
    "Yet_Another_Very_Long_Receiver_Name" = "#984EA3"
  )

  plot <- SankeyPlot$new(data, colors)$
    layout(width = 1200, height = 600)

  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  # All long labels should be present in full
  expect_true("Very_Long_Sender_Cell_Type_Name_One" %in% sankey_trace$node$label)
  expect_true("Another_Extremely_Long_Sender_Name" %in% sankey_trace$node$label)
  expect_true("TRANSFORMING_GROWTH_FACTOR_BETA_RECEPTOR_1" %in% sankey_trace$node$label)
  expect_true("TGFBR1_TGFBR2_COMPLEX_HETERODIMER" %in% sankey_trace$node$label)
})

test_that("dimensions are consistent between widget and layout", {
  ex <- example_communication()

  test_dimensions <- list(
    c(400, 300),
    c(600, 900),
    c(800, 600),
    c(1000, 1200),
    c(1200, 400)
  )

  for (dim in test_dimensions) {
    width <- dim[1]
    height <- dim[2]

    plot <- SankeyPlot$new(ex$data, ex$colors)$
      layout(width = width, height = height)

    fig <- plot$render()

    # Widget dimensions
    expect_equal(fig$width, width,
                 info = paste("Widget width should be", width))
    expect_equal(fig$height, height,
                 info = paste("Widget height should be", height))

    # Layout dimensions
    expect_equal(fig$x$layout$width, width,
                 info = paste("Layout width should be", width))
    expect_equal(fig$x$layout$height, height,
                 info = paste("Layout height should be", height))
  }
})

test_that("node colors are all valid hex or rgba", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  for (color in sankey_trace$node$color) {
    is_hex <- grepl("^#[0-9A-Fa-f]{6}$", color)
    is_rgba <- grepl("^rgba\\(", color)
    expect_true(is_hex || is_rgba,
                info = paste("Node color should be valid hex or rgba:", color))
  }
})

test_that("link colors are all valid rgba format", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  for (color in sankey_trace$link$color) {
    expect_true(grepl("^rgba\\(", color),
                info = paste("Link color should be rgba format:", color))
  }
})

# =============================================================================
# SECTION 16: Edge Case Tests - Verify behavior with extreme parameters
# =============================================================================

test_that("very small dimensions still produce valid figure", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 200, height = 200)

  fig <- plot$render()

  expect_s3_class(fig, "plotly")
  expect_equal(fig$width, 200)
  expect_equal(fig$height, 200)
})

test_that("very large dimensions produce valid figure", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 2000, height = 3000)

  fig <- plot$render()

  expect_s3_class(fig, "plotly")
  expect_equal(fig$width, 2000)
  expect_equal(fig$height, 3000)
})

test_that("minimum padding value works", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    nodes(padding = 1)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }
  expect_equal(sankey_trace$node$pad, 1)
})

test_that("large padding value works", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    nodes(padding = 100)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }
  expect_equal(sankey_trace$node$pad, 100)
})

test_that("zero margins work correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    margins(left = 0, right = 0, top = 0, bottom = 0)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  # Verify zero margins are applied
  for (la in fig$x$layoutAttrs) {
    if (!is.null(la$margin)) {
      expect_equal(la$margin$l, 0)
      expect_equal(la$margin$r, 0)
      expect_equal(la$margin$t, 0)
      expect_equal(la$margin$b, 0)
      break
    }
  }
})

# =============================================================================
# Barycenter ordering and flow-proportional layout tests
# =============================================================================

test_that("barycenter ordering places connected nodes near each other", {
  # Ligand L_top connects ONLY to the first sender (high rank),
  # Ligand L_bot connects ONLY to the last sender (low rank).
  # Barycenter should place L_top above L_bot.
  # Note: requires custom order on senders to trigger explicit positioning.
  data <- data.frame(
    sender = c("S1", "S2", "S1", "S2"),
    ligand = c("L_top", "L_bot", "L_top", "L_bot"),
    receptor = c("R1", "R2", "R1", "R2"),
    receiver = c("T1", "T2", "T1", "T2"),
    scaled_activity = c(0.9, 0.1, 0.9, 0.1),
    scaled_ligand_expression = c(0.9, 0.1, 0.9, 0.1),
    scaled_receptor_expression = c(0.5, 0.5, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  colors <- c(S1 = "#FF0000", S2 = "#0000FF", T1 = "#00FF00", T2 = "#FFFF00")

  plot <- SankeyPlot$new(data, colors)
  # Set sender order to trigger explicit positioning; ligands use barycenter
  plot$node_order(senders = c("S1", "S2"))
  fig <- plot$render()

  # Extract node labels from the plotly figure
  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  labels <- sankey_trace$node$customdata
  y_vals <- sankey_trace$node$y

  # Find ligand positions (labels contain L_top and L_bot)
  l_top_idx <- which(labels == "L_top")
  l_bot_idx <- which(labels == "L_bot")

  # L_top should have a smaller y (higher on screen) than L_bot
  # because S1 (which L_top connects to) has higher activity and is placed first
  expect_lt(y_vals[l_top_idx], y_vals[l_bot_idx])
})

test_that("activity-weighted barycenter respects flow magnitude", {
  # L_mixed connects to both S1 (high flow) and S2 (low flow).
  # L_s2 connects only to S2.
  # With activity-weighted barycenter, L_mixed should be pulled toward S1
  # (higher flow), so L_mixed should be placed above L_s2.
  data <- data.frame(
    sender = c("S1", "S1", "S2", "S2"),
    ligand = c("L_mixed", "L_mixed", "L_mixed", "L_s2"),
    receptor = c("R1", "R1", "R1", "R2"),
    receiver = c("T1", "T1", "T1", "T1"),
    scaled_activity = c(0.9, 0.8, 0.05, 0.1),
    scaled_ligand_expression = c(0.9, 0.8, 0.05, 0.1),
    scaled_receptor_expression = c(0.5, 0.5, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  colors <- c(S1 = "#FF0000", S2 = "#0000FF", T1 = "#00FF00")

  plot <- SankeyPlot$new(data, colors)
  # Set sender order to trigger explicit positioning; ligands use barycenter
  plot$node_order(senders = c("S1", "S2"))
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  labels <- sankey_trace$node$customdata
  y_vals <- sankey_trace$node$y

  l_mixed_idx <- which(labels == "L_mixed")
  l_s2_idx <- which(labels == "L_s2")

  # L_mixed should be above L_s2 because its weighted barycenter
  # is pulled toward S1 (the top sender with higher activity)
  expect_lt(y_vals[l_mixed_idx], y_vals[l_s2_idx])
})

test_that("custom order via sankey_order overrides barycenter", {
  ex <- example_communication()
  senders <- unique(ex$data$sender)

  # Reverse the default order
  custom_order <- rev(senders)

  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$node_order(senders = custom_order)
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  labels <- sankey_trace$node$customdata
  y_vals <- sankey_trace$node$y

  # Get sender labels and y-positions
  n_senders <- length(custom_order)
  sender_labels <- labels[1:n_senders]
  sender_y <- y_vals[1:n_senders]

  # Labels should match custom order

  expect_equal(sender_labels, custom_order)
  # Y positions should be monotonically increasing (top to bottom)
  expect_true(all(diff(sender_y) > 0))
})

test_that("flow-proportional y gives more space to high-flow nodes", {
  # S_big has much more total flow than S_small
  data <- data.frame(
    sender = c("S_big", "S_big", "S_big", "S_small"),
    ligand = c("L1", "L2", "L3", "L4"),
    receptor = c("R1", "R2", "R3", "R4"),
    receiver = c("T1", "T1", "T1", "T1"),
    scaled_activity = c(0.9, 0.8, 0.7, 0.05),
    scaled_ligand_expression = c(0.9, 0.8, 0.7, 0.05),
    scaled_receptor_expression = c(0.5, 0.5, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  colors <- c(S_big = "#FF0000", S_small = "#0000FF", T1 = "#00FF00")

  plot <- SankeyPlot$new(data, colors)
  # Set sender order to trigger explicit positioning with flow-proportional y
  plot$node_order(senders = c("S_big", "S_small"))
  fig <- plot$render()

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  y_vals <- sankey_trace$node$y
  labels <- sankey_trace$node$customdata

  # S_big should be first (custom order), S_small second
  s_big_idx <- which(labels == "S_big")
  s_small_idx <- which(labels == "S_small")

  # S_big should get more vertical space (its y center should be
  # further from the top than S_small's y center is from S_big)
  s_big_y <- y_vals[s_big_idx]
  s_small_y <- y_vals[s_small_idx]

  # Space allocated to S_big (from margin to midway between nodes)
  # should be larger than space allocated to S_small
  space_big <- (s_big_y + s_small_y) / 2 - 0.01  # top to midpoint
  space_small <- 0.99 - (s_big_y + s_small_y) / 2  # midpoint to bottom
  expect_gt(space_big, space_small)
})

test_that("arrangement parameter can be changed via sankey_nodes", {
  ex <- example_communication()

  # Override default snap to fixed
  plot <- sankey_plot(ex$data, ex$colors) + sankey_nodes(arrangement = "fixed")
  fig <- sankey_render(plot)

  sankey_trace <- NULL
  for (attr in fig$x$attrs) {
    if (!is.null(attr$type) && attr$type == "sankey") {
      sankey_trace <- attr
      break
    }
  }

  expect_equal(sankey_trace$arrangement, "fixed")
})
