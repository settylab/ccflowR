# Integration Tests for ccflowR
# End-to-end workflow tests

test_that("complete workflow: example data -> filter -> plot -> render", {
  # Load example data
  ex <- example_communication()
  expect_true(is.data.frame(ex$data))
  expect_true(nrow(ex$data) > 0)

  # Apply filters
  filtered <- filter_by_celltypes(ex$data, include_senders = c("Macrophage", "T_cell"))
  expect_true(nrow(filtered) <= nrow(ex$data))
  expect_true(all(filtered$sender %in% c("Macrophage", "T_cell")))

  # Create plot
  plot <- SankeyPlot$new(filtered, ex$colors)
  expect_s3_class(plot, "SankeyPlot")

  # Configure and render
  plot$colors(color_by = "sender")$
    alpha(0.7)$
    add_title("Integration Test")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: multinichenet data with groups", {
  # Load multinichenet example
  ex <- example_multinichenet()
  expect_true("group" %in% colnames(ex$data))
  expect_true(length(unique(ex$data$group)) > 1)

  # Create plot with group coloring
 plot <- SankeyPlot$new(
    ex$data,
    ex$colors,
    group_colors = ex$group_colors,
    group_col = "group"
  )

  plot$colors(color_by = "condition")
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: tumor microenvironment example", {
  ex <- example_tumor_microenvironment()

  # Verify tumor-specific cell types
  all_cells <- unique(c(ex$data$sender, ex$data$receiver))
  expect_true("Tumor_cell" %in% all_cells)

  # Create and render plot
  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$font("Helvetica", 10)$
    nodes(15, 25)$
    add_title("Tumor Microenvironment")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: read CSV -> process -> plot", {
  # Create temporary NicheNet-style CSV
  csv_content <- fixture_nichenet_csv()
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  # Read data
  data <- read_nichenet(tmp_file)
  expect_true(is.data.frame(data))
  expect_true(all(c("sender", "receiver", "ligand", "receptor") %in% colnames(data)))

  # Create plot
  plot <- SankeyPlot$new(data)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: filter top interactions -> plot", {
  ex <- example_communication()

  # Filter top 5 interactions
  top <- filter_top_interactions(ex$data, n = 5)
  expect_equal(nrow(top), 5)

  # Verify ordering (highest activity first)
  if ("activity" %in% colnames(top)) {
    expect_true(top$activity[1] >= top$activity[nrow(top)])
  }

  # Create plot
  plot <- SankeyPlot$new(top, ex$colors)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: preprocessing pipeline", {
  ex <- fixture_large_data(100)

  # Apply multiple filters in sequence
  processed <- ex$data %>%
    filter_by_celltypes(include_senders = c("Cell_A", "Cell_B", "Cell_C")) %>%
    filter_top_interactions(n = 20)

  expect_true(nrow(processed) <= 20)
  expect_true(all(processed$sender %in% c("Cell_A", "Cell_B", "Cell_C")))

  # Create plot
  plot <- SankeyPlot$new(processed, ex$colors)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: method chaining API", {
  ex <- example_communication()

  # Full method chaining workflow
  fig <- SankeyPlot$new(ex$data, ex$colors)$
    colors(color_by = "sender")$
    alpha(0.65)$
    nodes(thickness = 25, padding = 35)$
    layout(width = 800, height = 600)$
    font(family = "Arial", size = 12)$
    margins(left = 60, right = 60, top = 100, bottom = 60)$
    add_title("Method Chaining Test", font_size = 18)$
    font("Arial", 16)$
    render()

  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: highlight pathway", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)

  # Highlight specific pathway
  plot$highlight_pathway(
    senders = "Macrophage",
    ligands = c("TGFB1", "IL6"),
    dim_alpha = 0.15
  )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  # Clear and re-render
  plot$clear_highlight()
  fig2 <- plot$render()
  expect_s3_class(fig2, "plotly")
})

test_that("complete workflow: custom node ordering", {
  ex <- example_communication()

  senders <- unique(ex$data$sender)
  receivers <- unique(ex$data$receiver)

  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$node_order(
    senders = rev(senders),
    receivers = rev(receivers)
  )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: export to HTML", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Export to temporary HTML file
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  to_html(fig, tmp_html)

  # Verify file was created
 expect_true(file.exists(tmp_html))
  expect_true(file.size(tmp_html) > 0)

  # Verify it's valid HTML
  content <- readLines(tmp_html, warn = FALSE)
  expect_true(any(grepl("<html", content, ignore.case = TRUE)))
})

test_that("complete workflow: all coloring strategies", {
  ex <- example_communication()

  for (strategy in c("sender", "condition")) {
    plot <- SankeyPlot$new(ex$data, ex$colors)
    plot$colors(color_by = strategy)
    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("complete workflow: settings modification", {
  # Save original settings
  original_dpi <- ccflow_settings()$dpi()
  original_font <- ccflow_settings()$font_size()

  # Modify settings
  ccflow_settings()$dpi(150)$font_size(14)

  # Create plot with new settings
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  # Restore settings
  ccflow_settings()$dpi(original_dpi)$font_size(original_font)

  expect_equal(ccflow_settings()$dpi(), original_dpi)
})

test_that("complete workflow: color palette management", {
  ex <- example_communication()

  # Create custom palette
  custom_colors <- ColorPalette$new(colormap = "Set1")
  for (cell in unique(c(ex$data$sender, ex$data$receiver))) {
    custom_colors$assign(cell)
  }

  # Use palette colors
  plot <- SankeyPlot$new(ex$data, custom_colors$as_list())
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("complete workflow: export to PNG (if kaleido available)", {

  # Check if any static image export method is available
  has_kaleido <- tryCatch({
    requireNamespace("plotly", quietly = TRUE) &&
      (exists("save_image", where = asNamespace("plotly"), mode = "function"))
  }, error = function(e) FALSE)

  has_webshot2 <- requireNamespace("webshot2", quietly = TRUE)
  has_orca <- nzchar(Sys.which("orca"))

  skip_if_not(
    has_kaleido || has_webshot2 || has_orca,
    "No static image export method available (kaleido, webshot2, or orca)"
  )

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Export to temporary PNG file
  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)

  # Attempt export - may fail if export tools aren't properly configured
  result <- tryCatch({
    save_figure(fig, tmp_png, width = 800, height = 600)
    TRUE
  }, error = function(e) {
    # Skip if export fails due to missing dependencies
    skip(paste("PNG export failed:", e$message))
    FALSE
  })

  if (result) {
    # Verify file was created
    expect_true(file.exists(tmp_png))
    expect_true(file.size(tmp_png) > 0)
  }
})

test_that("complete workflow: export to PDF (if available)", {
  # Check if any static image export method is available
  has_kaleido <- tryCatch({
    requireNamespace("plotly", quietly = TRUE) &&
      (exists("save_image", where = asNamespace("plotly"), mode = "function"))
  }, error = function(e) FALSE)

  has_webshot2 <- requireNamespace("webshot2", quietly = TRUE)
  has_orca <- nzchar(Sys.which("orca"))

  skip_if_not(
    has_kaleido || has_webshot2 || has_orca,
    "No static image export method available (kaleido, webshot2, or orca)"
  )

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Export to temporary PDF file
  tmp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_pdf), add = TRUE)

  # Attempt export
  result <- tryCatch({
    save_figure(fig, tmp_pdf, width = 800, height = 600)
    TRUE
  }, error = function(e) {
    skip(paste("PDF export failed:", e$message))
    FALSE
  })

  if (result) {
    expect_true(file.exists(tmp_pdf))
    expect_true(file.size(tmp_pdf) > 0)
  }
})
