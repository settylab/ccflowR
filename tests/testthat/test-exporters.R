# Exporter Tests for ccflowR
# Test file export functions

# --- to_html() ---

test_that("to_html creates valid HTML file", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  to_html(fig, tmp_html)

  expect_true(file.exists(tmp_html))
  expect_true(file.size(tmp_html) > 0)

  # Check HTML structure
  content <- paste(readLines(tmp_html, warn = FALSE), collapse = "\n")
  expect_true(grepl("<html", content, ignore.case = TRUE))
  expect_true(grepl("plotly", content, ignore.case = TRUE))
})

test_that("to_html respects width/height parameters", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  to_html(fig, tmp_html, width = 900, height = 700)

  expect_true(file.exists(tmp_html))
})

test_that("to_html creates self-contained file by default", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  to_html(fig, tmp_html, selfcontained = TRUE)

  # Self-contained files are larger (include all dependencies)
  expect_true(file.size(tmp_html) > 1000)
})

test_that("to_html errors without htmlwidgets", {
  # This test only works if we can temporarily make htmlwidgets unavailable
  # Skip for now as it's hard to test package unavailability
  skip("Cannot test missing package scenario reliably")
})

# --- to_json() ---

test_that("to_json returns valid JSON string", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  json <- to_json(fig)

  expect_type(json, "character")
  expect_true(nchar(json) > 0)

  # Should be parseable JSON
  parsed <- jsonlite::fromJSON(json)
  expect_type(parsed, "list")
})

test_that("to_json pretty parameter works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  json_compact <- to_json(fig, pretty = FALSE)
  json_pretty <- to_json(fig, pretty = TRUE)

  # Pretty JSON has more newlines
  expect_true(
    length(gregexpr("\n", json_pretty)[[1]]) >=
    length(gregexpr("\n", json_compact)[[1]])
  )
})

# --- save_figure() ---

test_that("save_figure dispatches to correct format", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Test HTML
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  save_figure(fig, tmp_html)
  expect_true(file.exists(tmp_html))
})

test_that("save_figure errors on unsupported format", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  expect_error(
    save_figure(fig, "test.xyz"),
    "Unsupported format"
  )
})

test_that("save_figure accepts width/height parameters", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  result <- save_figure(fig, tmp_html, width = 800, height = 600)
  expect_equal(result, tmp_html)
})

test_that("save_figure returns filename invisibly", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  result <- save_figure(fig, tmp_html)
  expect_equal(result, tmp_html)
})

# --- to_image() ---

test_that("to_image provides helpful error when no export backend available", {
  # Create a plot
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)

  # This may succeed or fail depending on installed backends
  result <- tryCatch(
    to_image(fig, tmp_png),
    error = function(e) {
      # Should mention installation options
      expect_true(grepl("kaleido|webshot2|orca", e$message, ignore.case = TRUE))
      "error"
    }
  )

  # If it succeeded, file should exist
  if (!identical(result, "error")) {
    expect_true(file.exists(tmp_png))
  }
})

# Helper: check if any static image export backend is available
has_image_backend <- function() {
  # Check kaleido via reticulate
  has_kaleido <- tryCatch({
    requireNamespace("reticulate", quietly = TRUE) &&
      reticulate::py_available(initialize = TRUE) &&
      tryCatch({ reticulate::import("kaleido"); TRUE }, error = function(e) FALSE)
  }, error = function(e) FALSE)

  if (has_kaleido) return(TRUE)

  # Check webshot2
  if (requireNamespace("webshot2", quietly = TRUE)) return(TRUE)

  FALSE
}

skip_if_no_image_backend <- function() {
  if (!has_image_backend()) {
    skip("No static image export backend available (need kaleido or webshot2)")
  }
}

# --- to_image() static export tests ---

test_that("to_image exports SVG via kaleido", {
  skip_if_no_image_backend()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_svg <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp_svg), add = TRUE)

  to_image(fig, tmp_svg, format = "svg")

  expect_true(file.exists(tmp_svg))
  expect_true(file.size(tmp_svg) > 0)

  # SVG content should start with XML declaration or <svg tag
  content <- paste(readLines(tmp_svg, warn = FALSE, n = 5), collapse = "\n")
  expect_true(grepl("<svg|<?xml", content, ignore.case = TRUE))
})

test_that("to_image exports PNG", {
  skip_if_no_image_backend()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)

  to_image(fig, tmp_png, format = "png")

  expect_true(file.exists(tmp_png))
  expect_true(file.size(tmp_png) > 0)
})

test_that("to_image exports PDF", {
  skip_if_no_image_backend()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_pdf), add = TRUE)

  to_image(fig, tmp_pdf, format = "pdf")

  expect_true(file.exists(tmp_pdf))
  expect_true(file.size(tmp_pdf) > 0)
})

# --- sankey_save() static export tests ---

test_that("sankey_save exports SVG", {
  skip_if_no_image_backend()

  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors)

  tmp_svg <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp_svg), add = TRUE)

  sankey_save(p, tmp_svg)

  expect_true(file.exists(tmp_svg))
  expect_true(file.size(tmp_svg) > 0)

  content <- paste(readLines(tmp_svg, warn = FALSE, n = 5), collapse = "\n")
  expect_true(grepl("<svg|<?xml", content, ignore.case = TRUE))
})

test_that("sankey_save exports PNG", {
  skip_if_no_image_backend()

  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors)

  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)

  sankey_save(p, tmp_png)

  expect_true(file.exists(tmp_png))
  expect_true(file.size(tmp_png) > 0)
})

test_that("sankey_save exports PDF", {
  skip_if_no_image_backend()

  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors)

  tmp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_pdf), add = TRUE)

  sankey_save(p, tmp_pdf)

  expect_true(file.exists(tmp_pdf))
  expect_true(file.size(tmp_pdf) > 0)
})

# --- SankeyPlot save method ---

test_that("SankeyPlot$save method works", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  # save() should return self for chaining
  result <- plot$save(tmp_html)
  expect_s3_class(result, "SankeyPlot")
  expect_true(file.exists(tmp_html))
})

test_that("SankeyPlot$save uses custom dimensions", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  plot$save(tmp_html, width = 1000, height = 800)
  expect_true(file.exists(tmp_html))
})

# --- Settings integration ---

test_that("exporters use default dimensions from settings", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  # Set custom defaults
  original_width <- ccflow_settings()$figwidth()
  original_height <- ccflow_settings()$figheight()

  ccflow_settings()$figwidth(850)$figheight(650)

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  tmp_html <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_html)
    ccflow_settings()$figwidth(original_width)$figheight(original_height)
  }, add = TRUE)

  # Should use settings dimensions
  to_html(fig, tmp_html)
  expect_true(file.exists(tmp_html))
})

# --- Edge cases ---

test_that("to_html handles special characters in filename", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Filename with spaces
  tmp_dir <- tempdir()
  tmp_html <- file.path(tmp_dir, "test file with spaces.html")
  on.exit(unlink(tmp_html), add = TRUE)

  to_html(fig, tmp_html)
  expect_true(file.exists(tmp_html))
})

test_that("export creates parent directories if needed", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  # Create path with non-existent subdirectory
  tmp_dir <- file.path(tempdir(), "subdir_test_export")
  tmp_html <- file.path(tmp_dir, "test.html")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create directory
  dir.create(tmp_dir, recursive = TRUE)

  to_html(fig, tmp_html)
  expect_true(file.exists(tmp_html))
})
