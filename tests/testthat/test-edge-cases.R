# Edge Case Tests for ccflowR
# Test boundary conditions and error handling

# --- Empty Data ---

test_that("SankeyPlot handles empty data frame gracefully", {
  empty_data <- data.frame(
    sender = character(0),
    receiver = character(0),
    ligand = character(0),
    receptor = character(0),
    stringsAsFactors = FALSE
  )

  # Should create plot object but may fail on render
  plot <- SankeyPlot$new(empty_data)
  expect_s3_class(plot, "SankeyPlot")
})

test_that("filter functions handle empty results", {
  ex <- example_communication()

  # Filter that results in no matches
  filtered <- filter_by_celltypes(ex$data, include_senders = "NonExistentCell")
  expect_equal(nrow(filtered), 0)
})

# --- Single Row Data ---

test_that("SankeyPlot renders with single row data", {
  single <- fixture_single_row()

  plot <- SankeyPlot$new(single$data, single$colors)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("filter_top_interactions with n > nrow returns all rows", {
  ex <- fixture_small_data()
  n_rows <- nrow(ex$data)

  filtered <- filter_top_interactions(ex$data, n = 100)
  expect_equal(nrow(filtered), n_rows)
})

# --- Missing Optional Columns ---

test_that("SankeyPlot works without activity column", {
  data <- data.frame(
    sender = c("A", "B"),
    receiver = c("B", "C"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    stringsAsFactors = FALSE
  )

  plot <- SankeyPlot$new(data)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot works without explicit colors", {
  ex <- example_communication()

  # Create without providing colors - should auto-assign
  plot <- SankeyPlot$new(ex$data)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("condition coloring works without group column (falls back to sender)", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)
  # Try to use condition coloring without group column - should fall back to sender
  plot$colors(color_by = "condition")
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# --- Large Datasets ---

test_that("SankeyPlot handles large datasets (100+ interactions)", {
  large <- fixture_large_data(150)

  plot <- SankeyPlot$new(large$data, large$colors)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  # Verify summary
  summary <- plot$summary()
  expect_equal(summary$n_interactions, 150)
})

test_that("filtering works efficiently on large datasets", {
  large <- fixture_large_data(500)

  # Multiple filter operations
  start_time <- Sys.time()
  filtered <- large$data %>%
    filter_by_celltypes(include_senders = c("Cell_A", "Cell_B")) %>%
    filter_top_interactions(n = 50)
  elapsed <- as.numeric(Sys.time() - start_time)

  expect_true(nrow(filtered) <= 50)
  expect_true(elapsed < 5)  # Should complete in < 5 seconds
})

# --- Unicode Characters ---

test_that("SankeyPlot handles unicode cell type names", {
  unicode_data <- fixture_unicode_data()

  plot <- SankeyPlot$new(unicode_data$data, unicode_data$colors)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("ColorPalette handles unicode names", {
  palette <- ColorPalette$new()
  palette$set("日本語", "#FF0000")
  palette$set("中文", "#00FF00")

  expect_equal(palette$get("日本語"), "#FF0000")
  expect_equal(palette$get("中文"), "#00FF00")
})

# --- Special Characters ---

test_that("SankeyPlot handles special characters in names", {
  special <- fixture_special_chars_data()

  plot <- SankeyPlot$new(special$data, special$colors)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("filters handle special characters", {
  special <- fixture_special_chars_data()

  filtered <- filter_by_celltypes(
    special$data,
    include_senders = "CD4+ T-cell"
  )
  expect_true(nrow(filtered) > 0)
})

# --- Invalid Input Handling ---

test_that("SankeyPlot errors on missing required columns", {
  bad_data <- data.frame(x = 1, y = 2)

  expect_error(
    SankeyPlot$new(bad_data),
    "Missing required columns"
  )
})

test_that("validate_required_columns provides helpful error", {
  data <- data.frame(a = 1, b = 2)

  expect_error(
    validate_required_columns(data, c("a", "c", "d")),
    "Missing required columns.*c.*d"
  )
})

test_that("hex_to_rgb handles invalid hex gracefully", {
  # Short hex
  result <- hex_to_rgb("FFF")
  expect_true(all(is.na(result) | is.numeric(result)))

  # Empty string
  result2 <- hex_to_rgb("")
  expect_true(all(is.na(result2) | is.numeric(result2)))
})

test_that("rgb_to_hex clamps values to valid range", {
  # Values outside 0-255 should be clamped
  result <- rgb_to_hex(300, -50, 128)
  expect_match(result, "^#[0-9A-F]{6}$")
})

# --- Boundary Values ---

test_that("alpha handles boundary values", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  # Alpha = 0 (fully transparent)
  plot$alpha(0)
  fig1 <- plot$render()
  expect_s3_class(fig1, "plotly")

  # Alpha = 1 (fully opaque)
  plot$alpha(1)
  fig2 <- plot$render()
  expect_s3_class(fig2, "plotly")
})

test_that("node dimensions handle extreme values", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  # Very small nodes
  plot$nodes(thickness = 1, padding = 1)
  fig1 <- plot$render()
  expect_s3_class(fig1, "plotly")

  # Very large nodes
  plot$nodes(thickness = 100, padding = 100)
  fig2 <- plot$render()
  expect_s3_class(fig2, "plotly")
})

test_that("layout handles extreme dimensions", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  # Very small
  plot$layout(width = 100, height = 100)
  fig1 <- plot$render()
  expect_s3_class(fig1, "plotly")

  # Very large
  plot$layout(width = 2000, height = 2000)
  fig2 <- plot$render()
  expect_s3_class(fig2, "plotly")
})

# --- Duplicate Data ---

test_that("SankeyPlot handles duplicate interactions", {
  data <- data.frame(
    sender = c("A", "A", "A"),
    receiver = c("B", "B", "B"),
    ligand = c("L1", "L1", "L1"),
    receptor = c("R1", "R1", "R1"),
    activity = c(0.5, 0.6, 0.7),
    stringsAsFactors = FALSE
  )

  plot <- SankeyPlot$new(data)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("remove_duplicates handles all-duplicate data", {
  data <- data.frame(
    sender = rep("A", 5),
    receiver = rep("B", 5),
    ligand = rep("L1", 5),
    receptor = rep("R1", 5),
    activity = 1:5,
    stringsAsFactors = FALSE
  )

  dedup <- remove_duplicates(data, keep = "max_activity")
  expect_equal(nrow(dedup), 1)
  expect_equal(dedup$activity, 5)
})

# --- Self-loops ---

test_that("SankeyPlot handles self-loops (sender == receiver)", {
  data <- data.frame(
    sender = c("A", "A", "B"),
    receiver = c("A", "B", "B"),  # A->A is self-loop
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3"),
    stringsAsFactors = FALSE
  )

  plot <- SankeyPlot$new(data)
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# --- NA Values ---

test_that("SankeyPlot handles NA in activity column", {
  data <- data.frame(
    sender = c("A", "B", "C"),
    receiver = c("B", "C", "A"),
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3"),
    activity = c(0.5, NA, 0.7),
    stringsAsFactors = FALSE
  )

  plot <- SankeyPlot$new(data, activity_col = "activity")
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("filter functions handle NA values", {
  data <- data.frame(
    sender = c("A", "B", NA),
    receiver = c("B", "C", "A"),
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3"),
    stringsAsFactors = FALSE
  )

  # Should not error
  filtered <- filter_by_celltypes(data, include_senders = "A")
  expect_true(is.data.frame(filtered))
})

# --- Settings Edge Cases ---

test_that("settings handle repeated modifications", {
  original <- ccflow_settings()$dpi()

  # Rapid setting changes
  for (i in 1:10) {
    ccflow_settings()$dpi(100 + i)
  }

  expect_equal(ccflow_settings()$dpi(), 110)

  # Reset
  ccflow_settings()$dpi(original)
})

test_that("settings reset restores all defaults", {
  # Modify several settings
  ccflow_settings()$
    dpi(999)$
    font_size(99)$
    verbose(FALSE)

  # Reset
  ccflow_settings()$reset()

  # Check defaults restored
  expect_equal(ccflow_settings()$dpi(), 100)
  expect_equal(ccflow_settings()$font_size(), 12)
  expect_false(ccflow_settings()$verbose())
})
