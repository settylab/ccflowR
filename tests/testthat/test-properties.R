# Property-Based Tests for ccflowR
# Test invariants that should always hold

# --- Color Conversion Roundtrip ---

test_that("hex_to_rgb and rgb_to_hex are inverses", {
  # Test multiple colors
  test_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFFFF", "#000000",
                   "#123456", "#ABCDEF", "#7F7F7F")

  for (hex in test_colors) {
    rgb <- hex_to_rgb(hex)
    result <- rgb_to_hex(rgb["r"], rgb["g"], rgb["b"])
    expect_equal(toupper(result), toupper(hex),
                 info = paste("Color:", hex))
  }
})

test_that("rgb_to_hex and hex_to_rgb roundtrip for random values", {
  set.seed(42)

  for (i in 1:20) {
    r <- sample(0:255, 1)
    g <- sample(0:255, 1)
    b <- sample(0:255, 1)

    hex <- rgb_to_hex(r, g, b)
    rgb <- hex_to_rgb(hex)

    expect_equal(unname(rgb["r"]), r, info = paste("R:", r))
    expect_equal(unname(rgb["g"]), g, info = paste("G:", g))
    expect_equal(unname(rgb["b"]), b, info = paste("B:", b))
  }
})

# --- ColorPalette Properties ---

test_that("ColorPalette always returns valid hex colors", {
  palette <- ColorPalette$new()

  # Get colors for random names
  test_names <- c("TypeA", "TypeB", "TestCell", "RandomName", "123", "abc_def")

  for (name in test_names) {
    color <- palette$get(name)
    expect_match(color, "^#[0-9A-Fa-f]{6}$",
                 info = paste("Name:", name))
  }
})

test_that("ColorPalette$get is idempotent", {
  palette <- ColorPalette$new()

  # Same name should always return same color
  names <- c("Cell1", "Cell2", "Cell3")

  for (name in names) {
    color1 <- palette$get(name)
    color2 <- palette$get(name)
    color3 <- palette$get(name)

    expect_equal(color1, color2, info = paste("Name:", name))
    expect_equal(color2, color3, info = paste("Name:", name))
  }
})

test_that("ColorPalette$set overrides previous values", {
  palette <- ColorPalette$new()

  palette$set("Test", "#FF0000")
  expect_equal(palette$get("Test"), "#FF0000")

  palette$set("Test", "#00FF00")
  expect_equal(palette$get("Test"), "#00FF00")
})

# --- weighted_mix_colors Properties ---

test_that("weighted_mix_colors with single color returns that color", {
  colors <- c("#FF0000", "#00FF00", "#0000FF", "#ABCDEF")

  for (color in colors) {
    result <- weighted_mix_colors(color)
    expect_equal(toupper(result), toupper(color))
  }
})

test_that("weighted_mix_colors with equal weights is symmetric", {
  # Mix of A and B should equal mix of B and A
  result1 <- weighted_mix_colors(c("#FF0000", "#0000FF"), c(1, 1))
  result2 <- weighted_mix_colors(c("#0000FF", "#FF0000"), c(1, 1))

  expect_equal(result1, result2)
})

test_that("weighted_mix_colors output is always valid hex", {
  set.seed(42)

  for (i in 1:10) {
    n_colors <- sample(2:5, 1)
    colors <- sapply(1:n_colors, function(x) {
      rgb_to_hex(sample(0:255, 1), sample(0:255, 1), sample(0:255, 1))
    })
    weights <- runif(n_colors)

    result <- weighted_mix_colors(colors, weights)
    expect_match(result, "^#[0-9A-Fa-f]{6}$")
  }
})

# --- SankeyPlot Sankey Node Count ---

test_that("Sankey node count equals unique values across all layers", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  summary <- plot$summary()

  # Each unique value appears once per layer it's in
  n_senders <- length(unique(ex$data$sender))
  n_ligands <- length(unique(ex$data$ligand))
  n_receptors <- length(unique(ex$data$receptor))
  n_receivers <- length(unique(ex$data$receiver))

  expect_equal(summary$n_senders, n_senders)
  expect_equal(summary$n_ligands, n_ligands)
  expect_equal(summary$n_receptors, n_receptors)
  expect_equal(summary$n_receivers, n_receivers)
})

# --- Filter Properties ---

test_that("filter_by_celltypes reduces or maintains row count", {
  ex <- example_communication()
  original_rows <- nrow(ex$data)

  senders <- unique(ex$data$sender)

  for (sender in senders) {
    filtered <- filter_by_celltypes(ex$data, include_senders = sender)
    expect_true(nrow(filtered) <= original_rows,
                info = paste("Sender:", sender))
  }
})

test_that("filter_top_interactions returns at most n rows", {
  ex <- example_communication()

  for (n in c(1, 3, 5, 10, 100)) {
    filtered <- filter_top_interactions(ex$data, n = n)
    expect_true(nrow(filtered) <= n,
                info = paste("n:", n))
  }
})

test_that("remove_duplicates reduces or maintains row count", {
  ex <- example_communication()
  original_rows <- nrow(ex$data)

  for (keep in c("first", "last", "max_activity")) {
    dedup <- remove_duplicates(ex$data, keep = keep)
    expect_true(nrow(dedup) <= original_rows,
                info = paste("Keep:", keep))
  }
})

# --- Settings Properties ---

test_that("settings getters return set values", {
  original <- ccflow_settings()$dpi()

  test_values <- c(72, 100, 150, 300, 600)

  for (val in test_values) {
    ccflow_settings()$dpi(val)
    expect_equal(ccflow_settings()$dpi(), val,
                 info = paste("DPI:", val))
  }

  ccflow_settings()$dpi(original)
})

test_that("settings chaining returns self", {
  result <- ccflow_settings()$
    dpi(100)$
    font_size(12)$
    verbose(TRUE)

  expect_s3_class(result, "CCFlowSettings")
})

# --- Data Extraction Properties ---

test_that("extract_celltypes returns all unique cell types", {
  ex <- example_communication()
  result <- extract_celltypes(ex$data)

  # All senders should be in senders list
  for (sender in unique(ex$data$sender)) {
    expect_true(sender %in% result$senders,
                info = paste("Sender:", sender))
  }

  # All receivers should be in receivers list
  for (receiver in unique(ex$data$receiver)) {
    expect_true(receiver %in% result$receivers,
                info = paste("Receiver:", receiver))
  }

  # All should be in 'all' list
  all_cells <- unique(c(ex$data$sender, ex$data$receiver))
  expect_setequal(result$all, all_cells)
})

test_that("extract_ligands returns all unique ligands", {
  ex <- example_communication()
  result <- extract_ligands(ex$data)

  expect_setequal(result, unique(ex$data$ligand))
})

test_that("extract_receptors returns all unique receptors", {
  ex <- example_communication()
  result <- extract_receptors(ex$data)

  expect_setequal(result, unique(ex$data$receptor))
})

# --- Summarize Properties ---

test_that("summarize_data counts match manual counts", {
  ex <- example_communication()
  summary <- summarize_data(ex$data)

  expect_equal(summary$n_interactions, nrow(ex$data))
  expect_equal(summary$n_senders, length(unique(ex$data$sender)))
  expect_equal(summary$n_receivers, length(unique(ex$data$receiver)))
  expect_equal(summary$n_ligands, length(unique(ex$data$ligand)))
  expect_equal(summary$n_receptors, length(unique(ex$data$receptor)))
})

# --- Validate Properties ---

test_that("validate_required_columns passes for all present columns", {
  df <- data.frame(a = 1, b = 2, c = 3, d = 4)

  # Any subset of columns should pass
  expect_true(validate_required_columns(df, c("a")))
  expect_true(validate_required_columns(df, c("a", "b")))
  expect_true(validate_required_columns(df, c("a", "b", "c")))
  expect_true(validate_required_columns(df, c("a", "b", "c", "d")))
})
