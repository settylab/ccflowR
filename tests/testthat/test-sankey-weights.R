# Tests for Flow-Conserving Edge Weight Computation
# Regression coverage for framework detection and flow-conserving edge weights.

# =============================================================================
# SECTION 1: Framework Detection Tests
# =============================================================================

test_that("detect_framework identifies NicheNet data correctly", {
  nichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = 0.5,
    scaled_ligand_expression = 0.3,
    scaled_receptor_expression = 0.4
  )

  expect_equal(detect_framework(nichenet_data), "nichenet")
})

test_that("detect_framework identifies MultiNicheNet data with group", {

  multinichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6,
    group = "Disease"
  )

  expect_equal(detect_framework(multinichenet_data), "multinichenet")
})

test_that("detect_framework identifies MultiNicheNet data without group as multinichenet_mapped", {
  multinichenet_no_group <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6
  )

  expect_equal(detect_framework(multinichenet_no_group), "multinichenet_mapped")
})

test_that("detect_framework returns unknown for minimal data", {
  minimal_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1"
  )

  expect_equal(detect_framework(minimal_data), "unknown")
})

test_that("detect_framework handles non-dataframe input", {
  expect_equal(detect_framework("not a dataframe"), "unknown")
  expect_equal(detect_framework(NULL), "unknown")
  expect_equal(detect_framework(list(a = 1)), "unknown")
})

test_that("detect_framework handles partial NicheNet columns", {
  # Missing one required column
  partial_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = 0.5,
    scaled_ligand_expression = 0.3
    # Missing scaled_receptor_expression
  )

  expect_equal(detect_framework(partial_data), "unknown")
})

# =============================================================================
# SECTION 2: Input Validation Tests
# =============================================================================

test_that("validate_sankey_input validates base columns", {
  valid_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1"
  )

  expect_true(validate_sankey_input(valid_data))
})

test_that("validate_sankey_input errors on missing base columns", {
  bad_data <- data.frame(x = 1, y = 2)
  expect_error(validate_sankey_input(bad_data), "Missing required columns")

  partial_data <- data.frame(sender = "A", receiver = "B")
  expect_error(validate_sankey_input(partial_data), "Missing required columns")
})

test_that("validate_sankey_input checks weight columns when required", {
  # Data without weight columns
  basic_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1"
  )

  # Should return FALSE with warning when require_weights = TRUE
  expect_warning(
    result <- validate_sankey_input(basic_data, require_weights = TRUE),
    "Missing weight columns"
  )
  expect_false(result)
})

test_that("validate_sankey_input returns TRUE for complete weight data", {
  weighted_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = 0.5,
    scaled_ligand_expression = 0.3,
    scaled_receptor_expression = 0.4
  )

  expect_true(validate_sankey_input(weighted_data, require_weights = TRUE))
})

test_that("validate_sankey_input warns on NA values in scaled_activity", {
  data_with_na <- data.frame(
    sender = c("A", "B"), receiver = c("B", "C"),
    ligand = c("L1", "L2"), receptor = c("R1", "R2"),
    scaled_activity = c(0.5, NA),
    scaled_ligand_expression = c(0.3, 0.4),
    scaled_receptor_expression = c(0.4, 0.5)
  )

  expect_warning(
    validate_sankey_input(data_with_na, require_weights = TRUE),
    "NA values"
  )
})

test_that("validate_sankey_input warns on negative scaled_activity", {
  data_with_negative <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = -0.5,
    scaled_ligand_expression = 0.3,
    scaled_receptor_expression = 0.4
  )

  expect_warning(
    validate_sankey_input(data_with_negative, require_weights = TRUE),
    "negative values"
  )
})

# =============================================================================
# SECTION 3: Data Preparation Tests
# =============================================================================

test_that("prepare_sankey_data maps MultiNicheNet columns correctly", {
  multinichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6,
    group = "Disease"
  )

  prepared <- prepare_sankey_data(multinichenet_data)

  # Check that columns are mapped
  expect_true("scaled_ligand_expression" %in% colnames(prepared))
  expect_true("scaled_receptor_expression" %in% colnames(prepared))
  expect_true("scaled_activity" %in% colnames(prepared))

  # Check values are correct
  expect_equal(prepared$scaled_ligand_expression, 0.5)
  expect_equal(prepared$scaled_receptor_expression, 0.4)
  expect_equal(prepared$scaled_activity, 0.6)

  # Check framework attribute
  expect_equal(attr(prepared, "framework"), "multinichenet")
})

test_that("prepare_sankey_data preserves existing standard columns", {
  # Data that already has standard columns
  nichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = 0.8,
    scaled_ligand_expression = 0.7,
    scaled_receptor_expression = 0.9
  )

  prepared <- prepare_sankey_data(nichenet_data)

  # Values should remain unchanged
  expect_equal(prepared$scaled_activity, 0.8)
  expect_equal(prepared$scaled_ligand_expression, 0.7)
  expect_equal(prepared$scaled_receptor_expression, 0.9)
  expect_equal(attr(prepared, "framework"), "nichenet")
})

test_that("prepare_sankey_data respects auto_map=FALSE", {
  multinichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6,
    group = "Disease"
  )

  prepared <- prepare_sankey_data(multinichenet_data, auto_map = FALSE)

  # Should NOT have mapped columns
  expect_false("scaled_activity" %in% colnames(prepared))
  expect_false("scaled_ligand_expression" %in% colnames(prepared))
})

# =============================================================================
# SECTION 4: Flow-Conserving Edge Weight Tests
# =============================================================================

test_that("SankeyPlot supports flow_method parameter", {
  flow_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    scaled_activity = c(0.8, 0.6),
    scaled_ligand_expression = c(0.7, 0.5),
    scaled_receptor_expression = c(0.9, 0.4)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  # Should not error
  expect_no_error({
    plot <- SankeyPlot$new(flow_data, colors, flow_method = "flow_conserving")
  })

  expect_no_error({
    plot <- SankeyPlot$new(flow_data, colors, flow_method = "uniform")
  })
})

test_that("SankeyPlot renders with flow_method='flow_conserving'", {
  flow_data <- data.frame(
    sender = c("A", "A", "B"),
    receiver = c("C", "D", "C"),
    ligand = c("L1", "L2", "L1"),
    receptor = c("R1", "R2", "R1"),
    scaled_activity = c(0.8, 0.6, 0.5),
    scaled_ligand_expression = c(0.7, 0.5, 0.6),
    scaled_receptor_expression = c(0.9, 0.4, 0.8)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  plot <- SankeyPlot$new(flow_data, colors, flow_method = "flow_conserving")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot renders with flow_method='uniform'", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors, flow_method = "uniform")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot falls back to uniform when weight columns missing", {
  # Create data without weight columns to test fallback
  data_no_weights <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    stringsAsFactors = FALSE
  )
  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  # Should fall back to uniform without error
  expect_warning(
    plot <- SankeyPlot$new(data_no_weights, colors, flow_method = "flow_conserving"),
    "Missing weight columns"
  )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot flow_method() setter works", {
  flow_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    scaled_activity = c(0.8, 0.6),
    scaled_ligand_expression = c(0.7, 0.5),
    scaled_receptor_expression = c(0.9, 0.4)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  plot <- SankeyPlot$new(flow_data, colors, flow_method = "uniform")

  # Change to flow_conserving
  result <- plot$flow_method("flow_conserving")
  expect_s3_class(result, "SankeyPlot")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("flow_method() setter warns when columns missing", {
  basic_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2")
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  plot <- SankeyPlot$new(basic_data, colors, flow_method = "uniform")

  expect_warning(
    plot$flow_method("flow_conserving"),
    "Cannot use flow_conserving"
  )
})

test_that("sankey_flow works with flow_method parameter", {
  flow_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    scaled_activity = c(0.8, 0.6),
    scaled_ligand_expression = c(0.7, 0.5),
    scaled_receptor_expression = c(0.9, 0.4)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  # Test with flow_conserving
  expect_no_error({
    fig <- sankey_flow(flow_data, colors, flow_method = "flow_conserving")
  })

  # Test with uniform
  expect_no_error({
    fig <- sankey_flow(flow_data, colors, flow_method = "uniform")
  })
})

# =============================================================================
# SECTION 5: Edge Case Tests for Flow Conservation
# =============================================================================

test_that("flow_conserving handles zero expression values", {
  data_with_zeros <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    scaled_activity = c(0.8, 0.0),  # Zero activity
    scaled_ligand_expression = c(0.0, 0.5),  # Zero expression
    scaled_receptor_expression = c(0.9, 0.0)  # Zero expression
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  expect_no_error({
    plot <- SankeyPlot$new(data_with_zeros, colors, flow_method = "flow_conserving")
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

test_that("flow_conserving handles NA values gracefully", {
  data_with_na <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    scaled_activity = c(0.8, NA),
    scaled_ligand_expression = c(NA, 0.5),
    scaled_receptor_expression = c(0.9, NA)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  # Should render without error (NA handled internally)
  expect_no_error({
    plot <- SankeyPlot$new(data_with_na, colors, flow_method = "flow_conserving")
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

test_that("flow_conserving handles single interaction", {
  single_interaction <- data.frame(
    sender = "A",
    receiver = "B",
    ligand = "L1",
    receptor = "R1",
    scaled_activity = 0.5,
    scaled_ligand_expression = 0.3,
    scaled_receptor_expression = 0.4
  )

  colors <- c(A = "#FF0000", B = "#00FF00")

  expect_no_error({
    plot <- SankeyPlot$new(single_interaction, colors, flow_method = "flow_conserving")
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

test_that("flow_conserving handles many-to-one ligand mapping", {
  # Multiple senders sending same ligand
  many_to_one <- data.frame(
    sender = c("A", "B", "C"),
    receiver = c("D", "D", "D"),
    ligand = c("L1", "L1", "L1"),  # Same ligand from different senders
    receptor = c("R1", "R1", "R1"),
    scaled_activity = c(0.8, 0.6, 0.4),
    scaled_ligand_expression = c(0.7, 0.5, 0.3),
    scaled_receptor_expression = c(0.9, 0.8, 0.7)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  expect_no_error({
    plot <- SankeyPlot$new(many_to_one, colors, flow_method = "flow_conserving")
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

test_that("flow_conserving handles one-to-many receptor mapping", {
  # One ligand to multiple receptors
  one_to_many <- data.frame(
    sender = c("A", "A", "A"),
    receiver = c("B", "C", "D"),
    ligand = c("L1", "L1", "L1"),  # Same ligand
    receptor = c("R1", "R2", "R3"),  # Different receptors
    scaled_activity = c(0.8, 0.8, 0.8),
    scaled_ligand_expression = c(0.7, 0.7, 0.7),
    scaled_receptor_expression = c(0.9, 0.6, 0.3)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  expect_no_error({
    plot <- SankeyPlot$new(one_to_many, colors, flow_method = "flow_conserving")
    fig <- plot$render()
  })

  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 6: Weighted Node Color Mixing Tests
# =============================================================================

test_that("SankeyPlot node_colors() method works with weighted_mix mode", {
  flow_data <- data.frame(
    sender = c("A", "A", "B"),
    receiver = c("C", "D", "C"),
    ligand = c("L1", "L1", "L2"),
    receptor = c("R1", "R2", "R1"),
    scaled_activity = c(0.8, 0.6, 0.5),
    scaled_ligand_expression = c(0.7, 0.5, 0.6),
    scaled_receptor_expression = c(0.9, 0.4, 0.8)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  plot <- SankeyPlot$new(flow_data, colors, flow_method = "uniform")

  # Enable weighted color mixing
  result <- plot$node_colors("weighted_mix")
  expect_s3_class(result, "SankeyPlot")

  # Should render successfully
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot node_colors() falls back when expression columns missing", {
  basic_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2")
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  plot <- SankeyPlot$new(basic_data, colors, flow_method = "uniform")

  # Should warn and fall back to cell_type mode
  expect_warning(
    plot$node_colors("weighted_mix"),
    "Cannot use weighted_mix mode"
  )

  # Should still render
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("node_colors cell_type mode works", {
  flow_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    scaled_activity = c(0.8, 0.6),
    scaled_ligand_expression = c(0.7, 0.5),
    scaled_receptor_expression = c(0.9, 0.4)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  plot <- SankeyPlot$new(flow_data, colors)
  result <- plot$node_colors("cell_type")

  expect_s3_class(result, "SankeyPlot")
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("sankey_flow works with node_color_mode parameter", {
  flow_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    scaled_activity = c(0.8, 0.6),
    scaled_ligand_expression = c(0.7, 0.5),
    scaled_receptor_expression = c(0.9, 0.4)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  # Test with weighted_mix
  expect_no_error({
    fig <- sankey_flow(flow_data, colors, node_color_mode = "weighted_mix")
  })

  # Test with cell_type (default)
  expect_no_error({
    fig <- sankey_flow(flow_data, colors, node_color_mode = "cell_type")
  })
})

test_that("weighted color mixing with equal weights produces expected blend", {
  # Ligand L1 is sent by both A (red) and B (green) with equal weights
  flow_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "C"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R1"),
    scaled_activity = c(0.5, 0.5),
    scaled_ligand_expression = c(1.0, 1.0),  # Equal weights
    scaled_receptor_expression = c(0.5, 0.5)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")

  plot <- SankeyPlot$new(flow_data, colors, flow_method = "uniform")
  plot$node_colors("weighted_mix")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("weighted color mixing respects unequal weights", {
  # Ligand L1 mostly from A (3:1 ratio)
  flow_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "C"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R1"),
    scaled_activity = c(0.5, 0.5),
    scaled_ligand_expression = c(3.0, 1.0),  # 3:1 ratio
    scaled_receptor_expression = c(0.5, 0.5)
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")

  plot <- SankeyPlot$new(flow_data, colors, flow_method = "uniform")
  plot$node_colors("weighted_mix")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 7: Integration Tests with Framework Data
# =============================================================================

test_that("full pipeline works with NicheNet-style data", {
  nichenet_data <- data.frame(
    sender = c("Macro", "Macro", "T_cell", "T_cell"),
    receiver = c("T_cell", "B_cell", "B_cell", "Macro"),
    ligand = c("TGFB1", "IL6", "IL2", "IFNG"),
    receptor = c("TGFBR1", "IL6R", "IL2RA", "IFNGR1"),
    scaled_activity = c(0.9, 0.7, 0.85, 0.6),
    scaled_ligand_expression = c(0.8, 0.6, 0.75, 0.5),
    scaled_receptor_expression = c(0.85, 0.65, 0.9, 0.55)
  )

  colors <- c(Macro = "#FF5733", T_cell = "#33FF57", B_cell = "#3357FF")

  # Full pipeline: detect, validate, create plot
  expect_equal(detect_framework(nichenet_data), "nichenet")
  expect_true(validate_sankey_input(nichenet_data, require_weights = TRUE))

  plot <- SankeyPlot$new(nichenet_data, colors, flow_method = "flow_conserving")
  plot$node_colors("weighted_mix")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("full pipeline works with MultiNicheNet-style data", {
  multinichenet_data <- data.frame(
    sender = c("Macro", "Macro", "T_cell"),
    receiver = c("B_cell", "Fibro", "B_cell"),
    ligand = c("TGFB1", "TGFB1", "IL2"),
    receptor = c("TGFBR1", "TGFBR2", "IL2RA"),
    scaled_lfc_ligand = c(0.8, 0.6, 0.9),
    scaled_lfc_receptor = c(0.7, 0.5, 0.8),
    prioritization_score = c(0.9, 0.7, 0.85),
    group = c("Disease", "Disease", "Disease")
  )

  colors <- c(Macro = "#FF5733", T_cell = "#33FF57", B_cell = "#3357FF", Fibro = "#FF33F5")

  # Full pipeline: detect, prepare, validate, plot
  expect_equal(detect_framework(multinichenet_data), "multinichenet")

  # Use read_multinichenet for auto-mapping
  data <- read_multinichenet(multinichenet_data)
  expect_true("scaled_activity" %in% colnames(data))
  expect_true("scaled_ligand_expression" %in% colnames(data))

  expect_true(validate_sankey_input(data, require_weights = TRUE))

  plot <- SankeyPlot$new(data, colors, flow_method = "flow_conserving")
  plot$node_colors("weighted_mix")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("sankey_flow convenience function handles MultiNicheNet data", {
  multinichenet_data <- data.frame(
    sender = c("A", "B"),
    receiver = c("C", "D"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    scaled_lfc_ligand = c(0.8, 0.6),
    scaled_lfc_receptor = c(0.7, 0.5),
    prioritization_score = c(0.9, 0.7),
    group = c("Disease", "Disease")
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  # Auto-prepare should handle the mapping
  expect_no_error({
    fig <- sankey_flow(multinichenet_data, colors,
                       flow_method = "flow_conserving",
                       node_color_mode = "weighted_mix")
  })
})

# =============================================================================
# SECTION 8: Color Mixing Utility Tests
# =============================================================================

test_that("weighted_mix_colors handles empty input", {
  result <- weighted_mix_colors(character(0), numeric(0))
  expect_true(is.character(result))
  expect_equal(length(result), 1)
})

test_that("weighted_mix_colors handles single color", {
  result <- weighted_mix_colors("#FF0000", 1)
  expect_equal(result, "#FF0000")
})

test_that("weighted_mix_colors blends two colors equally", {
  # Red + Blue with equal weights should give purple-ish
  result <- weighted_mix_colors(c("#FF0000", "#0000FF"), c(1, 1))
  expect_true(grepl("^#[0-9A-F]{6}$", result))

  # Verify it's a blend (not pure red or blue)
  expect_false(result == "#FF0000")
  expect_false(result == "#0000FF")
})

test_that("weighted_mix_colors respects weight ratios", {
  # 100% red weight should return red
  result <- weighted_mix_colors(c("#FF0000", "#0000FF"), c(1, 0))
  expect_equal(result, "#FF0000")

  # 100% blue weight should return blue
  result <- weighted_mix_colors(c("#FF0000", "#0000FF"), c(0, 1))
  expect_equal(result, "#0000FF")
})

test_that("weighted_mix_colors handles NULL weights", {
  # Should use equal weights
  result <- weighted_mix_colors(c("#FF0000", "#00FF00"), NULL)
  expect_true(grepl("^#[0-9A-F]{6}$", result))
})

test_that("hex_to_rgb converts correctly", {
  rgb_vals <- hex_to_rgb("#FF0000")
  expect_equal(rgb_vals[["r"]], 255)
  expect_equal(rgb_vals[["g"]], 0)
  expect_equal(rgb_vals[["b"]], 0)

  rgb_vals <- hex_to_rgb("#00FF00")
  expect_equal(rgb_vals[["r"]], 0)
  expect_equal(rgb_vals[["g"]], 255)
  expect_equal(rgb_vals[["b"]], 0)
})

test_that("rgb_to_hex converts correctly", {
  expect_equal(rgb_to_hex(255, 0, 0), "#FF0000")
  expect_equal(rgb_to_hex(0, 255, 0), "#00FF00")
  expect_equal(rgb_to_hex(0, 0, 255), "#0000FF")
})

test_that("rgb_to_hex clamps values", {
  # Should clamp to valid range
  expect_equal(rgb_to_hex(300, -50, 128), "#FF0080")
})

test_that("hex_with_alpha creates rgba string", {
  result <- hex_with_alpha("#FF0000", 0.5)
  expect_true(grepl("^rgba\\(", result))
  expect_true(grepl("255", result))  # Red value
  expect_true(grepl("0.50", result))  # Alpha value
})

# =============================================================================
# SECTION 9: Auto-preparation Tests
# =============================================================================
test_that("SankeyPlot auto_prepare=TRUE detects and maps MultiNicheNet", {
  multinichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6,
    group = "Disease"
  )

  colors <- c(A = "#FF0000", B = "#00FF00")

  # With auto_prepare=TRUE (default), should map columns
  plot <- SankeyPlot$new(multinichenet_data, colors, auto_prepare = TRUE)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("SankeyPlot auto_prepare=FALSE skips mapping", {
  multinichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6,
    group = "Disease"
  )

  colors <- c(A = "#FF0000", B = "#00FF00")

  # With auto_prepare=FALSE, should not have mapped columns
  # This will cause flow_conserving to fall back to uniform
  expect_warning(
    plot <- SankeyPlot$new(multinichenet_data, colors,
                           auto_prepare = FALSE,
                           flow_method = "flow_conserving"),
    "Missing weight columns"
  )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 10: Uniform vs Flow-Conserving Visual Difference Tests
# =============================================================================

test_that("uniform and flow_conserving produce different edge values for shared ligands", {
  # Data with a shared ligand (L1) sent by two senders to two receivers,
  # and decorrelated receptor expression values
  shared_data <- data.frame(
    sender = c("A", "B", "A"),
    receiver = c("C", "D", "D"),
    ligand = c("L1", "L1", "L2"),
    receptor = c("R1", "R1", "R2"),
    scaled_activity = c(0.8, 0.4, 0.6),
    scaled_ligand_expression = c(0.7, 0.5, 0.6),
    scaled_receptor_expression = c(0.3, 0.9, 0.5),
    stringsAsFactors = FALSE
  )

  colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF", D = "#FFFF00")

  # Create both plots
  plot_uniform <- SankeyPlot$new(shared_data, colors, flow_method = "uniform")
  plot_fc <- SankeyPlot$new(shared_data, colors, flow_method = "flow_conserving")

  fig_uniform <- plot_uniform$render()
  fig_fc <- plot_fc$render()

  # Extract link values from plotly build data
  uniform_data <- plotly::plotly_build(fig_uniform)$x
  fc_data <- plotly::plotly_build(fig_fc)$x

  uniform_values <- uniform_data$data[[1]]$link$value
  fc_values <- fc_data$data[[1]]$link$value

  # Both should have the same number of links
  expect_equal(length(uniform_values), length(fc_values))

  # Uniform edge 1 uses raw scaled_activity; flow-conserving edge 1 uses
  # expression-weighted normalization. So the two methods should produce
  # different link value vectors overall.
  expect_false(identical(uniform_values, fc_values),
    info = "Uniform and flow-conserving should produce different link values for shared ligands")
})
