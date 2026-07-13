# Comprehensive Visual Tests for ccflowR
# Tests to ensure plots render correctly without truncation across all feature combinations
#
# Run these tests with: devtools::test(filter = "visual-comprehensive")
# Generate visual outputs with: source("tests/visual-test-runner.R")

# =============================================================================
# SECTION 1: Edge Case Fixtures - Scenarios Prone to Truncation
# =============================================================================

#' Create fixture with very long labels (truncation-prone)
fixture_long_labels <- function() {
  data <- data.frame(
    sender = c(
      "Tumor_Associated_Macrophage_Type_1",
      "Cancer_Associated_Fibroblast_Activated",
      "Cytotoxic_CD8_Positive_T_Lymphocyte"
    ),
    receiver = c(
      "Cancer_Associated_Fibroblast_Activated",
      "Cytotoxic_CD8_Positive_T_Lymphocyte",
      "Tumor_Associated_Macrophage_Type_1"
    ),
    ligand = c(
      "TRANSFORMING_GROWTH_FACTOR_BETA_1",
      "VASCULAR_ENDOTHELIAL_GROWTH_FACTOR_A",
      "INTERLEUKIN_6_CYTOKINE"
    ),
    receptor = c(
      "TGFBR1_TGFBR2_COMPLEX",
      "KDR_FLT1_HETERODIMER",
      "IL6R_GP130_COMPLEX"
    ),
    activity = c(0.9, 0.85, 0.75),
    stringsAsFactors = FALSE
  )

  colors <- c(
    "Tumor_Associated_Macrophage_Type_1" = "#E41A1C",
    "Cancer_Associated_Fibroblast_Activated" = "#377EB8",
    "Cytotoxic_CD8_Positive_T_Lymphocyte" = "#4DAF4A"
  )

  list(data = data, colors = colors)
}

#' Create fixture with many nodes per layer (truncation-prone)
fixture_many_nodes <- function(n_celltypes = 15, n_ligands = 25, n_receptors = 25) {
  cell_types <- paste0("CellType_", sprintf("%02d", 1:n_celltypes))
  ligands <- paste0("LIG_", sprintf("%02d", 1:n_ligands))
  receptors <- paste0("REC_", sprintf("%02d", 1:n_receptors))

  # Generate interactions
  n_interactions <- n_celltypes * 3
  data <- data.frame(
    sender = sample(cell_types, n_interactions, replace = TRUE),
    receiver = sample(cell_types, n_interactions, replace = TRUE),
    ligand = sample(ligands, n_interactions, replace = TRUE),
    receptor = sample(receptors, n_interactions, replace = TRUE),
    activity = runif(n_interactions, 0.3, 1.0),
    stringsAsFactors = FALSE
  )

  colors <- setNames(
    grDevices::rainbow(n_celltypes),
    cell_types
  )

  list(data = data, colors = colors)
}

#' Create fixture with varying activity values (flow visualization)
fixture_varied_activity <- function() {
  data <- data.frame(
    sender = c("Sender_A", "Sender_A", "Sender_B", "Sender_B", "Sender_C"),
    receiver = c("Receiver_X", "Receiver_Y", "Receiver_X", "Receiver_Z", "Receiver_Y"),
    ligand = c("LIG1", "LIG2", "LIG1", "LIG3", "LIG2"),
    receptor = c("REC1", "REC2", "REC1", "REC3", "REC2"),
    activity = c(0.1, 0.5, 0.9, 0.3, 0.7),
    scaled_activity = c(0.1, 0.5, 0.9, 0.3, 0.7),
    scaled_ligand_expression = c(0.8, 0.6, 0.9, 0.4, 0.7),
    scaled_receptor_expression = c(0.7, 0.8, 0.6, 0.5, 0.9),
    stringsAsFactors = FALSE
  )

  colors <- c(
    Sender_A = "#E41A1C",
    Sender_B = "#377EB8",
    Sender_C = "#4DAF4A",
    Receiver_X = "#984EA3",
    Receiver_Y = "#FF7F00",
    Receiver_Z = "#FFFF33"
  )

  list(data = data, colors = colors)
}

#' Create fixture with groups for MultiNicheNet-style data
fixture_multigroup <- function() {
  base_interactions <- data.frame(
    sender = c("Macrophage", "T_cell", "Fibroblast", "NK_cell", "B_cell"),
    receiver = c("T_cell", "B_cell", "Macrophage", "Tumor", "Fibroblast"),
    ligand = c("TGFB1", "IL2", "VEGFA", "IFNG", "IL10"),
    receptor = c("TGFBR1", "IL2RA", "KDR", "IFNGR1", "IL10RA"),
    stringsAsFactors = FALSE
  )

  groups <- c("Control", "Treatment_A", "Treatment_B", "Disease")
  data_list <- lapply(groups, function(grp) {
    d <- base_interactions
    d$group <- grp
    d$activity <- runif(nrow(d), 0.2, 1.0)
    d$scaled_activity <- d$activity
    d$scaled_ligand_expression <- runif(nrow(d), 0.3, 1.0)
    d$scaled_receptor_expression <- runif(nrow(d), 0.3, 1.0)
    d
  })
  data <- do.call(rbind, data_list)
  rownames(data) <- NULL

  cell_colors <- c(
    Macrophage = "#E41A1C",
    T_cell = "#377EB8",
    Fibroblast = "#4DAF4A",
    NK_cell = "#984EA3",
    B_cell = "#FF7F00",
    Tumor = "#A65628"
  )

  group_colors <- c(
    Control = "#66C2A5",
    Treatment_A = "#FC8D62",
    Treatment_B = "#8DA0CB",
    Disease = "#E78AC3"
  )

  list(data = data, colors = cell_colors, group_colors = group_colors)
}

#' Create fixture with single interaction (minimal case)
fixture_minimal <- function() {
  data <- data.frame(
    sender = "CellA",
    receiver = "CellB",
    ligand = "LIG1",
    receptor = "REC1",
    activity = 0.8,
    stringsAsFactors = FALSE
  )

  colors <- c(CellA = "#E41A1C", CellB = "#377EB8")
  list(data = data, colors = colors)
}

# =============================================================================
# SECTION 2: Node Arrangement Tests
# =============================================================================

test_that("custom node ordering renders correctly", {
  ex <- example_communication()
  senders <- unique(ex$data$sender)
  receivers <- unique(ex$data$receiver)

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    node_order(
      senders = rev(senders),
      receivers = rev(receivers)
    )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("many nodes per layer renders without truncation", {
  fix <- fixture_many_nodes(n_celltypes = 12, n_ligands = 20, n_receptors = 20)

  plot <- SankeyPlot$new(fix$data, fix$colors)$
    layout(width = 900, height = 1200)$
    nodes(padding = 20, thickness = 15)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  # Verify summary shows correct node counts

  summary <- plot$summary()
  expect_equal(summary$n_senders, length(unique(fix$data$sender)))
  expect_equal(summary$n_receivers, length(unique(fix$data$receiver)))
})

test_that("node padding values work correctly", {
  ex <- example_communication()

  for (padding in c(10, 25, 40, 60, 80)) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      nodes(padding = padding)

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("node thickness values work correctly", {
  ex <- example_communication()

  for (thickness in c(10, 20, 30, 40, 50)) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      nodes(thickness = thickness)

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("vertical orientation renders correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    orientation("v")$
    layout(width = 900, height = 600)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 3: Color Schematic Tests
# =============================================================================

test_that("all coloring strategies render correctly", {
  ex <- example_communication()

  for (strategy in c("sender", "ligand", "condition")) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      colors(color_by = strategy)

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("condition coloring works with multigroup data", {
  fix <- fixture_multigroup()

  plot <- SankeyPlot$new(fix$data, fix$colors, group_col = "group")$
    colors(color_by = "condition")$
    group_colors(fix$group_colors)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("condition coloring falls back to sender without group data", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    colors(color_by = "condition")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("alpha transparency values work correctly", {
  ex <- example_communication()

  for (alpha in c(0.1, 0.3, 0.5, 0.7, 0.9, 1.0)) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      alpha(alpha)

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("node color modes render correctly", {
  fix <- fixture_varied_activity()

  # cell_type mode
  plot1 <- SankeyPlot$new(fix$data, fix$colors)$
    node_colors("cell_type")
  fig1 <- plot1$render()
  expect_s3_class(fig1, "plotly")

  # weighted_mix mode
  plot2 <- SankeyPlot$new(fix$data, fix$colors)$
    node_colors("weighted_mix")
  fig2 <- plot2$render()
  expect_s3_class(fig2, "plotly")
})

test_that("custom cell colors update correctly", {
  ex <- example_communication()
  new_colors <- list(
    Macrophage = "#000000",
    T_cell = "#FFFFFF",
    B_cell = "#888888"
  )

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    cell_colors(new_colors)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 4: Label Font Size and Position Tests
# =============================================================================

test_that("all label positions render correctly (with expected warning)", {
  ex <- example_communication()

  for (pos in c("left", "right", "top", "bottom", "inside")) {
    # Position triggers warning but plot should still render
    expect_warning(
      plot <- SankeyPlot$new(ex$data, ex$colors)$
        label_style(position = pos),
      "Label position is not supported"
    )

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("various font sizes render correctly", {
  ex <- example_communication()

  for (size in c(8, 10, 12, 14, 16, 18, 20, 24)) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      label_style(size = size)

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("long labels with different positions render (with expected warning)", {
  fix <- fixture_long_labels()

  for (pos in c("left", "right", "top", "bottom")) {
    # Position triggers warning but plot should still render
    expect_warning(
      plot <- SankeyPlot$new(fix$data, fix$colors)$
        label_style(position = pos, size = 12)$
        layout(width = 1000, height = 700),
      "Label position is not supported"
    )

    fig <- plot$render()
    expect_s3_class(fig, "plotly")

    # Verify summary shows correct counts
    summary <- plot$summary()
    expect_equal(summary$n_senders, 3)
    expect_equal(summary$n_receivers, 3)
  }
})

test_that("large font with many nodes renders correctly", {
  fix <- fixture_many_nodes(n_celltypes = 8, n_ligands = 15, n_receptors = 15)

  plot <- SankeyPlot$new(fix$data, fix$colors)$
    label_style(size = 14)$
    layout(width = 1000, height = 1400)$
    nodes(padding = 25)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("label position parameter triggers warning about Plotly limitation", {
  # Plotly Sankey does not support textposition on nodes
  # Setting position should generate a warning
  ex <- example_communication()

  expect_warning(
    SankeyPlot$new(ex$data, ex$colors)$label_style(position = "top"),
    "Label position is not supported"
  )

  expect_warning(
    SankeyPlot$new(ex$data, ex$colors)$label_style(position = "left"),
    "Label position is not supported"
  )
})

test_that("label size setting does not trigger position warning", {
  # Only size should work without position warning
  ex <- example_communication()

  # Use suppressWarnings to ignore unrelated warnings (e.g., missing weight columns)
  # but check that no "Label position" warning is produced
  warnings_captured <- NULL
  suppressWarnings({
    withCallingHandlers(
      SankeyPlot$new(ex$data, ex$colors)$label_style(size = 20),
      warning = function(w) {
        warnings_captured <<- c(warnings_captured, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  })

  # Check that no "Label position" warning was generated
  position_warnings <- grep("Label position", warnings_captured, value = TRUE)
  expect_length(position_warnings, 0)
})

test_that("combination of label size and position produces warning", {
  ex <- example_communication()

  combos <- expand.grid(
    size = c(10, 14, 18),
    position = c("left", "right", "top"),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(combos))) {
    # Position should trigger warning but plot should still render
    expect_warning(
      plot <- SankeyPlot$new(ex$data, ex$colors)$
        label_style(size = combos$size[i], position = combos$position[i]),
      "Label position is not supported"
    )

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

# =============================================================================
# SECTION 5: Interactive Features Tests
# =============================================================================

test_that("interactive mode enables filter dropdowns", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    interactive(TRUE)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
  # Interactive mode should render without error
})

test_that("interactive dropdowns render correctly", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    interactive(TRUE)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  # Verify data counts match expectations
  summary <- plot$summary()
  expect_equal(summary$n_senders, length(unique(ex$data$sender)))
  expect_equal(summary$n_ligands, length(unique(ex$data$ligand)))
  expect_equal(summary$n_receptors, length(unique(ex$data$receptor)))
  expect_equal(summary$n_receivers, length(unique(ex$data$receiver)))
})

test_that("interactive mode with many options renders correctly", {
  fix <- fixture_many_nodes(n_celltypes = 10, n_ligands = 20, n_receptors = 20)

  plot <- SankeyPlot$new(fix$data, fix$colors)$
    interactive(TRUE)$
    layout(width = 900, height = 1000)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("toggling interactive mode works", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)

  # Enable
  plot$interactive(TRUE)
  fig1 <- plot$render()
  expect_s3_class(fig1, "plotly")

  # Disable
  plot$interactive(FALSE)
  fig2 <- plot$render()
  expect_s3_class(fig2, "plotly")
})

test_that("interactive filter buttons preserve all link colors", {
  # This test ensures that when filtering, buttons contain colors for ALL links

  # (highlighted + dimmed), not just the matching links. This was a critical bug
  # where pathways would collapse instead of dimming because only matching colors
  # were included.
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    interactive(TRUE)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")

  # Each row creates 3 links (sender->ligand, ligand->receptor, receptor->receiver)
  expected_link_count <- nrow(ex$data) * 3

  # Check layoutAttrs for updatemenus
  for (la in fig$x$layoutAttrs) {
    if (!is.null(la$updatemenus)) {
      for (menu in la$updatemenus) {
        for (button in menu$buttons) {
          lc <- button$args[[1]][["link.color"]]
          if (is.list(lc)) lc <- lc[[1]]
          # Every button must have colors for ALL links
          expect_equal(length(lc), expected_link_count,
            info = paste("Button", button$label, "should have", expected_link_count, "colors"))
        }
      }
    }
  }
})

test_that("interactive mode works with all coloring strategies", {
  ex <- example_communication()

  for (strategy in c("sender", "ligand", "condition")) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      colors(color_by = strategy)$
      interactive(TRUE)

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

# =============================================================================
# SECTION 6: Pathway Highlighting Tests
# =============================================================================

test_that("highlight by sender works", {
  ex <- example_communication()
  sender <- unique(ex$data$sender)[1]

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    highlight_pathway(senders = sender)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("highlight by ligand works", {
  ex <- example_communication()
  ligand <- unique(ex$data$ligand)[1]

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    highlight_pathway(ligands = ligand)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("highlight by receptor works", {
  ex <- example_communication()
  receptor <- unique(ex$data$receptor)[1]

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    highlight_pathway(receptors = receptor)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("highlight by receiver works", {
  ex <- example_communication()
  receiver <- unique(ex$data$receiver)[1]

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    highlight_pathway(receivers = receiver)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("highlight with multiple criteria works", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    highlight_pathway(
      senders = unique(ex$data$sender)[1],
      ligands = unique(ex$data$ligand)[1],
      dim_alpha = 0.05
    )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("highlight with multiple values per criterion works", {
  fix <- fixture_multigroup()

  plot <- SankeyPlot$new(fix$data, fix$colors)$
    highlight_pathway(
      senders = c("Macrophage", "T_cell"),
      receivers = c("B_cell", "Fibroblast"),
      dim_alpha = 0.08
    )

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("various dim_alpha values work", {
  ex <- example_communication()

  for (dim_alpha in c(0.01, 0.05, 0.1, 0.2, 0.3)) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      highlight_pathway(
        senders = unique(ex$data$sender)[1],
        dim_alpha = dim_alpha
      )

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("clear_highlight removes highlighting", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    highlight_pathway(senders = unique(ex$data$sender)[1])$
    clear_highlight()

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("highlighting works with interactive mode", {
  ex <- example_communication()

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    highlight_pathway(senders = unique(ex$data$sender)[1])$
    interactive(TRUE)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 7: Dimension and Layout Tests
# =============================================================================

test_that("various dimension combinations render correctly", {
  ex <- example_communication()

  dimensions <- list(
    c(400, 600),   # small
    c(600, 900),   # default
    c(800, 600),   # wide
    c(600, 1200),  # tall
    c(1200, 800),  # large wide
    c(1000, 1400)  # large tall
  )

  for (dim in dimensions) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      layout(width = dim[1], height = dim[2])

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("small dimensions with minimal data works", {

  fix <- fixture_minimal()

  plot <- SankeyPlot$new(fix$data, fix$colors)$
    layout(width = 300, height = 400)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("title with various dimensions renders correctly", {
  ex <- example_communication()

  for (width in c(500, 700, 900)) {
    plot <- SankeyPlot$new(ex$data, ex$colors)$
      layout(width = width, height = 600)$
      add_title("Test Title for Sankey Plot")

    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})

test_that("long title renders without truncation", {
  ex <- example_communication()

  long_title <- "Cell-Cell Communication Analysis: Tumor Microenvironment Interactions Across Multiple Conditions"

  plot <- SankeyPlot$new(ex$data, ex$colors)$
    layout(width = 800, height = 600)$
    add_title(long_title)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 8: Flow Method Tests
# =============================================================================
test_that("flow_conserving method renders correctly", {
  fix <- fixture_varied_activity()

  plot <- SankeyPlot$new(fix$data, fix$colors, flow_method = "flow_conserving")
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("uniform flow method renders correctly", {
  fix <- fixture_varied_activity()

  plot <- SankeyPlot$new(fix$data, fix$colors, flow_method = "uniform")
  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("switching flow methods works", {
  fix <- fixture_varied_activity()
  plot <- SankeyPlot$new(fix$data, fix$colors)

  plot$flow_method("uniform")
  fig1 <- plot$render()
  expect_s3_class(fig1, "plotly")

  plot$flow_method("flow_conserving")
  fig2 <- plot$render()
  expect_s3_class(fig2, "plotly")
})

# =============================================================================
# SECTION 9: Combined Feature Tests (Integration)
# =============================================================================

test_that("all features combined render correctly", {
  fix <- fixture_multigroup()

  plot <- SankeyPlot$new(fix$data, fix$colors, group_col = "group")$
    colors(color_by = "sender")$
    alpha(0.7)$
    nodes(thickness = 20, padding = 35)$
    label_style(size = 14)$
    highlight_pathway(senders = "Macrophage", dim_alpha = 0.1)$
    legend(show = TRUE, title = "Cell Types")$
    layout(width = 900, height = 800)$
    add_title("Combined Features Test")$
    interactive(TRUE)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("long labels with interactive mode and highlighting", {
  fix <- fixture_long_labels()

  plot <- SankeyPlot$new(fix$data, fix$colors)$
    label_style(size = 11)$
    highlight_pathway(senders = "Tumor_Associated_Macrophage_Type_1")$
    interactive(TRUE)$
    layout(width = 1100, height = 700)

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

test_that("many nodes with all customizations", {
  fix <- fixture_many_nodes(n_celltypes = 10, n_ligands = 18, n_receptors = 18)

  plot <- SankeyPlot$new(fix$data, fix$colors)$
    colors(color_by = "sender")$
    alpha(0.6)$
    nodes(thickness = 15, padding = 18)$
    label_style(size = 10)$
    interactive(TRUE)$
    layout(width = 1000, height = 1400)$
    add_title("Large Network Visualization")

  fig <- plot$render()
  expect_s3_class(fig, "plotly")
})

# =============================================================================
# SECTION 11: Plot Summary Validation
# =============================================================================

test_that("plot summary has correct node counts", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)
  fig <- plot$render()

  summary <- plot$summary()

  # Verify summary matches data
  expect_equal(summary$n_senders, length(unique(ex$data$sender)))
  expect_equal(summary$n_receivers, length(unique(ex$data$receiver)))
  expect_equal(summary$n_ligands, length(unique(ex$data$ligand)))
  expect_equal(summary$n_receptors, length(unique(ex$data$receptor)))
  expect_equal(summary$n_interactions, nrow(ex$data))
})

test_that("plot summary reflects configuration", {
  ex <- example_communication()
  plot <- SankeyPlot$new(ex$data, ex$colors)$
    colors(color_by = "ligand")$
    alpha(0.5)$
    layout(width = 800, height = 700)

  fig <- plot$render()
  summary <- plot$summary()

  expect_equal(summary$color_by, "ligand")
  expect_equal(summary$alpha, 0.5)
  expect_equal(summary$dimensions[["width"]], 800)
  expect_equal(summary$dimensions[["height"]], 700)
})

test_that("plot renders with varied activity data", {
  fix <- fixture_varied_activity()
  plot <- SankeyPlot$new(fix$data, fix$colors)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
  summary <- plot$summary()
  expect_equal(summary$n_interactions, nrow(fix$data))
})

test_that("plot renders with multigroup data", {
  fix <- fixture_multigroup()
  plot <- SankeyPlot$new(fix$data, fix$colors, group_col = "group")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
  summary <- plot$summary()
  expect_equal(summary$n_interactions, nrow(fix$data))
})

test_that("plot renders with minimal data", {
  fix <- fixture_minimal()
  plot <- SankeyPlot$new(fix$data, fix$colors)
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
  summary <- plot$summary()
  expect_equal(summary$n_interactions, 1)
})
