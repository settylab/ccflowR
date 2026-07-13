# Visual Test Runner for ccflowR
#
# This script generates HTML outputs for all visual test scenarios.
# Run this script to create a comprehensive set of visual tests for manual inspection.
#
# Usage:
#   source("tests/visual-test-runner.R")
#   run_visual_tests()  # Generates all test outputs
#   run_visual_tests(categories = "labels")  # Run specific category
#
# Output:
#   HTML files are saved to tests/visual-outputs/
#   An index.html file is generated listing all test cases

library(ccflowR)
library(htmlwidgets)

# =============================================================================
# Configuration
# =============================================================================

OUTPUT_DIR <- file.path(dirname(getwd()), "tests", "visual-outputs")
if (!dir.exists(OUTPUT_DIR)) {
  OUTPUT_DIR <- "tests/visual-outputs"
}

# =============================================================================
# Test Fixtures
# =============================================================================

#' Create fixture with very long labels
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

#' Create fixture with many nodes
fixture_many_nodes <- function(n_celltypes = 15, n_ligands = 25, n_receptors = 25) {
  set.seed(42)  # For reproducibility
  cell_types <- paste0("CellType_", sprintf("%02d", 1:n_celltypes))
  ligands <- paste0("LIG_", sprintf("%02d", 1:n_ligands))
  receptors <- paste0("REC_", sprintf("%02d", 1:n_receptors))

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

#' Create fixture with expression data for flow-conserving
fixture_with_expression <- function() {
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

#' Create multigroup fixture
fixture_multigroup <- function() {
  set.seed(42)
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
    Macrophage = "#E41A1C", T_cell = "#377EB8", Fibroblast = "#4DAF4A",
    NK_cell = "#984EA3", B_cell = "#FF7F00", Tumor = "#A65628"
  )

  group_colors <- c(
    Control = "#66C2A5", Treatment_A = "#FC8D62",
    Treatment_B = "#8DA0CB", Disease = "#E78AC3"
  )

  list(data = data, colors = cell_colors, group_colors = group_colors)
}

# =============================================================================
# Test Case Definitions
# =============================================================================

#' Define all visual test cases
get_test_cases <- function() {
  ex <- example_communication()
  long_labels <- fixture_long_labels()
  many_nodes <- fixture_many_nodes(n_celltypes = 12, n_ligands = 20, n_receptors = 20)
  with_expr <- fixture_with_expression()
  multigroup <- fixture_multigroup()

  list(
    # ----- Node Arrangement Tests -----
    node_arrangement = list(
      list(
        name = "default_arrangement",
        description = "Default node arrangement",
        plot = SankeyPlot$new(ex$data, ex$colors)
      ),
      list(
        name = "custom_node_order",
        description = "Custom node ordering (reversed)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          node_order(
            senders = rev(unique(ex$data$sender)),
            receivers = rev(unique(ex$data$receiver))
          )
      ),
      list(
        name = "many_nodes_compact",
        description = "Many nodes with compact padding",
        plot = SankeyPlot$new(many_nodes$data, many_nodes$colors)$
          nodes(padding = 15, thickness = 12)$
          layout(width = 900, height = 1200)$
          label_style(size = 10)
      ),
      list(
        name = "many_nodes_spacious",
        description = "Many nodes with spacious padding",
        plot = SankeyPlot$new(many_nodes$data, many_nodes$colors)$
          nodes(padding = 30, thickness = 20)$
          layout(width = 1000, height = 1600)$
          label_style(size = 11)
      ),
      list(
        name = "vertical_orientation",
        description = "Vertical orientation",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          orientation("v")$
          layout(width = 900, height = 600)
      ),
      list(
        name = "thick_nodes",
        description = "Thick nodes (40px)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          nodes(thickness = 40)
      ),
      list(
        name = "thin_nodes",
        description = "Thin nodes (10px)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          nodes(thickness = 10)
      )
    ),

    # ----- Color Schematic Tests -----
    color_schematics = list(
      list(
        name = "color_by_sender",
        description = "Color by sender cell type",
        plot = SankeyPlot$new(ex$data, ex$colors)$colors(color_by = "sender")
      ),
      list(
        name = "color_by_ligand",
        description = "Color by ligand",
        plot = SankeyPlot$new(ex$data, ex$colors)$colors(color_by = "ligand")
      ),
      list(
        name = "color_by_binary_blue",
        description = "Binary color (blue)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          colors(color_by = "binary", binary_color = "#1F77B4")
      ),
      list(
        name = "color_by_binary_red",
        description = "Binary color (red)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          colors(color_by = "binary", binary_color = "#E41A1C")
      ),
      list(
        name = "color_by_group",
        description = "Color by experimental group",
        plot = SankeyPlot$new(multigroup$data, multigroup$colors, group_col = "group")$
          colors(color_by = "group")$
          group_colors(multigroup$group_colors)$
          layout(width = 800, height = 700)
      ),
      list(
        name = "alpha_low",
        description = "Low alpha (0.3)",
        plot = SankeyPlot$new(ex$data, ex$colors)$alpha(0.3)
      ),
      list(
        name = "alpha_high",
        description = "High alpha (0.9)",
        plot = SankeyPlot$new(ex$data, ex$colors)$alpha(0.9)
      ),
      list(
        name = "node_color_weighted_mix",
        description = "Weighted mix node colors",
        plot = SankeyPlot$new(with_expr$data, with_expr$colors)$
          node_colors("weighted_mix")
      )
    ),

    # ----- Label Tests -----
    labels = list(
      list(
        name = "labels_right",
        description = "Labels on right (default)",
        plot = SankeyPlot$new(ex$data, ex$colors)$label_style(position = "right")
      ),
      list(
        name = "labels_left",
        description = "Labels on left",
        plot = SankeyPlot$new(ex$data, ex$colors)$label_style(position = "left")
      ),
      list(
        name = "labels_top",
        description = "Labels on top",
        plot = SankeyPlot$new(ex$data, ex$colors)$label_style(position = "top")
      ),
      list(
        name = "labels_bottom",
        description = "Labels on bottom",
        plot = SankeyPlot$new(ex$data, ex$colors)$label_style(position = "bottom")
      ),
      list(
        name = "labels_inside",
        description = "Labels inside nodes",
        plot = SankeyPlot$new(ex$data, ex$colors)$label_style(position = "inside")
      ),
      list(
        name = "labels_small_font",
        description = "Small font size (10px)",
        plot = SankeyPlot$new(ex$data, ex$colors)$label_style(size = 10)
      ),
      list(
        name = "labels_large_font",
        description = "Large font size (20px)",
        plot = SankeyPlot$new(ex$data, ex$colors)$label_style(size = 20)
      ),
      list(
        name = "long_labels_right",
        description = "Long labels, position right",
        plot = SankeyPlot$new(long_labels$data, long_labels$colors)$
          label_style(position = "right", size = 11)$
          layout(width = 1000, height = 600)
      ),
      list(
        name = "long_labels_left",
        description = "Long labels, position left",
        plot = SankeyPlot$new(long_labels$data, long_labels$colors)$
          label_style(position = "left", size = 11)$
          layout(width = 1000, height = 600)
      ),
      list(
        name = "long_labels_top",
        description = "Long labels, position top",
        plot = SankeyPlot$new(long_labels$data, long_labels$colors)$
          label_style(position = "top", size = 10)$
          layout(width = 1100, height = 700)
      ),
      list(
        name = "many_nodes_small_labels",
        description = "Many nodes with small labels",
        plot = SankeyPlot$new(many_nodes$data, many_nodes$colors)$
          label_style(size = 8, position = "right")$
          nodes(padding = 12)$
          layout(width = 900, height = 1400)
      )
    ),

    # ----- Interactive Feature Tests -----
    interactive = list(
      list(
        name = "interactive_basic",
        description = "Basic interactive filters",
        plot = SankeyPlot$new(ex$data, ex$colors)$interactive(TRUE)
      ),
      list(
        name = "interactive_many_options",
        description = "Interactive with many filter options",
        plot = SankeyPlot$new(many_nodes$data, many_nodes$colors)$
          interactive(TRUE)$
          layout(width = 1000, height = 1200)$
          label_style(size = 9)$
          nodes(padding = 15)
      ),
      list(
        name = "interactive_with_colors",
        description = "Interactive with sender coloring",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          colors(color_by = "sender")$
          interactive(TRUE)
      ),
      list(
        name = "interactive_multigroup",
        description = "Interactive with group data",
        plot = SankeyPlot$new(multigroup$data, multigroup$colors, group_col = "group")$
          interactive(TRUE)$
          layout(width = 900, height = 800)
      )
    ),

    # ----- Highlighting Tests -----
    highlighting = list(
      list(
        name = "highlight_sender",
        description = "Highlight by sender (Macrophage)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          highlight_pathway(senders = "Macrophage", dim_alpha = 0.1)
      ),
      list(
        name = "highlight_ligand",
        description = "Highlight by ligand (TGFB1)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          highlight_pathway(ligands = "TGFB1", dim_alpha = 0.1)
      ),
      list(
        name = "highlight_receptor",
        description = "Highlight by receptor (TGFBR1)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          highlight_pathway(receptors = "TGFBR1", dim_alpha = 0.1)
      ),
      list(
        name = "highlight_receiver",
        description = "Highlight by receiver (T_cell)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          highlight_pathway(receivers = "T_cell", dim_alpha = 0.1)
      ),
      list(
        name = "highlight_multi_criteria",
        description = "Highlight with multiple criteria",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          highlight_pathway(
            senders = c("Macrophage", "T_cell"),
            receivers = "Fibroblast",
            dim_alpha = 0.08
          )
      ),
      list(
        name = "highlight_low_dim",
        description = "Highlight with very low dim alpha (0.03)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          highlight_pathway(senders = "Macrophage", dim_alpha = 0.03)
      ),
      list(
        name = "highlight_moderate_dim",
        description = "Highlight with moderate dim alpha (0.2)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          highlight_pathway(senders = "Macrophage", dim_alpha = 0.2)
      ),
      list(
        name = "highlight_with_interactive",
        description = "Highlight combined with interactive mode",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          highlight_pathway(senders = "Macrophage", dim_alpha = 0.1)$
          interactive(TRUE)
      )
    ),

    # ----- Dimension Tests -----
    dimensions = list(
      list(
        name = "dim_small",
        description = "Small dimensions (400x600)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          layout(width = 400, height = 600)$
          label_style(size = 12)
      ),
      list(
        name = "dim_default",
        description = "Default dimensions (600x900)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          layout(width = 600, height = 900)
      ),
      list(
        name = "dim_wide",
        description = "Wide dimensions (1000x600)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          layout(width = 1000, height = 600)
      ),
      list(
        name = "dim_tall",
        description = "Tall dimensions (600x1200)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          layout(width = 600, height = 1200)
      ),
      list(
        name = "dim_large",
        description = "Large dimensions (1200x1000)",
        plot = SankeyPlot$new(ex$data, ex$colors)$
          layout(width = 1200, height = 1000)
      )
    ),

    # ----- Flow Method Tests -----
    flow_methods = list(
      list(
        name = "flow_conserving",
        description = "Flow-conserving method",
        plot = SankeyPlot$new(with_expr$data, with_expr$colors, flow_method = "flow_conserving")
      ),
      list(
        name = "flow_uniform",
        description = "Uniform flow method",
        plot = SankeyPlot$new(with_expr$data, with_expr$colors, flow_method = "uniform")
      )
    ),

    # ----- Combined Tests -----
    combined = list(
      list(
        name = "all_features",
        description = "All features combined",
        plot = SankeyPlot$new(multigroup$data, multigroup$colors, group_col = "group")$
          colors(color_by = "sender")$
          alpha(0.7)$
          nodes(thickness = 20, padding = 35)$
          label_style(size = 13, position = "right")$
          highlight_pathway(senders = "Macrophage", dim_alpha = 0.1)$
          legend(show = TRUE, title = "Cell Types")$
          layout(width = 900, height = 800)$
          add_title("Combined Features Test")$
          interactive(TRUE)
      ),
      list(
        name = "long_labels_interactive_highlight",
        description = "Long labels + interactive + highlighting",
        plot = SankeyPlot$new(long_labels$data, long_labels$colors)$
          label_style(size = 10, position = "right")$
          highlight_pathway(senders = "Tumor_Associated_Macrophage_Type_1")$
          interactive(TRUE)$
          layout(width = 1100, height = 700)
      ),
      list(
        name = "many_nodes_full_featured",
        description = "Many nodes with full features",
        plot = SankeyPlot$new(many_nodes$data, many_nodes$colors)$
          colors(color_by = "sender")$
          alpha(0.6)$
          nodes(thickness = 12, padding = 15)$
          label_style(size = 9, position = "right")$
          interactive(TRUE)$
          layout(width = 1000, height = 1400)$
          add_title("Large Network Visualization")
      )
    )
  )
}

# =============================================================================
# Test Runner Functions
# =============================================================================

#' Run visual tests and generate HTML outputs
#'
#' @param categories Character vector of test categories to run. If NULL, runs all.
#'   Valid categories: "node_arrangement", "color_schematics", "labels",
#'   "interactive", "highlighting", "dimensions", "flow_methods", "combined"
#' @param output_dir Directory to save HTML files
#' @param generate_index Whether to generate an index.html file
#'
#' @return Invisibly returns list of generated file paths
#' @export
run_visual_tests <- function(categories = NULL,
                             output_dir = "tests/visual-outputs",
                             generate_index = TRUE) {

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get all test cases
  all_tests <- get_test_cases()

  # Filter categories if specified
  if (!is.null(categories)) {
    all_tests <- all_tests[names(all_tests) %in% categories]
  }

  if (length(all_tests) == 0) {
    message("No test categories found. Available categories: ",
            paste(names(get_test_cases()), collapse = ", "))
    return(invisible(NULL))
  }

  # Track generated files
  generated_files <- list()
  results <- list()

  # Run tests
  for (category in names(all_tests)) {
    message("\n=== Running ", category, " tests ===")

    for (test_case in all_tests[[category]]) {
      test_name <- test_case$name
      description <- test_case$description
      plot <- test_case$plot

      message("  - ", test_name, ": ", description)

      tryCatch({
        # Render the plot
        fig <- plot$render()

        # Save to HTML
        filename <- paste0(category, "_", test_name, ".html")
        filepath <- file.path(output_dir, filename)

        htmlwidgets::saveWidget(
          fig,
          filepath,
          selfcontained = TRUE,
          title = paste0(category, " - ", description)
        )

        generated_files[[test_name]] <- list(
          category = category,
          name = test_name,
          description = description,
          filepath = filepath,
          filename = filename,
          status = "success"
        )

        results[[test_name]] <- "PASS"

      }, error = function(e) {
        message("    ERROR: ", e$message)
        generated_files[[test_name]] <- list(
          category = category,
          name = test_name,
          description = description,
          filepath = NA,
          filename = NA,
          status = "error",
          error = e$message
        )
        results[[test_name]] <- paste("FAIL:", e$message)
      })
    }
  }

  # Generate index file
  if (generate_index) {
    generate_index_html(generated_files, output_dir)
  }

  # Print summary
  message("\n=== Summary ===")
  n_pass <- sum(sapply(results, function(x) x == "PASS"))
  n_fail <- length(results) - n_pass
  message("Passed: ", n_pass, " / ", length(results))
  if (n_fail > 0) {
    message("Failed: ", n_fail)
    for (name in names(results)) {
      if (results[[name]] != "PASS") {
        message("  - ", name, ": ", results[[name]])
      }
    }
  }
  message("\nOutput directory: ", normalizePath(output_dir))
  if (generate_index) {
    message("Open index.html to view all test results")
  }

  invisible(generated_files)
}

#' Generate index.html file listing all test outputs
generate_index_html <- function(files, output_dir) {
  # Group files by category
  categories <- unique(sapply(files, function(x) x$category))

  # Build HTML header
  html <- paste0('<!DOCTYPE html>
<html>
<head>
  <title>ccflowR Visual Test Results</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; background: #f5f5f5; }
    h1 { color: #333; border-bottom: 2px solid #4CAF50; padding-bottom: 10px; }
    h2 { color: #555; margin-top: 30px; }
    .test-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; }
    .test-card {
      background: white;
      border-radius: 8px;
      padding: 15px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      transition: transform 0.2s;
    }
    .test-card:hover { transform: translateY(-2px); box-shadow: 0 4px 8px rgba(0,0,0,0.15); }
    .test-card a { text-decoration: none; color: #1976D2; font-weight: bold; }
    .test-card a:hover { text-decoration: underline; }
    .test-card p { color: #666; margin: 8px 0 0 0; font-size: 14px; }
    .status-success { border-left: 4px solid #4CAF50; }
    .status-error { border-left: 4px solid #f44336; }
    .error-msg { color: #f44336; font-size: 12px; }
    .summary { background: white; padding: 20px; border-radius: 8px; margin-bottom: 30px; }
    .summary-pass { color: #4CAF50; }
    .summary-fail { color: #f44336; }
  </style>
</head>
<body>
  <h1>ccflowR Visual Test Results</h1>
  <div class="summary">
    <strong>Generated:</strong> ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '<br>
    <strong>Total tests:</strong> ', length(files), '<br>
    <strong class="summary-pass">Passed:</strong> ', sum(sapply(files, function(x) x$status == "success")), '<br>
    <strong class="summary-fail">Failed:</strong> ', sum(sapply(files, function(x) x$status == "error")), '
  </div>')

  for (category in categories) {
    category_files <- files[sapply(files, function(x) x$category == category)]

    html <- paste0(html, '
  <h2>', gsub("_", " ", tools::toTitleCase(category)), '</h2>
  <div class="test-grid">')

    for (f in category_files) {
      status_class <- if (f$status == "success") "status-success" else "status-error"

      if (f$status == "success") {
        html <- paste0(html, '
    <div class="test-card ', status_class, '">
      <a href="', f$filename, '" target="_blank">', f$name, '</a>
      <p>', f$description, '</p>
    </div>')
      } else {
        html <- paste0(html, '
    <div class="test-card ', status_class, '">
      <span style="color: #999;">', f$name, '</span>
      <p>', f$description, '</p>
      <p class="error-msg">Error: ', f$error, '</p>
    </div>')
      }
    }

    html <- paste0(html, '
  </div>')
  }

  html <- paste0(html, '
</body>
</html>')

  writeLines(html, file.path(output_dir, "index.html"))
  message("Generated index.html")
}

#' Quick test runner for development
#'
#' @param test_name Name of a specific test to run
#' @return The rendered plotly figure
#' @export
quick_test <- function(test_name) {
  all_tests <- get_test_cases()

  for (category in names(all_tests)) {
    for (test_case in all_tests[[category]]) {
      if (test_case$name == test_name) {
        message("Running: ", test_case$description)
        fig <- test_case$plot$render()
        print(fig)
        return(invisible(fig))
      }
    }
  }

  message("Test not found: ", test_name)
  message("Available tests:")
  for (category in names(all_tests)) {
    for (test_case in all_tests[[category]]) {
      message("  - ", test_case$name)
    }
  }
}

#' List all available test cases
#' @export
list_tests <- function() {
  all_tests <- get_test_cases()

  for (category in names(all_tests)) {
    message("\n", toupper(category), ":")
    for (test_case in all_tests[[category]]) {
      message("  ", test_case$name, " - ", test_case$description)
    }
  }
}

message("Visual test runner loaded. Available commands:")
message("  run_visual_tests()        - Run all tests and generate HTML outputs")
message("  run_visual_tests('labels') - Run specific category")
message("  quick_test('labels_left') - Run and display a single test")
message("  list_tests()              - List all available tests")
