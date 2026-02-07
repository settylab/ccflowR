#' @title Sankey Plot Class
#' @description Main visualization class for cell-cell communication Sankey diagrams
#' @name SankeyPlot
#' @importFrom R6 R6Class
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr filter mutate select distinct group_by summarise ungroup left_join arrange bind_rows pull n row_number
#' @importFrom stats setNames
NULL

#' Sankey Plot R6 Class
#'
#' @description
#' Create interactive Sankey river plots for visualizing cell-cell communication.
#' Displays the flow from Sender -> Ligand -> Receptor -> Receiver with multiple
#' coloring strategies and extensive customization options.
#'
#' @export
#'
#' @examples
#' # Basic usage
#' ex <- example_communication()
#' plot <- SankeyPlot$new(ex$data, ex$colors)
#' plot$show()
#'
#' # With method chaining
#' SankeyPlot$new(ex$data, ex$colors)$
#'   colors(color_by = "sender")$
#'   add_title("Cell Communication")$
#'   layout(width = 800, height = 600)$
#'   show()
SankeyPlot <- R6::R6Class(
  "SankeyPlot",
  inherit = BasePlot,

  public = list(
    #' @description Initialize Sankey plot
    #' @param data Data frame with columns: sender, ligand, receptor, receiver
    #' @param cell_colors Named list/vector of colors for cell types
    #' @param group_colors Named list/vector of colors for groups (optional)
    #' @param sender_col Name of sender column
    #' @param ligand_col Name of ligand column
    #' @param receptor_col Name of receptor column
    #' @param receiver_col Name of receiver column
    #' @param group_col Name of group column (optional)
    #' @param activity_col Name of activity/weight column (optional). For flow-conserving
    #'   mode, use "scaled_activity" (default when flow_method = "flow_conserving")
    #' @param ligand_expr_col Name of ligand expression column for flow-conserving mode.
    #'   Default: "scaled_ligand_expression"
    #' @param receptor_expr_col Name of receptor expression column for flow-conserving mode.
    #'   Default: "scaled_receptor_expression"
    #' @param flow_method Edge weight computation method: "flow_conserving" (default) or
    #'   "uniform". Flow-conserving distributes flow based on expression weights.
    #' @param auto_prepare Logical. If TRUE, automatically detect framework and map
    #'   columns. Default: TRUE
    #' @param width Plot width in pixels
    #' @param height Plot height in pixels
    initialize = function(data,
                          cell_colors = NULL,
                          group_colors = NULL,
                          sender_col = "sender",
                          ligand_col = "ligand",
                          receptor_col = "receptor",
                          receiver_col = "receiver",
                          group_col = NULL,
                          activity_col = NULL,
                          ligand_expr_col = "scaled_ligand_expression",
                          receptor_expr_col = "scaled_receptor_expression",
                          flow_method = c("flow_conserving", "uniform"),
                          auto_prepare = TRUE,
                          width = NULL,
                          height = NULL) {

      flow_method <- match.arg(flow_method)

      # Auto-prepare data if requested (detect framework and map columns)
      if (auto_prepare) {
        data <- prepare_sankey_data(data, auto_map = TRUE)
      }

      # Validate required columns
      required <- c(sender_col, ligand_col, receptor_col, receiver_col)
      validate_required_columns(data, required)

      super$initialize(data, width, height)

      # Store column mappings
      private$.sender_col <- sender_col
      private$.ligand_col <- ligand_col
      private$.receptor_col <- receptor_col
      private$.receiver_col <- receiver_col
      private$.group_col <- group_col
      private$.ligand_expr_col <- ligand_expr_col
      private$.receptor_expr_col <- receptor_expr_col

      # Determine activity column
      if (is.null(activity_col)) {
        if ("scaled_activity" %in% colnames(data)) {
          activity_col <- "scaled_activity"
        } else if ("activity" %in% colnames(data)) {
          activity_col <- "activity"
        }
      }
      private$.activity_col <- activity_col

      # Determine if flow-conserving is possible
      can_flow_conserve <- validate_sankey_input(data, require_weights = TRUE)
      if (flow_method == "flow_conserving" && !can_flow_conserve) {
        ccflow_log("Falling back to uniform flow method")
        flow_method <- "uniform"
      }
      private$.flow_method <- flow_method

      # Initialize color palettes
      private$.cell_palette <- ColorPalette$new(cell_colors)
      private$.group_palette <- ColorPalette$new(group_colors)

      # Auto-assign colors for all cell types
      all_cells <- unique(c(data[[sender_col]], data[[receiver_col]]))
      for (cell in all_cells) {
        private$.cell_palette$assign(cell)
      }

      # Auto-assign colors for groups if group column exists
      if (!is.null(group_col) && group_col %in% colnames(data)) {
        for (grp in unique(data[[group_col]])) {
          private$.group_palette$assign(grp)
        }
      }

      # Default settings (presentation-ready values)
      private$.color_by <- "sender"
      private$.alpha <- 0.65
      private$.node_thickness <- 25
      private$.node_padding <- 40
      private$.orientation <- "h"  # horizontal
      private$.node_order <- NULL
      private$.highlight_pathway <- NULL
      private$.highlight_alpha <- 0.1
      private$.show_legend <- TRUE
      private$.legend_title <- NULL
      private$.label_font_size <- 16
      private$.label_position <- "right"
      private$.interactive_filter <- FALSE

      # Node color mode: "cell_type" uses sender/receiver colors for all nodes
      # "weighted_mix" computes ligand colors from weighted sender colors,
      # and receptor colors from weighted ligand colors
      private$.node_color_mode <- "cell_type"
      private$.ligand_colors <- NULL
      private$.receptor_colors <- NULL
    },

    #' @description Configure coloring strategy
    #' @param color_by One of "sender", "receiver", "ligand", "condition"
    #' @param binary_color Deprecated. Ignored.
    #' @return Self (invisibly) for chaining
    colors = function(color_by = c("sender", "receiver", "ligand", "condition"),
                      binary_color = NULL) {
      # Backward compat: "binary" -> "condition"
      if (!is.null(color_by) && length(color_by) == 1 && color_by == "binary") {
        warning('color_by = "binary" is deprecated. Use "condition" instead.', call. = FALSE)
        color_by <- "condition"
      }
      # Backward compat: "group" -> "condition"
      if (!is.null(color_by) && length(color_by) == 1 && color_by == "group") {
        color_by <- "condition"
      }
      color_by <- match.arg(color_by)
      private$.color_by <- color_by
      if (!is.null(binary_color)) {
        warning("The binary_color parameter is deprecated and ignored.", call. = FALSE)
      }
      invisible(self)
    },

    #' @description Update cell type colors
    #' @param colors Named list/vector of colors
    #' @return Self (invisibly) for chaining
    cell_colors = function(colors) {
      private$.cell_palette$update(colors)
      invisible(self)
    },

    #' @description Update group colors
    #' @param colors Named list/vector of colors
    #' @return Self (invisibly) for chaining
    group_colors = function(colors) {
      private$.group_palette$update(colors)
      invisible(self)
    },

    #' @description Set transparency level
    #' @param alpha Alpha value (0-1)
    #' @return Self (invisibly) for chaining
    alpha = function(alpha) {
      private$.alpha <- alpha
      invisible(self)
    },

    #' @description Configure node appearance
    #' @param thickness Node thickness in pixels
    #' @param padding Padding between nodes
    #' @return Self (invisibly) for chaining
    nodes = function(thickness = NULL, padding = NULL) {
      if (!is.null(thickness)) private$.node_thickness <- thickness
      if (!is.null(padding)) private$.node_padding <- padding
      invisible(self)
    },

    #' @description Set custom node ordering
    #' @param senders Order of sender nodes
    #' @param ligands Order of ligand nodes
    #' @param receptors Order of receptor nodes
    #' @param receivers Order of receiver nodes
    #' @return Self (invisibly) for chaining
    node_order = function(senders = NULL, ligands = NULL, receptors = NULL, receivers = NULL) {
      private$.node_order <- list(
        senders = senders,
        ligands = ligands,
        receptors = receptors,
        receivers = receivers
      )
      invisible(self)
    },

    #' @description Highlight specific pathways
    #' @param senders Sender cell types to highlight
    #' @param ligands Ligands to highlight
    #' @param receptors Receptors to highlight
    #' @param receivers Receiver cell types to highlight
    #' @param dim_alpha Alpha for non-highlighted flows
    #' @return Self (invisibly) for chaining
    highlight_pathway = function(senders = NULL, ligands = NULL,
                                  receptors = NULL, receivers = NULL,
                                  dim_alpha = 0.1) {
      private$.highlight_pathway <- list(
        senders = senders,
        ligands = ligands,
        receptors = receptors,
        receivers = receivers
      )
      private$.highlight_alpha <- dim_alpha
      invisible(self)
    },

    #' @description Clear pathway highlighting
    #' @return Self (invisibly) for chaining
    clear_highlight = function() {
      private$.highlight_pathway <- NULL
      invisible(self)
    },

    #' @description Configure legend
    #' @param show Whether to show legend
    #' @param title Legend title
    #' @return Self (invisibly) for chaining
    legend = function(show = TRUE, title = NULL) {
      private$.show_legend <- show
      private$.legend_title <- title
      invisible(self)
    },

    #' @description Set plot orientation
    #' @param orientation "h" for horizontal, "v" for vertical
    #' @return Self (invisibly) for chaining
    orientation = function(orientation = c("h", "v")) {
      private$.orientation <- match.arg(orientation)
      invisible(self)
    },

    #' @description Configure label appearance and position
    #' @param size Font size for node labels (default: 16)
    #' @param position Position of labels relative to nodes:
    #'   \itemize{
    #'     \item "left" - labels appear to the left of nodes
    #'     \item "right" - labels appear to the right of nodes (default)
    #'     \item "top" - labels appear above nodes
    #'     \item "bottom" - labels appear below nodes
    #'     \item "inside" - labels appear inside nodes
    #'   }
    #' @note Plotly's Sankey trace does not support the textposition property.
    #'   The position parameter is stored but has no effect on rendering.
    #'   Label positions are determined automatically by Plotly based on
    #'   node arrangement. This is a limitation of the underlying Plotly library.
    #' @return Self (invisibly) for chaining
    #' @examples
    #' plot$label_style(size = 14)  # Only size has effect
    label_style = function(size = NULL, position = NULL) {
      if (!is.null(size)) private$.label_font_size <- size
      if (!is.null(position)) {
        # Note: position is stored but not used by Plotly Sankey
        # This is kept for potential future implementation using annotations
        valid_positions <- c("left", "right", "top", "bottom", "inside")
        if (!(position %in% valid_positions)) {
          stop("position must be one of: ", paste(valid_positions, collapse = ", "))
        }
        private$.label_position <- position
        warning("Label position is not supported by Plotly Sankey diagrams. ",
                "Labels are positioned automatically by Plotly.")
      }
      invisible(self)
    },

    #' @description Enable interactive filtering dropdowns
    #' @param enable Whether to show interactive filter dropdowns
    #' @return Self (invisibly) for chaining
    interactive = function(enable = TRUE) {
      private$.interactive_filter <- enable
      invisible(self)
    },

    #' @description Configure edge weight computation method
    #' @param method One of "flow_conserving" or "uniform".
    #'   - "flow_conserving": Distributes flow based on expression weights,
    #'     ensuring flow conservation through intermediate nodes. Requires

    #'     scaled_activity, scaled_ligand_expression, and scaled_receptor_expression columns.
    #'   - "uniform": Uses uniform weights (activity value or 1) for all edges.
    #' @return Self (invisibly) for chaining
    #' @examples
    #' plot$flow_method("uniform")  # Use simple uniform weights
    #' plot$flow_method("flow_conserving")  # Use expression-weighted flow
    flow_method = function(method = c("flow_conserving", "uniform")) {
      method <- match.arg(method)

      # Check if flow-conserving is possible
      if (method == "flow_conserving") {
        can_flow_conserve <- validate_sankey_input(private$.data, require_weights = TRUE)
        if (!can_flow_conserve) {
          warning("Cannot use flow_conserving method: missing required columns. Using uniform instead.")
          method <- "uniform"
        }
      }

      private$.flow_method <- method
      invisible(self)
    },

    #' @description Configure node coloring mode for ligand and receptor nodes
    #' @param mode One of "cell_type" or "weighted_mix".
    #'   - "cell_type" (default): Ligand nodes use a neutral color, receptor nodes use
    #'     a neutral color. This is the simple default behavior.
    #'   - "weighted_mix": Ligand node colors are computed by mixing sender cell colors
    #'     weighted by scaled_ligand_expression. Receptor node colors are computed by
    #'     mixing ligand colors weighted by scaled_receptor_expression. This creates
    #'     a visual flow where colors blend through the network.
    #' @return Self (invisibly) for chaining
    #' @examples
    #' # Use weighted color mixing for ligand/receptor nodes

    #' plot$node_colors("weighted_mix")
    #'
    #' # Reset to default cell type coloring
    #' plot$node_colors("cell_type")
    node_colors = function(mode = c("cell_type", "weighted_mix")) {
      mode <- match.arg(mode)

      # Check if weighted_mix is possible (need expression columns)
      if (mode == "weighted_mix") {
        data <- private$.data
        ligand_expr_col <- private$.ligand_expr_col
        receptor_expr_col <- private$.receptor_expr_col

        has_ligand_expr <- !is.null(ligand_expr_col) && ligand_expr_col %in% colnames(data)
        has_receptor_expr <- !is.null(receptor_expr_col) && receptor_expr_col %in% colnames(data)

        if (!has_ligand_expr || !has_receptor_expr) {
          warning("Cannot use weighted_mix mode: missing expression columns. ",
                  "Need '", ligand_expr_col, "' and '", receptor_expr_col, "'. ",
                  "Using cell_type mode instead.")
          mode <- "cell_type"
        } else {
          # Compute the mixed colors
          private$.compute_weighted_node_colors()
        }
      }

      private$.node_color_mode <- mode
      invisible(self)
    },

    #' @description Render the Sankey plot
    #' @return Plotly figure object
    render = function() {
      # Build node and link data
      sankey_data <- private$.build_sankey_data()

      # Note: Plotly Sankey does not support textposition property on nodes.
      # Label positions are determined automatically by Plotly based on
      # node arrangement. The label_position setting is stored but not used.

      # Create Sankey trace
      # Note: We don't specify x/y positions - let Plotly auto-arrange for best results
      fig <- plotly::plot_ly(
        type = "sankey",
        orientation = private$.orientation,
        textfont = list(
          size = private$.label_font_size,
          family = private$.font$family %||% "Arial"
        ),
        node = list(
          pad = private$.node_padding,
          thickness = private$.node_thickness,
          line = list(color = "black", width = 1),
          label = sankey_data$node_labels,
          color = sankey_data$node_colors
        ),
        link = list(
          source = sankey_data$link_source,
          target = sankey_data$link_target,
          value = sankey_data$link_value,
          color = sankey_data$link_colors
        ),
        arrangement = "snap"
      )

      # Apply layout
      fig <- private$.apply_layout(fig)

      # Add interactive filter dropdowns if enabled
      if (private$.interactive_filter) {
        fig <- private$.add_filter_dropdowns(fig, sankey_data)
      }

      private$.figure <- fig
      fig
    },

    #' @description Get summary of current plot configuration
    #' @return List with configuration summary
    summary = function() {
      data <- private$.data
      list(
        n_interactions = nrow(data),
        n_senders = length(unique(data[[private$.sender_col]])),
        n_receivers = length(unique(data[[private$.receiver_col]])),
        n_ligands = length(unique(data[[private$.ligand_col]])),
        n_receptors = length(unique(data[[private$.receptor_col]])),
        color_by = private$.color_by,
        alpha = private$.alpha,
        dimensions = c(width = private$.width, height = private$.height)
      )
    }
  ),

  private = list(
    .sender_col = NULL,
    .ligand_col = NULL,
    .receptor_col = NULL,
    .receiver_col = NULL,
    .group_col = NULL,
    .activity_col = NULL,
    .ligand_expr_col = NULL,
    .receptor_expr_col = NULL,
    .flow_method = NULL,
    .node_color_mode = NULL,  # "cell_type" or "weighted_mix"
    .ligand_colors = NULL,    # Computed ligand colors (for weighted_mix mode)
    .receptor_colors = NULL,  # Computed receptor colors (for weighted_mix mode)
    .cell_palette = NULL,
    .group_palette = NULL,
    .color_by = NULL,
    .alpha = NULL,
    .node_thickness = NULL,
    .node_padding = NULL,
    .orientation = NULL,
    .node_order = NULL,
    .highlight_pathway = NULL,
    .highlight_alpha = NULL,
    .show_legend = NULL,
    .legend_title = NULL,
    .label_font_size = NULL,
    .label_position = NULL,
    .interactive_filter = NULL,

    # Build Sankey data structure for plotly
    .build_sankey_data = function() {
      data <- private$.data
      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      receiver_col <- private$.receiver_col

      # Get unique values for each layer
      senders <- private$.get_ordered_nodes("senders", unique(data[[sender_col]]))
      ligands <- private$.get_ordered_nodes("ligands", unique(data[[ligand_col]]))
      receptors <- private$.get_ordered_nodes("receptors", unique(data[[receptor_col]]))
      receivers <- private$.get_ordered_nodes("receivers", unique(data[[receiver_col]]))

      # Build node list with layer prefixes to ensure uniqueness
      node_labels <- c(
        paste0("S:", senders),
        paste0("L:", ligands),
        paste0("R:", receptors),
        paste0("T:", receivers)  # T for Target/receiver
      )

      # Create node index lookup
      node_idx <- setNames(seq_along(node_labels) - 1, node_labels)

      # Calculate node positions (x coordinates for layers)
      # Leave more space at edges for labels (15% on each side)
      n_nodes <- length(node_labels)
      node_x <- c(
        rep(0.001, length(senders)),    # Senders at left edge
        rep(0.333, length(ligands)),    # Ligands at 1/3
        rep(0.666, length(receptors)),  # Receptors at 2/3
        rep(0.999, length(receivers))   # Receivers at right edge
      )

      # Y positions - evenly distributed within each layer
      node_y <- c(
        seq(0.01, 0.99, length.out = max(length(senders), 2))[1:length(senders)],
        seq(0.01, 0.99, length.out = max(length(ligands), 2))[1:length(ligands)],
        seq(0.01, 0.99, length.out = max(length(receptors), 2))[1:length(receptors)],
        seq(0.01, 0.99, length.out = max(length(receivers), 2))[1:length(receivers)]
      )

      # Build links
      links <- private$.build_links(data, node_idx, senders, ligands, receptors, receivers)

      # Calculate node colors based on incoming link colors
      node_colors <- private$.calculate_node_colors(node_labels, links, node_idx)

      # Clean labels for display (remove prefixes)
      display_labels <- gsub("^[SLRT]:", "", node_labels)

      list(
        node_labels = display_labels,
        node_colors = node_colors,
        node_x = node_x,
        node_y = node_y,
        link_source = links$source,
        link_target = links$target,
        link_value = links$value,
        link_colors = links$colors
      )
    },

    # Get ordered nodes for a layer
    .get_ordered_nodes = function(layer, nodes) {
      if (!is.null(private$.node_order) && !is.null(private$.node_order[[layer]])) {
        custom_order <- private$.node_order[[layer]]
        # Include custom order first, then remaining nodes
        ordered <- c(custom_order[custom_order %in% nodes],
                     nodes[!nodes %in% custom_order])
        return(ordered)
      }
      nodes
    },

    # Build link data - dispatches to appropriate method based on flow_method
    .build_links = function(data, node_idx, senders, ligands, receptors, receivers) {
      if (private$.flow_method == "flow_conserving") {
        return(private$.build_links_flow_conserving(data, node_idx, senders, ligands, receptors, receivers))
      }
      # Default: uniform method
      private$.build_links_uniform(data, node_idx, senders, ligands, receptors, receivers)
    },

    # Build links with uniform weights
    # Uniform mode: Edge 1 uses scaled_activity, Edges 2-3 split flow equally
    # at shared nodes. This differs from flow-conserving which splits proportional
    # to receptor expression.
    .build_links_uniform = function(data, node_idx, senders, ligands, receptors, receivers) {
      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      receiver_col <- private$.receiver_col
      activity_col <- private$.activity_col

      # --- Step 1: Compute Edge 1 (Sender -> Ligand) values ---
      edge1_values <- numeric(nrow(data))
      for (i in seq_len(nrow(data))) {
        value <- if (!is.null(activity_col) && activity_col %in% colnames(data)) {
          as.numeric(data[[activity_col]][i])
        } else {
          1
        }
        if (is.na(value) || value <= 0) value <- 1
        edge1_values[i] <- value
      }

      # --- Step 2: Compute ligand total inflow and outgoing count ---
      ligand_total_in <- list()
      ligand_out_count <- list()
      for (i in seq_len(nrow(data))) {
        ligand <- data[[ligand_col]][i]
        if (is.null(ligand_total_in[[ligand]])) {
          ligand_total_in[[ligand]] <- 0
          ligand_out_count[[ligand]] <- 0L
        }
        ligand_total_in[[ligand]] <- ligand_total_in[[ligand]] + edge1_values[i]
        ligand_out_count[[ligand]] <- ligand_out_count[[ligand]] + 1L
      }

      # --- Step 3: Compute Edge 2 values (equal split from ligand) ---
      edge2_values <- numeric(nrow(data))
      for (i in seq_len(nrow(data))) {
        ligand <- data[[ligand_col]][i]
        total_in <- ligand_total_in[[ligand]]
        n_out <- ligand_out_count[[ligand]]
        edge2_values[i] <- if (n_out > 0) total_in / n_out else 1
      }

      # --- Step 4: Compute receptor total inflow and outgoing count ---
      receptor_total_in <- list()
      receptor_out_count <- list()
      for (i in seq_len(nrow(data))) {
        receptor <- data[[receptor_col]][i]
        if (is.null(receptor_total_in[[receptor]])) {
          receptor_total_in[[receptor]] <- 0
          receptor_out_count[[receptor]] <- 0L
        }
        receptor_total_in[[receptor]] <- receptor_total_in[[receptor]] + edge2_values[i]
        receptor_out_count[[receptor]] <- receptor_out_count[[receptor]] + 1L
      }

      # --- Step 5: Compute Edge 3 values (equal split from receptor) ---
      edge3_values <- numeric(nrow(data))
      for (i in seq_len(nrow(data))) {
        receptor <- data[[receptor_col]][i]
        total_in <- receptor_total_in[[receptor]]
        n_out <- receptor_out_count[[receptor]]
        edge3_values[i] <- if (n_out > 0) total_in / n_out else 1
      }

      # --- Step 6: Build all edges ---
      sources <- c()
      targets <- c()
      values <- c()
      colors <- c()

      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        sender <- row[[sender_col]]
        ligand <- row[[ligand_col]]
        receptor <- row[[receptor_col]]
        receiver <- row[[receiver_col]]

        # Get color for this flow
        flow_color <- private$.get_flow_color(row, i)

        # Check if this flow should be highlighted
        alpha <- private$.get_flow_alpha(sender, ligand, receptor, receiver)
        rgba_color <- hex_with_alpha(flow_color, alpha)

        # Edge 1: Sender -> Ligand
        sources <- c(sources, node_idx[[paste0("S:", sender)]])
        targets <- c(targets, node_idx[[paste0("L:", ligand)]])
        values <- c(values, edge1_values[i])
        colors <- c(colors, rgba_color)

        # Edge 2: Ligand -> Receptor (equal split)
        sources <- c(sources, node_idx[[paste0("L:", ligand)]])
        targets <- c(targets, node_idx[[paste0("R:", receptor)]])
        values <- c(values, edge2_values[i])
        colors <- c(colors, rgba_color)

        # Edge 3: Receptor -> Receiver (equal split)
        sources <- c(sources, node_idx[[paste0("R:", receptor)]])
        targets <- c(targets, node_idx[[paste0("T:", receiver)]])
        values <- c(values, edge3_values[i])
        colors <- c(colors, rgba_color)
      }

      list(
        source = sources,
        target = targets,
        value = values,
        colors = colors
      )
    },

    # Build links with flow-conserving weights
    # This method properly distributes flow based on expression levels:
    # - Edge 1 (Sender->Ligand): value = scaled_activity
    # - Edge 2 (Ligand->Receptor): value = scaled_receptor_expression * ligand_total_in / weight_norm_sum
    # - Edge 3 (Receptor->Receiver): value = scaled_receptor_expression * receptor_total_in / weight_norm_sum
    .build_links_flow_conserving = function(data, node_idx, senders, ligands, receptors, receivers) {
      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      receiver_col <- private$.receiver_col
      activity_col <- private$.activity_col
      ligand_expr_col <- private$.ligand_expr_col
      receptor_expr_col <- private$.receptor_expr_col

      # Initialize link vectors
      sources <- c()
      targets <- c()
      values <- c()
      colors <- c()

      # --- Step 1: Compute Edge 1 (Sender -> Ligand) values ---
      # Value = scaled_activity (direct)
      edge1_values <- list()
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        sender <- row[[sender_col]]
        ligand <- row[[ligand_col]]

        # Get scaled_activity value
        value <- if (!is.null(activity_col) && activity_col %in% colnames(data)) {
          as.numeric(row[[activity_col]])
        } else {
          1
        }
        if (is.na(value) || value <= 0) value <- 1  # Default edge weight

        key <- paste(sender, ligand, i, sep = "|||")
        edge1_values[[key]] <- value
      }

      # --- Step 2: Compute ligand total inflow ---
      ligand_total_in <- list()
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        ligand <- row[[ligand_col]]
        key <- paste(row[[sender_col]], ligand, i, sep = "|||")
        val <- edge1_values[[key]]

        if (is.null(ligand_total_in[[ligand]])) {
          ligand_total_in[[ligand]] <- 0
        }
        ligand_total_in[[ligand]] <- ligand_total_in[[ligand]] + val
      }

      # --- Step 3: Compute Edge 2 (Ligand -> Receptor) normalization factors ---
      # Group by ligand to compute weight_norm_sum
      ligand_weight_norm_sum <- list()
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        ligand <- row[[ligand_col]]

        receptor_expr <- if (!is.null(receptor_expr_col) && receptor_expr_col %in% colnames(data)) {
          as.numeric(row[[receptor_expr_col]])
        } else {
          1
        }
        if (is.na(receptor_expr)) receptor_expr <- 0

        if (is.null(ligand_weight_norm_sum[[ligand]])) {
          ligand_weight_norm_sum[[ligand]] <- 0
        }
        ligand_weight_norm_sum[[ligand]] <- ligand_weight_norm_sum[[ligand]] + receptor_expr
      }

      # --- Step 4: Compute Edge 2 values and receptor total inflow ---
      edge2_values <- list()
      receptor_total_in <- list()
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        ligand <- row[[ligand_col]]
        receptor <- row[[receptor_col]]

        receptor_expr <- if (!is.null(receptor_expr_col) && receptor_expr_col %in% colnames(data)) {
          as.numeric(row[[receptor_expr_col]])
        } else {
          1
        }
        if (is.na(receptor_expr)) receptor_expr <- 0

        total_in <- ligand_total_in[[ligand]]
        weight_sum <- ligand_weight_norm_sum[[ligand]]

        # Compute normalized edge value
        edge2_val <- if (is.null(total_in) || is.null(weight_sum) || weight_sum == 0) {
          1  # Default edge weight
        } else {
          receptor_expr * total_in / weight_sum
        }
        if (edge2_val <= 0) edge2_val <- 1  # Default edge weight

        key <- paste(ligand, receptor, i, sep = "|||")
        edge2_values[[key]] <- edge2_val

        # Accumulate receptor total inflow
        if (is.null(receptor_total_in[[receptor]])) {
          receptor_total_in[[receptor]] <- 0
        }
        receptor_total_in[[receptor]] <- receptor_total_in[[receptor]] + edge2_val
      }

      # --- Step 5: Compute Edge 3 (Receptor -> Receiver) normalization factors ---
      receptor_weight_norm_sum <- list()
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        receptor <- row[[receptor_col]]

        receptor_expr <- if (!is.null(receptor_expr_col) && receptor_expr_col %in% colnames(data)) {
          as.numeric(row[[receptor_expr_col]])
        } else {
          1
        }
        if (is.na(receptor_expr)) receptor_expr <- 0

        if (is.null(receptor_weight_norm_sum[[receptor]])) {
          receptor_weight_norm_sum[[receptor]] <- 0
        }
        receptor_weight_norm_sum[[receptor]] <- receptor_weight_norm_sum[[receptor]] + receptor_expr
      }

      # --- Step 6: Build all edges ---
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        sender <- row[[sender_col]]
        ligand <- row[[ligand_col]]
        receptor <- row[[receptor_col]]
        receiver <- row[[receiver_col]]

        # Get receptor expression for Edge 3 calculation
        receptor_expr <- if (!is.null(receptor_expr_col) && receptor_expr_col %in% colnames(data)) {
          as.numeric(row[[receptor_expr_col]])
        } else {
          1
        }
        if (is.na(receptor_expr)) receptor_expr <- 0

        # Get color for this flow
        flow_color <- private$.get_flow_color(row, i)

        # Check if this flow should be highlighted
        alpha <- private$.get_flow_alpha(sender, ligand, receptor, receiver)
        rgba_color <- hex_with_alpha(flow_color, alpha)

        # Edge 1: Sender -> Ligand
        edge1_key <- paste(sender, ligand, i, sep = "|||")
        edge1_val <- edge1_values[[edge1_key]]

        sources <- c(sources, node_idx[[paste0("S:", sender)]])
        targets <- c(targets, node_idx[[paste0("L:", ligand)]])
        values <- c(values, edge1_val)
        colors <- c(colors, rgba_color)

        # Edge 2: Ligand -> Receptor
        edge2_key <- paste(ligand, receptor, i, sep = "|||")
        edge2_val <- edge2_values[[edge2_key]]

        sources <- c(sources, node_idx[[paste0("L:", ligand)]])
        targets <- c(targets, node_idx[[paste0("R:", receptor)]])
        values <- c(values, edge2_val)
        colors <- c(colors, rgba_color)

        # Edge 3: Receptor -> Receiver
        total_in <- receptor_total_in[[receptor]]
        weight_sum <- receptor_weight_norm_sum[[receptor]]

        edge3_val <- if (is.null(total_in) || is.null(weight_sum) || weight_sum == 0) {
          1  # Default edge weight
        } else {
          receptor_expr * total_in / weight_sum
        }
        if (edge3_val <= 0) edge3_val <- 1  # Default edge weight

        sources <- c(sources, node_idx[[paste0("R:", receptor)]])
        targets <- c(targets, node_idx[[paste0("T:", receiver)]])
        values <- c(values, edge3_val)
        colors <- c(colors, rgba_color)
      }

      list(
        source = sources,
        target = targets,
        value = values,
        colors = colors
      )
    },

    # Get color for a flow based on coloring strategy
    .get_flow_color = function(row, idx) {
      color_by <- private$.color_by

      if (color_by == "sender") {
        sender <- row[[private$.sender_col]]
        return(private$.cell_palette$get(sender))
      }

      if (color_by == "receiver") {
        receiver <- row[[private$.receiver_col]]
        return(private$.cell_palette$get(receiver))
      }

      if (color_by == "ligand") {
        ligand <- row[[private$.ligand_col]]
        return(private$.get_ligand_color(ligand))
      }

      if (color_by == "condition") {
        # Use group palette if group column exists, otherwise fall back to sender
        if (!is.null(private$.group_col) && private$.group_col %in% names(row)) {
          group <- row[[private$.group_col]]
          return(private$.group_palette$get(group))
        }
        # Fallback: color by sender when no group column
        sender <- row[[private$.sender_col]]
        return(private$.cell_palette$get(sender))
      }

      # Fallback
      ccflow_settings()$fallback_color()
    },

    # Get color for a ligand
    .get_ligand_color = function(ligand) {
      # Use a palette for ligands
      if (!private$.cell_palette$has(ligand)) {
        private$.cell_palette$assign(ligand)
      }
      private$.cell_palette$get(ligand)
    },

    # Get alpha value for a flow (for highlighting)
    .get_flow_alpha = function(sender, ligand, receptor, receiver) {
      highlight <- private$.highlight_pathway

      if (is.null(highlight)) {
        return(private$.alpha)
      }

      # Check if any highlight criteria match
      matches <- TRUE

      if (!is.null(highlight$senders) && !(sender %in% highlight$senders)) {
        matches <- FALSE
      }
      if (!is.null(highlight$ligands) && !(ligand %in% highlight$ligands)) {
        matches <- FALSE
      }
      if (!is.null(highlight$receptors) && !(receptor %in% highlight$receptors)) {
        matches <- FALSE
      }
      if (!is.null(highlight$receivers) && !(receiver %in% highlight$receivers)) {
        matches <- FALSE
      }

      if (matches) {
        return(private$.alpha)
      } else {
        return(private$.highlight_alpha)
      }
    },

    # Calculate node colors based on incoming links or pre-computed weighted colors
    .calculate_node_colors = function(node_labels, links, node_idx) {
      node_colors <- rep(ccflow_settings()$fallback_color(), length(node_labels))
      node_color_mode <- private$.node_color_mode

      for (i in seq_along(node_labels)) {
        label <- node_labels[i]

        # Handle sender nodes - always use cell palette
        if (grepl("^S:", label)) {
          sender_name <- gsub("^S:", "", label)
          node_colors[i] <- private$.cell_palette$get(sender_name)
          next
        }

        # Handle receiver nodes - always use cell palette
        if (grepl("^T:", label)) {
          receiver_name <- gsub("^T:", "", label)
          node_colors[i] <- private$.cell_palette$get(receiver_name)
          next
        }

        # Handle ligand nodes
        if (grepl("^L:", label)) {
          ligand_name <- gsub("^L:", "", label)
          if (node_color_mode == "weighted_mix" && !is.null(private$.ligand_colors)) {
            # Use pre-computed weighted color
            if (ligand_name %in% names(private$.ligand_colors)) {
              node_colors[i] <- private$.ligand_colors[[ligand_name]]
            } else {
              node_colors[i] <- ccflow_settings()$fallback_color()
            }
          } else {
            # Default: use incoming link colors
            node_key <- i - 1
            incoming_idx <- which(links$target == node_key)
            if (length(incoming_idx) > 0) {
              incoming_colors <- private$.extract_hex_from_rgba(links$colors[incoming_idx])
              incoming_weights <- links$value[incoming_idx]
              node_colors[i] <- weighted_mix_colors(incoming_colors, incoming_weights)
            }
          }
          next
        }

        # Handle receptor nodes
        if (grepl("^R:", label)) {
          receptor_name <- gsub("^R:", "", label)
          if (node_color_mode == "weighted_mix" && !is.null(private$.receptor_colors)) {
            # Use pre-computed weighted color
            if (receptor_name %in% names(private$.receptor_colors)) {
              node_colors[i] <- private$.receptor_colors[[receptor_name]]
            } else {
              node_colors[i] <- ccflow_settings()$fallback_color()
            }
          } else {
            # Default: use incoming link colors
            node_key <- i - 1
            incoming_idx <- which(links$target == node_key)
            if (length(incoming_idx) > 0) {
              incoming_colors <- private$.extract_hex_from_rgba(links$colors[incoming_idx])
              incoming_weights <- links$value[incoming_idx]
              node_colors[i] <- weighted_mix_colors(incoming_colors, incoming_weights)
            }
          }
          next
        }
      }

      node_colors
    },

    # Extract hex color from rgba string
    .extract_hex_from_rgba = function(colors) {
      sapply(colors, function(c) {
        if (grepl("^rgba", c)) {
          rgb_match <- regmatches(c, regexec("rgba\\((\\d+),\\s*(\\d+),\\s*(\\d+)", c))[[1]]
          if (length(rgb_match) >= 4) {
            return(rgb_to_hex(as.integer(rgb_match[2]),
                              as.integer(rgb_match[3]),
                              as.integer(rgb_match[4])))
          }
        }
        c
      }, USE.NAMES = FALSE)
    },

    # Compute weighted node colors for ligands and receptors
    # Ligand color = weighted mix of sender colors by scaled_ligand_expression
    # Receptor color = weighted mix of ligand colors by scaled_receptor_expression
    .compute_weighted_node_colors = function() {
      data <- private$.data
      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      ligand_expr_col <- private$.ligand_expr_col
      receptor_expr_col <- private$.receptor_expr_col

      # --- Compute ligand colors ---
      # Group by (ligand, sender), sum scaled_ligand_expression
      ligand_sender_weights <- list()
      for (i in seq_len(nrow(data))) {
        ligand <- data[[ligand_col]][i]
        sender <- data[[sender_col]][i]
        weight <- as.numeric(data[[ligand_expr_col]][i])
        if (is.na(weight)) weight <- 0

        key <- ligand
        if (is.null(ligand_sender_weights[[key]])) {
          ligand_sender_weights[[key]] <- list(senders = list(), weights = list())
        }

        # Aggregate by sender
        sender_idx <- match(sender, ligand_sender_weights[[key]]$senders)
        if (is.na(sender_idx)) {
          ligand_sender_weights[[key]]$senders <- c(ligand_sender_weights[[key]]$senders, sender)
          ligand_sender_weights[[key]]$weights <- c(ligand_sender_weights[[key]]$weights, weight)
        } else {
          ligand_sender_weights[[key]]$weights[[sender_idx]] <-
            ligand_sender_weights[[key]]$weights[[sender_idx]] + weight
        }
      }

      # Compute ligand colors by mixing sender colors
      ligand_colors <- list()
      for (ligand in names(ligand_sender_weights)) {
        senders_list <- unlist(ligand_sender_weights[[ligand]]$senders)
        weights <- unlist(ligand_sender_weights[[ligand]]$weights)

        # Get sender colors
        sender_colors <- sapply(senders_list, function(s) {
          private$.cell_palette$get(s)
        }, USE.NAMES = FALSE)

        # Mix colors
        if (length(sender_colors) > 0 && sum(weights) > 0) {
          ligand_colors[[ligand]] <- weighted_mix_colors(sender_colors, weights)
        } else {
          ligand_colors[[ligand]] <- ccflow_settings()$fallback_color()
        }
      }
      private$.ligand_colors <- ligand_colors

      # --- Compute receptor colors ---
      # Group by (receptor, ligand), sum scaled_receptor_expression
      receptor_ligand_weights <- list()
      for (i in seq_len(nrow(data))) {
        receptor <- data[[receptor_col]][i]
        ligand <- data[[ligand_col]][i]
        weight <- as.numeric(data[[receptor_expr_col]][i])
        if (is.na(weight)) weight <- 0

        key <- receptor
        if (is.null(receptor_ligand_weights[[key]])) {
          receptor_ligand_weights[[key]] <- list(ligands = list(), weights = list())
        }

        # Aggregate by ligand
        ligand_idx <- match(ligand, receptor_ligand_weights[[key]]$ligands)
        if (is.na(ligand_idx)) {
          receptor_ligand_weights[[key]]$ligands <- c(receptor_ligand_weights[[key]]$ligands, ligand)
          receptor_ligand_weights[[key]]$weights <- c(receptor_ligand_weights[[key]]$weights, weight)
        } else {
          receptor_ligand_weights[[key]]$weights[[ligand_idx]] <-
            receptor_ligand_weights[[key]]$weights[[ligand_idx]] + weight
        }
      }

      # Compute receptor colors by mixing ligand colors
      receptor_colors <- list()
      for (receptor in names(receptor_ligand_weights)) {
        ligands_list <- unlist(receptor_ligand_weights[[receptor]]$ligands)
        weights <- unlist(receptor_ligand_weights[[receptor]]$weights)

        # Get ligand colors (use the computed ligand colors)
        ligand_color_list <- sapply(ligands_list, function(l) {
          if (l %in% names(ligand_colors)) {
            ligand_colors[[l]]
          } else {
            ccflow_settings()$fallback_color()
          }
        }, USE.NAMES = FALSE)

        # Mix colors
        if (length(ligand_color_list) > 0 && sum(weights) > 0) {
          receptor_colors[[receptor]] <- weighted_mix_colors(ligand_color_list, weights)
        } else {
          receptor_colors[[receptor]] <- ccflow_settings()$fallback_color()
        }
      }
      private$.receptor_colors <- receptor_colors

      invisible(NULL)
    },

    # Add interactive filter dropdowns to the plot
    # Uses highlighting approach: non-matching pathways are dimmed, not hidden
    # This ensures complete pathway visualization without truncation
    .add_filter_dropdowns = function(fig, sankey_data) {
      data <- private$.data
      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      receiver_col <- private$.receiver_col
      base_alpha <- private$.alpha
      dim_alpha <- 0.08  # Alpha for non-matching pathways

      # Get unique values for each category
      senders <- unique(data[[sender_col]])
      ligands <- unique(data[[ligand_col]])
      receptors <- unique(data[[receptor_col]])
      receivers <- unique(data[[receiver_col]])

      # Helper function to dim a color (set alpha to dim_alpha)
      dim_color <- function(color, alpha) {
        if (grepl("^rgba", color)) {
          # Replace alpha in existing rgba
          gsub(",[^,]+\\)$", paste0(",", alpha, ")"), color)
        } else if (grepl("^#", color)) {
          # Convert hex to rgba
          hex_with_alpha(color, alpha)
        } else {
          color
        }
      }

      # Helper function to restore a color (set alpha to base_alpha)
      highlight_color <- function(color, alpha) {
        if (grepl("^rgba", color)) {
          gsub(",[^,]+\\)$", paste0(",", alpha, ")"), color)
        } else if (grepl("^#", color)) {
          hex_with_alpha(color, alpha)
        } else {
          color
        }
      }

      # Create "Show All" option that resets all links to original colors
      all_button <- list(
        method = "restyle",
        args = list(list(
          "link.color" = list(sankey_data$link_colors)
        )),
        label = "Show All"
      )

      # Helper function to create highlight buttons for a category
      # Instead of hiding non-matching links, we dim them with low alpha
      create_highlight_buttons <- function(values, category) {
        buttons <- list(all_button)

        for (val in values) {
          # Find rows matching this value
          if (category == "sender") {
            mask <- data[[sender_col]] == val
          } else if (category == "ligand") {
            mask <- data[[ligand_col]] == val
          } else if (category == "receptor") {
            mask <- data[[receptor_col]] == val
          } else if (category == "receiver") {
            mask <- data[[receiver_col]] == val
          }

          # Get indices of matching rows (each row creates 3 links)
          row_indices <- which(mask)

          # Calculate link indices for matching rows
          matching_link_indices <- unlist(lapply(row_indices, function(i) {
            c((i - 1) * 3 + 1, (i - 1) * 3 + 2, (i - 1) * 3 + 3)
          }))

          # Create new color array: dim non-matching, highlight matching
          new_colors <- sapply(seq_along(sankey_data$link_colors), function(i) {
            if (i %in% matching_link_indices) {
              # Keep original color (highlighted)
              highlight_color(sankey_data$link_colors[i], base_alpha)
            } else {
              # Dim non-matching pathways
              dim_color(sankey_data$link_colors[i], dim_alpha)
            }
          })

          button <- list(
            method = "restyle",
            args = list(list(
              "link.color" = list(new_colors)
            )),
            label = val
          )
          buttons <- c(buttons, list(button))
        }
        buttons
      }

      # Create dropdown menus
      sender_buttons <- create_highlight_buttons(senders, "sender")

      updatemenus <- list(
        # Sender dropdown
        list(
          active = 0,
          buttons = sender_buttons,
          direction = "down",
          showactive = TRUE,
          x = 0.0,
          xanchor = "left",
          y = 1.18,
          yanchor = "top",
          pad = list(r = 10, t = 10),
          bgcolor = "white",
          bordercolor = "#cccccc",
          font = list(size = 11)
        ),
        # Ligand dropdown
        list(
          active = 0,
          buttons = create_highlight_buttons(ligands, "ligand"),
          direction = "down",
          showactive = TRUE,
          x = 0.25,
          xanchor = "left",
          y = 1.18,
          yanchor = "top",
          pad = list(r = 10, t = 10),
          bgcolor = "white",
          bordercolor = "#cccccc",
          font = list(size = 11)
        ),
        # Receptor dropdown
        list(
          active = 0,
          buttons = create_highlight_buttons(receptors, "receptor"),
          direction = "down",
          showactive = TRUE,
          x = 0.5,
          xanchor = "left",
          y = 1.18,
          yanchor = "top",
          pad = list(r = 10, t = 10),
          bgcolor = "white",
          bordercolor = "#cccccc",
          font = list(size = 11)
        ),
        # Receiver dropdown
        list(
          active = 0,
          buttons = create_highlight_buttons(receivers, "receiver"),
          direction = "down",
          showactive = TRUE,
          x = 0.75,
          xanchor = "left",
          y = 1.18,
          yanchor = "top",
          pad = list(r = 10, t = 10),
          bgcolor = "white",
          bordercolor = "#cccccc",
          font = list(size = 11)
        )
      )

      # Add annotations for dropdown labels
      annotations <- list(
        list(
          text = "Sender:",
          x = 0.0,
          y = 1.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 12, color = "#333333")
        ),
        list(
          text = "Ligand:",
          x = 0.25,
          y = 1.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 12, color = "#333333")
        ),
        list(
          text = "Receptor:",
          x = 0.5,
          y = 1.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 12, color = "#333333")
        ),
        list(
          text = "Receiver:",
          x = 0.75,
          y = 1.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 12, color = "#333333")
        )
      )

      # Apply to figure with extra top margin for dropdowns
      fig <- plotly::layout(
        fig,
        updatemenus = updatemenus,
        annotations = annotations,
        margin = list(t = 120)  # Extra top margin for dropdowns
      )

      fig
    }
  )
)

#' Create Sankey Flow Plot (Convenience Function)
#'
#' @description
#' Convenience function to create and display a Sankey plot in one call.
#' Equivalent to creating a SankeyPlot and calling show().
#'
#' @param data Data frame with communication data. Supports both NicheNet and
#'   MultiNicheNet output formats. MultiNicheNet columns are automatically mapped.
#' @param cell_colors Named list/vector of cell type colors
#' @param color_by Coloring strategy: "sender", "receiver", "ligand", or "condition"
#' @param title Plot title
#' @param width Plot width
#' @param height Plot height
#' @param alpha Link transparency 0-1 (default: 0.65)
#' @param label_size Font size for node labels (default: 16)
#' @param label_position Position of labels (NOTE: not supported by Plotly Sankey - has no effect)
#' @param flow_method Edge weight computation method (default: "flow_conserving"):
#'   \itemize{
#'     \item "flow_conserving": Distributes flow based on expression weights.
#'       Requires scaled_activity, scaled_ligand_expression, scaled_receptor_expression columns.
#'       Automatically falls back to "uniform" if columns are missing.
#'     \item "uniform": Uses uniform weights (activity value or 1) for all edges.
#'   }
#' @param node_color_mode Node coloring mode (default: "cell_type"):
#'   \itemize{
#'     \item "cell_type": Sender/receiver nodes use cell type colors. Ligand and receptor
#'       nodes are colored by incoming link colors.
#'     \item "weighted_mix": Ligand nodes are colored by mixing sender colors weighted by
#'       scaled_ligand_expression. Receptor nodes are colored by mixing ligand colors
#'       weighted by scaled_receptor_expression. Creates a visual color flow through the network.
#'   }
#' @param interactive Enable interactive filter dropdowns (default: FALSE)
#' @param ... Additional arguments passed to SankeyPlot$new()
#'
#' @return Plotly figure object
#' @export
#'
#' @examples
#' ex <- example_communication()
#' sankey_flow(ex$data, ex$colors)
#'
#' # With customization
#' sankey_flow(ex$data, ex$colors,
#'             color_by = "sender",
#'             title = "Cell Communication",
#'             alpha = 0.7)
#'
#' # With labels on top
#' sankey_flow(ex$data, ex$colors,
#'             label_position = "top",
#'             label_size = 14)
#'
#' # With interactive filters
#' sankey_flow(ex$data, ex$colors,
#'             interactive = TRUE)
#'
#' # Use uniform weights instead of flow-conserving
#' sankey_flow(ex$data, ex$colors,
#'             flow_method = "uniform")
#'
#' # Use weighted color mixing for ligand/receptor nodes
#' sankey_flow(ex$data, ex$colors,
#'             node_color_mode = "weighted_mix")
sankey_flow <- function(data,
                        cell_colors = NULL,
                        color_by = "sender",
                        title = NULL,
                        width = NULL,
                        height = NULL,
                        alpha = 0.65,
                        label_size = 16,
                        label_position = "right",
                        flow_method = c("flow_conserving", "uniform"),
                        node_color_mode = c("cell_type", "weighted_mix"),
                        interactive = FALSE,
                        ...) {

  flow_method <- match.arg(flow_method)
  node_color_mode <- match.arg(node_color_mode)

  plot <- SankeyPlot$new(data, cell_colors, width = width, height = height,
                          flow_method = flow_method, ...)

  plot$colors(color_by = color_by)
  plot$alpha(alpha)
  plot$label_style(size = label_size, position = label_position)
  plot$node_colors(node_color_mode)
  plot$interactive(interactive)

  if (!is.null(title)) {
    plot$add_title(title)
  }

  plot$show()
}
