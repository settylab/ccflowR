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
    #' @param activity_col Name of activity/weight column (optional). Weighted
    #'   flow uses this score directly for sender-to-ligand width and defaults
    #'   to "scaled_activity" when present.
    #' @param activity_unit_col Optional column identifying rows that repeat one
    #'   score-bearing communication unit. Repeated rows sharing an identifier
    #'   divide that unit's activity so it contributes exactly once. With
    #'   `NULL` (default), every row is an independent path-level activity unit.
    #' @param ligand_expr_col Optional ligand expression column used for weighted
    #'   ligand-node color mixing, not edge widths. Default:
    #'   "scaled_ligand_expression".
    #' @param receptor_expr_col Name of the receptor-side weight column used to
    #'   redistribute ligand and receptor inflow downstream. Default:
    #'   "scaled_receptor_expression".
    #' @param flow_method Edge weight computation method. "auto" (default) uses
    #'   weighted flow when required weight columns are present and uniform flow otherwise.
    #'   "weighted" and "flow_conserving" use direct activity-based sender flow
    #'   with receptor-weighted downstream redistribution.
    #'   "uniform" draws all communication paths with equal edge weights.
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
                          activity_unit_col = NULL,
                          ligand_expr_col = "scaled_ligand_expression",
                          receptor_expr_col = "scaled_receptor_expression",
                          flow_method = c("auto", "weighted", "flow_conserving", "uniform"),
                          auto_prepare = TRUE,
                          width = NULL,
                          height = NULL) {

      flow_method <- match.arg(flow_method)
      requested_flow_method <- flow_method
      if (flow_method == "weighted") flow_method <- "flow_conserving"

      # Auto-prepare data if requested (detect framework and map columns)
      if (auto_prepare) {
        data <- prepare_sankey_data(data, auto_map = TRUE)
      }

      # Determine if receptor layer is present
      if (is.null(receptor_col)) {
        private$.has_receptor <- FALSE
      } else {
        private$.has_receptor <- TRUE
      }

      # Validate required columns
      required <- c(sender_col, ligand_col, receiver_col)
      if (private$.has_receptor) required <- c(required, receptor_col)
      validate_required_columns(data, required)

      super$initialize(data, width, height)

      # Store column mappings
      private$.sender_col <- sender_col
      private$.ligand_col <- ligand_col
      private$.receptor_col <- receptor_col
      private$.receiver_col <- receiver_col
      private$.group_col <- group_col
      private$.activity_unit_col <- activity_unit_col
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

      if (!is.null(activity_unit_col) &&
          !(activity_unit_col %in% colnames(data))) {
        stop("activity_unit_col not found in data: ", activity_unit_col,
             call. = FALSE)
      }

      # Resolve edge-weighting mode. Auto is intentionally quiet: if users
      # provide all required weight columns, use weighted flow; otherwise draw
      # uniform-width paths so topology can still be inspected.
      can_flow_conserve <- private$.has_receptor && !is.null(activity_col) &&
        validate_sankey_input(
          data,
          require_weights = TRUE,
          warn = FALSE,
          sender_col = sender_col,
          ligand_col = ligand_col,
          receptor_col = receptor_col,
          receiver_col = receiver_col,
          activity_col = activity_col,
          receptor_expr_col = receptor_expr_col
        )

      if (flow_method == "auto") {
        flow_method <- if (can_flow_conserve) "flow_conserving" else "uniform"
      } else if (flow_method == "flow_conserving" && !can_flow_conserve) {
        flow_method <- "uniform"
      }
      private$.requested_flow_method <- requested_flow_method
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
      private$.outer_vertical_span <- c(senders = 0.7, receivers = 0.7)
      private$.outer_minimum_gap <- 4
      private$.orientation <- "h"  # horizontal
      private$.arrangement <- "snap"
      private$.node_order <- NULL
      private$.order_mode <- "constrained"
      private$.order_iterations <- 8L
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
    #' @param color_by One of "sender", "receiver", "ligand", "condition",
    #'   or any valid color string (e.g., "steelblue", "#3498db") to use a
    #'   single fixed color for all flows.
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
      # Check if color_by is a named strategy or a fixed color
      strategies <- c("sender", "receiver", "ligand", "condition")
      if (length(color_by) == 1 && !(color_by %in% strategies)) {
        # Treat as a fixed color string
        private$.color_by <- "fixed"
        private$.fixed_flow_color <- color_by
      } else {
        color_by <- match.arg(color_by)
        private$.color_by <- color_by
        private$.fixed_flow_color <- NULL
      }
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
    #' @param arrangement Plotly arrangement mode. The default is "snap" when
    #'   no custom order is supplied; custom ordering uses fixed coordinates.
    #' @param vertical_span Requested fraction of the full rendered sender and
    #'   receiver spans to retain after constrained ordering. Supply one value
    #'   for both outer layers or a named vector with `senders` and/or
    #'   `receivers`; an unnamed layer retains its current span. Values must be
    #'   in `(0, 1]`. The default is 0.7; set to 1 to retain the original
    #'   full-span coordinates. A requested span is increased when needed to
    #'   prevent node or label overlap. Ligand and receptor coordinates are
    #'   never changed.
    #' @param minimum_gap Minimum readable separation in pixels between adjacent
    #'   outer-layer node or label boxes after packing. Default: 4.
    #' @return Self (invisibly) for chaining
    nodes = function(thickness = NULL, padding = NULL, arrangement = NULL,
                     vertical_span = 0.7, minimum_gap = 4) {
      if (!is.null(thickness)) private$.node_thickness <- thickness
      if (!is.null(padding)) private$.node_padding <- padding
      if (!is.null(arrangement)) {
        arrangement <- match.arg(arrangement, c("fixed", "snap", "freeform", "perpendicular"))
        private$.arrangement <- arrangement
      }
      if (!is.null(vertical_span)) {
        private$.outer_vertical_span <- private$.validate_vertical_span(
          vertical_span
        )
      }
      if (length(minimum_gap) != 1L || !is.finite(minimum_gap) ||
          minimum_gap < 0) {
        stop("minimum_gap must be one nonnegative finite number", call. = FALSE)
      }
      private$.outer_minimum_gap <- as.numeric(minimum_gap)
      invisible(self)
    },

    #' @description Set custom node ordering
    #' @param senders Order of sender nodes
    #' @param ligands Order of ligand nodes
    #' @param receptors Order of receptor nodes
    #' @param receivers Order of receiver nodes
    #' @param mode Ordering mode. `"constrained"` (default) treats supplied
    #'   vectors as relative order constraints and optimizes all remaining
    #'   freedom with repeated activity-weighted barycenter sweeps. `"fixed"`
    #'   preserves the historical behavior: each supplied vector is placed
    #'   exactly at the top of its layer, followed by unlisted nodes.
    #' @param iterations Number of forward/reverse sweeps used in constrained
    #'   mode.
    #' @return Self (invisibly) for chaining
    node_order = function(senders = NULL, ligands = NULL, receptors = NULL,
                          receivers = NULL,
                          mode = c("constrained", "fixed"), iterations = 8L) {
      mode <- match.arg(mode)
      if (length(iterations) != 1L || is.na(iterations) ||
          iterations < 1 || iterations != as.integer(iterations)) {
        stop("iterations must be a positive integer", call. = FALSE)
      }
      if (!private$.has_receptor && !is.null(receptors)) {
        warning("receptors ordering ignored: receptor layer is not displayed", call. = FALSE)
        receptors <- NULL
      }
      private$.node_order <- list(
        senders = senders,
        ligands = ligands,
        receptors = receptors,
        receivers = receivers
      )
      private$.order_mode <- mode
      private$.order_iterations <- as.integer(iterations)
      # Lock positions when custom order is specified, so Plotly
      # doesn't rearrange nodes and override the user's order
      if (!is.null(senders) || !is.null(ligands) || !is.null(receptors) || !is.null(receivers)) {
        private$.arrangement <- "fixed"
      }
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
      if (!private$.has_receptor && !is.null(receptors)) {
        warning("receptors highlighting ignored: receptor layer is not displayed", call. = FALSE)
        receptors <- NULL
      }
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
    #' @param method One of "auto", "weighted", "flow_conserving", or "uniform".
    #'   - "auto": Uses weighted flow when required columns are present;
    #'     otherwise uses uniform edge widths without warning.
    #'   - "weighted"/"flow_conserving": Uses scaled_activity directly for
    #'     sender-to-ligand width and scaled_receptor_expression to redistribute
    #'     flow downstream. Ligand expression is not an edge-width multiplier.
    #'   - "uniform": Uses equal edge widths for all communication paths.
    #' @return Self (invisibly) for chaining
    #' @examples
    #' plot$flow_method("uniform")  # Draw equal-width paths
    #' plot$flow_method("weighted")  # Use direct activity and receptor-side weights
    flow_method = function(method = c("auto", "weighted", "flow_conserving", "uniform")) {
      method <- match.arg(method)
      requested_method <- method
      if (method == "weighted") method <- "flow_conserving"

      can_flow_conserve <- private$.has_receptor &&
        !is.null(private$.activity_col) &&
        validate_sankey_input(
          private$.data,
          require_weights = TRUE,
          warn = FALSE,
          sender_col = private$.sender_col,
          ligand_col = private$.ligand_col,
          receptor_col = private$.receptor_col,
          receiver_col = private$.receiver_col,
          activity_col = private$.activity_col,
          receptor_expr_col = private$.receptor_expr_col
        )

      if (method == "auto") {
        method <- if (can_flow_conserve) "flow_conserving" else "uniform"
      } else if (method == "flow_conserving" && !can_flow_conserve) {
        method <- "uniform"
      }

      private$.requested_flow_method <- requested_method
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

        has_ligand_expr <- !is.null(ligand_expr_col) && ligand_expr_col %in% colnames(data)

        if (private$.has_receptor) {
          receptor_expr_col <- private$.receptor_expr_col
          has_receptor_expr <- !is.null(receptor_expr_col) && receptor_expr_col %in% colnames(data)

          if (!has_ligand_expr || !has_receptor_expr) {
            warning("Cannot use weighted_mix mode: missing expression columns. ",
                    "Need '", ligand_expr_col, "' and '", receptor_expr_col, "'. ",
                    "Using cell_type mode instead.")
            mode <- "cell_type"
          } else {
            private$.compute_weighted_node_colors()
          }
        } else {
          # 3-layer mode: only need ligand expression
          if (!has_ligand_expr) {
            warning("Cannot use weighted_mix mode: missing '", ligand_expr_col, "'. ",
                    "Using cell_type mode instead.")
            mode <- "cell_type"
          } else {
            private$.compute_weighted_node_colors()
          }
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
      # When no custom order is specified, omit x/y to let Plotly fully
      # handle the layout (minimizes crossings, especially in L-R layer).
      # When custom order is set, provide explicit x/y with arrangement="fixed".
      node_config <- list(
        pad = private$.node_padding,
        thickness = private$.node_thickness,
        line = list(color = "black", width = 1),
        label = sankey_data$node_labels,
        color = sankey_data$node_colors,
        customdata = sankey_data$node_hover_labels,
        hovertemplate = "%{customdata}<extra></extra>"
      )

      # Only set explicit positions when user specified custom order
      if (!is.null(sankey_data$node_x)) {
        node_config$x <- sankey_data$node_x
        node_config$y <- sankey_data$node_y
      }

      fig <- plotly::plot_ly(
        type = "sankey",
        orientation = private$.orientation,
        textfont = list(
          size = private$.label_font_size,
          family = private$.font$family %||% "Arial"
        ),
        node = node_config,
        link = list(
          source = sankey_data$link_source,
          target = sankey_data$link_target,
          value = sankey_data$link_value,
          color = sankey_data$link_colors
        ),
        arrangement = private$.arrangement
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
      result <- list(
        n_interactions = nrow(data),
        n_senders = length(unique(data[[private$.sender_col]])),
        n_receivers = length(unique(data[[private$.receiver_col]])),
        n_ligands = length(unique(data[[private$.ligand_col]])),
        has_receptor = private$.has_receptor,
        color_by = private$.color_by,
        alpha = private$.alpha,
        dimensions = c(width = private$.width, height = private$.height),
        flow_method = private$.flow_method,
        requested_flow_method = private$.requested_flow_method,
        activity_unit_col = private$.activity_unit_col
      )
      if (private$.has_receptor) {
        result$n_receptors <- length(unique(data[[private$.receptor_col]]))
      }
      result
    }
  ),

  private = list(
    .sender_col = NULL,
    .ligand_col = NULL,
    .receptor_col = NULL,
    .receiver_col = NULL,
    .group_col = NULL,
    .activity_col = NULL,
    .activity_unit_col = NULL,
    .ligand_expr_col = NULL,
    .receptor_expr_col = NULL,
    .has_receptor = NULL,       # TRUE = 4-layer (S->L->R->T), FALSE = 3-layer (S->L->T)
    .flow_method = NULL,
    .requested_flow_method = NULL,
    .node_color_mode = NULL,  # "cell_type" or "weighted_mix"
    .ligand_colors = NULL,    # Computed ligand colors (for weighted_mix mode)
    .receptor_colors = NULL,  # Computed receptor colors (for weighted_mix mode)
    .cell_palette = NULL,
    .group_palette = NULL,
    .color_by = NULL,
    .fixed_flow_color = NULL,
    .alpha = NULL,
    .node_thickness = NULL,
    .node_padding = NULL,
    .outer_vertical_span = NULL,
    .outer_minimum_gap = NULL,
    .arrangement = NULL,
    .orientation = NULL,
    .node_order = NULL,
    .order_mode = NULL,
    .order_iterations = NULL,
    .highlight_pathway = NULL,
    .highlight_alpha = NULL,
    .show_legend = NULL,
    .legend_title = NULL,
    .label_font_size = NULL,
    .label_position = NULL,
    .interactive_filter = NULL,

    # Normalize the public outer-layer span control to a named two-element
    # vector. A scalar applies symmetrically; a named subset updates only the
    # requested outer layer and retains the other layer's current span.
    .validate_vertical_span = function(vertical_span) {
      if (!is.numeric(vertical_span) || length(vertical_span) < 1L ||
          length(vertical_span) > 2L || any(!is.finite(vertical_span)) ||
          any(vertical_span <= 0 | vertical_span > 1)) {
        stop(
          "vertical_span must contain one or two finite values in (0, 1]",
          call. = FALSE
        )
      }

      if (is.null(names(vertical_span))) {
        if (length(vertical_span) != 1L) {
          stop(
            "An unnamed vertical_span must contain exactly one value",
            call. = FALSE
          )
        }
        return(c(
          senders = as.numeric(vertical_span),
          receivers = as.numeric(vertical_span)
        ))
      }

      if (any(!nzchar(names(vertical_span))) ||
          anyDuplicated(names(vertical_span)) ||
          any(!names(vertical_span) %in% c("senders", "receivers"))) {
        stop(
          "Named vertical_span values must use unique 'senders' and/or 'receivers' names",
          call. = FALSE
        )
      }
      result <- private$.outer_vertical_span
      if (is.null(result)) {
        result <- c(senders = 0.7, receivers = 0.7)
      }
      result[names(vertical_span)] <- as.numeric(vertical_span)
      result
    },

    # Return the nonnegative activity contribution represented by each row.
    # By default every row is an independent path-level unit. When an explicit
    # activity-unit column is supplied, repeated rendering rows divide one
    # shared score so receptor expansion cannot multiply sender flow.
    .activity_row_values = function(data) {
      if (is.null(private$.activity_col) ||
          !(private$.activity_col %in% colnames(data))) {
        return(rep(1, nrow(data)))
      }

      activity <- suppressWarnings(as.numeric(data[[private$.activity_col]]))
      activity[!is.finite(activity) | activity <= 0] <- 0

      if (is.null(private$.activity_unit_col)) {
        return(activity)
      }

      unit_values <- as.character(data[[private$.activity_unit_col]])
      missing_unit <- is.na(unit_values) | !nzchar(unit_values)
      unit_keys <- paste0("unit:", unit_values)
      unit_keys[missing_unit] <- paste0("row:", which(missing_unit))
      unit_rows <- split(seq_len(nrow(data)), unit_keys)
      contributions <- numeric(nrow(data))

      for (rows in unit_rows) {
        if (length(rows) > 1L) {
          sender_ligand <- paste(
            data[[private$.sender_col]][rows],
            data[[private$.ligand_col]][rows],
            sep = " -> "
          )
          if (length(unique(sender_ligand)) != 1L) {
            stop(
              "Each activity unit must map to one sender-ligand pair; unit '",
              unit_values[[rows[[1L]]]], "' spans multiple pairs.",
              call. = FALSE
            )
          }

          unit_activity <- activity[rows]
          tolerance <- sqrt(.Machine$double.eps) *
            max(1, abs(unit_activity))
          if (max(unit_activity) - min(unit_activity) > tolerance) {
            stop(
              "Rows sharing activity unit '", unit_values[[rows[[1L]]]],
              "' have inconsistent activity scores.",
              call. = FALSE
            )
          }
        }

        contributions[rows] <- activity[[rows[[1L]]]] / length(rows)
      }

      contributions
    },

    # Build Sankey data structure for plotly
    .build_sankey_data = function() {
      data <- private$.data
      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      receiver_col <- private$.receiver_col

      # Get unique values for each layer
      raw_senders <- unique(data[[sender_col]])
      raw_ligands <- unique(data[[ligand_col]])
      raw_receptors <- if (private$.has_receptor) unique(data[[receptor_col]]) else character(0)
      raw_receivers <- unique(data[[receiver_col]])

      has_custom_order <- !is.null(private$.node_order) && any(
        !sapply(private$.node_order, is.null))

      if (has_custom_order) {
        # Custom order specified: compute deterministic positions in R and
        # render them with Plotly's fixed arrangement.
        senders <- private$.order_layer("senders", raw_senders, NULL, data)
        ligands <- private$.order_layer("ligands", raw_ligands, senders, data)

        if (private$.has_receptor) {
          receptors <- private$.order_layer("receptors", raw_receptors, ligands, data)
          receivers <- private$.order_layer("receivers", raw_receivers, receptors, data)

          if (identical(private$.order_mode, "fixed")) {
            # Backward-compatible three-pass behavior. Fully specified layers
            # remain fixed; only unspecified layers are refined.
            receptors <- private$.refine_order("receptors", receptors, receivers, data,
                                                receptor_col, receiver_col)
            ligands <- private$.refine_order("ligands", ligands, receptors, data,
                                              ligand_col, receptor_col)
            receptors <- private$.refine_order("receptors", receptors, ligands, data,
                                                receptor_col, ligand_col)
            receivers <- private$.refine_order("receivers", receivers, receptors, data,
                                                receiver_col, receptor_col)
          } else {
            optimized <- private$.optimize_constrained_order(
              list(
                senders = senders,
                ligands = ligands,
                receptors = receptors,
                receivers = receivers
              ),
              data
            )
            senders <- optimized$senders
            ligands <- optimized$ligands
            receptors <- optimized$receptors
            receivers <- optimized$receivers
          }
        } else {
          receptors <- character(0)
          receivers <- private$.order_layer("receivers", raw_receivers, ligands, data)

          if (identical(private$.order_mode, "fixed")) {
            ligands <- private$.refine_order("ligands", ligands, receivers, data,
                                              ligand_col, receiver_col)
            receivers <- private$.refine_order("receivers", receivers, ligands, data,
                                                receiver_col, ligand_col)
          } else {
            optimized <- private$.optimize_constrained_order(
              list(
                senders = senders,
                ligands = ligands,
                receivers = receivers
              ),
              data
            )
            senders <- optimized$senders
            ligands <- optimized$ligands
            receivers <- optimized$receivers
          }
        }
      } else {
        # No custom order: use data order; Plotly handles layout optimization
        senders <- raw_senders
        ligands <- raw_ligands
        receptors <- raw_receptors
        receivers <- raw_receivers
      }

      # Build node list with layer prefixes to ensure uniqueness
      if (private$.has_receptor) {
        node_labels <- c(
          paste0("S:", senders),
          paste0("L:", ligands),
          paste0("R:", receptors),
          paste0("T:", receivers)
        )
      } else {
        node_labels <- c(
          paste0("S:", senders),
          paste0("L:", ligands),
          paste0("T:", receivers)
        )
      }

      # Create node index lookup
      node_idx <- setNames(seq_along(node_labels) - 1, node_labels)

      # Build links
      links <- private$.build_links(data, node_idx, senders, ligands, receptors, receivers)

      # Only compute explicit x/y positions when custom order is specified.
      # Otherwise, omit them so Plotly's layout engine handles positioning
      # (minimizes crossings, especially in the L-R layer).
      node_x <- NULL
      node_y <- NULL

      if (has_custom_order) {
        if (private$.has_receptor) {
          node_x <- c(
            rep(0.001, length(senders)),
            rep(0.333, length(ligands)),
            rep(0.666, length(receptors)),
            rep(0.999, length(receivers))
          )
          layer_nodes_list <- list(
            senders = paste0("S:", senders),
            ligands = paste0("L:", ligands),
            receptors = paste0("R:", receptors),
            receivers = paste0("T:", receivers)
          )
        } else {
          node_x <- c(
            rep(0.001, length(senders)),
            rep(0.5, length(ligands)),
            rep(0.999, length(receivers))
          )
          layer_nodes_list <- list(
            senders = paste0("S:", senders),
            ligands = paste0("L:", ligands),
            receivers = paste0("T:", receivers)
          )
        }

        node_y <- private$.calculate_flow_y(
          layer_nodes_list, links, node_idx
        )

        # Sort links by full flow path for consistent stacking at nodes
        links <- private$.sort_links(links, node_y, data, node_idx)
      }

      # Calculate node colors based on incoming link colors
      node_colors <- private$.calculate_node_colors(node_labels, links, node_idx)

      # Apply highlighting to node colors and labels (dim non-highlighted nodes)
      highlight_result <- private$.apply_node_highlight(node_colors, node_labels, data)
      node_colors <- highlight_result$node_colors

      # Clean labels for display (remove prefixes)
      # If highlighting is active, use display_labels (non-highlighted = "")
      all_labels <- gsub("^[SLRT]:", "", node_labels)
      display_labels <- if (!is.null(highlight_result$display_labels)) {
        highlight_result$display_labels
      } else {
        all_labels
      }

      list(
        node_labels = display_labels,
        node_hover_labels = all_labels,
        node_colors = node_colors,
        node_x = node_x,
        node_y = node_y,
        link_source = links$source,
        link_target = links$target,
        link_value = links$value,
        link_colors = links$colors
      )
    },

    # Get ordered nodes for a layer (custom order only)
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

    # Apply a chain of relative-order constraints while otherwise retaining the
    # optimizer's candidate order. A stable priority topological sort allows
    # unnamed nodes to fall before, between, or after constrained nodes.
    .apply_relative_constraints = function(nodes, constraints) {
      nodes <- unique(nodes)
      anchors <- unique(constraints[constraints %in% nodes])
      if (length(nodes) <= 1 || length(anchors) <= 1) return(nodes)

      priority <- stats::setNames(seq_along(nodes), nodes)
      indegree <- stats::setNames(integer(length(nodes)), nodes)
      successors <- stats::setNames(vector("list", length(nodes)), nodes)

      for (i in seq_len(length(anchors) - 1L)) {
        before <- anchors[[i]]
        after <- anchors[[i + 1L]]
        successors[[before]] <- unique(c(successors[[before]], after))
        indegree[[after]] <- indegree[[after]] + 1L
      }

      available <- nodes[indegree[nodes] == 0L]
      ordered <- character(0)
      while (length(available) > 0) {
        pick_index <- which.min(priority[available])
        pick <- available[[pick_index]]
        available <- available[-pick_index]
        ordered <- c(ordered, pick)

        for (next_node in successors[[pick]]) {
          indegree[[next_node]] <- indegree[[next_node]] - 1L
          if (indegree[[next_node]] == 0L) {
            available <- unique(c(available, next_node))
          }
        }
      }

      if (length(ordered) != length(nodes)) {
        stop("Internal error: node-order constraints contain a cycle", call. = FALSE)
      }
      ordered
    },

    .apply_layer_constraints = function(layer_type, nodes) {
      if (!identical(private$.order_mode, "constrained") ||
          is.null(private$.node_order) ||
          is.null(private$.node_order[[layer_type]])) {
        return(nodes)
      }
      private$.apply_relative_constraints(
        nodes,
        private$.node_order[[layer_type]]
      )
    },

    # Determine node order for a layer: custom order or barycenter optimization.
    # Custom order (via sankey_order()) takes priority. When no custom order is
    # specified, uses activity-weighted barycenter heuristic to minimize link crossings:
    #   - First layer (senders): ordered by total activity (descending)
    #   - Subsequent layers: ordered by weighted mean rank of connected previous-layer
    #     nodes, where weights are the activity values of each connection
    .order_layer = function(layer_type, nodes, prev_nodes, data) {
      # Fixed mode retains the original exact custom-order behavior.
      if (identical(private$.order_mode, "fixed") &&
          !is.null(private$.node_order) &&
          !is.null(private$.node_order[[layer_type]])) {
        return(private$.get_ordered_nodes(layer_type, nodes))
      }

      if (length(nodes) <= 1) return(nodes)

      activity_col <- private$.activity_col
      has_activity <- !is.null(activity_col) && activity_col %in% colnames(data)
      activity_values <- if (has_activity) {
        private$.activity_row_values(data)
      } else {
        rep(1, nrow(data))
      }

      # First layer (senders): order by total activity (descending),
      # or alphabetically when no activity column is available
      if (is.null(prev_nodes)) {
        if (has_activity) {
          sender_col <- private$.sender_col
          total_act <- vapply(nodes, function(n) {
            vals <- activity_values[data[[sender_col]] == n]
            sum(vals, na.rm = TRUE)
          }, numeric(1))
          candidate <- nodes[order(-total_act, seq_along(nodes))]
          return(private$.apply_layer_constraints(layer_type, candidate))
        }
        return(private$.apply_layer_constraints(layer_type, sort(nodes)))
      }

      # Subsequent layers: activity-weighted barycenter of connected
      # previous-layer nodes. Connections with higher activity pull the
      # node closer to those positions than low-activity connections.
      prev_rank <- setNames(seq_along(prev_nodes), prev_nodes)

      if (private$.has_receptor) {
        col_map <- list(
          ligands = c(curr = private$.ligand_col, prev = private$.sender_col),
          receptors = c(curr = private$.receptor_col, prev = private$.ligand_col),
          receivers = c(curr = private$.receiver_col, prev = private$.receptor_col)
        )
      } else {
        col_map <- list(
          ligands = c(curr = private$.ligand_col, prev = private$.sender_col),
          receivers = c(curr = private$.receiver_col, prev = private$.ligand_col)
        )
      }
      cols <- col_map[[layer_type]]

      barycenters <- vapply(nodes, function(n) {
        mask <- data[[cols["curr"]]] == n
        prev_vals <- data[[cols["prev"]]][mask]
        prev_vals <- prev_vals[prev_vals %in% names(prev_rank)]
        if (length(prev_vals) == 0) return(length(prev_nodes) / 2)

        ranks <- prev_rank[prev_vals]

        if (has_activity) {
          weights <- activity_values[mask]
          # Match weights to the filtered prev_vals
          weights <- weights[data[[cols["prev"]]][mask] %in% names(prev_rank)]
          weights[is.na(weights)] <- 1
          weights <- pmax(weights, 1e-6)
          return(sum(ranks * weights) / sum(weights))
        }

        mean(ranks)
      }, numeric(1))

      candidate <- nodes[order(barycenters, seq_along(nodes))]
      private$.apply_layer_constraints(layer_type, candidate)
    },

    # Refine node order using barycenter with an arbitrary reference layer.
    # Used for reverse passes (right-to-left) and additional forward passes.
    # Takes explicit column names so it works in either direction.
    .refine_order = function(layer_type, nodes, ref_nodes, data, curr_col, ref_col) {
      # Don't change custom-ordered layers in backward-compatible fixed mode.
      if (identical(private$.order_mode, "fixed") &&
          !is.null(private$.node_order) &&
          !is.null(private$.node_order[[layer_type]])) {
        return(nodes)
      }
      if (length(nodes) <= 1) return(nodes)

      ref_rank <- setNames(seq_along(ref_nodes), ref_nodes)
      activity_col <- private$.activity_col
      has_activity <- !is.null(activity_col) && activity_col %in% colnames(data)
      activity_values <- if (has_activity) {
        private$.activity_row_values(data)
      } else {
        rep(1, nrow(data))
      }

      barycenters <- vapply(nodes, function(n) {
        mask <- data[[curr_col]] == n
        ref_vals <- data[[ref_col]][mask]
        ref_vals <- ref_vals[ref_vals %in% names(ref_rank)]
        if (length(ref_vals) == 0) return(length(ref_nodes) / 2)

        ranks <- ref_rank[ref_vals]

        if (has_activity) {
          weights <- activity_values[mask]
          weights <- weights[data[[ref_col]][mask] %in% names(ref_rank)]
          weights[is.na(weights)] <- 1
          weights <- pmax(weights, 1e-6)
          return(sum(ranks * weights) / sum(weights))
        }

        mean(ranks)
      }, numeric(1))

      candidate <- nodes[order(barycenters, seq_along(nodes))]
      private$.apply_layer_constraints(layer_type, candidate)
    },

    # Repeat forward/reverse barycenter sweeps, retain the candidate with the
    # smallest activity-weighted adjacent-layer crossing score, and apply the
    # user's relative-order constraints after every layer update.
    .optimize_constrained_order = function(orders, data) {
      best <- orders
      best_score <- private$.crossing_score(best, data)

      for (iteration in seq_len(private$.order_iterations)) {
        if (private$.has_receptor) {
          orders$receptors <- private$.refine_order(
            "receptors", orders$receptors, orders$receivers, data,
            private$.receptor_col, private$.receiver_col
          )
          orders$ligands <- private$.refine_order(
            "ligands", orders$ligands, orders$receptors, data,
            private$.ligand_col, private$.receptor_col
          )
          orders$senders <- private$.refine_order(
            "senders", orders$senders, orders$ligands, data,
            private$.sender_col, private$.ligand_col
          )

          orders$ligands <- private$.refine_order(
            "ligands", orders$ligands, orders$senders, data,
            private$.ligand_col, private$.sender_col
          )
          orders$receptors <- private$.refine_order(
            "receptors", orders$receptors, orders$ligands, data,
            private$.receptor_col, private$.ligand_col
          )
          orders$receivers <- private$.refine_order(
            "receivers", orders$receivers, orders$receptors, data,
            private$.receiver_col, private$.receptor_col
          )
        } else {
          orders$ligands <- private$.refine_order(
            "ligands", orders$ligands, orders$receivers, data,
            private$.ligand_col, private$.receiver_col
          )
          orders$senders <- private$.refine_order(
            "senders", orders$senders, orders$ligands, data,
            private$.sender_col, private$.ligand_col
          )
          orders$ligands <- private$.refine_order(
            "ligands", orders$ligands, orders$senders, data,
            private$.ligand_col, private$.sender_col
          )
          orders$receivers <- private$.refine_order(
            "receivers", orders$receivers, orders$ligands, data,
            private$.receiver_col, private$.ligand_col
          )
        }

        score <- private$.crossing_score(orders, data)
        if (is.finite(score) && score < best_score) {
          best <- orders
          best_score <- score
        }
      }
      best
    },

    # Activity-weighted number of crossings between every pair of adjacent
    # layers. Links sharing a source or target are not counted as crossings.
    .crossing_score = function(orders, data) {
      layer_pairs <- if (private$.has_receptor) {
        list(
          list(left = "senders", right = "ligands",
               left_col = private$.sender_col, right_col = private$.ligand_col),
          list(left = "ligands", right = "receptors",
               left_col = private$.ligand_col, right_col = private$.receptor_col),
          list(left = "receptors", right = "receivers",
               left_col = private$.receptor_col, right_col = private$.receiver_col)
        )
      } else {
        list(
          list(left = "senders", right = "ligands",
               left_col = private$.sender_col, right_col = private$.ligand_col),
          list(left = "ligands", right = "receivers",
               left_col = private$.ligand_col, right_col = private$.receiver_col)
        )
      }

      row_weights <- rep(1, nrow(data))
      if (!is.null(private$.activity_col) &&
          private$.activity_col %in% colnames(data)) {
        row_weights <- private$.activity_row_values(data)
      }

      pair_score <- function(pair) {
        edges <- data.frame(
          left = match(data[[pair$left_col]], orders[[pair$left]]),
          right = match(data[[pair$right_col]], orders[[pair$right]]),
          weight = row_weights
        )
        edges <- edges[stats::complete.cases(edges), , drop = FALSE]
        if (nrow(edges) <= 1) return(0)
        edges <- stats::aggregate(weight ~ left + right, edges, sum)
        edges <- edges[order(edges$left, edges$right), , drop = FALSE]
        if (nrow(edges) <= 1) return(0)

        score <- 0
        for (i in seq_len(nrow(edges) - 1L)) {
          later <- seq.int(i + 1L, nrow(edges))
          crossing <- edges$left[later] > edges$left[[i]] &
            edges$right[later] < edges$right[[i]]
          if (any(crossing)) {
            score <- score + edges$weight[[i]] *
              sum(edges$weight[later][crossing])
          }
        }
        score
      }

      sum(vapply(layer_pairs, pair_score, numeric(1)))
    },

    # Estimate the node heights Plotly will render before applying explicit
    # coordinates. Plotly's bundled d3-sankey implementation caps node padding
    # at two-thirds of the available height divided by the largest layer's gap
    # count, then uses one global value-to-pixel scale across all layers.
    .estimate_rendered_heights = function(layer_info, drawable_height) {
      layer_counts <- vapply(layer_info, function(info) info$n, integer(1))
      max_count <- max(layer_counts, 0L)
      effective_pad <- private$.node_padding
      if (max_count > 1L) {
        effective_pad <- min(
          effective_pad,
          (2 / 3) * drawable_height / (max_count - 1L)
        )
      }

      scale_candidates <- vapply(layer_info, function(info) {
        total_flow <- sum(info$render_flows)
        if (info$n == 0L || !is.finite(total_flow) || total_flow <= 0) {
          return(Inf)
        }
        available <- drawable_height - (info$n - 1L) * effective_pad
        max(0, available) / total_flow
      }, numeric(1))
      value_scale <- min(scale_candidates)
      if (!is.finite(value_scale)) value_scale <- 0

      lapply(layer_info, function(info) info$render_flows * value_scale)
    },

    # Reduce only the free space between already ordered nodes. Required gaps
    # account for both rendered node heights and single-line Plotly labels, so
    # compaction cannot introduce a node or label collision. No sorting occurs.
    .pack_layer_centers = function(centers, heights_px, requested_span,
                                   drawable_height) {
      n <- length(centers)
      if (n <= 1L) return(rep(0.5, n))
      if (requested_span >= 1) return(centers)

      center_px <- centers * drawable_height
      top_px <- center_px - heights_px / 2
      bottom_px <- center_px + heights_px / 2
      baseline_gaps <- top_px[-1L] - bottom_px[-n]
      baseline_span <- max(bottom_px) - min(top_px)

      # Arial/Helvetica's rendered line box is approximately 1.12 em. Use 1.2
      # em here to remain conservative across font substitutions.
      label_height <- 1.2 * private$.label_font_size
      minimum_gap <- private$.outer_minimum_gap
      required_gaps <- pmax(
        minimum_gap,
        label_height + minimum_gap -
          (heights_px[-n] + heights_px[-1L]) / 2
      )
      minimum_span <- sum(heights_px) + sum(required_gaps)
      target_span <- max(baseline_span * requested_span, minimum_span)

      excess <- pmax(baseline_gaps - required_gaps, 0)
      extra_budget <- max(0, target_span - minimum_span)
      if (sum(excess) > 0) {
        packed_gaps <- required_gaps + excess * extra_budget / sum(excess)
      } else {
        packed_gaps <- required_gaps
      }

      packed_span <- sum(heights_px) + sum(packed_gaps)
      current_top <- (drawable_height - packed_span) / 2
      packed_centers <- numeric(n)
      for (i in seq_len(n)) {
        packed_centers[[i]] <- current_top + heights_px[[i]] / 2
        if (i < n) {
          current_top <- current_top + heights_px[[i]] + packed_gaps[[i]]
        }
      }
      packed_centers / drawable_height
    },

    # Calculate flow-proportional Y positions for each layer. The baseline path
    # is unchanged at vertical_span = 1. Outer-layer packing runs only after all
    # node orders have been finalized and never changes ligand or receptor
    # coordinates.
    .calculate_flow_y = function(layer_nodes_list, links, node_idx) {
      layer_info <- lapply(layer_nodes_list, function(layer_nodes) {
        n <- length(layer_nodes)
        if (n == 0L) {
          return(list(n = 0L, render_flows = numeric(0), centers = numeric(0)))
        }

        # Total flow through each node (max of incoming, outgoing). These raw
        # values reproduce the heights Plotly assigns to node rectangles.
        render_flows <- vapply(seq_along(layer_nodes), function(i) {
          idx <- node_idx[[layer_nodes[[i]]]]
          incoming <- sum(links$value[links$target == idx])
          outgoing <- sum(links$value[links$source == idx])
          max(incoming, outgoing)
        }, numeric(1))

        if (n == 1L) {
          return(list(n = n, render_flows = render_flows, centers = 0.5))
        }

        # Retain the historical 5%-of-maximum floor for baseline coordinate
        # allocation. It affects positions only, not node or link widths.
        coordinate_flows <- render_flows
        max_flow <- max(coordinate_flows)
        if (max_flow == 0) max_flow <- 1
        coordinate_flows <- pmax(coordinate_flows, max_flow * 0.05)
        fractions <- coordinate_flows / sum(coordinate_flows)

        margin <- 0.01
        available <- 1.0 - 2 * margin
        pad_per_gap <- available * 0.03
        total_pad <- pad_per_gap * (n - 1L)
        content <- available - total_pad
        if (content < available * 0.5) {
          pad_per_gap <- (available * 0.5) / max(n - 1L, 1L)
          total_pad <- pad_per_gap * (n - 1L)
          content <- available - total_pad
        }

        coordinate_heights <- fractions * content
        centers <- numeric(n)
        current_y <- margin
        for (i in seq_len(n)) {
          centers[[i]] <- current_y + coordinate_heights[[i]] / 2
          current_y <- current_y + coordinate_heights[[i]] + pad_per_gap
        }
        list(n = n, render_flows = render_flows, centers = centers)
      })

      if (!is.null(private$.outer_vertical_span)) {
        if (!identical(private$.orientation, "h")) {
          stop(
            "vertical_span is currently supported only for horizontal Sankey layouts",
            call. = FALSE
          )
        }
        drawable_height <- private$.height - private$.margin$t - private$.margin$b
        if (!is.finite(drawable_height) || drawable_height <= 0) {
          stop("Plot height and margins leave no drawable vertical space", call. = FALSE)
        }
        rendered_heights <- private$.estimate_rendered_heights(
          layer_info, drawable_height
        )

        for (layer_name in intersect(
          c("senders", "receivers"), names(layer_info)
        )) {
          requested_span <- private$.outer_vertical_span[[layer_name]]
          layer_info[[layer_name]]$centers <- private$.pack_layer_centers(
            layer_info[[layer_name]]$centers,
            rendered_heights[[layer_name]],
            requested_span,
            drawable_height
          )
        }
      }

      unname(unlist(lapply(layer_info, function(info) info$centers)))
    },

    # Sort links by the full flow path (sender_y, receiver_y) so that at
    # every intermediate node (ligands, receptors), incoming and outgoing
    # links are stacked in the same order. Without this, a flow entering
    # near the top of a node can exit near the bottom, creating visual
    # crossings inside the node bar.
    .sort_links = function(links, node_y, data, node_idx) {
      n <- length(links$source)
      if (n <= 1) return(links)

      sender_col <- private$.sender_col
      receiver_col <- private$.receiver_col

      # Look up sender_y and receiver_y for each link's originating data row
      sender_y <- node_y[node_idx[paste0("S:", data[[sender_col]][links$row_id])] + 1]
      receiver_y <- node_y[node_idx[paste0("T:", data[[receiver_col]][links$row_id])] + 1]

      ord <- order(sender_y, receiver_y)

      list(
        source = links$source[ord],
        target = links$target[ord],
        value = links$value[ord],
        colors = links$colors[ord],
        row_id = links$row_id[ord]
      )
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
    # Uniform mode: each input row contributes one equal-width path. Intermediate
    # links split flow equally at shared nodes so topology is visible without
    # using activity or expression values.
    .build_links_uniform = function(data, node_idx, senders, ligands, receptors, receivers) {
      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      receiver_col <- private$.receiver_col
      has_receptor <- private$.has_receptor

      # --- Step 1: Compute Edge 1 (Sender -> Ligand) values ---
      edge1_values <- rep(1, nrow(data))

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

      if (has_receptor) {
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
      }

      # --- Step 6: Build all edges ---
      sources <- c()
      targets <- c()
      values <- c()
      colors <- c()
      row_ids <- c()

      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        sender <- row[[sender_col]]
        ligand <- row[[ligand_col]]
        receiver <- row[[receiver_col]]
        receptor <- if (has_receptor) row[[receptor_col]] else NULL

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

        if (has_receptor) {
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

          row_ids <- c(row_ids, i, i, i)
        } else {
          # Edge 2: Ligand -> Receiver (equal split, skip receptor)
          sources <- c(sources, node_idx[[paste0("L:", ligand)]])
          targets <- c(targets, node_idx[[paste0("T:", receiver)]])
          values <- c(values, edge2_values[i])
          colors <- c(colors, rgba_color)

          row_ids <- c(row_ids, i, i)
        }
      }

      list(
        source = sources,
        target = targets,
        value = values,
        colors = colors,
        row_id = row_ids
      )
    },

    # Build links with direct activity-based, flow-conserving weights.
    # Each score-bearing unit contributes its activity exactly once to the
    # sender-to-ligand layer. Ligand expression is deliberately absent from
    # edge-width calculations. Receptor-side weights redistribute inflow at
    # intermediate nodes without changing their total flow.
    .build_links_flow_conserving = function(data, node_idx, senders, ligands, receptors, receivers) {
      if (!private$.has_receptor) {
        stop("flow_conserving requires receptor column", call. = FALSE)
      }

      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      receiver_col <- private$.receiver_col
      receptor_expr_col <- private$.receptor_expr_col

      # Sender -> ligand values are the direct activity contribution represented
      # by each row. Shared activity units divide one score across their repeated
      # rendering rows; path-scoped rows retain their individual scores.
      edge1_values <- private$.activity_row_values(data)

      receptor_weights <- if (!is.null(receptor_expr_col) &&
                              receptor_expr_col %in% colnames(data)) {
        suppressWarnings(as.numeric(data[[receptor_expr_col]]))
      } else {
        rep(1, nrow(data))
      }
      receptor_weights[
        !is.finite(receptor_weights) | receptor_weights < 0
      ] <- 0

      # Redistribute the total incoming flow for each intermediate node across
      # its outgoing rendering rows. When all selected weights are zero or
      # missing, use an equal split so conservation still holds exactly.
      distribute_inflow <- function(incoming, groups, downstream_weights) {
        values <- numeric(length(incoming))
        group_keys <- as.character(groups)
        missing_group <- is.na(group_keys)
        group_keys[missing_group] <- paste0(
          "missing:", which(missing_group)
        )
        rows_by_group <- split(seq_along(group_keys), group_keys)

        for (rows in rows_by_group) {
          total_in <- sum(incoming[rows])
          weights <- downstream_weights[rows]
          weight_sum <- sum(weights)

          if (!is.finite(total_in) || total_in <= 0) {
            values[rows] <- 0
          } else if (is.finite(weight_sum) && weight_sum > 0) {
            values[rows] <- total_in * weights / weight_sum
          } else {
            values[rows] <- total_in / length(rows)
          }
        }

        values
      }

      edge2_values <- distribute_inflow(
        edge1_values,
        data[[ligand_col]],
        receptor_weights
      )
      edge3_values <- distribute_inflow(
        edge2_values,
        data[[receptor_col]],
        receptor_weights
      )

      row_ids <- integer(0)
      sources <- integer(0)
      targets <- integer(0)
      values <- numeric(0)
      colors <- character(0)

      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        sender <- row[[sender_col]]
        ligand <- row[[ligand_col]]
        receptor <- row[[receptor_col]]
        receiver <- row[[receiver_col]]

        flow_color <- private$.get_flow_color(row, i)
        alpha <- private$.get_flow_alpha(sender, ligand, receptor, receiver)
        rgba_color <- hex_with_alpha(flow_color, alpha)

        sources <- c(
          sources,
          node_idx[[paste0("S:", sender)]],
          node_idx[[paste0("L:", ligand)]],
          node_idx[[paste0("R:", receptor)]]
        )
        targets <- c(
          targets,
          node_idx[[paste0("L:", ligand)]],
          node_idx[[paste0("R:", receptor)]],
          node_idx[[paste0("T:", receiver)]]
        )
        values <- c(
          values,
          edge1_values[[i]],
          edge2_values[[i]],
          edge3_values[[i]]
        )
        colors <- c(colors, rgba_color, rgba_color, rgba_color)
        row_ids <- c(row_ids, i, i, i)
      }

      list(
        source = sources,
        target = targets,
        value = values,
        colors = colors,
        row_id = row_ids
      )
    },

    # Get color for a flow based on coloring strategy
    .get_flow_color = function(row, idx) {
      color_by <- private$.color_by

      if (color_by == "fixed") {
        return(private$.fixed_flow_color)
      }

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
      if (!is.null(highlight$receptors) && private$.has_receptor &&
          !(receptor %in% highlight$receptors)) {
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

    # Apply highlighting to node colors and labels
    # Nodes connected to at least one highlighted flow stay opaque with visible labels;
    # all other nodes get dim_alpha and their labels are hidden (shown on hover)
    .apply_node_highlight = function(node_colors, node_labels, data) {
      highlight <- private$.highlight_pathway
      if (is.null(highlight)) {
        return(list(node_colors = node_colors, display_labels = NULL))
      }

      sender_col <- private$.sender_col
      ligand_col <- private$.ligand_col
      receptor_col <- private$.receptor_col
      receiver_col <- private$.receiver_col
      has_receptor <- private$.has_receptor
      dim_alpha <- private$.highlight_alpha

      # Determine which rows (flows) match the highlight criteria
      highlighted_senders <- character(0)
      highlighted_ligands <- character(0)
      highlighted_receptors <- character(0)
      highlighted_receivers <- character(0)

      for (i in seq_len(nrow(data))) {
        sender <- data[[sender_col]][i]
        ligand <- data[[ligand_col]][i]
        receptor <- if (has_receptor) data[[receptor_col]][i] else NULL
        receiver <- data[[receiver_col]][i]

        matches <- TRUE
        if (!is.null(highlight$senders) && !(sender %in% highlight$senders)) matches <- FALSE
        if (!is.null(highlight$ligands) && !(ligand %in% highlight$ligands)) matches <- FALSE
        if (!is.null(highlight$receptors) && has_receptor &&
            !(receptor %in% highlight$receptors)) matches <- FALSE
        if (!is.null(highlight$receivers) && !(receiver %in% highlight$receivers)) matches <- FALSE

        if (matches) {
          highlighted_senders <- c(highlighted_senders, sender)
          highlighted_ligands <- c(highlighted_ligands, ligand)
          if (has_receptor) highlighted_receptors <- c(highlighted_receptors, receptor)
          highlighted_receivers <- c(highlighted_receivers, receiver)
        }
      }

      highlighted_senders <- unique(highlighted_senders)
      highlighted_ligands <- unique(highlighted_ligands)
      highlighted_receptors <- unique(highlighted_receptors)
      highlighted_receivers <- unique(highlighted_receivers)

      # Build display labels: highlighted nodes keep their label,
      # non-highlighted nodes get empty string (name still visible on hover)
      display_labels <- character(length(node_labels))
      for (i in seq_along(node_labels)) {
        label <- node_labels[i]
        name <- gsub("^[SLRT]:", "", label)
        is_highlighted <- FALSE

        if (grepl("^S:", label) && name %in% highlighted_senders) is_highlighted <- TRUE
        if (grepl("^L:", label) && name %in% highlighted_ligands) is_highlighted <- TRUE
        if (grepl("^R:", label) && name %in% highlighted_receptors) is_highlighted <- TRUE
        if (grepl("^T:", label) && name %in% highlighted_receivers) is_highlighted <- TRUE

        display_labels[i] <- name
        if (!is_highlighted) {
          # Receiver nodes keep their colors even when not highlighted
          if (!grepl("^T:", label)) {
            node_colors[i] <- hex_with_alpha(node_colors[i], dim_alpha)
          }
        }
      }

      list(node_colors = node_colors, display_labels = display_labels)
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
            # Default (cell_type): use the dominant incoming link color
            # (the color with the highest total flow weight)
            node_key <- i - 1
            incoming_idx <- which(links$target == node_key)
            if (length(incoming_idx) > 0) {
              node_colors[i] <- private$.dominant_link_color(
                links$colors[incoming_idx], links$value[incoming_idx])
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
            # Default (cell_type): use the dominant incoming link color
            node_key <- i - 1
            incoming_idx <- which(links$target == node_key)
            if (length(incoming_idx) > 0) {
              node_colors[i] <- private$.dominant_link_color(
                links$colors[incoming_idx], links$value[incoming_idx])
            }
          }
          next
        }
      }

      node_colors
    },

    # Get the dominant (highest-weight) link color as a solid hex color.
    # Used for cell_type mode: ligand/receptor nodes show the color of
    # the strongest incoming connection rather than a blended mix.
    .dominant_link_color = function(link_colors, link_weights) {
      hex_colors <- private$.extract_hex_from_rgba(link_colors)
      # Aggregate weights per unique color
      unique_colors <- unique(hex_colors)
      total_weights <- vapply(unique_colors, function(col) {
        sum(link_weights[hex_colors == col])
      }, numeric(1))
      unique_colors[which.max(total_weights)]
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

      # Skip receptor colors if no receptor layer
      if (!private$.has_receptor) {
        return(invisible(NULL))
      }

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

      has_receptor <- private$.has_receptor
      links_per_row <- if (has_receptor) 3L else 2L

      # Get unique values for each category
      senders <- unique(data[[sender_col]])
      ligands <- unique(data[[ligand_col]])
      receptors <- if (has_receptor) unique(data[[receptor_col]]) else character(0)
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

          # Get indices of matching rows
          row_indices <- which(mask)

          # Calculate link indices for matching rows
          matching_link_indices <- unlist(lapply(row_indices, function(i) {
            seq((i - 1) * links_per_row + 1, (i - 1) * links_per_row + links_per_row)
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

      if (has_receptor) {
        # 4 dropdowns: sender, ligand, receptor, receiver
        dropdown_x <- c(0.0, 0.25, 0.5, 0.75)
        dropdown_labels <- c("Sender:", "Ligand:", "Receptor:", "Receiver:")
        dropdown_buttons <- list(
          sender_buttons,
          create_highlight_buttons(ligands, "ligand"),
          create_highlight_buttons(receptors, "receptor"),
          create_highlight_buttons(receivers, "receiver")
        )
      } else {
        # 3 dropdowns: sender, ligand, receiver
        dropdown_x <- c(0.0, 0.33, 0.66)
        dropdown_labels <- c("Sender:", "Ligand:", "Receiver:")
        dropdown_buttons <- list(
          sender_buttons,
          create_highlight_buttons(ligands, "ligand"),
          create_highlight_buttons(receivers, "receiver")
        )
      }

      updatemenus <- lapply(seq_along(dropdown_x), function(j) {
        list(
          active = 0,
          buttons = dropdown_buttons[[j]],
          direction = "down",
          showactive = TRUE,
          x = dropdown_x[j],
          xanchor = "left",
          y = 1.18,
          yanchor = "top",
          pad = list(r = 10, t = 10),
          bgcolor = "white",
          bordercolor = "#cccccc",
          font = list(size = 11)
        )
      })

      # Add annotations for dropdown labels
      annotations <- lapply(seq_along(dropdown_x), function(j) {
        list(
          text = dropdown_labels[j],
          x = dropdown_x[j],
          y = 1.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 12, color = "#333333")
        )
      })

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
#' @param color_by Coloring strategy: "sender", "receiver", "ligand", "condition",
#'   or any valid color string (e.g., "steelblue", "#3498db") to use a single
#'   fixed color for all flows.
#' @param title Plot title
#' @param width Plot width
#' @param height Plot height
#' @param alpha Link transparency 0-1 (default: 0.65)
#' @param label_size Font size for node labels (default: 16)
#' @param label_position Position of labels (NOTE: not supported by Plotly Sankey - has no effect)
#' @param flow_method Edge weight computation method (default: "auto"):
#'   \itemize{
#'     \item "auto": Uses weighted flow when required columns are present;
#'       otherwise uses uniform edge widths without warning.
#'     \item "weighted"/"flow_conserving": Uses scaled_activity directly for
#'       sender-to-ligand width and scaled_receptor_expression to redistribute
#'       downstream flow. Ligand expression does not multiply edge widths.
#'     \item "uniform": Uses equal edge widths for all communication paths.
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
#' @param receptor_col Name of the receptor column. Set to NULL to omit
#'   the receptor layer (3-layer plot: sender -> ligand -> receiver).
#'   Default: "receptor".
#' @param activity_unit_col Optional column identifying rows that repeat one
#'   score-bearing communication unit. Shared rows divide one activity score;
#'   with `NULL`, rows are independent path-level activity units.
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
#' # With a smaller label font
#' sankey_flow(ex$data, ex$colors,
#'             label_size = 14)
#'
#' # With interactive filters
#' sankey_flow(ex$data, ex$colors,
#'             interactive = TRUE)
#'
#' # Use equal-width paths instead of weighted flow
#' sankey_flow(ex$data, ex$colors,
#'             flow_method = "uniform")
#'
#' # Explicitly request weighted edge widths when weight columns are present
#' sankey_flow(ex$data, ex$colors,
#'             flow_method = "weighted")
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
                        label_position = NULL,
                        flow_method = c("auto", "weighted", "flow_conserving", "uniform"),
                        node_color_mode = c("cell_type", "weighted_mix"),
                        interactive = FALSE,
                        receptor_col = "receptor",
                        activity_unit_col = NULL,
                        ...) {

  flow_explicit <- !missing(flow_method)
  flow_method <- match.arg(flow_method)
  node_color_mode <- match.arg(node_color_mode)

  args <- list(data = data, cell_colors = cell_colors, width = width,
               height = height, receptor_col = receptor_col,
               activity_unit_col = activity_unit_col)
  if (flow_explicit) args$flow_method <- flow_method
  extra <- list(...)
  if (length(extra) > 0) args <- c(args, extra)
  plot <- do.call(SankeyPlot$new, args)

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
