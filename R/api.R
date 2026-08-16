#' @title ggplot-like API for ccflowR
#' @description
#' User-friendly functional API for creating Sankey plots, following ggplot2 patterns.
#' This provides a simpler interface compared to the R6 class methods.
#'
#' @name ccflowR-api
#' @importFrom grDevices colorRampPalette
NULL

# =============================================================================
# S3 Class Definition: sankey
# =============================================================================

#' Create a Sankey Plot for Cell-Cell Communication
#'
#' @description
#' Main constructor function for creating Sankey diagrams. This is the recommended
#' way to create plots in ccflowR, providing a ggplot2-like experience with the
#' `+` operator for adding layers.
#'
#' @param data A data frame containing cell-cell communication data with columns:
#'   \describe{
#'     \item{sender}{Character. Sending cell type name.}
#'     \item{receiver}{Character. Receiving cell type name.}
#'     \item{ligand}{Character. Ligand gene symbol.}
#'     \item{receptor}{Character. (Optional) Receptor gene symbol. If absent and
#'       `receptor_col = NULL`, a 3-layer plot (sender -> ligand -> receiver) is created.}
#'     \item{scaled_activity}{Numeric. Activity score used directly for
#'       sender-to-ligand width when weighted flow is active.}
#'   }
#' @param colors Named character vector of colors for cell types. Names should match
#'   cell type names in the data. If NULL, colors are auto-generated.
#'   Example: `c(Macrophage = "#E41A1C", T_cell = "#377EB8")`
#' @param group_colors Named character vector of colors for experimental groups.
#'   Only needed when using `sankey_colors(by = "group")`.
#' @param group_col Name of the column containing group/condition labels.
#'   Required for group-based coloring.
#' @param receptor_col Name of the receptor column in the data. Set to NULL to omit
#'   the receptor layer, creating a 3-layer Sankey plot (sender -> ligand -> receiver).
#'   Default: "receptor".
#' @param activity_unit_col Optional column identifying rendering rows that
#'   repeat one score-bearing activity unit. Shared rows divide that activity
#'   so it contributes exactly once. With `NULL`, every row is an independent
#'   path-level activity unit.
#' @param flow Character. Edge weight computation method:
#'   \describe{
#'     \item{"auto"}{(Default) Uses weighted edge widths when the required
#'       weight columns are present; otherwise uses equal-width edges without warning.}
#'     \item{"weighted"}{Use `scaled_activity` directly for sender-to-ligand
#'       width and `scaled_receptor_expression` to redistribute downstream
#'       flow. `scaled_ligand_expression` is optional metadata and does not
#'       multiply edge widths. Equivalent to `"flow_conserving"`.}
#'     \item{"flow_conserving"}{Backward-compatible name for weighted flow.}
#'     \item{"uniform"}{Equal edge widths for all communication paths.}
#'   }
#' @param ... Additional arguments passed to the internal SankeyPlot constructor.
#'
#' @return A `sankey` object that can be customized with `+` and layer functions.
#'
#' @export
#'
#' @examples
#' # Basic usage with example data
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors)
#' p
#'
#' # Using a simple named vector for colors
#' colors <- c(Macrophage = "#E41A1C", T_cell = "#377EB8", B_cell = "#4DAF4A")
#' p <- sankey_plot(ex$data, colors)
#'
#' # Full customization with + operator
#' p <- sankey_plot(ex$data, ex$colors) +
#'   sankey_colors(by = "sender") +
#'   sankey_alpha(0.7) +
#'   sankey_font(family = "Helvetica", size = 14) +
#'   sankey_title("Cell Communication Network")
#'
#' # With highlighting
#' p <- sankey_plot(ex$data, ex$colors) +
#'   sankey_highlight(senders = "Macrophage", dim_alpha = 0.1)
#'
#' @seealso
#' \code{\link{sankey_colors}} for coloring options,
#' \code{\link{sankey_highlight}} for pathway highlighting,
#' \code{\link{sankey_save}} for exporting figures
sankey_plot <- function(data,
                        colors = NULL,
                        group_colors = NULL,
                        group_col = NULL,
                        receptor_col = "receptor",
                        activity_unit_col = NULL,
                        flow = c("auto", "weighted", "flow_conserving", "uniform"),
                        ...) {

  flow_explicit <- !missing(flow)
  flow <- match.arg(flow)

  # Create internal R6 object - only pass flow_method explicitly when user specified it
  args <- list(
    data = data,
    cell_colors = colors,
    group_colors = group_colors,
    group_col = group_col,
    receptor_col = receptor_col,
    activity_unit_col = activity_unit_col,
    ...
  )
  if (flow_explicit) {
    args$flow_method <- flow
  }
  internal <- do.call(SankeyPlot$new, args)

  # Wrap in S3 class
  structure(
    list(
      .internal = internal,
      .rendered = FALSE
    ),
    class = "sankey"
  )
}


# =============================================================================
# S3 Methods for sankey class
# =============================================================================

#' Print a Sankey Plot
#'
#' @description
#' Display a sankey plot. In interactive sessions, this renders and shows the plot.
#'
#' @param x A sankey object created by `sankey_plot()`
#' @param ... Additional arguments (ignored)
#'
#' @return The sankey object invisibly
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors)
#' print(p)  # or just: p
print.sankey <- function(x, ...) {
  fig <- x$.internal$render()

  # Check if we're in Jupyter/IRkernel
  if ("IRkernel" %in% loadedNamespaces() &&
      requireNamespace("IRdisplay", quietly = TRUE)) {
    # Use IRdisplay to show the htmlwidget
    IRdisplay::display(fig)
    return(invisible(x))
  }

  # For console/RStudio, use standard htmlwidget print
  print(fig)
  invisible(x)
}


#' Knit Print Method for Sankey Plots
#'
#' @description
#' Custom knit_print method for rendering sankey plots in knitr/RMarkdown contexts.
#'
#' @param x A sankey object
#' @param ... Additional arguments passed to knit_print
#'
#' @return A knit_asis object for rendering
#' @export
#' @importFrom knitr knit_print
#' @method knit_print sankey
knit_print.sankey <- function(x, ...) {
  fig <- x$.internal$render()
  knitr::knit_print(fig, ...)
}


#' HTML Representation for Jupyter Notebooks
#'
#' @description
#' Generate HTML representation for display in Jupyter notebooks via IRkernel.
#' This method is called by the repr package when displaying objects in Jupyter.
#'
#' DO NOT CHANGE the roxygen tags below. The `repr` package is optional (only
#' present in Jupyter/IRkernel). Using `@exportS3Method repr::repr_html`
#' generates `S3method(repr::repr_html,sankey)` in NAMESPACE, which tells R
#' to register this method ONLY when repr is available. Without the `repr::`
#' prefix, R tries to find `repr_html` at load time and fails with:
#'   "object 'repr_html' not found whilst loading namespace 'ccflowR'"
#'
#' @param obj A sankey object
#' @param ... Additional arguments (ignored)
#'
#' @return HTML string for display
#' @exportS3Method repr::repr_html
repr_html.sankey <- function(obj, ...) {
  fig <- obj$.internal$render()

  # Delegate to plotly/htmlwidget's repr_html which works correctly
  # in all Jupyter environments (VS Code, JupyterLab, classic Notebook)
  if (requireNamespace("repr", quietly = TRUE)) {
    repr::repr_html(fig, ...)
  } else {
    NULL
  }
}


#' Summarize a Sankey Plot
#'
#' @description
#' Get a summary of the sankey plot configuration and data.
#'
#' @param object A sankey object created by `sankey_plot()`
#' @param ... Additional arguments (ignored)
#'
#' @return A list with configuration summary
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors)
#' summary(p)
summary.sankey <- function(object, ...) {
  object$.internal$summary()
}


#' Add Layers to a Sankey Plot
#'
#' @description
#' Combine a sankey plot with layer modifications using the `+` operator,
#' similar to ggplot2.
#'
#' @param e1 A sankey object
#' @param e2 A sankey_layer object (from functions like `sankey_colors()`, `sankey_alpha()`, etc.)
#'
#' @return The modified sankey object
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors) +
#'   sankey_colors(by = "sender") +
#'   sankey_alpha(0.7)
`+.sankey` <- function(e1, e2) {
  if (!inherits(e2, "sankey_layer")) {
    stop("Can only add sankey_layer objects to a sankey plot.\n",
         "Use functions like sankey_colors(), sankey_alpha(), sankey_font(), etc.",
         call. = FALSE)
  }

  # Apply the layer to the internal R6 object
  apply_layer(e1, e2)
}


# =============================================================================
# Layer Functions
# =============================================================================

#' Set Coloring Strategy
#'
#' @description
#' Control how flows are colored in the Sankey diagram.
#'
#' @param by Coloring strategy. One of:
#'   \describe{
#'     \item{"sender"}{(Default) Color by sending cell type. Shows signal origin.}
#'     \item{"receiver"}{Color by receiving cell type. Shows signal destination.}
#'     \item{"ligand"}{Color by ligand molecule. Shows signaling pathways.}
#'     \item{"condition"}{Color by experimental condition/group. Uses group colors when
#'       `group_col` is set in `sankey_plot()`; falls back to sender coloring otherwise.}
#'   }
#'   Alternatively, pass any valid color string (e.g., `"steelblue"`, `"#3498db"`)
#'   to use a single fixed color for all flows.
#' @param binary_color Deprecated. Ignored.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Color by sender (default)
#' sankey_plot(ex$data, ex$colors) + sankey_colors(by = "sender")
#'
#' # Color by receiver
#' sankey_plot(ex$data, ex$colors) + sankey_colors(by = "receiver")
#'
#' # Color by condition (requires group_col)
#' mn_ex <- example_multinichenet()
#' sankey_plot(mn_ex$data, mn_ex$colors, group_col = "group") +
#'   sankey_colors(by = "condition")
sankey_colors <- function(by = c("sender", "receiver", "ligand", "condition"),
                          binary_color = NULL) {
  # Backward compatibility: convert "binary" to "condition" with deprecation warning
  if (!is.null(by) && length(by) == 1 && by == "binary") {
    warning('sankey_colors(by = "binary") is deprecated. Use by = "condition" instead.',
            call. = FALSE)
    by <- "condition"
  }
  # Backward compatibility: convert "group" to "condition"
  if (!is.null(by) && length(by) == 1 && by == "group") {
    by <- "condition"
  }
  # Check if by is a named strategy or a fixed color
  strategies <- c("sender", "receiver", "ligand", "condition")
  if (!(length(by) > 1 || by %in% strategies)) {
    # Fixed color string — pass through as-is
  } else {
    by <- match.arg(by)
  }

  if (!is.null(binary_color)) {
    warning("The binary_color parameter is deprecated and ignored.", call. = FALSE)
  }

  structure(
    list(
      type = "colors",
      by = by
    ),
    class = "sankey_layer"
  )
}


#' Set Group Colors
#'
#' @description
#' Set colors for experimental groups when using `sankey_colors(by = "group")`.
#'
#' @param colors Named character vector of colors for groups.
#'   Example: `c(Control = "#ADD8E6", Treatment = "#F08080")`
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' mn_ex <- example_multinichenet()
#' group_colors <- c(Control = "#ADD8E6", Treatment = "#F08080", Disease = "#90EE90")
#'
#' sankey_plot(mn_ex$data, mn_ex$colors, group_col = "group") +
#'   sankey_colors(by = "group") +
#'   sankey_group_colors(group_colors)
sankey_group_colors <- function(colors) {
  structure(
    list(
      type = "group_colors",
      colors = colors
    ),
    class = "sankey_layer"
  )
}


#' Set Flow Transparency
#'
#' @description
#' Control the opacity of flow links in the Sankey diagram.
#'
#' @param value Alpha value between 0 (fully transparent) and 1 (fully opaque).
#'   Default: 0.65. Lower values help see overlapping flows; higher values
#'   create bolder visuals.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # More transparent (see overlapping flows)
#' sankey_plot(ex$data, ex$colors) + sankey_alpha(0.3)
#'
#' # More opaque (bold visuals)
#' sankey_plot(ex$data, ex$colors) + sankey_alpha(0.9)
sankey_alpha <- function(value = 0.65) {
  if (value < 0 || value > 1) {
    stop("alpha must be between 0 and 1", call. = FALSE)
  }
  structure(
    list(
      type = "alpha",
      value = value
    ),
    class = "sankey_layer"
  )
}



#' Highlight Specific Pathways
#'
#' @description
#' Draw attention to specific communication pathways by dimming non-matching flows.
#' Multiple criteria can be combined (AND logic).
#'
#' @param senders Character vector of sender cell types to highlight.
#' @param ligands Character vector of ligands to highlight.
#' @param receptors Character vector of receptors to highlight.
#' @param receivers Character vector of receiver cell types to highlight.
#' @param dim_alpha Alpha value for non-highlighted (dimmed) flows. Default: 0.1.
#'   Lower values create more contrast with highlighted pathways.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Highlight pathways FROM Macrophages
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_highlight(senders = "Macrophage")
#'
#' # Highlight pathways TO T_cells
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_highlight(receivers = "T_cell")
#'
#' # Highlight specific ligands
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_highlight(ligands = c("TGFB1", "IL6"), dim_alpha = 0.05)
#'
#' # Combine criteria (AND): Macrophage signals via TGFB1
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_highlight(senders = "Macrophage", ligands = "TGFB1")
sankey_highlight <- function(senders = NULL,
                             ligands = NULL,
                             receptors = NULL,
                             receivers = NULL,
                             dim_alpha = 0.1) {
  structure(
    list(
      type = "highlight",
      senders = senders,
      ligands = ligands,
      receptors = receptors,
      receivers = receivers,
      dim_alpha = dim_alpha
    ),
    class = "sankey_layer"
  )
}


#' Clear Pathway Highlighting
#'
#' @description
#' Remove any pathway highlighting, restoring all flows to normal opacity.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors) +
#'   sankey_highlight(senders = "Macrophage")
#'
#' # Later, clear the highlighting
#' p <- p + sankey_clear_highlight()
sankey_clear_highlight <- function() {
  structure(
    list(type = "clear_highlight"),
    class = "sankey_layer"
  )
}


#' @rdname sankey_clear_highlight
#' @description
#' \code{sankey_unhighlight} is an alias for \code{sankey_clear_highlight}.
#' @export
sankey_unhighlight <- function() {
  sankey_clear_highlight()
}


#' Configure Node Appearance
#'
#' @description
#' Control the appearance of nodes (the vertical bars) in the Sankey diagram.
#'
#' @param thickness Node thickness in pixels. Default: 20. Range: 5-50.
#'   Thicker nodes are more prominent but take more space.
#' @param padding Vertical padding between nodes in pixels. Default: 30. Range: 10-100.
#'   More padding spreads nodes apart; less padding creates a denser layout.
#' @param arrangement Plotly arrangement mode. The underlying default is
#'   "snap" when no custom order is supplied. Adding `sankey_order()` switches
#'   to fixed coordinates so the constrained order is reproducible.
#' @param vertical_span Requested fraction of the full rendered sender and
#'   receiver spans to retain after constrained ordering. Supply one value for
#'   both outer layers or a named vector with `senders` and/or `receivers`;
#'   an unnamed layer retains its current span. Values must be in `(0, 1]`.
#'   The default is 0.7. Set to 1 to retain the original full-span coordinates.
#'   A requested span is increased when needed to prevent node or label overlap.
#'   Ligand and receptor coordinates are never changed.
#' @param minimum_gap Minimum readable separation in pixels between adjacent
#'   outer-layer node or label boxes after packing. Default: 4.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Thicker nodes with more spacing
#' sankey_plot(ex$data, ex$colors) + sankey_nodes(thickness = 30, padding = 45)
#'
#' # Compact layout
#' sankey_plot(ex$data, ex$colors) + sankey_nodes(thickness = 12, padding = 15)
#'
#' # Enable interactive node dragging
#' sankey_plot(ex$data, ex$colors) + sankey_nodes(arrangement = "snap")
#'
#' # Pack sender and receiver stacks without changing their order
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_order(ligands = c("TGFB1", "IL6")) +
#'   sankey_nodes(vertical_span = c(senders = 0.85, receivers = 0.7))
sankey_nodes <- function(thickness = NULL, padding = NULL, arrangement = NULL,
                         vertical_span = 0.7, minimum_gap = 4) {
  structure(
    list(
      type = "nodes",
      thickness = thickness,
      padding = padding,
      arrangement = arrangement,
      vertical_span = vertical_span,
      minimum_gap = minimum_gap
    ),
    class = "sankey_layer"
  )
}


#' Set Custom Node Ordering
#'
#' @description
#' Add relative vertical-order constraints for nodes within each layer. When
#' this layer is added, constrained mode is the default: ccflowR preserves the
#' named order while using its activity-weighted barycenter heuristic for the
#' remaining layout freedom. If this layer is not added, ccflowR continues to
#' omit explicit positions and lets Plotly arrange the nodes automatically.
#'
#' @param senders Character vector specifying order of sender nodes (top to bottom).
#' @param ligands Character vector specifying order of ligand nodes.
#' @param receptors Character vector specifying order of receptor nodes.
#' @param receivers Character vector specifying order of receiver nodes.
#' @param mode Ordering behavior. `"constrained"` (default) preserves the
#'   relative order of named nodes and optimizes all remaining nodes and layers
#'   to reduce crossings. `"fixed"` preserves the historical exact ordering.
#' @param iterations Positive integer giving the number of forward/reverse
#'   optimization sweeps in constrained mode.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Custom ordering for biological interpretation
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_order(
#'     senders = c("Macrophage", "T_cell", "B_cell"),
#'     receivers = c("Fibroblast", "Endothelial", "T_cell")
#'   )
sankey_order <- function(senders = NULL, ligands = NULL,
                         receptors = NULL, receivers = NULL,
                         mode = c("constrained", "fixed"), iterations = 8L) {
  mode <- match.arg(mode)
  if (length(iterations) != 1L || is.na(iterations) ||
      iterations < 1 || iterations != as.integer(iterations)) {
    stop("iterations must be a positive integer", call. = FALSE)
  }
  structure(
    list(
      type = "order",
      senders = senders,
      ligands = ligands,
      receptors = receptors,
      receivers = receivers,
      mode = mode,
      iterations = as.integer(iterations)
    ),
    class = "sankey_layer"
  )
}


#' Configure Label Appearance
#'
#' @description
#' Control the appearance of node labels.
#'
#' @param size Font size for labels in points. Default: 12. Range: 8-24.
#'
#' @return A sankey_layer object
#'
#' @note Label position cannot be customized due to Plotly Sankey limitations.
#'   Labels are positioned automatically by Plotly based on node arrangement.
#'
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Larger labels for presentations
#' sankey_plot(ex$data, ex$colors) + sankey_labels(size = 16)
#'
#' # Smaller labels for compact figures
#' sankey_plot(ex$data, ex$colors) + sankey_labels(size = 10)
sankey_labels <- function(size = NULL) {
  structure(
    list(
      type = "labels",
      size = size
    ),
    class = "sankey_layer"
  )
}


#' Enable Interactive Filtering
#'
#' @description
#' Add dropdown menus for interactive filtering by sender, ligand, receptor,
#' or receiver. When a filter is selected, matching pathways are highlighted
#' while non-matching pathways are dimmed.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Add interactive filter dropdowns
#' sankey_plot(ex$data, ex$colors) + sankey_interactive()
sankey_interactive <- function() {
  structure(
    list(
      type = "interactive",
      enable = TRUE
    ),
    class = "sankey_layer"
  )
}


#' Set Node Coloring Mode
#'
#' @description
#' Control how ligand and receptor nodes are colored.
#'
#' @param mode Coloring mode. One of:
#'   \describe{
#'     \item{"cell_type"}{(Default) Ligand/receptor nodes colored by incoming link colors.}
#'     \item{"weighted_mix"}{Ligand colors are weighted mix of sender colors by expression.
#'       Receptor colors are weighted mix of ligand colors. Creates visual color flow.
#'       Requires expression columns in data.}
#'   }
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Default cell type coloring
#' sankey_plot(ex$data, ex$colors) + sankey_node_colors("cell_type")
#'
#' # Weighted color mixing (requires expression data)
#' # sankey_plot(data_with_expression, colors) + sankey_node_colors("weighted_mix")
sankey_node_colors <- function(mode = c("cell_type", "weighted_mix")) {
  mode <- match.arg(mode)
  structure(
    list(
      type = "node_colors",
      mode = mode
    ),
    class = "sankey_layer"
  )
}


#' Set Plot Dimensions
#'
#' @description
#' Set the width and height of the plot in pixels.
#'
#' @param width Width in pixels. Default: 600.
#' @param height Height in pixels. Default: 900.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Wide layout for presentations
#' sankey_plot(ex$data, ex$colors) + sankey_layout(width = 1000, height = 500)
#'
#' # Tall layout for many nodes
#' sankey_plot(ex$data, ex$colors) + sankey_layout(width = 600, height = 800)
sankey_layout <- function(width = NULL, height = NULL) {
  structure(
    list(
      type = "layout",
      width = width,
      height = height
    ),
    class = "sankey_layer"
  )
}


#' Add Plot Title
#'
#' @description
#' Add a title to the plot.
#'
#' @param title Title text.
#' @param size Font size for title. Default: inherits from plot settings.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_title("Cell-Cell Communication Network")
sankey_title <- function(title, size = NULL) {
  structure(
    list(
      type = "title",
      title = title,
      size = size
    ),
    class = "sankey_layer"
  )
}


#' Set Plot Margins
#'
#' @description
#' Set the margins around the plot area in pixels.
#'
#' @param left Left margin. Default: 50.
#' @param right Right margin. Default: 50.
#' @param top Top margin. Default: 80.
#' @param bottom Bottom margin. Default: 50.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Increase margins for long labels
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_margins(left = 100, right = 100)
sankey_margins <- function(left = NULL, right = NULL, top = NULL, bottom = NULL) {
  structure(
    list(
      type = "margins",
      left = left,
      right = right,
      top = top,
      bottom = bottom
    ),
    class = "sankey_layer"
  )
}


#' Set Background Colors
#'
#' @description
#' Set the background colors for the plot.
#'
#' @param paper Paper (outer) background color. Default: "white".
#' @param plot Plot (inner) background color. Default: "white".
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Light gray background
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_background(paper = "#F5F5F5")
#'
#' # Dark theme
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_background(paper = "#1E1E1E", plot = "#2D2D2D")
sankey_background <- function(paper = NULL, plot = NULL) {
  structure(
    list(
      type = "background",
      paper = paper,
      plot = plot
    ),
    class = "sankey_layer"
  )
}


#' Set Font Properties
#'
#' @description
#' Set the default font for the plot.
#'
#' @param family Font family name. Default: "Arial".
#' @param size Base font size. Default: 12.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Use Helvetica with larger text
#' sankey_plot(ex$data, ex$colors) +
#'   sankey_font(family = "Helvetica", size = 14)
sankey_font <- function(family = NULL, size = NULL) {
  structure(
    list(
      type = "font",
      family = family,
      size = size
    ),
    class = "sankey_layer"
  )
}


#' Set Plot Orientation
#'
#' @description
#' Set the orientation of the Sankey diagram.
#'
#' @param direction Orientation direction:
#'   \describe{
#'     \item{"horizontal"}{(Default) Flow from left to right.}
#'     \item{"vertical"}{Flow from top to bottom.}
#'   }
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Vertical flow (top to bottom)
#' sankey_plot(ex$data, ex$colors) + sankey_orientation("vertical")
sankey_orientation <- function(direction = c("horizontal", "vertical")) {
  direction <- match.arg(direction)
  orient <- if (direction == "horizontal") "h" else "v"
  structure(
    list(
      type = "orientation",
      orientation = orient
    ),
    class = "sankey_layer"
  )
}


#' Configure Legend
#'
#' @description
#' Control legend display settings.
#'
#' @param show Logical. Whether to show the legend. Default: TRUE.
#' @param title Legend title text.
#'
#' @return A sankey_layer object
#' @export
#'
#' @examples
#' ex <- example_communication()
#'
#' # Hide legend
#' sankey_plot(ex$data, ex$colors) + sankey_legend(show = FALSE)
sankey_legend <- function(show = TRUE, title = NULL) {
  structure(
    list(
      type = "legend",
      show = show,
      title = title
    ),
    class = "sankey_layer"
  )
}


# =============================================================================
# Layer Application (Internal)
# =============================================================================

#' Apply a Layer to a Sankey Plot
#'
#' @description
#' Internal function to apply a sankey_layer to a sankey object.
#'
#' @param plot A sankey object
#' @param layer A sankey_layer object
#'
#' @return The modified sankey object
#' @keywords internal
apply_layer <- function(plot, layer) {
  internal <- plot$.internal

  switch(layer$type,
    colors = {
      internal$colors(color_by = layer$by)
    },
    group_colors = {
      internal$group_colors(layer$colors)
    },
    alpha = {
      internal$alpha(layer$value)
    },
    highlight = {
      internal$highlight_pathway(
        senders = layer$senders,
        ligands = layer$ligands,
        receptors = layer$receptors,
        receivers = layer$receivers,
        dim_alpha = layer$dim_alpha
      )
    },
    clear_highlight = {
      internal$clear_highlight()
    },
    nodes = {
      internal$nodes(thickness = layer$thickness, padding = layer$padding,
                     arrangement = layer$arrangement,
                     vertical_span = layer$vertical_span %||% 0.7,
                     minimum_gap = layer$minimum_gap %||% 4)
    },
    order = {
      internal$node_order(
        senders = layer$senders,
        ligands = layer$ligands,
        receptors = layer$receptors,
        receivers = layer$receivers,
        mode = layer$mode %||% "constrained",
        iterations = layer$iterations %||% 8L
      )
    },
    labels = {
      internal$label_style(size = layer$size)
    },
    interactive = {
      internal$interactive(layer$enable)
    },
    node_colors = {
      internal$node_colors(layer$mode)
    },
    layout = {
      internal$layout(width = layer$width, height = layer$height)
    },
    title = {
      internal$add_title(layer$title, font_size = layer$size)
    },
    margins = {
      internal$margins(
        left = layer$left,
        right = layer$right,
        top = layer$top,
        bottom = layer$bottom
      )
    },
    background = {
      internal$background(paper = layer$paper, plot = layer$plot)
    },
    font = {
      internal$font(family = layer$family, size = layer$size)
    },
    orientation = {
      internal$orientation(layer$orientation)
    },
    legend = {
      internal$legend(show = layer$show, title = layer$title)
    },
    stop("Unknown layer type: ", layer$type, call. = FALSE)
  )

  # Return the modified plot
  plot
}


# =============================================================================
# Export Function
# =============================================================================

#' Save a Sankey Plot
#'
#' @description
#' Save a sankey plot to a file. Supports HTML (interactive), PNG, PDF, SVG,
#' JPEG, and WebP formats. Format is auto-detected from the file extension.
#'
#' @param plot A sankey object created by `sankey_plot()`
#' @param filename Output filename with extension (.html, .png, .pdf, .svg, .jpg, .webp)
#' @param width Width in pixels. Default: uses plot's current width.
#' @param height Height in pixels. Default: uses plot's current height.
#' @param scale Scale factor for resolution (for static images). Default: 1.
#'
#' @return The sankey object invisibly.
#'
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors) +
#'   sankey_title("Cell Communication")
#'
#' # Save as interactive HTML
#' # sankey_save(p, "plot.html")
#'
#' # Save as PNG (requires kaleido or webshot2)
#' # sankey_save(p, "plot.png", width = 800, height = 600)
#'
#' # Save as PDF for publication
#' # sankey_save(p, "figure.pdf", width = 800, height = 600)
sankey_save <- function(plot, filename, width = NULL, height = NULL, scale = 1) {
  if (!inherits(plot, "sankey")) {
    stop("plot must be a sankey object created by sankey_plot()", call. = FALSE)
  }

  plot$.internal$save(
    filename = filename,
    width = width,
    height = height,
    scale = scale
  )

  invisible(plot)
}


#' Save a Sankey Plot as PDF
#'
#' @description
#' Convenience wrapper for saving a sankey plot as a PDF file.
#' PDF export is not supported by the browser-based camera icon (Plotly.js
#' limitation), so use this function instead.
#'
#' @param plot A sankey object created by `sankey_plot()`
#' @param filename Output PDF filename. The `.pdf` extension is added
#'   automatically if not present.
#' @param width Width in pixels. Default: uses plot's current width.
#' @param height Height in pixels. Default: uses plot's current height.
#' @param scale Scale factor for resolution. Default: 1.
#'
#' @return The sankey object invisibly.
#'
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors)
#'
#' # Quick PDF export
#' # sankey_pdf(p, "figure.pdf")
#'
#' # With custom dimensions
#' # sankey_pdf(p, "figure.pdf", width = 800, height = 600)
sankey_pdf <- function(plot, filename = "sankey_plot.pdf", width = NULL, height = NULL, scale = 1) {
  if (!inherits(plot, "sankey")) {
    stop("plot must be a sankey object created by sankey_plot()", call. = FALSE)
  }

  # Ensure .pdf extension
  if (!grepl("\\.pdf$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".pdf")
  }

  sankey_save(plot, filename, width = width, height = height, scale = scale)
}


#' Render a Sankey Plot to a Plotly Figure
#'
#' @description
#' Render a sankey plot and return the underlying plotly figure object.
#' Useful for further customization with plotly functions.
#'
#' @param plot A sankey object created by `sankey_plot()`
#'
#' @return A plotly figure object.
#'
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors)
#'
#' # Get the plotly figure for further customization
#' fig <- sankey_render(p)
sankey_render <- function(plot) {
  if (!inherits(plot, "sankey")) {
    stop("plot must be a sankey object created by sankey_plot()", call. = FALSE)
  }

  plot$.internal$render()
}


#' Display a Sankey Plot
#'
#' @description
#' Explicitly display a sankey plot. Use this function if the plot doesn't
#' display automatically when you type the plot name in a cell.
#'
#' @param plot A sankey object created by `sankey_plot()`
#'
#' @return The rendered plotly figure (invisibly in console, for display in Jupyter).
#'
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors)
#'
#' # Explicitly display the plot
#' sankey_show(p)
sankey_show <- function(plot) {
  if (!inherits(plot, "sankey")) {
    stop("plot must be a sankey object created by sankey_plot()", call. = FALSE)
  }

  # Delegate to print which handles Jupyter vs console
  print(plot)
  invisible(plot)
}



#' Display a Sankey Plot in a Jupyter Notebook
#'
#' @description
#' Explicit notebook display helper for Sankey plots. It renders the real
#' interactive Plotly/htmlwidget object with \code{sankey_render()} and, when
#' requested, appends a static PNG preview generated through
#' \code{sankey_save()}. This is the recommended helper for Jupyter notebooks
#' that need reliable saved outputs without downstream display wrappers.
#'
#' @param plot A sankey object created by \code{sankey_plot()}.
#' @param width Optional widget and static preview width in pixels.
#' @param height Optional widget and static preview height in pixels.
#' @param static Logical. If \code{TRUE}, also export and display a static PNG
#'   preview. Default: \code{TRUE}.
#' @param static_file Optional output path for the static PNG preview. If
#'   \code{NULL}, a temporary \code{.png} file is used.
#' @param scale Scale factor passed to \code{sankey_save()} for static preview
#'   export. Default: \code{1}.
#' @param quiet Logical. If \code{TRUE}, suppress informational messages.
#' @param ... Additional arguments passed to \code{sankey_save()} when
#'   \code{static = TRUE}.
#'
#' @return Invisibly returns a list with the rendered widget, static preview
#'   path, export status, and any static export error message.
#'
#' @export
#'
#' @examples
#' ex <- example_communication()
#' p <- sankey_plot(ex$data, ex$colors) +
#'   sankey_title("Notebook display") +
#'   sankey_layout(width = 760, height = 520)
#'
#' # In Jupyter, display the widget and try to append a PNG preview.
#' # sankey_display_notebook(p, width = 760, height = 520)
sankey_display_notebook <- function(plot,
                                    width = NULL,
                                    height = NULL,
                                    static = TRUE,
                                    static_file = NULL,
                                    scale = 1,
                                    quiet = FALSE,
                                    ...) {
  if (!inherits(plot, "sankey")) {
    stop("plot must be a sankey object created by sankey_plot()", call. = FALSE)
  }
  if (!is.logical(static) || length(static) != 1 || is.na(static)) {
    stop("static must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    stop("quiet must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(static_file) && !grepl("\\.png$", static_file, ignore.case = TRUE)) {
    stop("static_file must end in .png", call. = FALSE)
  }

  fig <- sankey_render(plot)
  fig <- sankey_set_widget_dimensions(fig, width = width, height = height)

  displayed <- sankey_display_widget(fig, quiet = quiet)

  result <- list(
    widget = fig,
    static_file = NULL,
    static_exported = FALSE,
    static_error = NULL
  )

  if (isTRUE(static)) {
    png_file <- static_file %||% tempfile(fileext = ".png")

    static_result <- tryCatch({
      sankey_save(plot, png_file, width = width, height = height, scale = scale, ...)

      if (!file.exists(png_file) || file.info(png_file)[["size"]] <= 0) {
        stop("Static PNG export did not create a nonempty file.", call. = FALSE)
      }

      if (is_jupyter_notebook() && requireNamespace("IRdisplay", quietly = TRUE)) {
        IRdisplay::display_png(file = png_file)
      } else if (!quiet && !displayed) {
        message("Static PNG preview written to: ", png_file)
      }

      list(file = png_file, error = NULL)
    }, error = function(e) {
      msg <- sankey_static_preview_error_message(conditionMessage(e))
      if (!quiet) {
        message(msg)
      }
      list(file = NULL, error = msg)
    })

    result$static_file <- static_result$file
    result$static_exported <- !is.null(static_result$file)
    result$static_error <- static_result$error
  }

  invisible(result)
}


# Internal helper so notebook display code can be tested separately from Plotly
# rendering and static export backends.
sankey_set_widget_dimensions <- function(fig, width = NULL, height = NULL) {
  if (!is.null(width)) {
    fig$width <- width
    if (!is.null(fig$x)) {
      fig$x$layout$width <- width
    }
  }
  if (!is.null(height)) {
    fig$height <- height
    if (!is.null(fig$x)) {
      fig$x$layout$height <- height
    }
  }
  fig
}


sankey_display_widget <- function(fig, quiet = FALSE) {
  displayed <- FALSE

  if (is_jupyter_notebook() &&
      requireNamespace("repr", quietly = TRUE) &&
      requireNamespace("IRdisplay", quietly = TRUE)) {
    displayed <- tryCatch({
      html_repr <- repr::repr_html(fig)
      if (!is.null(html_repr)) {
        IRdisplay::display_html(html_repr)
        TRUE
      } else {
        FALSE
      }
    }, error = function(e) FALSE)
  }

  if (!displayed && is_jupyter_notebook() &&
      requireNamespace("IRdisplay", quietly = TRUE)) {
    IRdisplay::display(fig)
    displayed <- TRUE
  }

  if (!displayed && interactive()) {
    print(fig)
    displayed <- TRUE
  }

  if (!displayed && !quiet) {
    message("Not running in a Jupyter notebook; returning the rendered Plotly widget.")
  }

  displayed
}


sankey_static_preview_error_message <- function(error_message) {
  paste0(
    "Static PNG preview failed, but the interactive Sankey widget is still available.\n\n",
    "Static export error:\n",
    error_message,
    "\n\n",
    "Install one static export backend and restart R:\n",
    "  - Kaleido/Python: pip install 'kaleido<1.0' in the Python used by reticulate.\n",
    "  - webshot2 + Chrome/Chromium: install.packages('webshot2'); webshot2::install_chromium().\n",
    "  - Orca: install the Plotly orca command line tool and make sure it is on PATH.\n",
    "If using reticulate, set RETICULATE_PYTHON to a Python with Plotly and Kaleido available."
  )
}
