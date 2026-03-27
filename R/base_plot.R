#' @title Base Plot Class
#' @description Abstract base class for all ccflowR plots
#' @name BasePlot
#' @importFrom R6 R6Class
#' @importFrom plotly plot_ly layout add_trace
NULL

#' Base Plot R6 Class
#'
#' @description
#' Abstract base class for all plotting in ccflowR. Provides common methods
#' for styling, layout, and rendering that are inherited by specific plot
#' types like SankeyPlot.
#'
#' @export
BasePlot <- R6::R6Class(
  "BasePlot",

  public = list(
    #' @description Initialize base plot
    #' @param data Data frame with plot data
    #' @param width Plot width in pixels
    #' @param height Plot height in pixels
    initialize = function(data = NULL, width = NULL, height = NULL) {
      private$.data <- data
      private$.width <- width %||% ccflow_settings()$figwidth()
      private$.height <- height %||% ccflow_settings()$figheight()
      private$.title <- NULL
      private$.title_font <- list(
        family = ccflow_settings()$font_family(),
        size = ccflow_settings()$font_size() + 4
      )
      private$.font <- list(
        family = ccflow_settings()$font_family(),
        size = ccflow_settings()$font_size()
      )
      private$.margin <- list(l = 50, r = 50, t = 80, b = 50)
      private$.paper_bgcolor <- "white"
      private$.plot_bgcolor <- "white"
      private$.figure <- NULL
    },

    #' @description Set plot dimensions
    #' @param width Plot width in pixels
    #' @param height Plot height in pixels
    #' @return Self (invisibly) for chaining
    layout = function(width = NULL, height = NULL) {
      if (!is.null(width)) private$.width <- width
      if (!is.null(height)) private$.height <- height
      invisible(self)
    },

    #' @description Set plot title
    #' @param title Title text
    #' @param font_family Font family for title
    #' @param font_size Font size for title
    #' @param x Horizontal position (0-1)
    #' @param y Vertical position (0-1)
    #' @return Self (invisibly) for chaining
    add_title = function(title, font_family = NULL, font_size = NULL, x = 0.5, y = 0.98) {
      private$.title <- list(
        text = title,
        x = x,
        y = y,
        xanchor = "center",
        yanchor = "top",
        font = list(
          family = font_family %||% private$.title_font$family,
          size = font_size %||% private$.title_font$size
        )
      )
      invisible(self)
    },

    #' @description Set font settings
    #' @param family Font family name
    #' @param size Font size
    #' @return Self (invisibly) for chaining
    font = function(family = NULL, size = NULL) {
      if (!is.null(family)) private$.font$family <- family
      if (!is.null(size)) private$.font$size <- size
      invisible(self)
    },

    #' @description Set plot margins
    #' @param left Left margin in pixels
    #' @param right Right margin in pixels
    #' @param top Top margin in pixels
    #' @param bottom Bottom margin in pixels
    #' @return Self (invisibly) for chaining
    margins = function(left = NULL, right = NULL, top = NULL, bottom = NULL) {
      if (!is.null(left)) private$.margin$l <- left
      if (!is.null(right)) private$.margin$r <- right
      if (!is.null(top)) private$.margin$t <- top
      if (!is.null(bottom)) private$.margin$b <- bottom
      invisible(self)
    },

    #' @description Set background colors
    #' @param paper Paper (outer) background color
    #' @param plot Plot (inner) background color
    #' @return Self (invisibly) for chaining
    background = function(paper = NULL, plot = NULL) {
      if (!is.null(paper)) private$.paper_bgcolor <- paper
      if (!is.null(plot)) private$.plot_bgcolor <- plot
      invisible(self)
    },

    #' @description Render the plot (must be implemented by subclasses)
    #' @return Plotly figure object
    render = function() {
      stop("render() must be implemented by subclasses", call. = FALSE)
    },

    #' @description Display the plot
    #' @return Plotly figure object (invisibly)
    show = function() {
      fig <- self$render()

      # Set dimensions
      fig$width <- private$.width
      fig$height <- private$.height

      # Check if we're in Jupyter/IRkernel
      if ("IRkernel" %in% loadedNamespaces() &&
          requireNamespace("IRdisplay", quietly = TRUE)) {
        # Use IRdisplay to show the htmlwidget
        IRdisplay::display(fig)
        return(invisible(fig))
      }

      # For console/RStudio, use standard htmlwidget print
      print(fig)
      invisible(fig)
    },

    #' @description Save the plot to a file
    #' @param filename Output filename
    #' @param width Width in pixels (default: current width)
    #' @param height Height in pixels (default: current height)
    #' @param scale Scale factor for resolution
    #' @return Self (invisibly) for chaining
    save = function(filename, width = NULL, height = NULL, scale = 1) {
      fig <- self$render()
      # Apply export config (SVG/PNG/PDF buttons) for saved files
      fig <- private$.apply_export_config(fig)
      save_figure(
        fig,
        filename,
        width = width %||% private$.width,
        height = height %||% private$.height,
        scale = scale
      )
      invisible(self)
    },

    #' @description Get the current data
    #' @return Data frame
    get_data = function() {
      private$.data
    },

    #' @description Get the rendered figure
    #' @return Plotly figure object or NULL if not rendered
    get_figure = function() {
      private$.figure
    }
  ),

  private = list(
    .data = NULL,
    .width = NULL,
    .height = NULL,
    .title = NULL,
    .title_font = NULL,
    .font = NULL,
    .margin = NULL,
    .paper_bgcolor = NULL,
    .plot_bgcolor = NULL,
    .figure = NULL,

    # Apply common layout settings to a plotly figure
    .apply_layout = function(fig) {
      layout_args <- list(
        p = fig,
        font = private$.font,
        margin = private$.margin,
        paper_bgcolor = private$.paper_bgcolor,
        plot_bgcolor = private$.plot_bgcolor,
        # Disable autosize to respect explicit dimensions
        # This prevents Plotly from resizing based on container
        autosize = FALSE
      )

      if (!is.null(private$.title)) {
        layout_args$title <- private$.title
      }

      fig <- do.call(plotly::layout, layout_args)

      # Set width/height on the figure object (recommended approach for plotly)
      # This is the proper way to set dimensions without deprecation warnings
      fig$width <- private$.width
      fig$height <- private$.height

      # Also store in layout$width and layout$height for Jupyter compatibility
      # Some rendering contexts check these values directly
      fig$x$layout$width <- private$.width
      fig$x$layout$height <- private$.height

      # Set default download button to SVG (plain list, no JS objects).
      # Safe for IRdisplay::display() in Jupyter.
      # Note: PDF is not supported client-side by Plotly.js; use sankey_save() for PDF.
      fig <- plotly::config(fig, toImageButtonOptions = list(
        format = "svg",
        width = private$.width,
        height = private$.height
      ))

      fig
    },

    # Apply additional export config (PNG/PDF custom buttons with JS handlers).
    # IMPORTANT: Do NOT call this in render(). htmlwidgets::JS() objects break
    # IRdisplay::display() in Jupyter (see todo-jupyter-display.md).
    # Only apply when saving to HTML files.
    .apply_export_config = function(fig) {
      w <- private$.width
      h <- private$.height
      # Default camera button exports SVG; add one custom button for PNG (2x scale)
      plotly::config(fig,
        toImageButtonOptions = list(format = "svg", width = w, height = h),
        modeBarButtonsToAdd = list(
          list(
            name = "Download as PNG (2x)",
            icon = htmlwidgets::JS(paste0(
              "{width:1000,height:1000,path:'",
              "M500 450Q500 500 450 500L150 500Q100 500 100 450L100 150Q100 100 150 100L450 100Q500 100 500 150Z",
              "M350 200L250 325L200 275L150 350L450 350L350 200Z",
              "M225 225A35 35 0 1 0 225 155A35 35 0 1 0 225 225Z",
              "',ascent:1000,descent:0}"
            )),
            click = htmlwidgets::JS(sprintf(
              "function(gd) { Plotly.downloadImage(gd, {format: 'png', width: %d, height: %d, scale: 2}); }",
              w, h
            ))
          )
        )
      )
    }
  )
)
