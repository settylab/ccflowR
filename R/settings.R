#' @title CCFlow Settings
#' @description Global settings for ccflowR package
#' @name ccflow_settings
NULL

#' CCFlow Settings Class
#'
#' @description
#' R6 class for managing global ccflowR settings. Provides methods for
#' getting and setting configuration values.
#'
#' @export
CCFlowSettings <- R6::R6Class(

"CCFlowSettings",

  public = list(
    #' @description Initialize settings with defaults
    initialize = function() {
      private$.dpi <- 100L
      private$.dpi_save <- 300L
      private$.figwidth <- 600L
      private$.figheight <- 900L
      private$.font_family <- "Arial"
      private$.font_size <- 12L
      private$.fallback_color <- "#A0A0A0"
      private$.default_colormap <- "Set2"
      private$.verbose <- FALSE
    },

    #' @description Get or set display DPI
    #' @param value New DPI value (optional)
    #' @return Current DPI value or self (invisibly) for chaining
    dpi = function(value) {
      if (missing(value)) return(private$.dpi)
      private$.dpi <- as.integer(value)
      invisible(self)
    },

    #' @description Get or set save DPI
    #' @param value New DPI value (optional)
    #' @return Current DPI value or self (invisibly) for chaining
    dpi_save = function(value) {
      if (missing(value)) return(private$.dpi_save)
      private$.dpi_save <- as.integer(value)
      invisible(self)
    },

    #' @description Get or set figure width
    #' @param value New width value (optional)
    #' @return Current width or self (invisibly) for chaining
    figwidth = function(value) {
      if (missing(value)) return(private$.figwidth)
      private$.figwidth <- as.integer(value)
      invisible(self)
    },

    #' @description Get or set figure height
    #' @param value New height value (optional)
    #' @return Current height or self (invisibly) for chaining
    figheight = function(value) {
      if (missing(value)) return(private$.figheight)
      private$.figheight <- as.integer(value)
      invisible(self)
    },

    #' @description Get or set font family
    #' @param value New font family (optional)
    #' @return Current font family or self (invisibly) for chaining
    font_family = function(value) {
      if (missing(value)) return(private$.font_family)
      private$.font_family <- as.character(value)
      invisible(self)
    },

    #' @description Get or set font size
    #' @param value New font size (optional)
    #' @return Current font size or self (invisibly) for chaining
    font_size = function(value) {
      if (missing(value)) return(private$.font_size)
      private$.font_size <- as.integer(value)
      invisible(self)
    },

    #' @description Get or set fallback color
    #' @param value New fallback color (optional)
    #' @return Current fallback color or self (invisibly) for chaining
    fallback_color = function(value) {
      if (missing(value)) return(private$.fallback_color)
      private$.fallback_color <- as.character(value)
      invisible(self)
    },

    #' @description Get or set default colormap
    #' @param value New colormap name (optional)
    #' @return Current colormap or self (invisibly) for chaining
    default_colormap = function(value) {
      if (missing(value)) return(private$.default_colormap)
      private$.default_colormap <- as.character(value)
      invisible(self)
    },

    #' @description Get or set verbose mode
    #' @param value New verbose value (optional)
    #' @return Current verbose value or self (invisibly) for chaining
    verbose = function(value) {
      if (missing(value)) return(private$.verbose)
      private$.verbose <- as.logical(value)
      invisible(self)
    },

    #' @description Set multiple figure parameters at once
    #' @param dpi Display DPI
    #' @param dpi_save Save DPI
    #' @param figwidth Figure width in pixels
    #' @param figheight Figure height in pixels
    #' @param font_family Font family name
    #' @param font_size Font size
    #' @return Self (invisibly) for chaining
    set_figure_params = function(dpi = NULL, dpi_save = NULL,
                                  figwidth = NULL, figheight = NULL,
                                  font_family = NULL, font_size = NULL) {
      if (!is.null(dpi)) private$.dpi <- as.integer(dpi)
      if (!is.null(dpi_save)) private$.dpi_save <- as.integer(dpi_save)
      if (!is.null(figwidth)) private$.figwidth <- as.integer(figwidth)
      if (!is.null(figheight)) private$.figheight <- as.integer(figheight)
      if (!is.null(font_family)) private$.font_family <- as.character(font_family)
      if (!is.null(font_size)) private$.font_size <- as.integer(font_size)
      invisible(self)
    },

    #' @description Reset all settings to defaults
    #' @return Self (invisibly) for chaining
    reset = function() {
      self$initialize()
      invisible(self)
    },

    #' @description Print current settings
    print = function() {
      cat("CCFlow Settings:\n")
      cat("  dpi:", private$.dpi, "\n")
      cat("  dpi_save:", private$.dpi_save, "\n")
      cat("  figwidth:", private$.figwidth, "\n")
      cat("  figheight:", private$.figheight, "\n")
      cat("  font_family:", private$.font_family, "\n")
      cat("  font_size:", private$.font_size, "\n")
      cat("  fallback_color:", private$.fallback_color, "\n")
      cat("  default_colormap:", private$.default_colormap, "\n")
      cat("  verbose:", private$.verbose, "\n")
      invisible(self)
    }
  ),

  private = list(
    .dpi = NULL,
    .dpi_save = NULL,
    .figwidth = NULL,
    .figheight = NULL,
    .font_family = NULL,
    .font_size = NULL,
    .fallback_color = NULL,
    .default_colormap = NULL,
    .verbose = NULL
  )
)

#' Global settings instance
#'
#' @description
#' Access the global ccflowR settings. This is a singleton instance of
#' CCFlowSettings that can be used to configure package-wide defaults.
#'
#' @return CCFlowSettings R6 object
#' @export
#'
#' @examples
#' # Get current DPI
#' ccflow_settings()$dpi()
#'
#' # Set new DPI
#' ccflow_settings()$dpi(150)
#'
#' # Chain multiple settings
#' ccflow_settings()$dpi(150)$font_size(14)$verbose(FALSE)
#'
#' # Set multiple figure parameters
#' ccflow_settings()$set_figure_params(dpi = 150, figwidth = 800)
ccflow_settings <- function() {
  if (is.null(.ccflow_env$settings)) {
    .ccflow_env$settings <- CCFlowSettings$new()
  }
  .ccflow_env$settings
}

# Package environment for storing settings
.ccflow_env <- new.env(parent = emptyenv())
