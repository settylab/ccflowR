#' @title Color Utilities
#' @description Color manipulation and palette management for ccflowR
#' @name colors
#' @importFrom grDevices col2rgb rgb
NULL

#' Convert Hex Color to RGB
#'
#' @description
#' Convert a hexadecimal color string to RGB values (0-255).
#'
#' @param hex Hex color string (e.g., "#FF0000" or "FF0000")
#'
#' @return Named numeric vector with r, g, b components (0-255)
#' @export
#'
#' @examples
#' hex_to_rgb("#FF0000")
#' hex_to_rgb("00FF00")
hex_to_rgb <- function(hex) {
  # Use col2rgb which handles hex, named colors ("red"), and 8-char hex with alpha
  rgb_mat <- tryCatch(
    grDevices::col2rgb(hex),
    error = function(e) {
      # Fallback: try parsing as raw hex (e.g. "FFF" without #)
      hex_clean <- gsub("^#", "", hex)
      if (nchar(hex_clean) == 3) {
        # Expand shorthand (FFF -> FFFFFF)
        hex_clean <- paste0(
          substr(hex_clean, 1, 1), substr(hex_clean, 1, 1),
          substr(hex_clean, 2, 2), substr(hex_clean, 2, 2),
          substr(hex_clean, 3, 3), substr(hex_clean, 3, 3)
        )
      }
      r <- strtoi(substr(hex_clean, 1, 2), base = 16)
      g <- strtoi(substr(hex_clean, 3, 4), base = 16)
      b <- strtoi(substr(hex_clean, 5, 6), base = 16)
      matrix(c(r, g, b), nrow = 3, dimnames = list(c("red", "green", "blue"), NULL))
    }
  )
  c(r = unname(rgb_mat[1, 1]), g = unname(rgb_mat[2, 1]), b = unname(rgb_mat[3, 1]))
}

#' Convert RGB to Hex Color
#'
#' @description
#' Convert RGB values (0-255) to a hexadecimal color string.
#'
#' @param r Red component (0-255)
#' @param g Green component (0-255)
#' @param b Blue component (0-255)
#'
#' @return Hex color string (e.g., "#FF0000")
#' @export
#'
#' @examples
#' rgb_to_hex(255, 0, 0)
#' rgb_to_hex(0, 255, 0)
rgb_to_hex <- function(r, g, b) {
  r <- as.integer(pmax(0, pmin(255, r)))
  g <- as.integer(pmax(0, pmin(255, g)))
  b <- as.integer(pmax(0, pmin(255, b)))
  sprintf("#%02X%02X%02X", r, g, b)
}

#' Add Alpha to Hex Color
#'
#' @description
#' Convert a hex color to RGBA format with specified alpha (transparency).
#'
#' @param hex Hex color string
#' @param alpha Alpha value (0-1, where 0 is transparent and 1 is opaque)
#'
#' @return RGBA color string (e.g., "rgba(255, 0, 0, 0.5)")
#' @export
#'
#' @examples
#' hex_with_alpha("#FF0000", 0.5)
#' hex_to_rgba("#FF0000", 0.5)  # Alias
hex_with_alpha <- function(hex, alpha = 1) {
  rgb_vals <- hex_to_rgb(hex)
  sprintf("rgba(%d, %d, %d, %.2f)", rgb_vals["r"], rgb_vals["g"], rgb_vals["b"], alpha)
}


#' @rdname hex_with_alpha
#' @description
#' \code{hex_to_rgba} is an alias for \code{hex_with_alpha} with a more
#' descriptive name.
#' @export
hex_to_rgba <- function(hex, alpha = 1) {
  hex_with_alpha(hex, alpha)
}

#' Mix Colors with Weights
#'
#' @description
#' Blend multiple colors together using weighted averaging.
#'
#' @param colors Character vector of hex colors
#' @param weights Numeric vector of weights (will be normalized)
#'
#' @return Hex color string of blended result
#' @export
#'
#' @examples
#' # Equal mix of red and blue
#' weighted_mix_colors(c("#FF0000", "#0000FF"), c(1, 1))
#'
#' # 75% red, 25% blue
#' weighted_mix_colors(c("#FF0000", "#0000FF"), c(3, 1))
weighted_mix_colors <- function(colors, weights = NULL) {
  if (length(colors) == 0) {
    return(ccflow_settings()$fallback_color())
  }

  if (length(colors) == 1) {
    return(colors)
  }

  # Default to equal weights
  if (is.null(weights)) {
    weights <- rep(1, length(colors))
  }

  # Normalize weights
  weights <- weights / sum(weights)

  # Convert all colors to RGB
  rgb_matrices <- lapply(colors, hex_to_rgb)

  # Weighted average
  r <- sum(sapply(seq_along(colors), function(i) rgb_matrices[[i]]["r"] * weights[i]))
  g <- sum(sapply(seq_along(colors), function(i) rgb_matrices[[i]]["g"] * weights[i]))
  b <- sum(sapply(seq_along(colors), function(i) rgb_matrices[[i]]["b"] * weights[i]))

  rgb_to_hex(r, g, b)
}

#' Color Palette Class
#'
#' @description
#' R6 class for managing color palettes. Supports automatic color assignment,
#' custom color mappings, and generation from colormaps.
#'
#' @export
ColorPalette <- R6::R6Class(
  "ColorPalette",

  public = list(
    #' @description Initialize a color palette
    #' @param colors Named list or vector of colors (name -> hex color)
    #' @param colormap Name of a colormap to use for auto-assignment (e.g., "Set2", "Set1", "Paired")
    initialize = function(colors = NULL, colormap = NULL) {
      private$.colors <- list()
      private$.colormap <- colormap %||% ccflow_settings()$default_colormap()

      if (!is.null(colors)) {
        if (is.list(colors)) {
          private$.colors <- colors
        } else if (!is.null(names(colors))) {
          private$.colors <- as.list(colors)
        }
      }

      # Initialize colormap colors
      private$.init_colormap()
    },

    #' @description Get color for a category
    #' @param name Category name
    #' @return Hex color string
    get = function(name) {
      if (name %in% names(private$.colors)) {
        return(private$.colors[[name]])
      }
      # Auto-assign new color
      self$assign(name)
      private$.colors[[name]]
    },

    #' @description Set color for a category
    #' @param name Category name
    #' @param color Hex color string
    #' @return Self (invisibly) for chaining
    set = function(name, color) {
      private$.colors[[name]] <- color
      invisible(self)
    },

    #' @description Auto-assign a color to a category
    #' @param name Category name
    #' @return Self (invisibly) for chaining
    assign = function(name) {
      if (!(name %in% names(private$.colors))) {
        idx <- (length(private$.colors) %% length(private$.colormap_colors)) + 1
        private$.colors[[name]] <- private$.colormap_colors[idx]
      }
      invisible(self)
    },

    #' @description Update palette with multiple colors
    #' @param colors Named list or vector of colors
    #' @return Self (invisibly) for chaining
    update = function(colors) {
      if (is.list(colors)) {
        for (name in names(colors)) {
          private$.colors[[name]] <- colors[[name]]
        }
      } else if (!is.null(names(colors))) {
        for (name in names(colors)) {
          private$.colors[[name]] <- colors[[name]]
        }
      }
      invisible(self)
    },

    #' @description Convert palette to named list
    #' @return Named list of colors
    as_list = function() {
      private$.colors
    },

    #' @description Get all category names
    #' @return Character vector of names
    names = function() {
      names(private$.colors)
    },

    #' @description Check if category has a color
    #' @param name Category name
    #' @return Logical
    has = function(name) {
      name %in% names(private$.colors)
    },

    #' @description Print palette
    print = function() {
      cat("ColorPalette with", length(private$.colors), "colors:\n")
      for (name in names(private$.colors)) {
        cat(sprintf("  %s: %s\n", name, private$.colors[[name]]))
      }
      invisible(self)
    }
  ),

  private = list(
    .colors = NULL,
    .colormap = NULL,
    .colormap_colors = NULL,

    .init_colormap = function() {
      # Define colormap palettes
      colormaps <- list(
        Set1 = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                 "#FFFF33", "#A65628", "#F781BF", "#999999"),
        Set2 = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
                 "#FFD92F", "#E5C494", "#B3B3B3"),
        Set3 = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
                 "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD",
                 "#CCEBC5", "#FFED6F"),
        Paired = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
                   "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
                   "#FFFF99", "#B15928"),
        Dark2 = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
                  "#E6AB02", "#A6761D", "#666666"),
        Pastel1 = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6",
                    "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2"),
        Pastel2 = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9",
                    "#FFF2AE", "#F1E2CC", "#CCCCCC"),
        tab10 = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                  "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"),
        tab20 = c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C",
                  "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5",
                  "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F",
                  "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5")
      )

      cmap <- private$.colormap
      if (cmap %in% names(colormaps)) {
        private$.colormap_colors <- colormaps[[cmap]]
      } else {
        # Default to Set2
        private$.colormap_colors <- colormaps[["Set2"]]
      }
    }
  )
)

#' Create Color Palette from Categories
#'
#' @description
#' Create a ColorPalette with colors auto-assigned to categories.
#'
#' @param categories Character vector of category names
#' @param colormap Name of colormap to use
#'
#' @return ColorPalette object
#' @export
#'
#' @examples
#' palette <- create_palette(c("TypeA", "TypeB", "TypeC"))
#' palette$get("TypeA")
create_palette <- function(categories, colormap = "Set2") {
  palette <- ColorPalette$new(colormap = colormap)
  for (cat in categories) {
    palette$assign(cat)
  }
  palette
}

# Null coalesce operator
`%||%` <- function(a, b) if (is.null(a)) b else a
