#' @title Figure Exporters
#' @description Functions for exporting plotly figures to various formats
#' @name exporters
#' @importFrom jsonlite toJSON
NULL

#' Save Figure to File
#'
#' @description
#' Save a plotly figure to various file formats. Supports HTML (interactive),
#' PNG, PDF, SVG, JPEG, and WebP (static images require the webshot2 package
#' or orca/kaleido).
#'
#' @param fig Plotly figure object.
#' @param filename Output filename. Extension determines format (.html, .png,
#'   .pdf, .svg, .jpeg, .webp).
#' @param width Width in pixels. Default: from \code{ccflow_settings()$figwidth()}
#'   (typically 800).
#' @param height Height in pixels. Default: from \code{ccflow_settings()$figheight()}
#'   (typically 600).
#' @param scale Scale factor for resolution (for static images). Default: \code{1}
#'
#' @return Path to saved file (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' ex <- example_communication()
#' plot <- SankeyPlot$new(ex$data, ex$colors)
#' fig <- plot$render()
#'
#' save_figure(fig, "plot.html")
#' save_figure(fig, "plot.png", width = 800, height = 600)
#' }
save_figure <- function(fig, filename, width = NULL, height = NULL, scale = 1) {
  # Get default dimensions from settings
  width <- width %||% ccflow_settings()$figwidth()
  height <- height %||% ccflow_settings()$figheight()

  # Determine format from extension
  ext <- tolower(tools::file_ext(filename))

  if (ext == "html") {
    to_html(fig, filename, width = width, height = height)
  } else if (ext %in% c("png", "pdf", "svg", "jpeg", "jpg", "webp")) {
    to_image(fig, filename, format = ext, width = width, height = height, scale = scale)
  } else {
    stop("Unsupported format: ", ext, ". Supported: html, png, pdf, svg, jpeg, webp",
         call. = FALSE)
  }

  invisible(filename)
}

#' Export to HTML
#'
#' @description
#' Export a plotly figure to an interactive HTML file.
#'
#' @param fig Plotly figure object.
#' @param filename Output filename (should end in .html).
#' @param width Width in pixels. Default: \code{NULL} (use figure's default)
#' @param height Height in pixels. Default: \code{NULL} (use figure's default)
#' @param selfcontained Logical. Create a self-contained HTML with all
#'   dependencies embedded? Default: \code{TRUE}
#'
#' @return Path to saved file (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' ex <- example_communication()
#' plot <- SankeyPlot$new(ex$data, ex$colors)
#' to_html(plot$render(), "plot.html")
#' }
to_html <- function(fig, filename, width = NULL, height = NULL, selfcontained = TRUE) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' required for HTML export. Install with: install.packages('htmlwidgets')",
         call. = FALSE)
  }

  # Set dimensions if provided
  if (!is.null(width)) fig$width <- width
  if (!is.null(height)) fig$height <- height

  htmlwidgets::saveWidget(
    widget = fig,
    file = filename,
    selfcontained = selfcontained
  )

  ccflow_log("Saved interactive HTML to: ", filename)
  invisible(filename)
}

#' Export to Static Image
#'
#' @description
#' Export a plotly figure to a static image format (PNG, PDF, SVG, etc.).
#' Requires either the webshot2 package or orca command line tool.
#'
#' @param fig Plotly figure object.
#' @param filename Output filename.
#' @param format Image format. One of \code{"png"} (default), \code{"pdf"},
#'   \code{"svg"}, \code{"jpeg"}, or \code{"webp"}.
#' @param width Width in pixels. Default: from \code{ccflow_settings()$figwidth()}.
#' @param height Height in pixels. Default: from \code{ccflow_settings()$figheight()}.
#' @param scale Scale factor for resolution. Default: \code{1}
#'
#' @return Path to saved file (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' ex <- example_communication()
#' plot <- SankeyPlot$new(ex$data, ex$colors)
#' to_image(plot$render(), "plot.png", width = 800, height = 600)
#' }
to_image <- function(fig, filename, format = "png", width = NULL, height = NULL, scale = 1) {

  width <- width %||% ccflow_settings()$figwidth()
  height <- height %||% ccflow_settings()$figheight()

  # Track methods tried for error reporting
  methods_tried <- character()

  # Try different export methods
  exported <- FALSE

  # Method 1: Try plotly's built-in export (requires kaleido Python package)
  if (!exported && !requireNamespace("reticulate", quietly = TRUE)) {
    methods_tried <- c(methods_tried, "kaleido (reticulate package not installed)")
  } else if (!exported && requireNamespace("plotly", quietly = TRUE) &&
      requireNamespace("reticulate", quietly = TRUE)) {
    tryCatch({
      # Try to find Python with kaleido in common locations
      python_path <- Sys.getenv("RETICULATE_PYTHON", "")

      # If not set, try common conda/micromamba locations
      if (!nzchar(python_path) || !file.exists(python_path)) {
        conda_prefixes <- c(
          Sys.getenv("CONDA_PREFIX", ""),
          file.path(Sys.getenv("HOME"), "miniconda3"),
          file.path(Sys.getenv("HOME"), "anaconda3"),
          file.path(Sys.getenv("HOME"), "miniforge3"),
          file.path(Sys.getenv("HOME"), "mambaforge"),
          file.path(Sys.getenv("HOME"), ".local/share/mamba"),
          "/usr/local/miniconda3",
          "/opt/conda"
        )

        # Also search micromamba environments
        micromamba_env_dirs <- c(
          file.path(Sys.getenv("HOME"), "micromamba", "envs"),
          file.path(Sys.getenv("MAMBA_ROOT_PREFIX", ""), "envs")
        )
        for (env_dir in micromamba_env_dirs) {
          if (nzchar(env_dir) && dir.exists(env_dir)) {
            env_names <- list.dirs(env_dir, full.names = TRUE, recursive = FALSE)
            for (env_path in env_names) {
              conda_prefixes <- c(conda_prefixes, env_path)
            }
          }
        }

        for (prefix in conda_prefixes) {
          if (nzchar(prefix)) {
            candidate <- file.path(prefix, "bin", "python")
            if (file.exists(candidate)) {
              python_path <- candidate
              break
            }
          }
        }
      }

      if (nzchar(python_path) && file.exists(python_path)) {
        reticulate::use_python(python_path, required = FALSE)
      }

      # Verify kaleido is available
      if (reticulate::py_available(initialize = TRUE)) {
        kaleido_available <- tryCatch({
          reticulate::import("kaleido")
          TRUE
        }, error = function(e) FALSE)

        if (kaleido_available) {
          plotly::save_image(fig, filename, width = width, height = height, scale = scale)
          exported <- TRUE
          ccflow_log("Saved static image to: ", filename)
        } else {
          methods_tried <- c(methods_tried, "kaleido (not installed)")
        }
      } else {
        methods_tried <- c(methods_tried, "kaleido (Python not available)")
      }
    }, error = function(e) {
      methods_tried <<- c(methods_tried, paste0("kaleido (error: ", e$message, ")"))
    })
  }

  # Method 2: For SVG format, try chromote directly (uses Plotly.js to generate SVG)
  if (!exported && format == "svg" && !requireNamespace("chromote", quietly = TRUE)) {
    methods_tried <- c(methods_tried, "chromote (package not installed)")
  } else if (!exported && format == "svg" && requireNamespace("chromote", quietly = TRUE)) {
    tryCatch({
      temp_html <- tempfile(fileext = ".html")
      on.exit(unlink(temp_html), add = TRUE)

      # Save figure as HTML
      if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
        stop("htmlwidgets package required")
      }
      fig$width <- width
      fig$height <- height
      htmlwidgets::saveWidget(fig, temp_html, selfcontained = TRUE)

      # Use chromote to render and export SVG via Plotly.js
      b <- chromote::ChromoteSession$new()
      on.exit(tryCatch(b$close(), error = function(e) NULL), add = TRUE)

      # Navigate to the HTML file
      b$Page$navigate(paste0("file://", temp_html))
      b$Page$loadEventFired()

      # Wait for plotly to render
      Sys.sleep(2)

      # Get SVG using Plotly.toImage
      svg_result <- b$Runtime$evaluate(sprintf("
        (async function() {
          var gd = document.querySelector('.js-plotly-plot') ||
                   document.querySelector('[class*=plotly]');
          if (!gd) throw new Error('Plot element not found');
          if (typeof Plotly === 'undefined') throw new Error('Plotly not loaded');
          var svg = await Plotly.toImage(gd, {format: 'svg', width: %d, height: %d});
          return svg;
        })()
      ", width, height), awaitPromise = TRUE)

      svg_data <- svg_result$result$value

      # Check if we got valid SVG data (starts with data:image/svg+xml)
      if (is.character(svg_data) && grepl("^data:image/svg\\+xml", svg_data)) {
        # Decode the base64/URL-encoded SVG
        # Format: data:image/svg+xml,<url-encoded-svg>
        svg_content <- sub("^data:image/svg\\+xml,", "", svg_data)
        svg_content <- utils::URLdecode(svg_content)

        # Write to file
        writeLines(svg_content, filename)
        exported <- TRUE
        ccflow_log("Saved SVG to: ", filename, " (via chromote)")
      } else {
        methods_tried <- c(methods_tried, "chromote (invalid SVG data)")
      }

      b$close()
    }, error = function(e) {
      methods_tried <<- c(methods_tried, paste0("chromote (", e$message, ")"))
    })
  }

  # Method 3: Try webshot2 (requires Chrome/Chromium) - for PNG, PDF, JPEG
  if (!exported && format %in% c("png", "pdf", "jpeg", "jpg", "webp") &&
      !requireNamespace("webshot2", quietly = TRUE)) {
    methods_tried <- c(methods_tried, "webshot2 (package not installed)")
  } else if (!exported && format %in% c("png", "pdf", "jpeg", "jpg", "webp") &&
      requireNamespace("webshot2", quietly = TRUE)) {
    tryCatch({
      # Save as HTML first, then convert
      temp_html <- tempfile(fileext = ".html")
      on.exit(unlink(temp_html), add = TRUE)

      to_html(fig, temp_html, width = width, height = height)

      webshot2::webshot(
        url = temp_html,
        file = filename,
        vwidth = width,
        vheight = height
      )

      # Verify file was created
      if (file.exists(filename) && file.size(filename) > 0) {
        exported <- TRUE
        ccflow_log("Saved static image to: ", filename, " (via webshot2)")
      } else {
        methods_tried <- c(methods_tried, "webshot2 (file not created)")
      }
    }, error = function(e) {
      methods_tried <<- c(methods_tried, paste0("webshot2 (", e$message, ")"))
    })
  }

  # Method 4: Try system orca command
  if (!exported) {
    orca_path <- Sys.which("orca")
    if (nzchar(orca_path)) {
      tryCatch({
        # Save as JSON and use orca
        temp_json <- tempfile(fileext = ".json")
        on.exit(unlink(temp_json), add = TRUE)

        # Export plotly figure to JSON
        fig_json <- plotly::plotly_json(fig, jsonedit = FALSE)
        writeLines(fig_json, temp_json)

        result <- system2(
          orca_path,
          args = c("graph", temp_json, "-o", filename,
                   "--width", width, "--height", height,
                   "--scale", scale),
          stdout = TRUE, stderr = TRUE
        )

        if (file.exists(filename) && file.size(filename) > 0) {
          exported <- TRUE
          ccflow_log("Saved static image to: ", filename, " (via orca)")
        } else {
          methods_tried <- c(methods_tried, "orca (file not created)")
        }
      }, error = function(e) {
        methods_tried <<- c(methods_tried, paste0("orca (", e$message, ")"))
      })
    } else {
      methods_tried <- c(methods_tried, "orca (not found in PATH)")
    }
  }

  if (!exported) {
    msg <- paste0(
      "Could not export ", format, " image.\n",
      "Methods tried:\n  - ", paste(methods_tried, collapse = "\n  - "), "\n\n",
      "To fix, install one of:\n",
      "  1. Python kaleido: pip install 'kaleido<1.0'\n",
      "  2. R chromote (for SVG): install.packages('chromote')\n",
      "  3. R webshot2 (for PNG/PDF): install.packages('webshot2')\n",
      "  4. Plotly orca: https://github.com/plotly/orca"
    )
    stop(msg, call. = FALSE)
  }

  invisible(filename)
}

#' Get Figure as JSON
#'
#' @description
#' Convert a plotly figure to JSON format for further processing.
#'
#' @param fig Plotly figure object.
#' @param pretty Logical. Use pretty-printing (indentation) for JSON?
#'   Default: \code{FALSE}
#'
#' @return JSON string
#' @export
#'
#' @examples
#' \dontrun{
#' ex <- example_communication()
#' plot <- SankeyPlot$new(ex$data, ex$colors)
#' json <- to_json(plot$render())
#' }
to_json <- function(fig, pretty = FALSE) {
  plotly::plotly_json(fig, jsonedit = FALSE, pretty = pretty)
}
