#' @title Utility Functions
#' @description Helper utilities for ccflowR
#' @name utils
NULL

#' Validate Required Columns in Data Frame
#'
#' @description
#' Check that a data frame contains all required columns. Raises an error
#' with a helpful message if any columns are missing.
#'
#' @param data A data frame to validate
#' @param required_columns Character vector of required column names
#' @param data_name Name of the data frame for error messages (default: "data")
#'
#' @return TRUE invisibly if validation passes
#' @export
#'
#' @examples
#' df <- data.frame(sender = "A", receiver = "B", ligand = "L1")
#' validate_required_columns(df, c("sender", "receiver", "ligand"))
validate_required_columns <- function(data, required_columns, data_name = "data") {
  if (!is.data.frame(data)) {
    stop(sprintf("'%s' must be a data frame", data_name), call. = FALSE)
  }

  missing_cols <- setdiff(required_columns, colnames(data))

  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in '%s': %s\nAvailable columns: %s",
        data_name,
        paste(missing_cols, collapse = ", "),
        paste(colnames(data), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Get Unique Values Preserving Order
#'
#' @description
#' Extract unique values from a vector while preserving the order of
#' first occurrence.
#'
#' @param x A vector
#'
#' @return Vector of unique values in order of first occurrence
#' @export
#'
#' @examples
#' get_unique_values(c("B", "A", "B", "C", "A"))
#' # Returns: c("B", "A", "C")
get_unique_values <- function(x) {
  unique(x)
}

#' Auto-detect Expression Columns
#'
#' @description
#' Automatically detect ligand/receptor expression or activity columns
#' based on common naming patterns.
#'
#' @param data A data frame
#' @param type One of "ligand", "receptor", or "activity"
#'
#' @return Column name if found, NULL otherwise
#' @export
#'
#' @examples
#' df <- data.frame(ligand_expr = 1, receptor_expr = 2)
#' auto_detect_expression_columns(df, "ligand")
auto_detect_expression_columns <- function(data, type = c("ligand", "receptor", "activity")) {
  type <- match.arg(type)
  cols <- colnames(data)

  patterns <- switch(
    type,
    ligand = c("ligand_expr", "ligand_expression", "lig_expr", "ligand_activity"),
    receptor = c("receptor_expr", "receptor_expression", "rec_expr", "receptor_activity"),
    activity = c("activity", "score", "weight", "value", "strength", "priority")
  )

  for (pattern in patterns) {
    matches <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      return(matches[1])
    }
  }

  NULL
}

#' Merge Dictionaries/Lists
#'
#' @description
#' Merge two named lists, with values from the second list taking
#' precedence over the first.
#'
#' @param base Base list
#' @param override Override list (values take precedence)
#'
#' @return Merged list
#' @export
#'
#' @examples
#' merge_lists(list(a = 1, b = 2), list(b = 3, c = 4))
#' # Returns: list(a = 1, b = 3, c = 4)
merge_lists <- function(base, override) {
  if (is.null(base)) base <- list()
  if (is.null(override)) override <- list()

  result <- base
  for (name in names(override)) {
    result[[name]] <- override[[name]]
  }
  result
}

#' Ensure Value is a List
#'
#' @description
#' Convert a single value to a list if it isn't already.
#'
#' @param x Value to convert
#'
#' @return A list
#' @export
#'
#' @examples
#' ensure_list("a")
#' ensure_list(c("a", "b"))
#' ensure_list(list("a", "b"))
ensure_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.list(x) && !is.data.frame(x)) return(x)
  as.list(x)
}

#' Ensure Value is a Vector
#'
#' @description
#' Convert a single value to a vector if it isn't already.
#'
#' @param x Value to convert
#'
#' @return A vector
#' @export
#'
#' @examples
#' ensure_vector("a")
#' ensure_vector(c("a", "b"))
ensure_vector <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x) && !is.data.frame(x)) return(unlist(x))
  as.vector(x)
}

#' Extract Cell Types from Data
#'
#' @description
#' Extract unique sender and receiver cell types from communication data.
#'
#' @param data Data frame with sender and receiver columns
#' @param sender_col Name of sender column (default: "sender")
#' @param receiver_col Name of receiver column (default: "receiver")
#'
#' @return List with 'senders', 'receivers', and 'all' cell types
#' @export
#'
#' @examples
#' df <- data.frame(sender = c("A", "B"), receiver = c("B", "C"))
#' extract_celltypes(df)
extract_celltypes <- function(data, sender_col = "sender", receiver_col = "receiver") {
  validate_required_columns(data, c(sender_col, receiver_col))

  senders <- unique(data[[sender_col]])
  receivers <- unique(data[[receiver_col]])
  all_types <- unique(c(senders, receivers))

  list(
    senders = senders,
    receivers = receivers,
    all = all_types
  )
}

#' Get All Unique Cell Types from Data
#'
#' @description
#' Extract all unique cell types (senders and receivers combined) from
#' communication data. This is a convenience function that returns a simple
#' character vector, unlike \code{extract_celltypes} which returns a list.
#'
#' @param data Data frame with sender and receiver columns
#' @param sender_col Name of sender column (default: "sender")
#' @param receiver_col Name of receiver column (default: "receiver")
#'
#' @return Character vector of unique cell types
#' @export
#'
#' @examples
#' df <- data.frame(sender = c("A", "B"), receiver = c("B", "C"))
#' get_all_celltypes(df)  # Returns: c("A", "B", "C")
#'
#' # Compare with extract_celltypes which returns a list
#' extract_celltypes(df)  # Returns: list(senders = ..., receivers = ..., all = ...)
get_all_celltypes <- function(data, sender_col = "sender", receiver_col = "receiver") {
  validate_required_columns(data, c(sender_col, receiver_col))
  unique(c(data[[sender_col]], data[[receiver_col]]))
}

#' Extract Ligands from Data
#'
#' @description
#' Extract unique ligand molecules from communication data.
#'
#' @param data Data frame with ligand column
#' @param ligand_col Name of ligand column (default: "ligand")
#'
#' @return Character vector of unique ligands
#' @export
#'
#' @examples
#' df <- data.frame(ligand = c("L1", "L2", "L1"))
#' extract_ligands(df)
extract_ligands <- function(data, ligand_col = "ligand") {
  validate_required_columns(data, ligand_col)
  unique(data[[ligand_col]])
}

#' Extract Receptors from Data
#'
#' @description
#' Extract unique receptor molecules from communication data.
#'
#' @param data Data frame with receptor column
#' @param receptor_col Name of receptor column (default: "receptor")
#'
#' @return Character vector of unique receptors
#' @export
#'
#' @examples
#' df <- data.frame(receptor = c("R1", "R2", "R1"))
#' extract_receptors(df)
extract_receptors <- function(data, receptor_col = "receptor") {
  validate_required_columns(data, receptor_col)
  unique(data[[receptor_col]])
}

#' Summarize Communication Data
#'
#' @description
#' Generate summary statistics for cell-cell communication data.
#'
#' @param data Data frame with communication data
#' @param sender_col Name of sender column (default: "sender")
#' @param receiver_col Name of receiver column (default: "receiver")
#' @param ligand_col Name of ligand column (default: "ligand")
#' @param receptor_col Name of receptor column (default: "receptor")
#'
#' @return List with summary statistics
#' @export
#'
#' @examples
#' df <- example_communication()$data
#' summarize_data(df)
summarize_data <- function(data,
                           sender_col = "sender",
                           receiver_col = "receiver",
                           ligand_col = "ligand",
                           receptor_col = "receptor") {
  validate_required_columns(data, c(sender_col, receiver_col, ligand_col, receptor_col))

  list(
    n_interactions = nrow(data),
    n_senders = length(unique(data[[sender_col]])),
    n_receivers = length(unique(data[[receiver_col]])),
    n_ligands = length(unique(data[[ligand_col]])),
    n_receptors = length(unique(data[[receptor_col]])),
    senders = unique(data[[sender_col]]),
    receivers = unique(data[[receiver_col]]),
    ligands = unique(data[[ligand_col]]),
    receptors = unique(data[[receptor_col]])
  )
}

#' Log Message (Internal)
#'
#' @description
#' Print a message if verbose mode is enabled.
#'
#' @param ... Message components to print
#'
#' @return NULL invisibly
#' @keywords internal
ccflow_log <- function(...) {

  if (ccflow_settings()$verbose()) {
    message(...)
  }
  invisible(NULL)
}

#' Check if Running in Jupyter Notebook
#'
#' @description
#' Detect if the current R session is running within a Jupyter notebook
#' using the IRkernel. This is used to determine how to properly display
#' interactive htmlwidgets.
#'
#' @return Logical. TRUE if running in a Jupyter notebook, FALSE otherwise.
#' @keywords internal
is_jupyter_notebook <- function() {
  # Check if IRkernel is loaded - this indicates we're in a Jupyter R kernel

  "IRkernel" %in% loadedNamespaces()
}

#' Display htmlwidget in Jupyter Notebook
#'
#' @description
#' Properly display an htmlwidget (like plotly) in a Jupyter notebook
#' by converting it to HTML and using IRdisplay.
#'
#' @param widget An htmlwidget object (e.g., plotly figure)
#' @param width Width in pixels (optional)
#' @param height Height in pixels (optional)
#'
#' @return The widget invisibly
#' @keywords internal
display_in_jupyter <- function(widget, width = NULL, height = NULL) {
  # Set dimensions if provided
  if (!is.null(width)) widget$width <- width
  if (!is.null(height)) widget$height <- height

  # For Jupyter notebooks, we need to explicitly publish the widget HTML
  # The repr package (used by IRkernel) handles htmlwidget conversion
  if (requireNamespace("repr", quietly = TRUE)) {
    # Get HTML representation using repr
    html_repr <- repr::repr_html(widget)
    if (!is.null(html_repr) && requireNamespace("IRdisplay", quietly = TRUE)) {
      IRdisplay::display_html(html_repr)
      return(invisible(widget))
    }
  }

  # Fallback: use print which triggers knit_print for htmlwidgets
  print(widget)
  invisible(widget)
}

#' Display Plotly Widget in Jupyter Using CDN
#'
#' @description
#' Display a plotly widget in Jupyter using CDN for plotly.js to avoid
#' size limits. Uses an iframe with srcdoc to create an isolated context
#' where scripts can execute (Jupyter sanitizes scripts in main output).
#'
#' @param fig A plotly figure object
#' @param width Width in pixels
#' @param height Height in pixels
#'
#' @return NULL invisibly
#' @keywords internal
display_plotly_cdn <- function(fig, width = 600, height = 900) {
  # Set dimensions on the figure
  fig$width <- width
  fig$height <- height

  # Simple approach: just print the figure
  # The IRkernel repr system handles plotly htmlwidgets automatically
  print(fig)
  invisible(fig)
}

#' Detect Analysis Framework
#'
#' @description
#' Automatically detect whether input data is from NicheNet or MultiNicheNet
#' based on available columns.
#'
#' @param data A data frame with communication data
#'
#' @return Character string: "nichenet", "multinichenet", or "unknown"
#' @export
#'
#' @examples
#' # NicheNet-style data
#' df <- data.frame(
#'   sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
#'   scaled_activity = 0.5, scaled_ligand_expression = 0.3,
#'   scaled_receptor_expression = 0.4
#' )
#' detect_framework(df)  # Returns "nichenet"
detect_framework <- function(data) {
  if (!is.data.frame(data)) {
    return("unknown")
  }

  cols <- colnames(data)

  # Check for MultiNicheNet-specific columns
  has_multinichenet <- all(c("scaled_lfc_ligand", "scaled_lfc_receptor",
                             "prioritization_score") %in% cols)

  # Check for NicheNet/standard columns
  # NicheNet requires direct activity and a receptor-side redistribution weight.
  # Ligand expression is optional visualization metadata.
  has_nichenet_full <- all(c("scaled_activity", "scaled_ligand_expression",
                              "scaled_receptor_expression") %in% cols)
  has_nichenet_minimal <- all(c("scaled_activity", "scaled_receptor_expression") %in% cols) &&
                          !has_multinichenet

  if (has_multinichenet && "group" %in% cols) {
    return("multinichenet")
  } else if (has_nichenet_full) {
    return("nichenet")
  } else if (has_nichenet_minimal) {
    # NicheNet with the columns required for weighted flow.
    return("nichenet")
  } else if (has_multinichenet) {
    return("multinichenet_mapped")
  } else {
    return("unknown")
  }
}

#' Validate Sankey Input Data
#'
#' @description
#' Validate that input data has the required columns for sankey plot
#' generation. Checks for base required columns and optionally for
#' weight columns needed for weighted edge widths.
#'
#' @param data A data frame with communication data
#' @param require_weights Logical. If TRUE, also check for weight columns.
#'   Default: FALSE
#' @param warn Logical. If TRUE, warn about missing or invalid optional weight
#'   values. Internal auto-mode checks set this to FALSE so plots can quietly
#'   use uniform edge widths when weights are unavailable. Default: TRUE
#' @param sender_col,ligand_col,receptor_col,receiver_col Column names for the
#'   four displayed layers.
#' @param activity_col Activity-score column used directly for sender-to-ligand
#'   width.
#' @param receptor_expr_col Receptor-side weight column used to redistribute
#'   flow downstream. Ligand expression is not required for edge widths.
#'
#' @return Logical. TRUE if data is valid for the requested mode.
#'   Returns FALSE with a warning if weight columns are missing and `warn = TRUE`.
#' @export
#'
#' @examples
#' df <- data.frame(sender = "A", receiver = "B", ligand = "L1", receptor = "R1")
#' validate_sankey_input(df)  # TRUE (base columns present)
#' validate_sankey_input(df, require_weights = TRUE)  # FALSE (weight cols missing)
validate_sankey_input <- function(data, require_weights = FALSE, warn = TRUE,
                                  sender_col = "sender",
                                  ligand_col = "ligand",
                                  receptor_col = "receptor",
                                  receiver_col = "receiver",
                                  activity_col = "scaled_activity",
                                  receptor_expr_col = "scaled_receptor_expression") {
  # Required columns for all frameworks
  required_base <- c(sender_col, ligand_col, receptor_col, receiver_col)
  required_base <- required_base[!vapply(required_base, is.null, logical(1))]

  # Check base columns
  missing_base <- setdiff(required_base, colnames(data))
  if (length(missing_base) > 0) {
    stop("Missing required columns: ", paste(missing_base, collapse = ", "),
         call. = FALSE)
  }

  if (!require_weights) {
    return(TRUE)
  }

  # Check weight columns for weighted flow
  required_weights <- c(activity_col, receptor_expr_col)
  required_weights <- required_weights[
    !vapply(required_weights, is.null, logical(1))
  ]
  missing_weights <- setdiff(required_weights, colnames(data))

  if (length(missing_weights) > 0) {
    if (warn) {
      warning("Missing weight columns for weighted flow: ",
              paste(missing_weights, collapse = ", "))
    }
    return(FALSE)
  }

  # Validate numeric ranges
  if (warn && !is.null(activity_col) && activity_col %in% colnames(data)) {
    activity <- suppressWarnings(as.numeric(data[[activity_col]]))
    if (any(!is.finite(activity))) {
      warning(activity_col, " contains NA values or other non-finite values; ",
              "they will be treated as 0")
    }
    if (any(activity < 0, na.rm = TRUE)) {
      warning(activity_col, " contains negative values; ",
              "they will be treated as 0")
    }
  }

  TRUE
}

#' Prepare Data for Sankey Plot
#'
#' @description
#' Prepare input data for sankey plotting by detecting framework,
#' mapping columns, and validating the result.
#'
#' @param data A data frame with communication data
#' @param auto_map Logical. If TRUE, automatically map MultiNicheNet columns
#'   to standard names. Default: TRUE
#'
#' @return Data frame with standardized column names
#' @export
#'
#' @examples
#' # MultiNicheNet data will have columns mapped automatically
#' df <- data.frame(
#'   sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
#'   scaled_lfc_ligand = 0.5, scaled_lfc_receptor = 0.4,
#'   prioritization_score = 0.6, group = "Disease"
#' )
#' prepared <- prepare_sankey_data(df)
prepare_sankey_data <- function(data, auto_map = TRUE) {
  framework <- detect_framework(data)

  if (auto_map && framework %in% c("multinichenet", "multinichenet_mapped")) {
    # Map MultiNicheNet columns to standard names
    # MultiNicheNet: scaled_ligand_expression = scaled_lfc_ligand (differential expression)
    if ("scaled_lfc_ligand" %in% colnames(data) &&
        !"scaled_ligand_expression" %in% colnames(data)) {
      data$scaled_ligand_expression <- data$scaled_lfc_ligand
    }
    if ("scaled_lfc_receptor" %in% colnames(data) &&
        !"scaled_receptor_expression" %in% colnames(data)) {
      data$scaled_receptor_expression <- data$scaled_lfc_receptor
    }
    if ("prioritization_score" %in% colnames(data) &&
        !"scaled_activity" %in% colnames(data)) {
      data$scaled_activity <- data$prioritization_score
    }
    ccflow_log("Mapped MultiNicheNet columns to standard names")
  }

  # Store framework as attribute
  attr(data, "framework") <- framework
  data
}
