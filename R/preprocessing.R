#' @title Preprocessing Tools
#' @description Data preprocessing and filtering functions
#' @name preprocessing
#' @importFrom dplyr filter group_by summarise ungroup slice_head arrange n
#' @importFrom stats setNames
NULL

#' Filter by Expression Levels
#'
#' @description
#' Filter interactions by ligand and/or receptor expression thresholds.
#'
#' @param data Data frame with communication data.
#' @param ligand_threshold Minimum ligand expression value. Default: \code{NULL}
#'   (no filtering).
#' @param receptor_threshold Minimum receptor expression value. Default: \code{NULL}
#'   (no filtering).
#' @param ligand_col Column name for ligand expression. Default: \code{NULL}
#'   (auto-detect).
#' @param receptor_col Column name for receptor expression. Default: \code{NULL}
#'   (auto-detect).
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
#' \dontrun{
#' filtered <- filter_by_expression(data, ligand_threshold = 0.5)
#' }
filter_by_expression <- function(data,
                                  ligand_threshold = NULL,
                                  receptor_threshold = NULL,
                                  ligand_col = NULL,
                                  receptor_col = NULL) {

  # Auto-detect expression columns if not provided
  if (is.null(ligand_col)) {
    ligand_col <- auto_detect_expression_columns(data, "ligand")
  }
  if (is.null(receptor_col)) {
    receptor_col <- auto_detect_expression_columns(data, "receptor")
  }

  result <- data

  # Filter by ligand expression
  if (!is.null(ligand_threshold) && !is.null(ligand_col) && ligand_col %in% colnames(data)) {
    result <- result[result[[ligand_col]] >= ligand_threshold, ]
    ccflow_log("Filtered by ligand expression >= ", ligand_threshold,
               ": ", nrow(result), " rows remaining")
  }

  # Filter by receptor expression
  if (!is.null(receptor_threshold) && !is.null(receptor_col) && receptor_col %in% colnames(data)) {
    result <- result[result[[receptor_col]] >= receptor_threshold, ]
    ccflow_log("Filtered by receptor expression >= ", receptor_threshold,
               ": ", nrow(result), " rows remaining")
  }

  result
}

#' Filter by Cell Types
#'
#' @description
#' Filter interactions to include or exclude specific cell types.
#'
#' @param data Data frame with communication data.
#' @param include_senders Character vector of cell types to include as senders.
#'   Default: \code{NULL} (include all).
#' @param include_receivers Character vector of cell types to include as receivers.
#'   Default: \code{NULL} (include all).
#' @param exclude_senders Character vector of cell types to exclude as senders.
#'   Default: \code{NULL} (exclude none).
#' @param exclude_receivers Character vector of cell types to exclude as receivers.
#'   Default: \code{NULL} (exclude none).
#' @param sender_col Column name for senders. Default: \code{"sender"}
#' @param receiver_col Column name for receivers. Default: \code{"receiver"}
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
#' ex <- example_communication()
#' # Keep only interactions from specific senders
#' filtered <- filter_by_celltypes(ex$data, include_senders = c("Macrophage", "T_cell"))
filter_by_celltypes <- function(data,
                                 include_senders = NULL,
                                 include_receivers = NULL,
                                 exclude_senders = NULL,
                                 exclude_receivers = NULL,
                                 sender_col = "sender",
                                 receiver_col = "receiver") {

  validate_required_columns(data, c(sender_col, receiver_col))

  result <- data

  # Include filters

  if (!is.null(include_senders)) {
    result <- result[result[[sender_col]] %in% include_senders, ]
    ccflow_log("Filtered to senders: ", paste(include_senders, collapse = ", "))
  }

  if (!is.null(include_receivers)) {
    result <- result[result[[receiver_col]] %in% include_receivers, ]
    ccflow_log("Filtered to receivers: ", paste(include_receivers, collapse = ", "))
  }

  # Exclude filters
  if (!is.null(exclude_senders)) {
    result <- result[!result[[sender_col]] %in% exclude_senders, ]
    ccflow_log("Excluded senders: ", paste(exclude_senders, collapse = ", "))
  }

  if (!is.null(exclude_receivers)) {
    result <- result[!result[[receiver_col]] %in% exclude_receivers, ]
    ccflow_log("Excluded receivers: ", paste(exclude_receivers, collapse = ", "))
  }

  ccflow_log(nrow(result), " rows remaining after cell type filtering")
  result
}

#' Filter Top Pathways
#'
#' @description
#' Keep only the top N pathways by activity/score.
#'
#' @param data Data frame with communication data.
#' @param n Number of top pathways to keep. Default: \code{10}
#' @param by Column to rank by. Default: \code{"activity"}
#' @param per_group Logical. If \code{TRUE} and group column exists, get top N
#'   per group. Default: \code{FALSE}
#' @param group_col Column name for groups. Default: \code{"group"}
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
#' ex <- example_communication()
#' top <- filter_top_pathways(ex$data, n = 5)
filter_top_pathways <- function(data,
                                 n = 10,
                                 by = "activity",
                                 per_group = FALSE,
                                 group_col = "group") {

  # Check if sorting column exists
  if (!(by %in% colnames(data))) {
    # Try to auto-detect
    by <- auto_detect_expression_columns(data, "activity")
    if (is.null(by)) {
      ccflow_log("No activity column found, returning first ", n, " rows")
      return(utils::head(data, n))
    }
  }

  if (per_group && group_col %in% colnames(data)) {
    # Get top N per group
    result <- data %>%
      dplyr::group_by(.data[[group_col]]) %>%
      dplyr::arrange(dplyr::desc(.data[[by]])) %>%
      dplyr::slice_head(n = n) %>%
      dplyr::ungroup()
  } else {
    # Get top N overall
    result <- data %>%
      dplyr::arrange(dplyr::desc(.data[[by]])) %>%
      dplyr::slice_head(n = n)
  }

  ccflow_log("Selected top ", n, " pathways by ", by)
  result
}


#' Filter Top Interactions
#'
#' @description
#' Filter to keep only the top N interactions by activity score.
#' This is a simpler alternative to \code{filter_top_pathways} that
#' works directly on rows without grouping.
#'
#' @param data Data frame with communication data.
#' @param n Number of top interactions to keep. Default: \code{20}
#' @param by Column to rank by. Default: \code{"activity"}. If column not found,
#'   returns first \code{n} rows.
#' @param descending Logical. Sort in descending order? Default: \code{TRUE}
#'   (highest values first)
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
#' ex <- example_communication()
#' top_data <- filter_top_interactions(ex$data, n = 5)
filter_top_interactions <- function(data, n = 20, by = "activity", descending = TRUE) {
  if (!(by %in% colnames(data))) {
    ccflow_log("Column '", by, "' not found. Returning first ", n, " rows.")
    return(utils::head(data, n))
  }

  if (descending) {
    data <- data[order(data[[by]], decreasing = TRUE), ]
  } else {
    data <- data[order(data[[by]], decreasing = FALSE), ]
  }

  utils::head(data, n)
}


#' Aggregate by Group
#'
#' @description
#' Aggregate interaction data by experimental groups/conditions.
#'
#' @param data Data frame with communication data.
#' @param group_col Column name for groups. Default: \code{"group"}
#' @param sender_col Column name for senders. Default: \code{"sender"}
#' @param receiver_col Column name for receivers. Default: \code{"receiver"}
#' @param ligand_col Column name for ligands. Default: \code{"ligand"}
#' @param receptor_col Column name for receptors. Default: \code{"receptor"}
#' @param agg_fun Aggregation function to apply. One of \code{"mean"} (default),
#'   \code{"sum"}, \code{"max"}, \code{"min"}, or \code{"median"}.
#' @param value_col Column to aggregate. Default: \code{"activity"}
#'
#' @return Aggregated data frame
#' @export
#'
#' @examples
#' \dontrun{
#' # Aggregate activity scores by group
#' agg_data <- aggregate_by_group(data, group_col = "condition")
#' }
aggregate_by_group <- function(data,
                                group_col = "group",
                                sender_col = "sender",
                                receiver_col = "receiver",
                                ligand_col = "ligand",
                                receptor_col = "receptor",
                                agg_fun = "mean",
                                value_col = "activity") {

  validate_required_columns(data, c(group_col, sender_col, receiver_col,
                                     ligand_col, receptor_col))

  # Define aggregation function
  agg <- switch(agg_fun,
                mean = mean,
                sum = sum,
                max = max,
                min = min,
                median = stats::median,
                mean)  # default

  # Check if value column exists
  if (!(value_col %in% colnames(data))) {
    ccflow_log("Value column '", value_col, "' not found, using count")
    value_col <- NULL
  }

  # Group and aggregate
  if (!is.null(value_col)) {
    result <- data %>%
      dplyr::group_by(
        .data[[group_col]],
        .data[[sender_col]],
        .data[[receiver_col]],
        .data[[ligand_col]],
        .data[[receptor_col]]
      ) %>%
      dplyr::summarise(
        activity = agg(.data[[value_col]], na.rm = TRUE),
        n_interactions = dplyr::n(),
        .groups = "drop"
      )
  } else {
    result <- data %>%
      dplyr::group_by(
        .data[[group_col]],
        .data[[sender_col]],
        .data[[receiver_col]],
        .data[[ligand_col]],
        .data[[receptor_col]]
      ) %>%
      dplyr::summarise(
        n_interactions = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(activity = n_interactions)
  }

  # Rename columns back to standard names
  colnames(result)[colnames(result) == group_col] <- "group"
  colnames(result)[colnames(result) == sender_col] <- "sender"
  colnames(result)[colnames(result) == receiver_col] <- "receiver"
  colnames(result)[colnames(result) == ligand_col] <- "ligand"
  colnames(result)[colnames(result) == receptor_col] <- "receptor"

  result
}

#' Filter by Ligands
#'
#' @description
#' Filter interactions to include only specific ligands.
#'
#' @param data Data frame with communication data.
#' @param include_ligands Character vector of ligands to include.
#' @param ligand_col Column name for ligands. Default: \code{"ligand"}
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
#' ex <- example_communication()
#' filtered <- filter_by_ligands(ex$data, include_ligands = c("TGFB1", "IL6"))
filter_by_ligands <- function(data, include_ligands, ligand_col = "ligand") {
  validate_required_columns(data, ligand_col)
  result <- data[data[[ligand_col]] %in% include_ligands, ]
  ccflow_log("Filtered to ligands: ", paste(include_ligands, collapse = ", "),
             " (", nrow(result), " rows)")
  result
}

#' Filter by Receptors
#'
#' @description
#' Filter interactions to include only specific receptors.
#'
#' @param data Data frame with communication data.
#' @param include_receptors Character vector of receptors to include.
#' @param receptor_col Column name for receptors. Default: \code{"receptor"}
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
#' ex <- example_communication()
#' filtered <- filter_by_receptors(ex$data, include_receptors = c("TGFBR1", "IL6R"))
filter_by_receptors <- function(data, include_receptors, receptor_col = "receptor") {
  validate_required_columns(data, receptor_col)
  result <- data[data[[receptor_col]] %in% include_receptors, ]
  ccflow_log("Filtered to receptors: ", paste(include_receptors, collapse = ", "),
             " (", nrow(result), " rows)")
  result
}

#' Remove Duplicate Interactions
#'
#' @description
#' Remove duplicate interaction entries, optionally keeping highest activity.
#'
#' @param data Data frame with communication data.
#' @param keep Which duplicate to keep. One of \code{"first"} (default),
#'   \code{"last"}, or \code{"max_activity"}.
#' @param sender_col Column name for senders. Default: \code{"sender"}
#' @param receiver_col Column name for receivers. Default: \code{"receiver"}
#' @param ligand_col Column name for ligands. Default: \code{"ligand"}
#' @param receptor_col Column name for receptors. Default: \code{"receptor"}
#' @param activity_col Column name for activity (used when keep =
#'   \code{"max_activity"}). Default: \code{"activity"}
#'
#' @return Deduplicated data frame
#' @export
#'
#' @examples
#' ex <- example_communication()
#' dedup <- remove_duplicates(ex$data)
remove_duplicates <- function(data,
                               keep = c("first", "last", "max_activity"),
                               sender_col = "sender",
                               receiver_col = "receiver",
                               ligand_col = "ligand",
                               receptor_col = "receptor",
                               activity_col = "activity") {

  keep <- match.arg(keep)
  validate_required_columns(data, c(sender_col, receiver_col, ligand_col, receptor_col))

  n_before <- nrow(data)

  if (keep == "max_activity" && activity_col %in% colnames(data)) {
    result <- data %>%
      dplyr::group_by(
        .data[[sender_col]],
        .data[[receiver_col]],
        .data[[ligand_col]],
        .data[[receptor_col]]
      ) %>%
      dplyr::arrange(dplyr::desc(.data[[activity_col]])) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
  } else if (keep == "last") {
    result <- data %>%
      dplyr::group_by(
        .data[[sender_col]],
        .data[[receiver_col]],
        .data[[ligand_col]],
        .data[[receptor_col]]
      ) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()
  } else {
    # keep == "first" or default
    result <- data %>%
      dplyr::group_by(
        .data[[sender_col]],
        .data[[receiver_col]],
        .data[[ligand_col]],
        .data[[receptor_col]]
      ) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
  }

  n_after <- nrow(result)
  ccflow_log("Removed ", n_before - n_after, " duplicates (", n_before, " -> ", n_after, ")")

  result
}
