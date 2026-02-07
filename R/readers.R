#' @title Data Readers
#' @description Functions for reading cell-cell communication data from various formats
#' @name readers
#' @importFrom utils read.csv head
NULL

#' Read NicheNet Output
#'
#' @description
#' Read NicheNet analysis output and prepare it for visualization.
#' Supports both simple activity-based data and flow-conserving data
#' with expression columns.
#'
#' @param file Path to CSV/TSV file or a data frame. If a data frame is
#'   provided, it is returned as-is after column renaming.
#' @param sender_col Column name for sender cell types. Default: \code{"sender"}
#' @param receiver_col Column name for receiver cell types. Default: \code{"receiver"}
#' @param ligand_col Column name for ligands. Default: \code{"ligand"}
#' @param receptor_col Column name for receptors. Default: \code{"receptor"}
#' @param activity_col Column name for activity scores. Default: \code{"scaled_activity"}.
#'   Falls back to \code{"activity"} if not found. Set to \code{NULL} to skip.
#' @param ligand_expr_col Column name for ligand expression. Default:
#'   \code{"scaled_ligand_expression"}. Used for flow-conserving edge weights.
#' @param receptor_expr_col Column name for receptor expression. Default:
#'   \code{"scaled_receptor_expression"}. Used for flow-conserving edge weights.
#' @param sep Column separator. Default: \code{NULL} (auto-detect based on
#'   file extension: comma for .csv, tab for .tsv/.txt)
#'
#' @return Data frame with standardized column names and framework attribute set to "nichenet"
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple NicheNet output
#' data <- read_nichenet("nichenet_output.csv")
#'
#' # With flow-conserving columns
#' data <- read_nichenet("nichenet_sankey.csv",
#'                       activity_col = "scaled_activity",
#'                       ligand_expr_col = "scaled_ligand_expression",
#'                       receptor_expr_col = "scaled_receptor_expression")
#' }
read_nichenet <- function(file,
                          sender_col = "sender",
                          receiver_col = "receiver",
                          ligand_col = "ligand",
                          receptor_col = "receptor",
                          activity_col = "scaled_activity",
                          ligand_expr_col = "scaled_ligand_expression",
                          receptor_expr_col = "scaled_receptor_expression",
                          sep = NULL) {

  # Read file or use data frame
  if (is.data.frame(file)) {
    data <- file
  } else {
    data <- read_delimited(file, sep)
  }

  # Rename columns to standard names
  col_map <- list(
    sender = sender_col,
    receiver = receiver_col,
    ligand = ligand_col,
    receptor = receptor_col
  )

  data <- rename_columns(data, col_map)

  # Handle activity column with fallback
  if (!is.null(activity_col)) {
    if (activity_col %in% colnames(data)) {
      if (activity_col != "scaled_activity") {
        data$scaled_activity <- data[[activity_col]]
      }
    } else if ("activity" %in% colnames(data)) {
      data$scaled_activity <- data[["activity"]]
      ccflow_log("Using 'activity' column as scaled_activity")
    }
  }

  # Handle expression columns
  if (!is.null(ligand_expr_col) && ligand_expr_col %in% colnames(data)) {
    if (ligand_expr_col != "scaled_ligand_expression") {
      data$scaled_ligand_expression <- data[[ligand_expr_col]]
    }
  }

  if (!is.null(receptor_expr_col) && receptor_expr_col %in% colnames(data)) {
    if (receptor_expr_col != "scaled_receptor_expression") {
      data$scaled_receptor_expression <- data[[receptor_expr_col]]
    }
  }

  # Set framework attribute
  attr(data, "framework") <- "nichenet"
  data
}

#' Read MultiNicheNet Output
#'
#' @description
#' Read MultiNicheNet analysis output with group/contrast information.
#' Automatically maps MultiNicheNet-specific columns (scaled_lfc_ligand, etc.)
#' to standard names for flow-conserving edge weight computation.
#'
#' @param file Path to CSV/TSV file or a data frame.
#' @param sender_col Column name for sender cell types. Default: \code{"sender"}
#' @param receiver_col Column name for receiver cell types. Default: \code{"receiver"}
#' @param ligand_col Column name for ligands. Default: \code{"ligand"}
#' @param receptor_col Column name for receptors. Default: \code{"receptor"}
#' @param group_col Column name for groups/conditions. Default: \code{"group"}
#' @param activity_col Column name for activity/prioritization scores. Default:
#'   \code{"prioritization_score"} (the standard MultiNicheNet output column).
#' @param lfc_ligand_col Column name for ligand log fold-change. Default:
#'   \code{"scaled_lfc_ligand"}. Will be mapped to \code{scaled_ligand_expression}.
#' @param lfc_receptor_col Column name for receptor log fold-change. Default:
#'   \code{"scaled_lfc_receptor"}. Will be mapped to \code{scaled_receptor_expression}.
#' @param auto_map Logical. If TRUE (default), automatically map MultiNicheNet columns
#'   to standard flow-conserving column names.
#' @param sep Column separator. Default: \code{NULL} (auto-detect)
#'
#' @return Data frame with standardized column names including \code{group} and
#'   framework attribute set to "multinichenet"
#' @export
#'
#' @details
#' MultiNicheNet output uses differential expression columns (scaled_lfc_*) rather
#' than absolute expression. When \code{auto_map = TRUE}, these are mapped to the
#' standard column names expected by the sankey plot:
#' \itemize{
#'   \item \code{scaled_lfc_ligand} -> \code{scaled_ligand_expression}
#'   \item \code{scaled_lfc_receptor} -> \code{scaled_receptor_expression}
#'   \item \code{prioritization_score} -> \code{scaled_activity}
#' }
#'
#' @examples
#' \dontrun{
#' # Standard MultiNicheNet output
#' data <- read_multinichenet("multinichenet_output.csv")
#'
#' # Custom column names
#' data <- read_multinichenet("results.csv",
#'                            group_col = "contrast",
#'                            activity_col = "priority_score")
#'
#' # Disable auto-mapping to keep original column names
#' data <- read_multinichenet("results.csv", auto_map = FALSE)
#' }
read_multinichenet <- function(file,
                               sender_col = "sender",
                               receiver_col = "receiver",
                               ligand_col = "ligand",
                               receptor_col = "receptor",
                               group_col = "group",
                               activity_col = "prioritization_score",
                               lfc_ligand_col = "scaled_lfc_ligand",
                               lfc_receptor_col = "scaled_lfc_receptor",
                               auto_map = TRUE,
                               sep = NULL) {

  # Read file or use data frame
  if (is.data.frame(file)) {
    data <- file
  } else {
    data <- read_delimited(file, sep)
  }

  # Rename columns to standard names
  col_map <- list(
    sender = sender_col,
    receiver = receiver_col,
    ligand = ligand_col,
    receptor = receptor_col,
    group = group_col
  )

  data <- rename_columns(data, col_map)

  # Auto-map MultiNicheNet columns to standard flow-conserving names
  if (auto_map) {
    # Map prioritization_score to scaled_activity
    if (!is.null(activity_col) && activity_col %in% colnames(data)) {
      data$scaled_activity <- data[[activity_col]]
    }

    # Map scaled_lfc_ligand to scaled_ligand_expression
    if (!is.null(lfc_ligand_col) && lfc_ligand_col %in% colnames(data)) {
      data$scaled_ligand_expression <- data[[lfc_ligand_col]]
    }

    # Map scaled_lfc_receptor to scaled_receptor_expression
    if (!is.null(lfc_receptor_col) && lfc_receptor_col %in% colnames(data)) {
      data$scaled_receptor_expression <- data[[lfc_receptor_col]]
    }

    ccflow_log("Mapped MultiNicheNet columns to standard flow-conserving names")
  } else {
    # Just add activity column without mapping
    if (!is.null(activity_col) && activity_col %in% colnames(data)) {
      data$activity <- data[[activity_col]]
    }
  }

  # Set framework attribute
  attr(data, "framework") <- "multinichenet"
  data
}

#' Read LIANA Output
#'
#' @description
#' Read LIANA cell-cell communication analysis output.
#'
#' @param file Path to CSV/TSV file or a data frame.
#' @param sender_col Column name for sender cell types. Default: \code{"source"}
#'   (LIANA's standard column name).
#' @param receiver_col Column name for receiver cell types. Default: \code{"target"}
#'   (LIANA's standard column name).
#' @param ligand_col Column name for ligands. Default: \code{"ligand.complex"}
#' @param receptor_col Column name for receptors. Default: \code{"receptor.complex"}
#' @param score_col Column name for interaction scores. Default:
#'   \code{"aggregate_rank"}. This is inverted to create an activity score
#'   (lower rank = higher activity).
#' @param sep Column separator. Default: \code{NULL} (auto-detect)
#'
#' @return Data frame with standardized column names
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_liana("liana_results.csv")
#' }
read_liana <- function(file,
                       sender_col = "source",
                       receiver_col = "target",
                       ligand_col = "ligand.complex",
                       receptor_col = "receptor.complex",
                       score_col = "aggregate_rank",
                       sep = NULL) {

  # Read file or use data frame
  if (is.data.frame(file)) {
    data <- file
  } else {
    data <- read_delimited(file, sep)
  }

  # Rename columns
  col_map <- list(
    sender = sender_col,
    receiver = receiver_col,
    ligand = ligand_col,
    receptor = receptor_col
  )

  data <- rename_columns(data, col_map)

  # Add activity from score column
  if (!is.null(score_col) && score_col %in% colnames(data)) {
    # LIANA rank is lower = better, so invert for visualization
    data$activity <- 1 / (data[[score_col]] + 0.001)
  }

  # Set framework attribute
  attr(data, "framework") <- "liana"
  data
}

#' Read CellPhoneDB Output
#'
#' @description
#' Read CellPhoneDB analysis output.
#'
#' @param file Path to CSV/TSV file or a data frame.
#' @param means_file Path to means file for activity values. Default: \code{NULL}
#' @param sep Column separator. Default: \code{NULL} (auto-detect)
#'
#' @return Data frame with standardized column names
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_cellphonedb("significant_means.txt")
#' }
read_cellphonedb <- function(file, means_file = NULL, sep = NULL) {

  # Read main file
  if (is.data.frame(file)) {
    data <- file
  } else {
    data <- read_delimited(file, sep)
  }

  # CellPhoneDB has a specific format - need to reshape
  # Typically: interacting_pair, partner_a, partner_b, cell_pair columns

  # Try to detect and reshape the format
  if ("interacting_pair" %in% colnames(data)) {
    data <- reshape_cellphonedb(data)
  }

  # Set framework attribute
  attr(data, "framework") <- "cellphonedb"
  data
}

# Internal helper functions

#' Read Delimited File (Internal)
#' @keywords internal
read_delimited <- function(file, sep = NULL) {
  # Auto-detect separator
  if (is.null(sep)) {
    # Read first line to detect
    first_line <- readLines(file, n = 1)
    if (grepl("\t", first_line)) {
      sep <- "\t"
    } else if (grepl(",", first_line)) {
      sep <- ","
    } else {
      sep <- ","  # default
    }
  }

  utils::read.csv(file, sep = sep, stringsAsFactors = FALSE)
}

#' Rename Columns (Internal)
#' @keywords internal
rename_columns <- function(data, col_map) {
  for (new_name in names(col_map)) {
    old_name <- col_map[[new_name]]
    if (old_name %in% colnames(data) && old_name != new_name) {
      colnames(data)[colnames(data) == old_name] <- new_name
    }
  }
  data
}

#' Reshape CellPhoneDB Data (Internal)
#' @keywords internal
reshape_cellphonedb <- function(data) {
  # CellPhoneDB format: rows are interactions, columns include cell pair columns
  # Need to identify cell pair columns and reshape to long format

  # Get interaction info columns
  info_cols <- c("id_cp_interaction", "interacting_pair", "partner_a", "partner_b",
                 "gene_a", "gene_b", "secreted", "receptor_a", "receptor_b",
                 "annotation_strategy", "is_integrin")
  info_cols <- info_cols[info_cols %in% colnames(data)]

  # Get cell pair columns (format: "celltype1|celltype2")
  all_cols <- colnames(data)
  pair_cols <- all_cols[grepl("\\|", all_cols)]

  if (length(pair_cols) == 0) {
    warning("Could not detect cell pair columns in CellPhoneDB format")
    return(data)
  }

  # Reshape to long format
  result_rows <- list()

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]

    # Parse interaction pair
    pair <- row$interacting_pair
    parts <- strsplit(pair, "_")[[1]]
    ligand <- parts[1]
    receptor <- if (length(parts) > 1) parts[2] else parts[1]

    for (pair_col in pair_cols) {
      value <- row[[pair_col]]
      if (!is.na(value) && value > 0) {
        cell_pair <- strsplit(pair_col, "\\|")[[1]]
        result_rows[[length(result_rows) + 1]] <- data.frame(
          sender = cell_pair[1],
          receiver = cell_pair[2],
          ligand = ligand,
          receptor = receptor,
          activity = value,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(result_rows) > 0) {
    do.call(rbind, result_rows)
  } else {
    data.frame(
      sender = character(0),
      receiver = character(0),
      ligand = character(0),
      receptor = character(0),
      activity = numeric(0)
    )
  }
}
