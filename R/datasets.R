#' @title Example Datasets
#' @description Example data generators for demonstrating ccflowR functionality
#' @name datasets
NULL

#' Example Cell-Cell Communication Data
#'
#' @description
#' Generate an example dataset for demonstrations. Contains 5 cell types
#' and 15 interactions with activity and receptor-side weights for
#' flow-conserving mode.
#' Includes shared ligands (TGFB1, TNF, IL6) and shared receptors to
#' create convergence/divergence patterns in the Sankey diagram, and a
#' wide weight range to clearly show flow-conserving vs uniform modes.
#'
#' @return List with:
#'   \itemize{
#'     \item data: Data frame with sender, receiver, ligand, receptor, activity,
#'       scaled_activity, and scaled_receptor_expression columns
#'     \item colors: Named vector of colors for cell types
#'   }
#' @export
#'
#' @examples
#' ex <- example_communication()
#' head(ex$data)
#' ex$colors
example_communication <- function() {
  data <- data.frame(
    sender = c(
      # TGFB1 hub: 2 senders, 2 receivers
      "Macrophage", "Fibroblast",
      # TNF hub: 2 senders, 2 receivers
      "Macrophage", "T_cell",
      # IL6 hub: 2 senders, 2 receivers
      "Macrophage", "Fibroblast",
      # IFNG: 1 sender, 2 receivers (divergence)
      "T_cell", "T_cell",
      # Unique pathways
      "Fibroblast", "Endothelial", "T_cell", "B_cell",
      "Macrophage", "Endothelial", "B_cell"
    ),
    receiver = c(
      "T_cell", "Endothelial",
      "Fibroblast", "Macrophage",
      "T_cell", "B_cell",
      "Macrophage", "B_cell",
      "Endothelial", "Fibroblast", "B_cell", "T_cell",
      "Endothelial", "T_cell", "Macrophage"
    ),
    ligand = c(
      "TGFB1", "TGFB1",
      "TNF", "TNF",
      "IL6", "IL6",
      "IFNG", "IFNG",
      "VEGFA", "PDGFB", "IL2", "IL4",
      "CCL2", "CXCL12", "IL10"
    ),
    receptor = c(
      "TGFBR1", "TGFBR1",
      "TNFRSF1A", "TNFRSF1A",
      "IL6R", "IL6R",
      "IFNGR1", "IFNGR1",
      "KDR", "PDGFRB", "IL2RA", "IL4R",
      "CCR2", "CXCR4", "IL10R"
    ),
    activity = c(
      0.90, 0.52, 0.82, 0.40, 0.75, 0.33,
      0.85, 0.23, 0.93, 0.58, 0.48, 0.15,
      0.62, 0.28, 0.20
    ),
    scaled_activity = c(
      0.92, 0.55, 0.85, 0.42, 0.78, 0.35,
      0.88, 0.25, 0.95, 0.60, 0.50, 0.18,
      0.65, 0.30, 0.22
    ),
    scaled_receptor_expression = c(
      0.40, 0.85, 0.30, 0.75, 0.90, 0.60,
      0.25, 0.70, 0.45, 0.80, 0.35, 0.65,
      0.50, 0.55, 0.72
    ),
    stringsAsFactors = FALSE
  )

  colors <- c(
    Macrophage = "#E41A1C",
    T_cell = "#377EB8",
    Fibroblast = "#4DAF4A",
    Endothelial = "#984EA3",
    B_cell = "#FF7F00"
  )

  list(data = data, colors = colors)
}

#' Example MultiNicheNet-Style Data
#'
#' @description
#' Generate example data with group/condition information, similar to
#' MultiNicheNet output format. Includes prioritization scores and
#' path-level prioritization scores and differential expression columns for
#' flow-conserving mode and optional node-color metadata.
#'
#' Features condition-specific interactions to demonstrate multi-condition
#' comparison: Control has 6 core interactions, Treatment adds 3 immune-
#' activation pathways (9 total), and Disease adds 3 more inflammatory
#' pathways (12 total). Shared ligands (TGFB1, TNF, IL6, IFNG) create
#' convergence patterns visible in the Sankey diagram.
#'
#' @return List with:
#'   \itemize{
#'     \item data: Data frame with group, prioritization_score,
#'       scaled_lfc_ligand, and scaled_lfc_receptor columns
#'     \item colors: Named vector of cell type colors
#'     \item group_colors: Named vector of group colors
#'   }
#' @export
#'
#' @examples
#' ex <- example_multinichenet()
#' table(ex$data$group)
example_multinichenet <- function() {
  # Core interactions present in all 3 conditions (6 interactions)
  core <- data.frame(
    sender = c("Macrophage", "Macrophage", "T_cell",
               "Fibroblast", "T_cell", "Endothelial"),
    receiver = c("T_cell", "Fibroblast", "Macrophage",
                 "Endothelial", "B_cell", "Fibroblast"),
    ligand = c("TGFB1", "TNF", "IFNG",
               "VEGFA", "IL2", "PDGFB"),
    receptor = c("TGFBR1", "TNFRSF1A", "IFNGR1",
                 "KDR", "IL2RA", "PDGFRB"),
    stringsAsFactors = FALSE
  )

  # Immune-activation interactions: Treatment + Disease only (3 interactions)
  # These create shared ligand hubs (TGFB1, TNF, IL6)
  activated <- data.frame(
    sender = c("Fibroblast", "T_cell", "Macrophage"),
    receiver = c("Endothelial", "Macrophage", "T_cell"),
    ligand = c("TGFB1", "TNF", "IL6"),
    receptor = c("TGFBR1", "TNFRSF1A", "IL6R"),
    stringsAsFactors = FALSE
  )

  # Inflammatory interactions: Disease only (3 interactions)
  # These create additional shared hubs (IL6, IFNG) and a unique pathway (CCL2)
  inflammatory <- data.frame(
    sender = c("Fibroblast", "T_cell", "Macrophage"),
    receiver = c("B_cell", "B_cell", "Endothelial"),
    ligand = c("IL6", "IFNG", "CCL2"),
    receptor = c("IL6R", "IFNGR1", "CCR2"),
    stringsAsFactors = FALSE
  )

  # --- Control: 6 core interactions (moderate baseline) ---
  ctrl <- core
  ctrl$group <- "Control"
  ctrl$prioritization_score <- c(0.45, 0.40, 0.50, 0.42, 0.38, 0.35)
  ctrl$scaled_lfc_ligand    <- c(0.40, 0.35, 0.45, 0.38, 0.32, 0.30)
  ctrl$scaled_lfc_receptor  <- c(0.50, 0.45, 0.55, 0.48, 0.42, 0.40)

  # --- Treatment: 6 core + 3 activated = 9 interactions (upregulated) ---
  trt_core <- core
  trt_core$group <- "Treatment"
  trt_core$prioritization_score <- c(0.72, 0.65, 0.80, 0.55, 0.60, 0.48)
  trt_core$scaled_lfc_ligand    <- c(0.68, 0.60, 0.75, 0.50, 0.55, 0.42)
  trt_core$scaled_lfc_receptor  <- c(0.75, 0.70, 0.82, 0.58, 0.62, 0.52)

  trt_new <- activated
  trt_new$group <- "Treatment"
  trt_new$prioritization_score <- c(0.58, 0.52, 0.68)
  trt_new$scaled_lfc_ligand    <- c(0.52, 0.48, 0.62)
  trt_new$scaled_lfc_receptor  <- c(0.60, 0.55, 0.70)

  # --- Disease: 6 core + 3 activated + 3 inflammatory = 12 interactions ---
  dis_core <- core
  dis_core$group <- "Disease"
  dis_core$prioritization_score <- c(0.90, 0.85, 0.92, 0.75, 0.70, 0.62)
  dis_core$scaled_lfc_ligand    <- c(0.88, 0.82, 0.90, 0.70, 0.65, 0.58)
  dis_core$scaled_lfc_receptor  <- c(0.85, 0.80, 0.88, 0.72, 0.68, 0.60)

  dis_activated <- activated
  dis_activated$group <- "Disease"
  dis_activated$prioritization_score <- c(0.78, 0.72, 0.88)
  dis_activated$scaled_lfc_ligand    <- c(0.75, 0.68, 0.85)
  dis_activated$scaled_lfc_receptor  <- c(0.72, 0.70, 0.82)

  dis_new <- inflammatory
  dis_new$group <- "Disease"
  dis_new$prioritization_score <- c(0.65, 0.58, 0.80)
  dis_new$scaled_lfc_ligand    <- c(0.60, 0.55, 0.78)
  dis_new$scaled_lfc_receptor  <- c(0.62, 0.52, 0.75)

  data <- rbind(ctrl, trt_core, trt_new, dis_core, dis_activated, dis_new)
  data$activity <- data$prioritization_score

  colors <- c(
    Macrophage = "#E41A1C",
    T_cell = "#377EB8",
    Fibroblast = "#4DAF4A",
    Endothelial = "#984EA3",
    B_cell = "#FF7F00"
  )

  group_colors <- c(
    Control = "#66C2A5",
    Treatment = "#FC8D62",
    Disease = "#8DA0CB"
  )

  list(data = data, colors = colors, group_colors = group_colors)
}

#' Example Tumor Microenvironment Data
#'
#' @description
#' Generate example data representing cell-cell communication in a tumor
#' microenvironment, with cancer cells, immune cells, and stromal cells.
#'
#' @return List with:
#'   \itemize{
#'     \item data: Data frame with tumor-relevant interactions
#'     \item colors: Named vector of cell type colors
#'   }
#' @export
#'
#' @examples
#' ex <- example_tumor_microenvironment()
#' unique(ex$data$sender)
example_tumor_microenvironment <- function() {
  data <- data.frame(
    sender = c(
      # Tumor cell signaling
      "Tumor_cell", "Tumor_cell", "Tumor_cell", "Tumor_cell",
      # TAM signaling
      "TAM", "TAM", "TAM",
      # CAF signaling
      "CAF", "CAF", "CAF",
      # T cell signaling
      "CD8_T_cell", "CD8_T_cell", "Treg",
      # NK signaling
      "NK_cell", "NK_cell"
    ),
    receiver = c(
      # Tumor receives from various
      "TAM", "CAF", "Endothelial", "CD8_T_cell",
      # TAM signals to
      "Tumor_cell", "CD8_T_cell", "Treg",
      # CAF signals to
      "Tumor_cell", "Endothelial", "TAM",
      # T cells signal to
      "Tumor_cell", "TAM", "CD8_T_cell",
      # NK signals to
      "Tumor_cell", "TAM"
    ),
    ligand = c(
      "VEGFA", "TGFB1", "PDGFB", "PD-L1",
      "IL10", "ARG1", "CCL22",
      "HGF", "CXCL12", "IL6",
      "IFNG", "GZMB", "IL10",
      "TRAIL", "IFNG"
    ),
    receptor = c(
      "KDR", "TGFBR2", "PDGFRB", "PD1",
      "IL10R", "CD247", "CCR4",
      "MET", "CXCR4", "IL6R",
      "IFNGR1", "GZMB_receptor", "IL10R",
      "TRAILR1", "IFNGR1"
    ),
    activity = c(
      0.95, 0.88, 0.82, 0.90,
      0.75, 0.70, 0.68,
      0.85, 0.78, 0.72,
      0.65, 0.60, 0.55,
      0.58, 0.52
    ),
    stringsAsFactors = FALSE
  )

  colors <- c(
    Tumor_cell = "#D62728",   # Red for tumor
    TAM = "#9467BD",          # Purple for TAMs
    CAF = "#8C564B",          # Brown for CAFs
    CD8_T_cell = "#2CA02C",   # Green for cytotoxic T cells
    Treg = "#17BECF",         # Cyan for Tregs
    NK_cell = "#BCBD22",      # Yellow-green for NK
    Endothelial = "#E377C2"   # Pink for endothelial
  )

  list(data = data, colors = colors)
}

#' Example Large Dataset
#'
#' @description
#' Generate a larger example dataset for testing performance and
#' complex visualizations.
#'
#' @param n_interactions Number of interactions to generate
#' @param n_celltypes Number of cell types
#'
#' @return List with data and colors
#' @export
#'
#' @examples
#' ex <- example_large(n_interactions = 50)
#' nrow(ex$data)
example_large <- function(n_interactions = 100, n_celltypes = 8) {
  # Generate cell type names
  cell_types <- paste0("CellType_", LETTERS[1:min(n_celltypes, 26)])

  # Generate ligand/receptor names
  ligands <- paste0("LIG", 1:20)
  receptors <- paste0("REC", 1:20)

  # Generate random interactions
  data <- data.frame(
    sender = sample(cell_types, n_interactions, replace = TRUE),
    receiver = sample(cell_types, n_interactions, replace = TRUE),
    ligand = sample(ligands, n_interactions, replace = TRUE),
    receptor = sample(receptors, n_interactions, replace = TRUE),
    activity = stats::runif(n_interactions, 0.1, 1.0),
    stringsAsFactors = FALSE
  )

  # Generate colors
  palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
               "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
               "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5",
               "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5")
  colors <- setNames(palette[1:length(cell_types)], cell_types)

  list(data = data, colors = colors)
}
