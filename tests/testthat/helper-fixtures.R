# Test Fixtures for ccflowR
# Reusable test data and helper functions

#' Check if pandoc is available
#' @return Logical indicating if pandoc is installed
has_pandoc <- function() {
 rmarkdown::pandoc_available()
}

#' Skip test if pandoc is not available
skip_if_no_pandoc <- function() {
  if (!has_pandoc()) {
    testthat::skip("Pandoc not available")
  }
}

#' Small test data fixture
#' @return List with data and colors
fixture_small_data <- function() {
 data <- data.frame(
    sender = c("CellA", "CellA", "CellB"),
    receiver = c("CellB", "CellC", "CellC"),
    ligand = c("LIG1", "LIG2", "LIG1"),
    receptor = c("REC1", "REC2", "REC1"),
    activity = c(0.8, 0.6, 0.9),
    stringsAsFactors = FALSE
  )

  colors <- c(CellA = "#E41A1C", CellB = "#377EB8", CellC = "#4DAF4A")

  list(data = data, colors = colors)
}

#' Large test data fixture (100+ interactions)
#' @param n Number of interactions
#' @return List with data and colors
fixture_large_data <- function(n = 150) {
  cell_types <- paste0("Cell_", LETTERS[1:10])
  ligands <- paste0("LIG", 1:20)
  receptors <- paste0("REC", 1:20)

  data <- data.frame(
    sender = sample(cell_types, n, replace = TRUE),
    receiver = sample(cell_types, n, replace = TRUE),
    ligand = sample(ligands, n, replace = TRUE),
    receptor = sample(receptors, n, replace = TRUE),
    activity = runif(n, 0.1, 1.0),
    stringsAsFactors = FALSE
  )

  colors <- setNames(
    c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
      "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5"),
    cell_types
  )

  list(data = data, colors = colors)
}

#' MultiNicheNet-style test data with groups
#' @return List with data, colors, and group_colors
fixture_multinichenet_data <- function() {
  base_data <- data.frame(
    sender = c("Macrophage", "T_cell", "Fibroblast", "Macrophage"),
    receiver = c("T_cell", "B_cell", "Macrophage", "Fibroblast"),
    ligand = c("TGFB1", "IL2", "VEGFA", "IL6"),
    receptor = c("TGFBR1", "IL2RA", "KDR", "IL6R"),
    stringsAsFactors = FALSE
  )

  # Create data for multiple groups
  groups <- c("Control", "Treatment", "Disease")
  data_list <- lapply(groups, function(grp) {
    d <- base_data
    d$group <- grp
    d$activity <- runif(nrow(d), 0.3, 1.0)
    d
  })

  data <- do.call(rbind, data_list)
  rownames(data) <- NULL

  colors <- c(
    Macrophage = "#E41A1C",
    T_cell = "#377EB8",
    Fibroblast = "#4DAF4A",
    B_cell = "#FF7F00"
  )

  group_colors <- c(
    Control = "#66C2A5",
    Treatment = "#FC8D62",
    Disease = "#8DA0CB"
  )

  list(data = data, colors = colors, group_colors = group_colors)
}

#' Single row test data
#' @return List with data and colors
fixture_single_row <- function() {
  data <- data.frame(
    sender = "CellA",
    receiver = "CellB",
    ligand = "LIG1",
    receptor = "REC1",
    activity = 0.5,
    stringsAsFactors = FALSE
  )

  colors <- c(CellA = "#E41A1C", CellB = "#377EB8")

  list(data = data, colors = colors)
}

#' Test data with unicode characters
#' @return List with data and colors
fixture_unicode_data <- function() {
  data <- data.frame(
    sender = c("Tセル", "Bセル", "マクロファージ"),
    receiver = c("Bセル", "マクロファージ", "Tセル"),
    ligand = c("IL-2α", "TNF-β", "IFN-γ"),
    receptor = c("IL2Rα", "TNFR1", "IFNGR"),
    activity = c(0.7, 0.8, 0.6),
    stringsAsFactors = FALSE
  )

  colors <- c(
    "Tセル" = "#E41A1C",
    "Bセル" = "#377EB8",
    "マクロファージ" = "#4DAF4A"
  )

  list(data = data, colors = colors)
}

#' Test data with special characters in names
#' @return List with data and colors
fixture_special_chars_data <- function() {
  data <- data.frame(
    sender = c("CD4+ T-cell", "NK/NKT cell", "B-cell (naive)"),
    receiver = c("NK/NKT cell", "B-cell (naive)", "CD4+ T-cell"),
    ligand = c("IL-2/15", "TNF-alpha", "IFN-gamma"),
    receptor = c("IL2R-beta", "TNFR1/2", "IFNGR1"),
    activity = c(0.75, 0.85, 0.65),
    stringsAsFactors = FALSE
  )

  colors <- c(
    "CD4+ T-cell" = "#E41A1C",
    "NK/NKT cell" = "#377EB8",
    "B-cell (naive)" = "#4DAF4A"
  )

  list(data = data, colors = colors)
}

#' NicheNet-style CSV content for testing readers
#' @return Character string of CSV content
fixture_nichenet_csv <- function() {
  "sender,receiver,ligand,receptor,activity
Macrophage,T_cell,TGFB1,TGFBR1,0.9
T_cell,B_cell,IL2,IL2RA,0.85
Fibroblast,Endothelial,VEGFA,KDR,0.95"
}

#' LIANA-style CSV content for testing readers
#' @return Character string of CSV content
fixture_liana_csv <- function() {
  "source,target,ligand.complex,receptor.complex,aggregate_rank
Macrophage,T_cell,TGFB1,TGFBR1,0.1
T_cell,B_cell,IL2,IL2RA,0.05
Fibroblast,Endothelial,VEGFA,KDR,0.02"
}

#' Create a temporary file with content
#' @param content File content
#' @param ext File extension
#' @return Path to temporary file
create_temp_file <- function(content, ext = "csv") {
  tmp <- tempfile(fileext = paste0(".", ext))
  writeLines(content, tmp)
  tmp
}
