# ccflowR

Cell-Cell Communication Flow Visualization for R

## Overview

ccflowR creates interactive Sankey diagrams from cell-cell communication analysis results. The package visualizes signaling flow as a four-layer network:

**Sender Cell &rarr; Ligand &rarr; Receptor &rarr; Receiver Cell**

This representation reveals which cell types communicate, through which signaling molecules, and — in flow-conserving mode — how strongly each interaction is supported by expression data.

### Supported Frameworks

| Framework | Reader | Description |
|-----------|--------|-------------|
| [NicheNet](https://github.com/saeyslab/nichenetr) | `read_nichenet()` | Ligand-receptor prioritization |
| [MultiNicheNet](https://github.com/saeyslab/multinichenetr) | `read_multinichenet()` | Multi-condition differential analysis |
| [LIANA](https://github.com/saezlab/liana) | `read_liana()` | Ligand-receptor inference |
| [CellPhoneDB](https://github.com/ventolab/CellphoneDB) | `read_cellphonedb()` | Statistical cell-cell communication |

## Installation

```r
# Install from GitHub
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install_github("ccflow/ccflowR")
```

## Quick Start

```r
library(ccflowR)

# Load example data
ex <- example_communication()

# Create a Sankey plot
sankey_plot(ex$data, ex$colors)
```

## Usage

ccflowR provides a ggplot2-style API where you build plots by adding layers with the `+` operator:

```r
library(ccflowR)

# Load your data
data <- read_nichenet("path/to/sankey_river.csv")

# Define cell type colors
celltype_colors <- c(
  Macrophage  = "#E41A1C",
  T_cell      = "#377EB8",
  B_cell      = "#4DAF4A",
  Fibroblast  = "#984EA3",
  Endothelial = "#FF7F00"
)

# Build a publication-ready figure
p <- sankey_plot(data, celltype_colors) +
  sankey_colors(by = "receiver") +
  sankey_alpha(0.6) +
  sankey_highlight(senders = "Macrophage", dim_alpha = 0.1) +
  sankey_order(
    senders = c("Macrophage", "T_cell", "B_cell", "Fibroblast", "Endothelial"),
    receivers = c("T_cell", "B_cell", "Fibroblast", "Endothelial", "Macrophage")
  ) +
  sankey_title("Macrophage Signaling Network")

# Save
sankey_save(p, "figure.html")                              # interactive
sankey_save(p, "figure.png", width = 700, height = 900)    # static
```

### Multi-Condition Comparison (MultiNicheNet)

```r
data <- read_multinichenet("path/to/multinichenet_output.csv")

group_colors <- c(Control = "#ADD8E6", Treatment = "#F08080", Disease = "#90EE90")

p <- sankey_plot(data, celltype_colors, group_col = "group") +
  sankey_colors(by = "condition") +
  sankey_group_colors(group_colors) +
  sankey_alpha(0.6) +
  sankey_title("Condition Comparison")
```

## Key Features

### Coloring Strategies

| Strategy | Question it answers |
|----------|-------------------|
| `"sender"` (default) | Which cell types are the dominant signal **sources**? |
| `"receiver"` | Which cell types are the primary signal **targets**? |
| `"ligand"` | Which **signaling molecules** are shared across cell pairs? |
| `"condition"` | Which pathways are active in which **experimental condition**? |

### Flow Modes

| Mode | Description |
|------|-------------|
| `"flow_conserving"` (default) | Edge thickness proportional to expression levels |
| `"uniform"` | All edges same thickness (network topology only) |

Flow-conserving mode activates automatically when expression columns are present in the data.

### Customization

```r
sankey_plot(data, colors) +
  sankey_alpha(0.6) +                              # flow transparency
  sankey_highlight(senders = "Macrophage") +        # focus on specific pathways
  sankey_order(senders = c("A", "B", "C")) +       # custom node arrangement
  sankey_nodes(thickness = 25, padding = 35) +      # node appearance
  sankey_labels(size = 14) +                        # label font size
  sankey_font(family = "Helvetica") +               # font family
  sankey_layout(width = 800, height = 600) +        # plot dimensions
  sankey_orientation("vertical") +                  # vertical flow
  sankey_interactive() +                            # dropdown filters
  sankey_title("My Plot", size = 18)                # title
```

### Preprocessing

```r
# Filter by cell types
filtered <- filter_by_celltypes(data,
  include_senders = c("Macrophage", "T_cell"),
  exclude_receivers = "B_cell"
)

# Keep top interactions
top <- filter_top_interactions(data, n = 20)

# Balanced top-N per condition (MultiNicheNet)
top <- filter_top_pathways(data, n = 5, per_group = TRUE, group_col = "group")

# Filter by specific molecules
filtered <- filter_by_ligands(data, include_ligands = c("TGFB1", "IL6"))
```

### Export

| Format | Extension | Requirements |
|--------|-----------|-------------|
| HTML | `.html` | None (always available) |
| PNG | `.png` | kaleido or webshot2 |
| PDF | `.pdf` | kaleido or webshot2 |
| SVG | `.svg` | kaleido or chromote |

```r
sankey_save(p, "figure.html")
sankey_save(p, "figure.png", width = 700, height = 900, scale = 2)  # retina
sankey_save(p, "figure.svg")
```

## Data Format

ccflowR expects a data frame with these columns:

| Column | Required | Description |
|--------|----------|-------------|
| `sender` | Yes | Sending cell type |
| `receiver` | Yes | Receiving cell type |
| `ligand` | Yes | Ligand gene symbol |
| `receptor` | Yes | Receptor gene symbol |
| `scaled_activity` | No | Activity score for flow-conserving mode |
| `scaled_receptor_expression` | No | Receptor expression for flow-conserving mode |
| `group` | No | Experimental condition (for MultiNicheNet) |

Reader functions (`read_nichenet()`, `read_multinichenet()`, etc.) automatically map framework-specific column names to these standard names.

## Tutorials

Jupyter notebook tutorials (with IRkernel) are in the `notebooks/` directory:

| Tutorial | Description |
|----------|-------------|
| [NicheNet Workflow](notebooks/tutorial-contextnet-nichenet.ipynb) | Single-condition analysis from data loading to export |
| [MultiNicheNet Workflow](notebooks/tutorial-contextnet-multinichenet.ipynb) | Multi-condition comparison, per-group filtering |
| [Parameter Reference](notebooks/tutorial-parameter-guide.ipynb) | Visual catalog of every parameter with side-by-side examples |

R Markdown vignettes are also available:

```r
vignette("contextnet-nichenet", package = "ccflowR")
vignette("contextnet-multinichenet", package = "ccflowR")
```

## Example Datasets

```r
example_communication()            # 5 cell types, 15 interactions
example_multinichenet()            # 3 conditions (Control/Treatment/Disease)
example_tumor_microenvironment()   # 7 cell types, tumor microenvironment
example_large(n = 100)             # Large dataset for stress testing
```

## Dependencies

**Required:** R (>= 4.0.0), plotly, dplyr, tidyr, R6, jsonlite, htmlwidgets

**Optional:** kaleido or webshot2 (static image export), IRdisplay (Jupyter display)

## License

MIT
