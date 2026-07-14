# ccflowR

`ccflowR` visualizes cell-cell communication results as interactive river/Sankey plots in R. It was built as the visualization companion for [ContextNet](https://github.com/settylab/ContextNet), a method that uses a context-specific gene regulatory network (GRN), ligand-target priors, sender ligand expression, and receiver receptor expression to prioritize communication paths. `ccflowR` takes the path-level output from ContextNet or related tools and displays it as:

```text
sender cell -> ligand -> receptor -> receiver cell
```

The package is useful when a large ligand-receptor result needs path-level inspection: which cells send a signal, which ligand-receptor pair carries it, which cells receive it, and how strong the path appears under the chosen scoring columns. The plot is a visualization of model-supported hypotheses; it is not direct proof of secretion, receptor binding, spatial contact, or causality.

## Installation

### Direct R or RStudio

Install from the public GitHub repository:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("settylab/ccflowR", upgrade = "never")
```

Load the package and run a smoke test:

```r
library(ccflowR)

ex <- example_communication()
summarize_data(ex$data)

p <- sankey_plot(ex$data, ex$colors)
summary(p)
```

Expected smoke-test features:

- 15 interactions
- 5 sender cell types
- 5 receiver cell types
- 11 ligands
- 11 receptors
- a `sankey` object with default dimensions `600 x 900`

### Micromamba and Jupyter

The repository includes an `environment.yml` for notebook users:

```bash
micromamba create -n ccflowr -f environment.yml -y
micromamba run -n ccflowr R -e "remotes::install_local('.', dependencies = NA, upgrade = 'never')"
micromamba run -n ccflowr R -e "IRkernel::installspec(name = 'ccflowr_r', displayname = 'ccflowR R')"
```

Run a notebook from the repository root:

```bash
micromamba run -n ccflowr jupyter nbconvert --to notebook --execute \
  --ExecutePreprocessor.kernel_name=ccflowr_r \
  notebooks/tutorial-contextnet-nichenet.ipynb
```

The notebooks are committed with the `ccflowr_r` kernel name and the display name `ccflowR R`.

### Static Export Dependencies

Interactive HTML export works with the required R dependencies. Static PNG/PDF/SVG export needs one working image backend:

- Python `plotly` plus `kaleido<1.0`, installed in the Python used by `reticulate`
- `webshot2` plus Chrome/Chromium
- Plotly orca on `PATH`

The provided `environment.yml` installs Python `plotly` and `kaleido<1.0`. If static export fails, check `RETICULATE_PYTHON`; a stale value can point R to the wrong Python environment.

## Input Structure

`ccflowR` expects one row per communication path. Reader functions standardize common formats, but the internal plotting schema is:

| Column | Required | Meaning | Used For |
|--------|----------|---------|----------|
| `sender` | yes | Cell type or cell state producing the ligand | first plot layer and sender coloring |
| `ligand` | yes | Ligand gene or ligand complex | second plot layer |
| `receptor` | yes for 4-layer plots | Receptor gene or receptor complex | third plot layer |
| `receiver` | yes | Cell type or cell state receiving the signal | final plot layer and receiver coloring |
| `scaled_activity` | optional | Ligand/path activity or prioritization score | weighted edge widths |
| `scaled_ligand_expression` | optional | Sender-side ligand expression or differential expression | sender -> ligand edge weights |
| `scaled_receptor_expression` | optional | Receiver-side receptor expression or differential expression | receptor -> receiver edge weights |
| `group` | optional | Condition, sample group, contrast, or time point | multi-condition coloring |

By default, `flow = "auto"` uses weighted edge widths when `scaled_activity`, `scaled_ligand_expression`, and `scaled_receptor_expression` are all present. If any required weight column is absent, `ccflowR` draws equal-width paths without warning so the plot still shows topology. Use `flow = "weighted"` or `flow = "flow_conserving"` to request weighted widths explicitly, and use `flow = "uniform"` when the plot should show topology only.

For ligand-only displays, set `receptor_col = NULL`; the plot becomes:

```text
sender cell -> ligand -> receiver cell
```

### ContextNet Input

ContextNet can write a `sankey_river.csv`-style table after ligand prioritization. Read it with:

```r
data <- read_nichenet("path/to/sankey_river.csv")
```

ContextNet-derived tables should include `sender`, `receiver`, `ligand`, `receptor`, and score/expression columns such as `scaled_activity`, `scaled_ligand_expression`, and `scaled_receptor_expression`.

### MultiNicheNet Input

MultiNicheNet-style tables include a condition column and differential expression scores. `read_multinichenet()` maps the usual columns onto the ccflowR schema:

```r
data <- read_multinichenet("path/to/multinichenet_output.csv")
table(data$group)
```

By default:

- `prioritization_score` becomes `scaled_activity`
- `scaled_lfc_ligand` becomes `scaled_ligand_expression`
- `scaled_lfc_receptor` becomes `scaled_receptor_expression`

## Basic Use

```r
library(ccflowR)

ex <- example_communication()
data <- ex$data
celltype_colors <- ex$colors

p <- sankey_plot(data, celltype_colors) +
  sankey_colors(by = "receiver") +
  sankey_highlight(senders = "Macrophage", dim_alpha = 0.1) +
  sankey_alpha(0.6) +
  sankey_title("Macrophage signaling paths")

p
```

In Jupyter notebooks, use the package-native display helper so saved notebooks include the interactive htmlwidget and, when available, a static PNG preview:

```r
sankey_display_notebook(p, width = 760, height = 520, static = TRUE)
```

## Export

```r
sankey_save(p, "figure.html")
sankey_save(p, "figure.png", width = 700, height = 900, scale = 2)
sankey_pdf(p, "figure.pdf")
```

| Format | Extension | Requirements |
|--------|-----------|--------------|
| Interactive HTML | `.html` | required R dependencies only |
| PNG | `.png` | Kaleido or webshot2/Chrome |
| PDF | `.pdf` | Kaleido or webshot2/Chrome |
| SVG | `.svg` | Kaleido or chromote |

## Tutorials

Tutorial source files live under `notebooks/`. RStudio users can open the `.Rmd` companions directly; Jupyter users can open the `.ipynb` notebooks.

| Tutorial | Question Addressed | Main Output | Files |
|----------|--------------------|-------------|-------|
| ContextNet + NicheNet | How can a single-condition ContextNet/NicheNet communication table be inspected as sender -> ligand -> receptor -> receiver paths? | Single-condition Sankey plots, highlighting, ordering, export | [`ipynb`](notebooks/tutorial-contextnet-nichenet.ipynb), [`Rmd`](notebooks/tutorial-contextnet-nichenet.Rmd) |
| ContextNet + MultiNicheNet | How do communication paths differ across groups or conditions? | Group-colored Sankey plots and per-condition comparisons | [`ipynb`](notebooks/tutorial-contextnet-multinichenet.ipynb), [`Rmd`](notebooks/tutorial-contextnet-multinichenet.Rmd) |
| Parameter Reference | Which visual parameter changes which part of the Sankey plot? | Executed parameter checks and side-by-side visual examples | [`ipynb`](notebooks/tutorial-parameter-guide.ipynb), [`Rmd`](notebooks/tutorial-parameter-guide.Rmd) |

See [`notebooks/README.md`](notebooks/README.md) for tutorial-running notes and the expected notebook kernel.

## Troubleshooting

| Symptom | Likely Cause | What To Try |
|---------|--------------|-------------|
| `Missing required columns` | Input table does not use ccflowR column names | Use `read_nichenet()`, `read_multinichenet()`, or pass custom column names to a reader |
| All flows have equal width | Auto mode did not find all weight columns, or uniform mode was requested | Add `scaled_activity`, `scaled_ligand_expression`, and `scaled_receptor_expression`, or use `flow = "uniform"` intentionally |
| Static PNG export fails | Kaleido, Python plotly, Chrome, or webshot2 is unavailable | Use the micromamba environment, install `kaleido<1.0` plus Python `plotly`, or configure `webshot2` with Chrome |
| Jupyter shows no plot | Kernel is not the ccflowR R environment or widget output is hidden | Select `ccflowR R` and use `sankey_display_notebook()` |
| `RETICULATE_PYTHON` points to a missing Python | R is trying to use a stale Python path | Unset `RETICULATE_PYTHON` or point it to the micromamba environment Python |

## License

MIT
