# ccflowR Tutorials

These tutorials are the public, runnable examples for ccflowR. ccflowR is the Sankey/river-plot companion to [ContextNet](https://github.com/settylab/ContextNet): ContextNet prioritizes context-aware cell-cell communication paths using a context-specific gene regulatory network (GRN), ligand-target priors, sender ligand expression, and receiver receptor expression; ccflowR visualizes those paths as sender -> ligand -> receptor -> receiver flows.

RStudio users can open the `.Rmd` files in this directory. Jupyter users can open the `.ipynb` notebooks. The Rmd files are tutorial companions for interactive exploration, not package-build vignettes.

## Tutorial Map

| Tutorial | Question addressed | Main output | Files |
|----------|--------------------|-------------|-------|
| ContextNet + NicheNet | How can a single-condition ContextNet/NicheNet communication table be checked and visualized as sender -> ligand -> receptor -> receiver paths? | Input summaries, Sankey display, highlighting, custom order, export | [ipynb](tutorial-contextnet-nichenet.ipynb) / [Rmd](tutorial-contextnet-nichenet.Rmd) |
| ContextNet + MultiNicheNet | How do communication paths differ across conditions or groups? | Group-colored Sankey plots, condition-specific views, balanced top-path filtering | [ipynb](tutorial-contextnet-multinichenet.ipynb) / [Rmd](tutorial-contextnet-multinichenet.Rmd) |
| Parameter Reference | Which tuning function changes which part of the Sankey object? | Executed checks for color, alpha, node geometry, ordering, filters, dimensions, orientation | [ipynb](tutorial-parameter-guide.ipynb) / [Rmd](tutorial-parameter-guide.Rmd) |

## Running In Jupyter

Create and register the environment from the repository root:

```bash
micromamba create -n ccflowr -f environment.yml -y
micromamba run -n ccflowr R -e "remotes::install_local('.', dependencies = NA, upgrade = 'never')"
micromamba run -n ccflowr R -e "IRkernel::installspec(name = 'ccflowr_r', displayname = 'ccflowR R')"
```

The notebooks use this kernelspec:

```text
name: ccflowr_r
display_name: ccflowR R
language: R
```

Execute a notebook from the command line:

```bash
micromamba run -n ccflowr jupyter nbconvert --to notebook --execute \
  --ExecutePreprocessor.kernel_name=ccflowr_r \
  notebooks/tutorial-contextnet-nichenet.ipynb
```

## Running In RStudio

Open the companion `.Rmd` files from this directory. The Rmd setup chunks load `ccflowR`, use the same packaged example data as the notebooks, and render interactive htmlwidgets when knitted to HTML.

## Input Data Used Here

The tutorials use packaged example data:

- `example_communication()` for single-condition ContextNet/NicheNet-style paths;
- `example_multinichenet()` for condition-aware MultiNicheNet-style paths.

Both examples use the public ccflowR schema documented in the repository README: `sender`, `ligand`, `receptor`, `receiver`, optional score/expression columns for flow width, and optional `group` for conditions.

## Display And Export Expectations

Jupyter tutorials call `sankey_display_notebook()`, which displays the real ccflowR/Plotly htmlwidget and attempts a package-native static PNG preview through `sankey_save()`. The static preview is included because GitHub and untrusted notebook viewers may hide JavaScript widgets.

Static PNG/PDF/SVG export requires a working backend such as Python `plotly` plus `kaleido<1.0`, or `webshot2` plus Chrome/Chromium. The project `environment.yml` installs the Kaleido route.
