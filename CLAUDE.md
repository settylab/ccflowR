# Claude Instructions for ccflowR

## Persona

You are a senior software engineer with deep expertise in computational biology.

## Development Installation

During development, install ccflowR from the local directory:

```r
devtools::install_local("/fh/fast/setty_m/user/yhuang2/ccflowR", force = TRUE)
```

## Project Overview

ccflowR is an R package for visualizing cell-cell communication data as interactive Sankey diagrams. It supports data from NicheNet, MultiNicheNet, LIANA, and CellPhoneDB.

## Important: Do Not Change

- **Default figure dimensions**: Keep as 600x900 pixels. Do not change unless explicitly requested.

### Plot Display Code (DO NOT MODIFY)

The following functions handle plot rendering in Jupyter, RStudio, and console environments. They were carefully debugged and are tightly coupled. Do not change unless explicitly requested and you fully understand the Jupyter/IRkernel display pipeline.

**`R/api.R`:**
- `print.sankey()` — Must use `IRdisplay::display(fig)` for Jupyter and `print(fig)` for console. The return value of `print()` is ALWAYS DISCARDED by R, so the function must use side effects (not return values) to display content.
- `repr_html.sankey()` — The roxygen tag MUST be `@exportS3Method repr::repr_html`. Do NOT change to `@export`, `@method repr_html sankey`, or `@keywords internal`. The `repr::` prefix in NAMESPACE is required for conditional registration (repr is optional, only present in Jupyter).
- `knit_print.sankey()` — Standard knitr delegation. Do not modify.
- `sankey_show()` — Delegates to `print()`. Do not modify.

**`R/base_plot.R`:**
- `BasePlot$show()` — Same IRdisplay/console pattern as `print.sankey()`. Do not modify.
- `BasePlot$.apply_layout()` — Must NOT contain `htmlwidgets::JS()` objects. Plain `toImageButtonOptions` list (no JS) is safe. JS objects break `IRdisplay::display()` in Jupyter.
- `BasePlot$.apply_export_config()` — This is the ONLY place where `htmlwidgets::JS()` objects (custom PNG/PDF buttons) are allowed. This method is called ONLY from `BasePlot$save()`, never from `render()` or `show()`.

### Plot Export Code (DO NOT MODIFY)

The following functions handle saving plots to files. The export pipeline (kaleido → chromote → webshot2 → orca fallback chain) was carefully implemented. Do not change unless explicitly requested.

**`R/api.R`:**
- `sankey_save()` — Delegates to `BasePlot$save()`. Do not modify.
- `sankey_render()` — Returns the plotly figure object. Do not modify.

**`R/exporters.R`:**
- `save_figure()` — Format dispatch (HTML vs static). Do not modify.
- `to_html()` — HTML export via `htmlwidgets::saveWidget()`. Do not modify.
- `to_image()` — Multi-method fallback chain (kaleido → chromote → webshot2 → orca). Do not modify.
- `to_json()` — JSON export. Do not modify.

## Changelog

For major changes in the code, summarize the change and add it to `CHANGELOG.md`.

## Key Files

- `R/` - Package source code
- `tests/testthat/` - Test files
- `notebooks/` - Tutorial Jupyter notebooks
- `vignettes/` - R vignettes

## Tutorials

**IMPORTANT**: All tutorials should be built in **Jupyter notebook format** (`.ipynb`) first in the `notebooks/` directory. Rmd versions for vignettes will be created later.

Tutorial naming convention:
- `notebooks/tutorial-contextnet-nichenet.ipynb` - ContextNet + NicheNet workflow
- `notebooks/tutorial-contextnet-multinichenet.ipynb` - ContextNet + MultiNicheNet workflow
- `notebooks/tutorial-parameter-guide.ipynb` - Parameter reference with visual examples

## Environment Setup

**IMPORTANT**: Use micromamba to run R commands in the test environment:

```bash
/usr/local/bin/micromamba run -n ccflowr-test <command>
```

Available environments (check with `/usr/local/bin/micromamba env list`):
- `ccflowr-test` - Main test environment for ccflowR

## Testing

Run all tests:
```bash
/usr/local/bin/micromamba run -n ccflowr-test Rscript -e "devtools::test()"
```

Run specific test file:
```bash
/usr/local/bin/micromamba run -n ccflowr-test Rscript -e "devtools::test(filter = 'visual-comprehensive')"
```

Run visual tests to generate HTML outputs:
```bash
/usr/local/bin/micromamba run -n ccflowr-test Rscript -e "source('tests/visual-test-runner.R'); run_visual_tests()"
```

### Test Files

- `tests/testthat/test-visual-comprehensive.R` - Comprehensive visual tests (133 tests)
- `tests/visual-test-runner.R` - HTML generator for visual inspection
- `tests/visual-outputs/` - Generated HTML files (open index.html to review)

### Visual Test Categories

Run specific category:
```bash
/usr/local/bin/micromamba run -n ccflowr-test Rscript -e "source('tests/visual-test-runner.R'); run_visual_tests(categories = 'labels')"
```

Categories: `node_arrangement`, `color_schematics`, `labels`, `interactive`, `highlighting`, `dimensions`, `flow_methods`, `combined`

## Building Documentation

```r
devtools::document()
```
