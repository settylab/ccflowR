# ccflowR Testing Guide

This directory contains comprehensive tests for the ccflowR package, including unit tests and visual tests.

## Test Structure

```
tests/
├── testthat/
│   ├── test-visual-comprehensive.R   # Comprehensive visual tests (testthat)
│   ├── test-sankey.R                  # Core SankeyPlot tests
│   ├── test-sankey-weights.R          # Flow-conserving edge weight tests
│   ├── test-*.R                       # Other unit tests
│   └── helper-fixtures.R              # Test fixtures
├── visual-test-runner.R               # Visual test HTML generator
├── visual-outputs/                    # Generated HTML outputs (gitignored)
└── README.md                          # This file
```

## Running Unit Tests

Run all unit tests with testthat:

```r
# From R console
devtools::test()

# Run specific test file
devtools::test(filter = "visual-comprehensive")

# Run with verbose output
devtools::test(reporter = "progress")
```

## Running Visual Tests

Visual tests generate HTML files that you can inspect in a browser to verify plots render correctly.

### Quick Start

```r
# Load the visual test runner
source("tests/visual-test-runner.R")

# Run all visual tests (generates HTML files)
run_visual_tests()

# Run specific category
run_visual_tests(categories = "labels")
run_visual_tests(categories = c("labels", "highlighting"))

# Quick test a single case (displays in viewer)
quick_test("long_labels_right")

# List all available tests
list_tests()
```

### Test Categories

| Category | Description |
|----------|-------------|
| `node_arrangement` | Node ordering, padding, thickness, orientation |
| `color_schematics` | Coloring strategies (sender, ligand, group, binary), alpha, node colors |
| `labels` | Label positions (left, right, top, bottom, inside), font sizes |
| `interactive` | Dropdown filter functionality |
| `highlighting` | Pathway highlighting with various criteria |
| `dimensions` | Different plot dimensions (small, default, wide, tall, large) |
| `flow_methods` | Flow-conserving vs uniform flow methods |
| `combined` | Multiple features combined |

### Viewing Results

After running `run_visual_tests()`:

1. Open `tests/visual-outputs/index.html` in a browser
2. Click on any test to view the generated plot
3. Check for:
   - Label truncation
   - Node overlap
   - Color correctness
   - Interactive feature functionality
   - Proper highlighting

### Adding New Visual Tests

1. Open `tests/visual-test-runner.R`
2. Add your test case to the appropriate category in `get_test_cases()`
3. Follow this structure:

```r
list(
  name = "my_new_test",
  description = "Description of what this tests",
  plot = SankeyPlot$new(data, colors)$
    # ... configuration methods
)
```

4. Run `run_visual_tests(categories = "your_category")` to test

## Test Fixtures for Truncation-Prone Scenarios

The visual tests include fixtures specifically designed to test edge cases:

| Fixture | Purpose |
|---------|---------|
| `fixture_long_labels()` | Very long cell type and ligand/receptor names |
| `fixture_many_nodes()` | High number of nodes per layer (stress test) |
| `fixture_with_expression()` | Data with expression columns for flow-conserving |
| `fixture_multigroup()` | Multi-condition/group data |
| `fixture_minimal()` | Single interaction (minimal case) |

## Checklist for Visual QA

When reviewing visual test outputs, check:

### Node Arrangement
- [ ] All nodes visible
- [ ] No node overlap
- [ ] Custom ordering respected
- [ ] Vertical/horizontal orientation correct

### Labels
- [ ] No label truncation
- [ ] Labels don't overlap
- [ ] Font size readable
- [ ] Position correct (left/right/top/bottom/inside)

### Colors
- [ ] Correct coloring strategy applied
- [ ] Alpha/transparency as expected
- [ ] Group colors distinct
- [ ] Node colors match links

### Interactive Features
- [ ] All 4 dropdowns visible (Sender, Ligand, Receptor, Receiver)
- [ ] Dropdowns don't overlap content
- [ ] Filtering works (pathways dim/highlight)
- [ ] "Show All" resets correctly

### Highlighting
- [ ] Correct pathways highlighted
- [ ] Non-matching pathways dimmed
- [ ] dim_alpha value respected

### Dimensions
- [ ] Content fits within specified dimensions
- [ ] No content cut off at edges
- [ ] Aspect ratio maintained

## Continuous Integration

For CI/CD, run the unit tests:

```bash
Rscript -e "devtools::test()"
```

For visual regression testing, generate outputs and compare:

```bash
Rscript -e "source('tests/visual-test-runner.R'); run_visual_tests()"
```
