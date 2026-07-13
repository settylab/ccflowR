test_that("example_communication returns valid data", {
  ex <- example_communication()

  expect_type(ex, "list")
  expect_true("data" %in% names(ex))
  expect_true("colors" %in% names(ex))

  # Data has required columns
  expect_true(all(c("sender", "receiver", "ligand", "receptor") %in% colnames(ex$data)))

  # Data has rows

  expect_gt(nrow(ex$data), 0)

  # Colors are named
  expect_true(!is.null(names(ex$colors)))
})

test_that("example_multinichenet returns data with groups", {
  ex <- example_multinichenet()

  expect_type(ex, "list")
  expect_true("data" %in% names(ex))
  expect_true("group_colors" %in% names(ex))

  # Data has group column
  expect_true("group" %in% colnames(ex$data))

  # Has multiple groups
  expect_gt(length(unique(ex$data$group)), 1)
})

test_that("example_tumor_microenvironment returns valid data", {
  ex <- example_tumor_microenvironment()

  expect_type(ex, "list")
  expect_true("data" %in% names(ex))
  expect_true("colors" %in% names(ex))

  # Contains tumor-related cell types
  all_cells <- unique(c(ex$data$sender, ex$data$receiver))
  expect_true("Tumor_cell" %in% all_cells)
})

test_that("example_large generates specified size", {
  n <- 50
  ex <- example_large(n_interactions = n)

  expect_equal(nrow(ex$data), n)
})

test_that("example_large generates specified cell types", {
  n_types <- 5
  ex <- example_large(n_interactions = 30, n_celltypes = n_types)

  all_cells <- unique(c(ex$data$sender, ex$data$receiver))
  expect_lte(length(all_cells), n_types)
  expect_equal(length(ex$colors), n_types)
})

test_that("all example datasets work with SankeyPlot", {
  # Test each dataset can create a plot
  datasets <- list(
    example_communication(),
    example_tumor_microenvironment()
  )

  for (ex in datasets) {
    plot <- SankeyPlot$new(ex$data, ex$colors)
    expect_s3_class(plot, "SankeyPlot")
    fig <- plot$render()
    expect_s3_class(fig, "plotly")
  }
})
