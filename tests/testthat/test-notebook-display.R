# Notebook display helper tests

test_that("sankey_display_notebook returns rendered widget without static preview", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors) +
    sankey_title("ccflowR notebook display smoke test") +
    sankey_layout(width = 760, height = 520)

  result <- sankey_display_notebook(
    p,
    width = 760,
    height = 520,
    static = FALSE,
    quiet = TRUE
  )

  expect_type(result, "list")
  expect_s3_class(result$widget, "plotly")
  expect_equal(result$widget$width, 760)
  expect_equal(result$widget$height, 520)
  expect_false(result$static_exported)
  expect_null(result$static_file)
  expect_null(result$static_error)
})

test_that("sankey_display_notebook validates inputs", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors)

  expect_error(sankey_display_notebook("not a plot", static = FALSE), "sankey object")
  expect_error(sankey_display_notebook(p, static = NA), "static must be TRUE or FALSE")
  expect_error(sankey_display_notebook(p, quiet = NA, static = FALSE), "quiet must be TRUE or FALSE")
  expect_error(
    sankey_display_notebook(p, static_file = tempfile(fileext = ".jpg"), static = FALSE),
    "static_file must end in .png"
  )
})

test_that("static preview failure message is actionable", {
  msg <- ccflowR:::sankey_static_preview_error_message(paste(
    "Could not export png image.",
    "Methods tried:",
    "  - kaleido (not installed)",
    "  - webshot2 (Chrome unavailable)",
    "  - orca (not found in PATH)",
    sep = "\n"
  ))

  expect_true(grepl("Static PNG preview failed", msg, fixed = TRUE))
  expect_true(grepl("Kaleido", msg, fixed = TRUE))
  expect_true(grepl("webshot2", msg, fixed = TRUE))
  expect_true(grepl("Chrome", msg, fixed = TRUE))
  expect_true(grepl("Orca", msg, fixed = TRUE))
  expect_true(grepl("RETICULATE_PYTHON", msg, fixed = TRUE))
})

test_that("package-native notebook smoke flow renders widget and handles PNG export", {
  ex <- example_communication()

  p <- sankey_plot(ex[["data"]], ex[["colors"]]) +
    sankey_title("ccflowR notebook display smoke test") +
    sankey_layout(width = 760, height = 520)

  fig <- sankey_render(p)
  expect_s3_class(fig, "plotly")

  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)

  export_result <- tryCatch({
    sankey_save(p, out, width = 760, height = 520)
    "exported"
  }, error = function(e) {
    conditionMessage(e)
  })

  if (identical(export_result, "exported")) {
    expect_true(file.exists(out))
    expect_gt(file.info(out)[["size"]], 0)

    if (requireNamespace("png", quietly = TRUE)) {
      image <- png::readPNG(out)
      expect_gt(length(unique(as.vector(image))), 1)
    }
  } else {
    expect_true(grepl(
      "kaleido|webshot2|Chrome|Chromium|orca|Python|reticulate",
      export_result,
      ignore.case = TRUE
    ))
  }
})
