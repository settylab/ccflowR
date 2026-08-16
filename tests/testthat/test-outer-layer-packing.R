packing_trace <- function(plot) {
  rendered <- plot$render()
  traces <- Filter(
    function(x) !is.null(x$type) && identical(x$type, "sankey"),
    rendered$x$attrs
  )
  expect_length(traces, 1L)
  traces[[1L]]
}

packing_data <- data.frame(
  sender = c("S1", "S2", "S3", "S4", "S5"),
  ligand = c("L1", "L2", "L1", "L2", "L1"),
  receptor = c("R1", "R2", "R1", "R2", "R1"),
  receiver = c("T1", "T2", "T3", "T4", "T5"),
  stringsAsFactors = FALSE
)

make_packing_plot <- function(span = NULL) {
  plot <- SankeyPlot$new(packing_data, width = 800, height = 600)
  plot$node_order(ligands = c("L1", "L2"), iterations = 4L)
  if (!is.null(span)) {
    plot$nodes(vertical_span = span, minimum_gap = 4)
  }
  plot
}

test_that("ordered layouts default to a 0.7 outer-layer span", {
  default <- packing_trace(make_packing_plot())
  explicit_default <- packing_trace(make_packing_plot(0.7))
  full_span <- packing_trace(make_packing_plot(1))
  sizes <- c(senders = 5L, ligands = 2L, receptors = 2L, receivers = 5L)
  layers <- rep(names(sizes), sizes)
  outer <- layers %in% c("senders", "receivers")

  expect_identical(formals(sankey_nodes)$vertical_span, 0.7)
  expect_identical(default$node$x, explicit_default$node$x)
  expect_identical(default$node$y, explicit_default$node$y)
  expect_identical(default$node$y[!outer], full_span$node$y[!outer])
  expect_false(identical(default$node$y[outer], full_span$node$y[outer]))

  for (layer in c("senders", "receivers")) {
    expect_lt(
      diff(range(default$node$y[layers == layer])),
      diff(range(full_span$node$y[layers == layer]))
    )
  }
})

test_that("outer packing changes only sender and receiver coordinates", {
  baseline <- packing_trace(make_packing_plot(1))
  packed <- packing_trace(make_packing_plot(
    c(senders = 0.7, receivers = 0.7)
  ))
  sizes <- c(senders = 5L, ligands = 2L, receptors = 2L, receivers = 5L)
  layers <- rep(names(sizes), sizes)
  outer <- layers %in% c("senders", "receivers")

  expect_identical(packed$node$customdata, baseline$node$customdata)
  expect_identical(packed$node$x, baseline$node$x)
  expect_identical(packed$node$y[!outer], baseline$node$y[!outer])
  expect_false(identical(packed$node$y[outer], baseline$node$y[outer]))
  expect_identical(packed$link$source, baseline$link$source)
  expect_identical(packed$link$target, baseline$link$target)
  expect_identical(packed$link$value, baseline$link$value)
  expect_identical(packed$link$color, baseline$link$color)
})

test_that("outer packing is deterministic", {
  first <- packing_trace(make_packing_plot(0.7))
  second <- packing_trace(make_packing_plot(0.7))

  expect_identical(first$node$customdata, second$node$customdata)
  expect_identical(first$node$x, second$node$x)
  expect_identical(first$node$y, second$node$y)
  expect_identical(first$link$value, second$link$value)
})

test_that("a named span updates only the requested outer layer", {
  default <- packing_trace(make_packing_plot())
  sender_plot <- make_packing_plot()
  sender_plot$nodes(vertical_span = c(senders = 0.85))
  sender_only <- packing_trace(sender_plot)
  sizes <- c(senders = 5L, ligands = 2L, receptors = 2L, receivers = 5L)
  layers <- rep(names(sizes), sizes)

  expect_false(identical(
    sender_only$node$y[layers == "senders"],
    default$node$y[layers == "senders"]
  ))
  expect_identical(
    sender_only$node$y[layers == "receivers"],
    default$node$y[layers == "receivers"]
  )
})

test_that("vertical span validates layer names and readable gap", {
  plot <- make_packing_plot()

  expect_error(plot$nodes(vertical_span = 0), "values in \\(0, 1\\]")
  expect_error(plot$nodes(vertical_span = 1.1), "values in \\(0, 1\\]")
  expect_error(
    plot$nodes(vertical_span = c(ligands = 0.8)),
    "senders.*receivers"
  )
  expect_error(
    plot$nodes(vertical_span = c(0.8, 0.7)),
    "unnamed vertical_span"
  )
  expect_error(plot$nodes(minimum_gap = -1), "nonnegative")
})

test_that("packing stays inactive without a custom order", {
  plot <- SankeyPlot$new(packing_data, width = 800, height = 600)
  plot$nodes(vertical_span = 0.7)
  trace <- packing_trace(plot)

  expect_null(trace$node$x)
  expect_null(trace$node$y)
  expect_identical(trace$arrangement, "snap")
})

test_that("vertical packing rejects a vertical Sankey orientation", {
  plot <- make_packing_plot(0.7)
  plot$orientation("v")

  expect_error(plot$render(), "horizontal Sankey")
})
