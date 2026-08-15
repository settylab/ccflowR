extract_sankey_trace <- function(fig) {
  traces <- Filter(
    function(x) !is.null(x$type) && identical(x$type, "sankey"),
    fig$x$attrs
  )
  expect_length(traces, 1)
  traces[[1]]
}

extract_layer_orders <- function(plot, layer_sizes) {
  trace <- extract_sankey_trace(plot$render())
  labels <- as.character(trace$node$customdata)
  stops <- cumsum(layer_sizes)
  starts <- c(1L, head(stops, -1L) + 1L)
  stats::setNames(
    Map(function(first, last) labels[first:last], starts, stops),
    names(layer_sizes)
  )
}

canonical_links <- function(plot) {
  trace <- extract_sankey_trace(plot$render())
  labels <- as.character(trace$node$customdata)
  links <- data.frame(
    source = labels[as.integer(trace$link$source) + 1L],
    target = labels[as.integer(trace$link$target) + 1L],
    value = as.numeric(trace$link$value),
    stringsAsFactors = FALSE
  )
  links <- links[order(links$source, links$target, links$value), , drop = FALSE]
  rownames(links) <- NULL
  links
}

count_pair_crossings <- function(data, left_col, right_col,
                                 left_order, right_order) {
  edges <- unique(data.frame(
    left = match(data[[left_col]], left_order),
    right = match(data[[right_col]], right_order)
  ))
  edges <- edges[order(edges$left, edges$right), , drop = FALSE]
  if (nrow(edges) <= 1L) return(0L)

  total <- 0L
  for (i in seq_len(nrow(edges) - 1L)) {
    later <- seq.int(i + 1L, nrow(edges))
    total <- total + sum(
      edges$left[later] > edges$left[[i]] &
        edges$right[later] < edges$right[[i]]
    )
  }
  total
}

count_all_crossings <- function(data, orders, has_receptor = TRUE) {
  total <- count_pair_crossings(
    data, "sender", "ligand", orders$senders, orders$ligands
  )
  if (has_receptor) {
    total <- total + count_pair_crossings(
      data, "ligand", "receptor", orders$ligands, orders$receptors
    )
    total <- total + count_pair_crossings(
      data, "receptor", "receiver", orders$receptors, orders$receivers
    )
  } else {
    total <- total + count_pair_crossings(
      data, "ligand", "receiver", orders$ligands, orders$receivers
    )
  }
  total
}

diagonal_four_layer <- data.frame(
  sender = c("S1", "S2", "S3"),
  ligand = c("L1", "L2", "L3"),
  receptor = c("R1", "R2", "R3"),
  receiver = c("T1", "T2", "T3"),
  stringsAsFactors = FALSE
)

test_that("explicit fixed mode preserves the historical ordering", {
  plot <- SankeyPlot$new(diagonal_four_layer)
  plot$node_order(ligands = c("L1", "L3"), mode = "fixed")
  orders <- extract_layer_orders(
    plot,
    c(senders = 3L, ligands = 3L, receptors = 3L, receivers = 3L)
  )

  expect_equal(orders$ligands, c("L1", "L3", "L2"))
})

test_that("constrained ordering is the default when an order is supplied", {
  plot <- SankeyPlot$new(diagonal_four_layer)
  plot$node_order(ligands = c("L1", "L3"))
  orders <- extract_layer_orders(
    plot,
    c(senders = 3L, ligands = 3L, receptors = 3L, receivers = 3L)
  )

  expect_equal(orders$ligands, c("L1", "L2", "L3"))
  expect_equal(count_all_crossings(diagonal_four_layer, orders), 0L)
})

test_that("constrained mode can interleave unnamed nodes and reduce crossings", {
  fixed <- SankeyPlot$new(diagonal_four_layer)
  fixed$node_order(ligands = c("L1", "L3"), mode = "fixed")
  fixed_orders <- extract_layer_orders(
    fixed,
    c(senders = 3L, ligands = 3L, receptors = 3L, receivers = 3L)
  )

  constrained <- SankeyPlot$new(diagonal_four_layer)
  constrained$node_order(
    ligands = c("L1", "L3"), mode = "constrained", iterations = 8L
  )
  constrained_orders <- extract_layer_orders(
    constrained,
    c(senders = 3L, ligands = 3L, receptors = 3L, receivers = 3L)
  )

  expect_lt(
    match("L1", constrained_orders$ligands),
    match("L3", constrained_orders$ligands)
  )
  expect_equal(constrained_orders$ligands, c("L1", "L2", "L3"))
  expect_lt(
    count_all_crossings(diagonal_four_layer, constrained_orders),
    count_all_crossings(diagonal_four_layer, fixed_orders)
  )
  expect_equal(count_all_crossings(diagonal_four_layer, constrained_orders), 0L)
})

test_that("layout optimization does not change edge identity or weight", {
  fixed <- SankeyPlot$new(diagonal_four_layer)
  fixed$node_order(ligands = c("L1", "L3"), mode = "fixed")

  constrained <- SankeyPlot$new(diagonal_four_layer)
  constrained$node_order(
    ligands = c("L1", "L3"), mode = "constrained", iterations = 8L
  )

  expect_equal(canonical_links(constrained), canonical_links(fixed))
})

test_that("a complete constrained vector remains exact", {
  plot <- SankeyPlot$new(diagonal_four_layer)
  plot$node_order(
    ligands = c("L3", "L2", "L1"),
    mode = "constrained",
    iterations = 8L
  )
  orders <- extract_layer_orders(
    plot,
    c(senders = 3L, ligands = 3L, receptors = 3L, receivers = 3L)
  )

  expect_equal(orders$ligands, c("L3", "L2", "L1"))
})

test_that("relative constraints can be supplied for every displayed layer", {
  plot <- SankeyPlot$new(diagonal_four_layer)
  plot$node_order(
    senders = c("S3", "S1"),
    ligands = c("L3", "L1"),
    receptors = c("R3", "R1"),
    receivers = c("T3", "T1"),
    mode = "constrained",
    iterations = 8L
  )
  orders <- extract_layer_orders(
    plot,
    c(senders = 3L, ligands = 3L, receptors = 3L, receivers = 3L)
  )

  expect_lt(match("S3", orders$senders), match("S1", orders$senders))
  expect_lt(match("L3", orders$ligands), match("L1", orders$ligands))
  expect_lt(match("R3", orders$receptors), match("R1", orders$receptors))
  expect_lt(match("T3", orders$receivers), match("T1", orders$receivers))
})

test_that("constrained optimization is deterministic", {
  build_orders <- function() {
    plot <- SankeyPlot$new(diagonal_four_layer)
    plot$node_order(
      ligands = c("L1", "L3"), mode = "constrained", iterations = 12L
    )
    extract_layer_orders(
      plot,
      c(senders = 3L, ligands = 3L, receptors = 3L, receivers = 3L)
    )
  }

  expect_identical(build_orders(), build_orders())
})

test_that("constrained mode works without a receptor layer", {
  data <- diagonal_four_layer[c("sender", "ligand", "receiver")]
  plot <- SankeyPlot$new(data, receptor_col = NULL)
  plot$node_order(
    ligands = c("L1", "L3"), mode = "constrained", iterations = 8L
  )
  orders <- extract_layer_orders(
    plot,
    c(senders = 3L, ligands = 3L, receivers = 3L)
  )

  expect_equal(orders$ligands, c("L1", "L2", "L3"))
  expect_equal(count_all_crossings(data, orders, has_receptor = FALSE), 0L)
})

test_that("functional API records constrained-order controls", {
  layer <- sankey_order(
    senders = c("S1", "S3"), mode = "constrained", iterations = 5L
  )

  expect_equal(layer$mode, "constrained")
  expect_equal(layer$iterations, 5L)
  expect_error(sankey_order(iterations = 0), "positive integer")
  expect_error(sankey_order(iterations = 1.5), "positive integer")
})
