direct_flow_links <- function(data, ..., flow_method = "weighted") {
  plot <- SankeyPlot$new(
    data,
    flow_method = flow_method,
    ...
  )
  traces <- Filter(
    function(x) !is.null(x$type) && identical(x$type, "sankey"),
    plot$render()$x$attrs
  )
  expect_length(traces, 1)
  trace <- traces[[1]]
  labels <- as.character(trace$node$customdata)

  data.frame(
    source = as.integer(trace$link$source),
    target = as.integer(trace$link$target),
    source_label = labels[as.integer(trace$link$source) + 1L],
    target_label = labels[as.integer(trace$link$target) + 1L],
    value = as.numeric(trace$link$value),
    stringsAsFactors = FALSE
  )
}

sender_ligand_links <- function(links) {
  sender_nodes <- setdiff(unique(links$source), unique(links$target))
  links[links$source %in% sender_nodes, , drop = FALSE]
}

sender_ligand_total <- function(links, sender, ligand) {
  first_layer <- sender_ligand_links(links)
  sum(first_layer$value[
    first_layer$source_label == sender & first_layer$target_label == ligand
  ])
}

test_that("weighted flow no longer requires ligand expression", {
  data <- data.frame(
    sender = c("A", "B"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    receiver = c("C", "D"),
    scaled_activity = c(0.8, 0.4),
    scaled_receptor_expression = c(0.7, 0.5)
  )

  expect_true(validate_sankey_input(data, require_weights = TRUE))
  plot <- SankeyPlot$new(data)
  expect_equal(plot$summary()$flow_method, "flow_conserving")
})

test_that("ligand expression is invariant for every edge width", {
  data <- data.frame(
    sender = c("A", "A", "B"),
    ligand = c("L1", "L2", "L1"),
    receptor = c("R1", "R2", "R1"),
    receiver = c("C", "D", "D"),
    scaled_activity = c(0.8, 0.3, 0.6),
    scaled_receptor_expression = c(0.9, 0.4, 0.7),
    scaled_ligand_expression = c(0.01, 10, 100)
  )

  changed <- data
  changed$scaled_ligand_expression <- c(1000, 0, NA)
  absent <- data[c(
    "sender", "ligand", "receptor", "receiver",
    "scaled_activity", "scaled_receptor_expression"
  )]

  expect_equal(
    direct_flow_links(data)$value,
    direct_flow_links(changed)$value,
    tolerance = 1e-12
  )
  expect_equal(
    direct_flow_links(data)$value,
    direct_flow_links(absent)$value,
    tolerance = 1e-12
  )
})

test_that("adding another ligand does not reweight an existing sender edge", {
  base <- data.frame(
    sender = "A", ligand = "L1", receptor = "R1", receiver = "C",
    scaled_activity = 0.25,
    scaled_receptor_expression = 0.8
  )
  expanded <- rbind(
    base,
    data.frame(
      sender = "A", ligand = "L2", receptor = "R2", receiver = "D",
      scaled_activity = 0.9,
      scaled_receptor_expression = 0.6
    )
  )

  expect_equal(sender_ligand_total(direct_flow_links(base), "A", "L1"), 0.25)
  expect_equal(sender_ligand_total(direct_flow_links(expanded), "A", "L1"), 0.25)
})

test_that("single-ligand senders retain different direct activity widths", {
  data <- data.frame(
    sender = c("A", "B"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    receiver = c("C", "D"),
    scaled_activity = c(0.2, 0.9),
    scaled_receptor_expression = c(0.5, 0.5)
  )
  links <- direct_flow_links(data)

  expect_equal(sender_ligand_total(links, "A", "L1"), 0.2)
  expect_equal(sender_ligand_total(links, "B", "L2"), 0.9)
})

test_that("a shared ContextNet activity unit contributes exactly once", {
  data <- data.frame(
    sender = c("A", "A"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R2"),
    receiver = c("C", "C"),
    scaled_activity = c(0.8, 0.8),
    scaled_receptor_expression = c(0.75, 0.25),
    activity_unit = c("A|L1|C", "A|L1|C")
  )
  links <- direct_flow_links(data, activity_unit_col = "activity_unit")
  first_layer <- sender_ligand_links(links)

  expect_equal(first_layer$value, c(0.4, 0.4))
  expect_equal(sender_ligand_total(links, "A", "L1"), 0.8)
})

test_that("path-level MultiNicheNet activities remain independent by default", {
  data <- data.frame(
    sender = c("A", "A"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R2"),
    receiver = c("C", "D"),
    prioritization_score = c(0.8, 0.6),
    scaled_lfc_receptor = c(0.7, 0.3),
    scaled_lfc_ligand = c(100, 0.01),
    group = c("Disease", "Disease")
  )
  links <- direct_flow_links(data)

  expect_equal(sender_ligand_total(links, "A", "L1"), 1.4)
  expect_equal(sender_ligand_links(links)$value, c(0.8, 0.6))
})

test_that("activity-unit identifiers reject ambiguous score scope", {
  inconsistent <- data.frame(
    sender = c("A", "A"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R2"),
    receiver = c("C", "C"),
    scaled_activity = c(0.8, 0.6),
    scaled_receptor_expression = c(0.5, 0.5),
    activity_unit = c("unit-1", "unit-1")
  )
  plot <- SankeyPlot$new(
    inconsistent,
    activity_unit_col = "activity_unit",
    flow_method = "weighted"
  )
  expect_error(plot$render(), "inconsistent activity scores")

  multiple_pairs <- inconsistent
  multiple_pairs$scaled_activity <- 0.8
  multiple_pairs$ligand[[2]] <- "L2"
  plot <- SankeyPlot$new(
    multiple_pairs,
    activity_unit_col = "activity_unit",
    flow_method = "weighted"
  )
  expect_error(plot$render(), "one sender-ligand pair")
})

test_that("ligand and receptor nodes conserve direct activity flow", {
  data <- data.frame(
    sender = c("A", "B", "A", "C"),
    ligand = c("L1", "L1", "L2", "L2"),
    receptor = c("R1", "R2", "R1", "R1"),
    receiver = c("X", "Y", "Y", "Z"),
    scaled_activity = c(0.8, 0.5, 0.4, 0.3),
    scaled_receptor_expression = c(0.9, 0.1, 0, NA)
  )
  links <- direct_flow_links(data)
  intermediate_nodes <- intersect(unique(links$target), unique(links$source))

  for (node in intermediate_nodes) {
    expect_equal(
      sum(links$value[links$target == node]),
      sum(links$value[links$source == node]),
      tolerance = 1e-12
    )
  }
})

test_that("nonpositive and missing activities do not become maximal widths", {
  data <- data.frame(
    sender = c("A", "B", "C", "D"),
    ligand = c("L1", "L2", "L3", "L4"),
    receptor = c("R1", "R2", "R3", "R4"),
    receiver = c("W", "X", "Y", "Z"),
    scaled_activity = c(0.8, 0, -1, NA),
    scaled_receptor_expression = c(1, 1, 1, 1)
  )
  values <- sender_ligand_links(direct_flow_links(data))$value

  expect_equal(values, c(0.8, 0, 0, 0))
})

test_that("uniform mode remains an equal-width topology view", {
  data <- data.frame(
    sender = c("A", "A"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R2"),
    receiver = c("C", "D"),
    scaled_activity = c(0.1, 10),
    scaled_receptor_expression = c(0.2, 0.8),
    activity_unit = c("shared", "shared")
  )
  links <- direct_flow_links(
    data,
    activity_unit_col = "activity_unit",
    flow_method = "uniform"
  )

  expect_equal(sender_ligand_links(links)$value, c(1, 1))
})

test_that("NicheNet preparation does not fabricate ligand expression", {
  data <- data.frame(
    sender = "A", ligand = "L1", receptor = "R1", receiver = "B",
    scaled_activity = 0.8,
    scaled_receptor_expression = 0.6
  )
  prepared <- prepare_sankey_data(data)

  expect_equal(attr(prepared, "framework"), "nichenet")
  expect_false("scaled_ligand_expression" %in% colnames(prepared))
})

test_that("functional API forwards the explicit activity unit column", {
  data <- data.frame(
    sender = c("A", "A"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R2"),
    receiver = c("B", "B"),
    scaled_activity = c(0.7, 0.7),
    scaled_receptor_expression = c(0.5, 0.5),
    activity_unit = c("unit-1", "unit-1")
  )
  plot <- sankey_plot(
    data,
    flow = "weighted",
    activity_unit_col = "activity_unit"
  )

  expect_equal(summary(plot)$activity_unit_col, "activity_unit")
})
