test_that("filter_by_celltypes includes specified senders", {
  ex <- example_communication()
  filtered <- filter_by_celltypes(ex$data, include_senders = c("Macrophage"))

  expect_true(all(filtered$sender == "Macrophage"))
  expect_true(nrow(filtered) < nrow(ex$data))
})

test_that("filter_by_celltypes excludes specified senders", {
  ex <- example_communication()
  filtered <- filter_by_celltypes(ex$data, exclude_senders = c("Macrophage"))

  expect_false("Macrophage" %in% filtered$sender)
})

test_that("filter_by_celltypes includes specified receivers", {
  ex <- example_communication()
  filtered <- filter_by_celltypes(ex$data, include_receivers = c("T_cell"))

  expect_true(all(filtered$receiver == "T_cell"))
})

test_that("filter_top_pathways returns correct number", {
  ex <- example_communication()
  n <- 3
  filtered <- filter_top_pathways(ex$data, n = n)

  expect_equal(nrow(filtered), n)
})

test_that("filter_top_interactions returns correct number", {
  ex <- example_communication()
  n <- 5
  filtered <- filter_top_interactions(ex$data, n = n)

  expect_equal(nrow(filtered), n)
})

test_that("filter_by_ligands filters correctly", {
  ex <- example_communication()
  target_ligands <- c("TGFB1", "IL6")
  filtered <- filter_by_ligands(ex$data, target_ligands)

  expect_true(all(filtered$ligand %in% target_ligands))
})

test_that("filter_by_receptors filters correctly", {
  ex <- example_communication()
  target_receptors <- c("TGFBR1", "IL6R")
  filtered <- filter_by_receptors(ex$data, target_receptors)

  expect_true(all(filtered$receptor %in% target_receptors))
})

test_that("remove_duplicates removes duplicates", {
  # Create data with duplicates
  data <- data.frame(
    sender = c("A", "A", "B"),
    receiver = c("B", "B", "A"),
    ligand = c("L1", "L1", "L2"),
    receptor = c("R1", "R1", "R2"),
    activity = c(0.5, 0.8, 0.6),
    stringsAsFactors = FALSE
  )

  dedup <- remove_duplicates(data, keep = "first")
  expect_equal(nrow(dedup), 2)  # One duplicate removed
})

test_that("remove_duplicates keep=max_activity works", {
  data <- data.frame(
    sender = c("A", "A", "B"),
    receiver = c("B", "B", "A"),
    ligand = c("L1", "L1", "L2"),
    receptor = c("R1", "R1", "R2"),
    activity = c(0.5, 0.8, 0.6),
    stringsAsFactors = FALSE
  )

  dedup <- remove_duplicates(data, keep = "max_activity")

  # Should keep the row with higher activity
  dup_row <- dedup[dedup$sender == "A" & dedup$ligand == "L1", ]
  expect_equal(dup_row$activity, 0.8)
})

test_that("remove_duplicates keep=last works", {
  data <- data.frame(
    sender = c("A", "A", "B"),
    receiver = c("B", "B", "A"),
    ligand = c("L1", "L1", "L2"),
    receptor = c("R1", "R1", "R2"),
    activity = c(0.5, 0.8, 0.6),
    stringsAsFactors = FALSE
  )

  dedup <- remove_duplicates(data, keep = "last")

  expect_equal(nrow(dedup), 2)
  dup_row <- dedup[dedup$sender == "A" & dedup$ligand == "L1", ]
  expect_equal(dup_row$activity, 0.8)
})

test_that("filter_by_celltypes excludes specified receivers", {
  ex <- example_communication()
  filtered <- filter_by_celltypes(ex$data, exclude_receivers = c("T_cell"))

  expect_false("T_cell" %in% filtered$receiver)
})

test_that("filter_by_expression works with ligand threshold", {
  data <- data.frame(
    sender = c("A", "B", "C"),
    receiver = c("B", "C", "A"),
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3"),
    ligand_expr = c(0.5, 0.8, 0.3),
    stringsAsFactors = FALSE
  )

  filtered <- filter_by_expression(data, ligand_threshold = 0.4, ligand_col = "ligand_expr")
  expect_equal(nrow(filtered), 2)
})

test_that("filter_by_expression works with receptor threshold", {
  data <- data.frame(
    sender = c("A", "B", "C"),
    receiver = c("B", "C", "A"),
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3"),
    receptor_expr = c(0.5, 0.8, 0.3),
    stringsAsFactors = FALSE
  )

  filtered <- filter_by_expression(data, receptor_threshold = 0.6, receptor_col = "receptor_expr")
  expect_equal(nrow(filtered), 1)
})

test_that("filter_by_expression returns all rows when no threshold set", {
  data <- data.frame(
    sender = c("A", "B"),
    receiver = c("B", "A"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_by_expression(data)
  expect_equal(nrow(filtered), 2)
})

test_that("filter_top_pathways with per_group", {
  data <- data.frame(
    sender = c("A", "A", "B", "B"),
    receiver = c("B", "B", "A", "A"),
    ligand = c("L1", "L2", "L1", "L2"),
    receptor = c("R1", "R2", "R1", "R2"),
    activity = c(0.5, 0.8, 0.3, 0.9),
    group = c("G1", "G1", "G2", "G2"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_top_pathways(data, n = 1, per_group = TRUE, group_col = "group")
  expect_equal(nrow(filtered), 2)  # 1 per group
})

test_that("filter_top_pathways handles missing activity column", {
  data <- data.frame(
    sender = c("A", "B", "C"),
    receiver = c("B", "C", "A"),
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_top_pathways(data, n = 2)
  expect_equal(nrow(filtered), 2)
})

test_that("aggregate_by_group works with mean", {
  data <- data.frame(
    sender = c("A", "A", "A"),
    receiver = c("B", "B", "B"),
    ligand = c("L1", "L1", "L1"),
    receptor = c("R1", "R1", "R1"),
    activity = c(0.2, 0.4, 0.6),
    group = c("G1", "G1", "G1"),
    stringsAsFactors = FALSE
  )

  agg <- aggregate_by_group(data)
  expect_equal(nrow(agg), 1)
  expect_equal(agg$activity, 0.4)  # mean of 0.2, 0.4, 0.6
})

test_that("aggregate_by_group works with sum", {
  data <- data.frame(
    sender = c("A", "A"),
    receiver = c("B", "B"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R1"),
    activity = c(0.2, 0.4),
    group = c("G1", "G1"),
    stringsAsFactors = FALSE
  )

  agg <- aggregate_by_group(data, agg_fun = "sum")
  expect_equal(agg$activity, 0.6)
})

test_that("aggregate_by_group works with max", {
  data <- data.frame(
    sender = c("A", "A"),
    receiver = c("B", "B"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R1"),
    activity = c(0.2, 0.8),
    group = c("G1", "G1"),
    stringsAsFactors = FALSE
  )

  agg <- aggregate_by_group(data, agg_fun = "max")
  expect_equal(agg$activity, 0.8)
})

test_that("aggregate_by_group works with min", {
  data <- data.frame(
    sender = c("A", "A"),
    receiver = c("B", "B"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R1"),
    activity = c(0.2, 0.8),
    group = c("G1", "G1"),
    stringsAsFactors = FALSE
  )

  agg <- aggregate_by_group(data, agg_fun = "min")
  expect_equal(agg$activity, 0.2)
})

test_that("aggregate_by_group works with median", {
  data <- data.frame(
    sender = c("A", "A", "A"),
    receiver = c("B", "B", "B"),
    ligand = c("L1", "L1", "L1"),
    receptor = c("R1", "R1", "R1"),
    activity = c(0.2, 0.4, 0.9),
    group = c("G1", "G1", "G1"),
    stringsAsFactors = FALSE
  )

  agg <- aggregate_by_group(data, agg_fun = "median")
  expect_equal(agg$activity, 0.4)
})

test_that("aggregate_by_group handles missing value column", {
  data <- data.frame(
    sender = c("A", "A"),
    receiver = c("B", "B"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R1"),
    group = c("G1", "G1"),
    stringsAsFactors = FALSE
  )

  agg <- aggregate_by_group(data)
  expect_equal(nrow(agg), 1)
  expect_equal(agg$n_interactions, 2)
})
