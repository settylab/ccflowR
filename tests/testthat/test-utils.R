test_that("validate_required_columns passes with valid data", {
  df <- data.frame(a = 1, b = 2, c = 3)

  expect_silent(validate_required_columns(df, c("a", "b")))
  expect_true(validate_required_columns(df, c("a", "b", "c")))
})

test_that("validate_required_columns fails with missing columns", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    validate_required_columns(df, c("a", "x")),
    "Missing required columns"
  )
})

test_that("validate_required_columns fails with non-dataframe", {
  expect_error(
    validate_required_columns("not a dataframe", c("a")),
    "must be a data frame"
  )
})

test_that("get_unique_values preserves order", {
  result <- get_unique_values(c("B", "A", "B", "C", "A"))
  expect_equal(result, c("B", "A", "C"))
})

test_that("merge_lists combines correctly", {
  base <- list(a = 1, b = 2)
  override <- list(b = 3, c = 4)

  result <- merge_lists(base, override)

  expect_equal(result$a, 1)
  expect_equal(result$b, 3)  # Overridden

  expect_equal(result$c, 4)
})

test_that("ensure_list converts values", {
  expect_type(ensure_list("a"), "list")
  expect_type(ensure_list(c("a", "b")), "list")
  expect_type(ensure_list(list("a")), "list")
  expect_equal(ensure_list(NULL), list())
})

test_that("extract_celltypes works correctly", {
  df <- data.frame(
    sender = c("A", "B", "A"),
    receiver = c("B", "C", "C")
  )

  result <- extract_celltypes(df)

  expect_type(result, "list")
  expect_setequal(result$senders, c("A", "B"))
  expect_setequal(result$receivers, c("B", "C"))
  expect_setequal(result$all, c("A", "B", "C"))
})

test_that("extract_ligands works correctly", {
  df <- data.frame(ligand = c("L1", "L2", "L1", "L3"))
  result <- extract_ligands(df)
  expect_setequal(result, c("L1", "L2", "L3"))
})

test_that("extract_receptors works correctly", {
  df <- data.frame(receptor = c("R1", "R2", "R1"))
  result <- extract_receptors(df)
  expect_setequal(result, c("R1", "R2"))
})

test_that("summarize_data returns correct structure", {
  ex <- example_communication()
  result <- summarize_data(ex$data)

  expect_type(result, "list")
  expect_true("n_interactions" %in% names(result))
  expect_true("n_senders" %in% names(result))
  expect_true("n_ligands" %in% names(result))
  expect_equal(result$n_interactions, nrow(ex$data))
})

test_that("auto_detect_expression_columns finds columns", {
  df <- data.frame(
    ligand_expr = 1,
    receptor_expression = 2,
    activity_score = 3
  )

  expect_equal(auto_detect_expression_columns(df, "ligand"), "ligand_expr")
  expect_equal(auto_detect_expression_columns(df, "receptor"), "receptor_expression")

  # Returns NULL if not found
  df2 <- data.frame(x = 1)
  expect_null(auto_detect_expression_columns(df2, "ligand"))
})

# =============================================================================
# Framework Detection Tests
# =============================================================================

test_that("detect_framework identifies NicheNet data", {
  nichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = 0.5,
    scaled_ligand_expression = 0.3,
    scaled_receptor_expression = 0.4
  )

  expect_equal(detect_framework(nichenet_data), "nichenet")
})

test_that("detect_framework identifies MultiNicheNet data", {
  multinichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6,
    group = "Disease"
  )

  expect_equal(detect_framework(multinichenet_data), "multinichenet")
})

test_that("detect_framework identifies MultiNicheNet without group", {
  multinichenet_no_group <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6
  )

  expect_equal(detect_framework(multinichenet_no_group), "multinichenet_mapped")
})

test_that("detect_framework returns unknown for minimal data", {
  minimal_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1"
  )

  expect_equal(detect_framework(minimal_data), "unknown")
})

test_that("detect_framework handles non-dataframe input", {
  expect_equal(detect_framework("not a dataframe"), "unknown")
  expect_equal(detect_framework(NULL), "unknown")
})

# =============================================================================
# Sankey Input Validation Tests
# =============================================================================

test_that("validate_sankey_input validates base columns", {
  valid_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1"
  )

  expect_true(validate_sankey_input(valid_data))
})

test_that("validate_sankey_input errors on missing columns", {
  bad_data <- data.frame(x = 1, y = 2)
  expect_error(validate_sankey_input(bad_data), "Missing required columns")
})

test_that("validate_sankey_input checks weight columns", {
  basic_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1"
  )

  expect_warning(
    result <- validate_sankey_input(basic_data, require_weights = TRUE),
    "Missing weight columns"
  )
  expect_false(result)

  weighted_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = 0.5,
    scaled_ligand_expression = 0.3,
    scaled_receptor_expression = 0.4
  )

  expect_true(validate_sankey_input(weighted_data, require_weights = TRUE))
})

test_that("validate_sankey_input warns on negative activity", {
  data_negative <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = -0.5,
    scaled_ligand_expression = 0.3,
    scaled_receptor_expression = 0.4
  )

  expect_warning(
    validate_sankey_input(data_negative, require_weights = TRUE),
    "negative values"
  )
})

# =============================================================================
# Data Preparation Tests
# =============================================================================

test_that("prepare_sankey_data maps MultiNicheNet columns", {
  multinichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6,
    group = "Disease"
  )

  prepared <- prepare_sankey_data(multinichenet_data)

  expect_true("scaled_ligand_expression" %in% colnames(prepared))
  expect_true("scaled_receptor_expression" %in% colnames(prepared))
  expect_true("scaled_activity" %in% colnames(prepared))
  expect_equal(prepared$scaled_activity, 0.6)
  expect_equal(attr(prepared, "framework"), "multinichenet")
})

test_that("prepare_sankey_data preserves NicheNet columns", {
  nichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_activity = 0.8,
    scaled_ligand_expression = 0.7,
    scaled_receptor_expression = 0.9
  )

  prepared <- prepare_sankey_data(nichenet_data)

  expect_equal(prepared$scaled_activity, 0.8)
  expect_equal(attr(prepared, "framework"), "nichenet")
})

test_that("prepare_sankey_data respects auto_map=FALSE", {
  multinichenet_data <- data.frame(
    sender = "A", receiver = "B", ligand = "L1", receptor = "R1",
    scaled_lfc_ligand = 0.5,
    scaled_lfc_receptor = 0.4,
    prioritization_score = 0.6,
    group = "Disease"
  )

  prepared <- prepare_sankey_data(multinichenet_data, auto_map = FALSE)

  expect_false("scaled_activity" %in% colnames(prepared))
})
