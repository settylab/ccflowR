# Reader Tests for ccflowR
# Test data import functions for various formats

# --- read_nichenet() ---

test_that("read_nichenet reads CSV file correctly", {
  csv_content <- fixture_nichenet_csv()
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_nichenet(tmp_file)

  expect_true(is.data.frame(data))
  expect_equal(nrow(data), 3)
  expect_true(all(c("sender", "receiver", "ligand", "receptor") %in% colnames(data)))
})

test_that("read_nichenet handles TSV files", {
  tsv_content <- "sender\treceiver\tligand\treceptor\tactivity
A\tB\tL1\tR1\t0.5
B\tC\tL2\tR2\t0.6"
  tmp_file <- create_temp_file(tsv_content, "tsv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_nichenet(tmp_file)

  expect_true(is.data.frame(data))
  expect_equal(nrow(data), 2)
})

test_that("read_nichenet with custom column names", {
  csv_content <- "source_cell,target_cell,lig,rec,score
A,B,L1,R1,0.5"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_nichenet(
    tmp_file,
    sender_col = "source_cell",
    receiver_col = "target_cell",
    ligand_col = "lig",
    receptor_col = "rec",
    activity_col = "score"
  )

  expect_true("sender" %in% colnames(data))
  expect_true("receiver" %in% colnames(data))
})

test_that("read_nichenet accepts data frame input", {
  df <- data.frame(
    sender = "A",
    receiver = "B",
    ligand = "L1",
    receptor = "R1",
    stringsAsFactors = FALSE
  )

  result <- read_nichenet(df)
  # Check data content (ignoring attributes)
  result_clean <- result
  attr(result_clean, "framework") <- NULL
  expect_equal(result_clean, df)
  expect_equal(attr(result, "framework"), "nichenet")
})

# --- read_multinichenet() ---

test_that("read_multinichenet reads file with groups", {
  csv_content <- "sender,receiver,ligand,receptor,group,prioritization_score
A,B,L1,R1,Control,0.5
A,B,L1,R1,Treatment,0.8"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_multinichenet(tmp_file)

  expect_true(is.data.frame(data))
  expect_true("group" %in% colnames(data))
  expect_equal(length(unique(data$group)), 2)
})

test_that("read_multinichenet handles custom group column", {
  csv_content <- "sender,receiver,ligand,receptor,condition,score
A,B,L1,R1,Ctrl,0.5
A,B,L1,R1,Treat,0.8"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_multinichenet(
    tmp_file,
    group_col = "condition",
    activity_col = "score"
  )

  expect_true("group" %in% colnames(data))
})

# --- read_liana() ---

test_that("read_liana reads LIANA format correctly", {
  liana_content <- fixture_liana_csv()
  tmp_file <- create_temp_file(liana_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_liana(tmp_file)

  expect_true(is.data.frame(data))
  expect_true(all(c("sender", "receiver", "ligand", "receptor") %in% colnames(data)))
  # LIANA rank is inverted, so activity should be calculated
  expect_true("activity" %in% colnames(data))
})

test_that("read_liana handles LIANA column names", {
  # LIANA uses source/target instead of sender/receiver
  liana_content <- "source,target,ligand.complex,receptor.complex,aggregate_rank
Macrophage,T_cell,TGFB1,TGFBR1,0.1"
  tmp_file <- create_temp_file(liana_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_liana(tmp_file)

  expect_equal(data$sender[1], "Macrophage")
  expect_equal(data$receiver[1], "T_cell")
})

test_that("read_liana inverts rank to activity", {
  liana_content <- "source,target,ligand.complex,receptor.complex,aggregate_rank
A,B,L1,R1,0.01
C,D,L2,R2,0.10"
  tmp_file <- create_temp_file(liana_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_liana(tmp_file)

  # Lower rank = higher activity
  expect_true(data$activity[1] > data$activity[2])
})

# --- read_cellphonedb() ---

test_that("read_cellphonedb accepts data frame", {
  # Simple test - actual CellPhoneDB format is complex
  df <- data.frame(
    sender = "A",
    receiver = "B",
    ligand = "L1",
    receptor = "R1",
    stringsAsFactors = FALSE
  )

  result <- read_cellphonedb(df)
  expect_true(is.data.frame(result))
})

# --- filter_top_interactions() ---

test_that("filter_top_interactions returns correct number", {
  ex <- example_communication()

  top5 <- filter_top_interactions(ex$data, n = 5)
  expect_equal(nrow(top5), 5)

  top3 <- filter_top_interactions(ex$data, n = 3)
  expect_equal(nrow(top3), 3)
})
test_that("filter_top_interactions sorts by activity descending", {
  data <- data.frame(
    sender = c("A", "B", "C"),
    receiver = c("B", "C", "A"),
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3"),
    activity = c(0.3, 0.9, 0.6),
    stringsAsFactors = FALSE
  )

  top <- filter_top_interactions(data, n = 3, by = "activity")

  expect_equal(top$activity[1], 0.9)
  expect_equal(top$activity[3], 0.3)
})

test_that("filter_top_interactions handles ascending order", {
  data <- data.frame(
    sender = c("A", "B", "C"),
    receiver = c("B", "C", "A"),
    ligand = c("L1", "L2", "L3"),
    receptor = c("R1", "R2", "R3"),
    activity = c(0.3, 0.9, 0.6),
    stringsAsFactors = FALSE
  )

  top <- filter_top_interactions(data, n = 2, by = "activity", descending = FALSE)

  expect_equal(top$activity[1], 0.3)
})

test_that("filter_top_interactions handles missing column", {
  data <- data.frame(
    sender = c("A", "B"),
    receiver = c("B", "A"),
    ligand = c("L1", "L2"),
    receptor = c("R1", "R2"),
    stringsAsFactors = FALSE
  )

  # Should return first n rows when column missing
  result <- filter_top_interactions(data, n = 1, by = "nonexistent")
  expect_equal(nrow(result), 1)
})

# --- Auto-detection ---

test_that("read functions auto-detect CSV vs TSV", {
  # CSV
  csv_content <- "a,b,c\n1,2,3"
  csv_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(csv_file), add = TRUE)

  # TSV
  tsv_content <- "a\tb\tc\n1\t2\t3"
  tsv_file <- create_temp_file(tsv_content, "tsv")
  on.exit(unlink(tsv_file), add = TRUE)

  # Both should read correctly
  csv_data <- read_nichenet(csv_file, sender_col = "a", receiver_col = "b",
                             ligand_col = "c", receptor_col = "c")
  tsv_data <- read_nichenet(tsv_file, sender_col = "a", receiver_col = "b",
                             ligand_col = "c", receptor_col = "c")

  expect_equal(nrow(csv_data), 1)
  expect_equal(nrow(tsv_data), 1)
})

# --- Error Handling ---

test_that("readers handle non-existent files", {
  expect_error(
    read_nichenet("/nonexistent/path/file.csv"),
    regexp = "cannot open|No such file"
  )
})

test_that("readers handle empty files", {
  empty_file <- tempfile(fileext = ".csv")
  on.exit(unlink(empty_file), add = TRUE)
  writeLines("", empty_file)

  # Should error or return empty
  result <- tryCatch(
    read_nichenet(empty_file),
    error = function(e) NULL
  )

  expect_true(is.null(result) || nrow(result) == 0)
})

# --- Real Sample Files ---

test_that("read_nichenet works with package sample data", {
  sample_file <- system.file("extdata", "sample_nichenet.csv", package = "ccflowR")

  if (nzchar(sample_file) && file.exists(sample_file)) {
    data <- read_nichenet(sample_file)
    expect_true(is.data.frame(data))
    expect_true(nrow(data) > 0)
  } else {
    skip("Sample file not installed")
  }
})

test_that("read_multinichenet works with package sample data", {
  sample_file <- system.file("extdata", "sample_multinichenet.csv", package = "ccflowR")

  if (nzchar(sample_file) && file.exists(sample_file)) {
    data <- read_multinichenet(sample_file)
    expect_true(is.data.frame(data))
    expect_true("group" %in% colnames(data))
  } else {
    skip("Sample file not installed")
  }
})

test_that("read_liana works with package sample data", {
  sample_file <- system.file("extdata", "sample_liana.csv", package = "ccflowR")

  if (nzchar(sample_file) && file.exists(sample_file)) {
    data <- read_liana(sample_file)
    expect_true(is.data.frame(data))
    expect_true(all(c("sender", "receiver") %in% colnames(data)))
  } else {
    skip("Sample file not installed")
  }
})

test_that("read_liana accepts data frame directly", {
  df <- data.frame(
    source = c("A", "B"),
    target = c("B", "A"),
    ligand.complex = c("L1", "L2"),
    receptor.complex = c("R1", "R2"),
    aggregate_rank = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )

  result <- read_liana(df)

  expect_true("sender" %in% colnames(result))
  expect_true("activity" %in% colnames(result))
  expect_true(result$activity[1] > result$activity[2])  # lower rank = higher activity
})

test_that("read_nichenet handles custom activity column", {
  csv_content <- "sender,receiver,ligand,receptor,score
A,B,L1,R1,0.5
B,C,L2,R2,0.8"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_nichenet(tmp_file, activity_col = "score")

  expect_true("scaled_activity" %in% colnames(data))
  expect_equal(data$scaled_activity, c(0.5, 0.8))
})

test_that("read_nichenet handles no activity column gracefully", {
  csv_content <- "sender,receiver,ligand,receptor
A,B,L1,R1
B,C,L2,R2"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_nichenet(tmp_file, activity_col = NULL)

  expect_false("activity" %in% colnames(data))
})

test_that("read_multinichenet accepts data frame directly", {
  df <- data.frame(
    sender = c("A", "A"),
    receiver = c("B", "B"),
    ligand = c("L1", "L1"),
    receptor = c("R1", "R1"),
    group = c("G1", "G2"),
    prioritization_score = c(0.5, 0.8),
    stringsAsFactors = FALSE
  )

  result <- read_multinichenet(df)

  expect_true("group" %in% colnames(result))
  expect_true("scaled_activity" %in% colnames(result))
})

test_that("read_cellphonedb handles data without interacting_pair", {
  df <- data.frame(
    sender = "A",
    receiver = "B",
    ligand = "L1",
    receptor = "R1",
    stringsAsFactors = FALSE
  )

  result <- read_cellphonedb(df)
  expect_true(is.data.frame(result))
})

test_that("read_delimited auto-detects comma separator", {
  csv_content <- "a,b,c\n1,2,3"
  tmp_file <- create_temp_file(csv_content, "txt")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_nichenet(tmp_file, sender_col = "a", receiver_col = "b",
                         ligand_col = "c", receptor_col = "c")
  expect_equal(nrow(data), 1)
})

# --- Flow-conserving column handling ---

test_that("read_nichenet handles flow-conserving columns", {
  csv_content <- "sender,receiver,ligand,receptor,scaled_activity,scaled_ligand_expression,scaled_receptor_expression
A,B,L1,R1,0.8,0.7,0.9
B,C,L2,R2,0.6,0.5,0.4"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_nichenet(tmp_file)

  expect_true("scaled_activity" %in% colnames(data))
  expect_true("scaled_ligand_expression" %in% colnames(data))
  expect_true("scaled_receptor_expression" %in% colnames(data))
  expect_equal(attr(data, "framework"), "nichenet")
})

test_that("read_nichenet falls back to activity column", {
  csv_content <- "sender,receiver,ligand,receptor,activity
A,B,L1,R1,0.8"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_nichenet(tmp_file)

  # Should map activity to scaled_activity
  expect_true("scaled_activity" %in% colnames(data))
  expect_equal(data$scaled_activity[1], 0.8)
})

test_that("read_multinichenet maps differential expression columns", {
  csv_content <- "sender,receiver,ligand,receptor,group,prioritization_score,scaled_lfc_ligand,scaled_lfc_receptor
A,B,L1,R1,Disease,0.9,0.7,0.8
A,B,L1,R1,Control,0.3,0.2,0.1"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_multinichenet(tmp_file)

  # Check auto-mapped columns
  expect_true("scaled_activity" %in% colnames(data))
  expect_true("scaled_ligand_expression" %in% colnames(data))
  expect_true("scaled_receptor_expression" %in% colnames(data))

  # Check values are correctly mapped
  expect_equal(data$scaled_activity[1], 0.9)
  expect_equal(data$scaled_ligand_expression[1], 0.7)
  expect_equal(data$scaled_receptor_expression[1], 0.8)

  expect_equal(attr(data, "framework"), "multinichenet")
})

test_that("read_multinichenet respects auto_map=FALSE", {
  csv_content <- "sender,receiver,ligand,receptor,group,prioritization_score,scaled_lfc_ligand,scaled_lfc_receptor
A,B,L1,R1,Disease,0.9,0.7,0.8"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_multinichenet(tmp_file, auto_map = FALSE)

  # Should not have mapped columns
  expect_false("scaled_activity" %in% colnames(data))
  expect_false("scaled_ligand_expression" %in% colnames(data))

  # But should have the activity column (renamed from prioritization_score)
  expect_true("activity" %in% colnames(data))
})

test_that("read_multinichenet with custom lfc column names", {
  csv_content <- "sender,receiver,ligand,receptor,group,priority,lig_lfc,rec_lfc
A,B,L1,R1,Disease,0.9,0.7,0.8"
  tmp_file <- create_temp_file(csv_content, "csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data <- read_multinichenet(
    tmp_file,
    activity_col = "priority",
    lfc_ligand_col = "lig_lfc",
    lfc_receptor_col = "rec_lfc"
  )

  expect_true("scaled_activity" %in% colnames(data))
  expect_true("scaled_ligand_expression" %in% colnames(data))
  expect_true("scaled_receptor_expression" %in% colnames(data))
})
