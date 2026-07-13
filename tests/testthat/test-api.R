# Tests for the new ggplot-like API (R/api.R)

# ==============================================================================
# sankey_plot() constructor tests
# ==============================================================================

test_that("sankey_plot creates sankey object", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors)

  expect_s3_class(p, "sankey")
  expect_true(".internal" %in% names(p))
})

test_that("sankey_plot works with named vector colors", {
  ex <- example_communication()
  colors <- c(Macrophage = "#E41A1C", T_cell = "#377EB8", B_cell = "#4DAF4A")
  p <- sankey_plot(ex$data, colors)


  expect_s3_class(p, "sankey")
})

test_that("sankey_plot works without colors", {
  ex <- example_communication()
  p <- sankey_plot(ex$data)

  expect_s3_class(p, "sankey")
})

test_that("sankey_plot accepts flow parameter", {
  ex <- example_communication()

  # uniform flow should work without expression columns
  p <- sankey_plot(ex$data, ex$colors, flow = "uniform")
  expect_s3_class(p, "sankey")

  # flow_conserving should fall back to uniform when columns missing
  p <- sankey_plot(ex$data, ex$colors, flow = "flow_conserving")
  expect_s3_class(p, "sankey")
})

test_that("sankey_plot accepts group_col parameter", {
  mn_ex <- example_multinichenet()
  p <- sankey_plot(mn_ex$data, mn_ex$colors, group_col = "group")

  expect_s3_class(p, "sankey")
})


# ==============================================================================
# + operator tests
# ==============================================================================

test_that("+ operator works with sankey_colors", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors) + sankey_colors(by = "sender")

  expect_s3_class(p, "sankey")
})

test_that("+ operator chains multiple layers", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors) +
    sankey_colors(by = "receiver") +
    sankey_alpha(0.5) +
    sankey_title("Test")

  expect_s3_class(p, "sankey")
})

test_that("+ operator fails with non-layer objects", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors)

  expect_error(p + "not a layer", "sankey_layer")
  expect_error(p + 123, "sankey_layer")
  expect_error(p + list(a = 1), "sankey_layer")
})


# ==============================================================================
# Layer function tests
# ==============================================================================

test_that("sankey_colors creates layer", {
  layer <- sankey_colors(by = "sender")
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "colors")
  expect_equal(layer$by, "sender")
})

test_that("sankey_colors accepts fixed color string", {
  layer <- sankey_colors(by = "steelblue")
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "colors")
  expect_equal(layer$by, "steelblue")
})

test_that("sankey_alpha creates layer", {
  layer <- sankey_alpha(0.7)
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "alpha")
  expect_equal(layer$value, 0.7)
})

test_that("sankey_alpha validates range", {
  expect_error(sankey_alpha(-0.1), "between 0 and 1")
  expect_error(sankey_alpha(1.5), "between 0 and 1")
})

test_that("sankey_highlight creates layer", {
  layer <- sankey_highlight(senders = "Macrophage", dim_alpha = 0.1)
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "highlight")
  expect_equal(layer$senders, "Macrophage")
  expect_equal(layer$dim_alpha, 0.1)
})

test_that("sankey_nodes creates layer", {
  layer <- sankey_nodes(thickness = 25, padding = 35)
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "nodes")
  expect_equal(layer$thickness, 25)
  expect_equal(layer$padding, 35)
})

test_that("sankey_order creates layer", {
  layer <- sankey_order(senders = c("A", "B"), receivers = c("C", "D"))
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "order")
  expect_equal(layer$senders, c("A", "B"))
  expect_equal(layer$receivers, c("C", "D"))
})

test_that("sankey_labels creates layer", {
  layer <- sankey_labels(size = 14)
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "labels")
  expect_equal(layer$size, 14)
})

test_that("sankey_layout creates layer", {
  layer <- sankey_layout(width = 800, height = 600)
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "layout")
  expect_equal(layer$width, 800)
  expect_equal(layer$height, 600)
})

test_that("sankey_title creates layer", {
  layer <- sankey_title("My Title")
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "title")
  expect_equal(layer$title, "My Title")
})

test_that("sankey_interactive creates layer", {
  layer <- sankey_interactive()
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "interactive")
  expect_equal(layer$enable, TRUE)
})

test_that("sankey_margins creates layer", {
  layer <- sankey_margins(left = 100, right = 100, top = 80, bottom = 50)
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "margins")
  expect_equal(layer$left, 100)
})

test_that("sankey_background creates layer", {
  layer <- sankey_background(paper = "#F5F5F5", plot = "#FFFFFF")
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "background")
  expect_equal(layer$paper, "#F5F5F5")
})

test_that("sankey_font creates layer", {
  layer <- sankey_font(family = "Helvetica", size = 14)
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "font")
  expect_equal(layer$family, "Helvetica")
})

test_that("sankey_orientation creates layer", {
  layer <- sankey_orientation("vertical")
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "orientation")
  expect_equal(layer$orientation, "v")
})

test_that("sankey_legend creates layer", {
  layer <- sankey_legend(show = FALSE, title = "Cell Types")
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "legend")
  expect_equal(layer$show, FALSE)
})

test_that("sankey_group_colors creates layer", {
  colors <- c(Control = "#ADD8E6", Treatment = "#F08080")
  layer <- sankey_group_colors(colors)
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "group_colors")
})

test_that("sankey_node_colors creates layer", {
  layer <- sankey_node_colors("weighted_mix")
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "node_colors")
  expect_equal(layer$mode, "weighted_mix")
})

test_that("sankey_clear_highlight creates layer", {
  layer <- sankey_clear_highlight()
  expect_s3_class(layer, "sankey_layer")
  expect_equal(layer$type, "clear_highlight")
})


# ==============================================================================
# S3 method tests
# ==============================================================================

test_that("summary.sankey returns configuration", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors)
  s <- summary(p)

  expect_type(s, "list")
  expect_true("n_interactions" %in% names(s))
  expect_true("n_senders" %in% names(s))
  expect_true("color_by" %in% names(s))
})


# ==============================================================================
# sankey_render tests
# ==============================================================================

test_that("sankey_render returns plotly figure", {
  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors) + sankey_title("Test")
  fig <- sankey_render(p)

  expect_s3_class(fig, "plotly")
})

test_that("sankey_render fails with non-sankey object", {
  expect_error(sankey_render("not a sankey"), "sankey object")
  expect_error(sankey_render(list(a = 1)), "sankey object")
})


# ==============================================================================
# sankey_save tests
# ==============================================================================

test_that("sankey_save saves HTML file", {
  skip_if_not_installed("htmlwidgets")
  skip_if_no_pandoc()

  ex <- example_communication()
  p <- sankey_plot(ex$data, ex$colors) + sankey_title("Save Test")

  tmp_file <- tempfile(fileext = ".html")
  result <- sankey_save(p, tmp_file)

  expect_true(file.exists(tmp_file))
  expect_s3_class(result, "sankey")  # Returns plot for chaining

  unlink(tmp_file)
})

test_that("sankey_save fails with non-sankey object", {
  expect_error(sankey_save("not a sankey", "test.html"), "sankey object")
})


# ==============================================================================
# Integration tests
# ==============================================================================

test_that("all color strategies work with new API", {
  ex <- example_communication()

  for (color_by in c("sender", "receiver", "ligand", "condition")) {
    p <- sankey_plot(ex$data, ex$colors) + sankey_colors(by = color_by)
    expect_s3_class(p, "sankey")
    fig <- sankey_render(p)
    expect_s3_class(fig, "plotly")
  }
})

test_that("complex composition works", {
  ex <- example_communication()

  p <- sankey_plot(ex$data, ex$colors, flow = "uniform") +
    sankey_colors(by = "sender") +
    sankey_alpha(0.6) +
    sankey_font(family = "Helvetica", size = 10) +
    sankey_title("Complex Test") +
    sankey_layout(width = 800, height = 600) +
    sankey_nodes(thickness = 25, padding = 35) +
    sankey_labels(size = 12) +
    sankey_margins(left = 60, right = 60)

  expect_s3_class(p, "sankey")
  fig <- sankey_render(p)
  expect_s3_class(fig, "plotly")
})

test_that("MultiNicheNet group coloring works with new API", {
  mn_ex <- example_multinichenet()

  p <- sankey_plot(mn_ex$data, mn_ex$colors, group_col = "group") +
    sankey_colors(by = "condition") +
    sankey_group_colors(mn_ex$group_colors) +
    sankey_title("MultiNicheNet Test")

  expect_s3_class(p, "sankey")
  fig <- sankey_render(p)
  expect_s3_class(fig, "plotly")
})

test_that("highlighting works with new API", {
  ex <- example_communication()

  p <- sankey_plot(ex$data, ex$colors) +
    sankey_highlight(senders = "Macrophage", dim_alpha = 0.1)

  expect_s3_class(p, "sankey")
  fig <- sankey_render(p)
  expect_s3_class(fig, "plotly")
})

test_that("clearing highlight works with new API", {
  ex <- example_communication()

  p <- sankey_plot(ex$data, ex$colors) +
    sankey_highlight(senders = "Macrophage") +
    sankey_clear_highlight()

  expect_s3_class(p, "sankey")
})


# ==============================================================================
# Backward compatibility tests
# ==============================================================================

test_that("old R6 API still works", {
  ex <- example_communication()

  # Old API should still work
  plot <- SankeyPlot$new(ex$data, ex$colors)
  plot$colors(color_by = "sender")
  plot$alpha(0.7)
  plot$add_title("Old API Test")
  fig <- plot$render()

  expect_s3_class(fig, "plotly")
})

test_that("sankey_flow convenience function still works", {
  ex <- example_communication()

  # Old convenience function should still work
  expect_no_error({
    fig <- sankey_flow(ex$data, ex$colors,
                       color_by = "sender",
                       title = "Flow Test",
                       alpha = 0.7)
  })
})


# ==============================================================================
# Alias function tests
# ==============================================================================

test_that("sankey_unhighlight is alias for sankey_clear_highlight", {
  ex <- example_communication()

  p <- sankey_plot(ex$data, ex$colors) +
    sankey_highlight(senders = "Macrophage") +
    sankey_unhighlight()

  expect_s3_class(p, "sankey")
})

test_that("hex_to_rgba is alias for hex_with_alpha", {
  # Both functions should produce the same result
  result1 <- hex_with_alpha("#FF0000", 0.5)
  result2 <- hex_to_rgba("#FF0000", 0.5)

  expect_equal(result1, result2)
  expect_equal(result1, "rgba(255, 0, 0, 0.50)")
})

test_that("get_all_celltypes returns vector", {
  df <- data.frame(sender = c("A", "B", "A"), receiver = c("B", "C", "C"))

  # get_all_celltypes returns a vector
  celltypes <- get_all_celltypes(df)
  expect_type(celltypes, "character")
  expect_true(is.vector(celltypes))
  expect_setequal(celltypes, c("A", "B", "C"))

  # extract_celltypes returns a list (different behavior by design)
  celltypes_list <- extract_celltypes(df)
  expect_type(celltypes_list, "list")
  expect_true("senders" %in% names(celltypes_list))
  expect_true("receivers" %in% names(celltypes_list))
  expect_true("all" %in% names(celltypes_list))
})
