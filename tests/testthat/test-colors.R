test_that("hex_to_rgb converts correctly", {
  rgb <- hex_to_rgb("#FF0000")
  expect_equal(unname(rgb["r"]), 255)
  expect_equal(unname(rgb["g"]), 0)
  expect_equal(unname(rgb["b"]), 0)

  rgb2 <- hex_to_rgb("00FF00")  # Without #
  expect_equal(unname(rgb2["r"]), 0)
  expect_equal(unname(rgb2["g"]), 255)
  expect_equal(unname(rgb2["b"]), 0)
})

test_that("rgb_to_hex converts correctly", {
  hex <- rgb_to_hex(255, 0, 0)
  expect_equal(hex, "#FF0000")

  hex2 <- rgb_to_hex(0, 255, 0)
  expect_equal(hex2, "#00FF00")
})

test_that("hex_with_alpha creates rgba string", {
  rgba <- hex_with_alpha("#FF0000", 0.5)
  expect_match(rgba, "^rgba\\(255, 0, 0, 0\\.50\\)$")
})

test_that("weighted_mix_colors mixes correctly", {
  # Equal mix of red and blue should give purple-ish
  mixed <- weighted_mix_colors(c("#FF0000", "#0000FF"), c(1, 1))
  expect_type(mixed, "character")
  expect_match(mixed, "^#[0-9A-F]{6}$")

  # Single color returns itself
  single <- weighted_mix_colors("#FF0000")
  expect_equal(single, "#FF0000")

  # Empty returns fallback
  empty <- weighted_mix_colors(character(0))
  expect_type(empty, "character")
})

test_that("ColorPalette creates and manages colors", {
  palette <- ColorPalette$new()

  # Should auto-assign colors
  color1 <- palette$get("TypeA")
  expect_type(color1, "character")

  # Same type should return same color
  color1_again <- palette$get("TypeA")
  expect_equal(color1, color1_again)

  # Can set custom colors
  palette$set("Custom", "#123456")
  expect_equal(palette$get("Custom"), "#123456")
})

test_that("ColorPalette initializes with colors", {
  initial_colors <- list(A = "#FF0000", B = "#00FF00")
  palette <- ColorPalette$new(colors = initial_colors)

  expect_equal(palette$get("A"), "#FF0000")
  expect_equal(palette$get("B"), "#00FF00")
})

test_that("ColorPalette update works", {
  palette <- ColorPalette$new()
  palette$update(list(X = "#111111", Y = "#222222"))

  expect_equal(palette$get("X"), "#111111")
  expect_equal(palette$get("Y"), "#222222")
})

test_that("create_palette assigns colors to categories", {
  categories <- c("Cat1", "Cat2", "Cat3")
  palette <- create_palette(categories)

  for (cat in categories) {
    expect_true(palette$has(cat))
    expect_type(palette$get(cat), "character")
  }
})
