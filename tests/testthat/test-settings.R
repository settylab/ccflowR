test_that("CCFlowSettings can be created directly", {
  settings <- CCFlowSettings$new()

  expect_s3_class(settings, "CCFlowSettings")
  expect_equal(settings$dpi(), 100)
  expect_equal(settings$dpi_save(), 300)
  expect_equal(settings$figwidth(), 600)
  expect_equal(settings$figheight(), 900)
  expect_equal(settings$font_family(), "Arial")
  expect_equal(settings$font_size(), 12)
  expect_equal(settings$fallback_color(), "#A0A0A0")
  expect_equal(settings$default_colormap(), "Set2")
  expect_equal(settings$verbose(), FALSE)
})

test_that("CCFlowSettings setters return self for chaining", {
  settings <- CCFlowSettings$new()

  result <- settings$dpi(150)
  expect_identical(result, settings)

  result <- settings$dpi_save(400)
  expect_identical(result, settings)

  result <- settings$figwidth(900)
  expect_identical(result, settings)

  result <- settings$figheight(700)
  expect_identical(result, settings)

  result <- settings$font_family("Helvetica")
  expect_identical(result, settings)

  result <- settings$font_size(14)
  expect_identical(result, settings)

  result <- settings$fallback_color("#CCCCCC")
  expect_identical(result, settings)

  result <- settings$default_colormap("Paired")
  expect_identical(result, settings)

  result <- settings$verbose(FALSE)
  expect_identical(result, settings)
})

test_that("CCFlowSettings set_figure_params works", {
  settings <- CCFlowSettings$new()

  result <- settings$set_figure_params(
    dpi = 200,
    dpi_save = 600,
    figwidth = 1000,
    figheight = 800,
    font_family = "Times",
    font_size = 16
  )

  expect_identical(result, settings)
  expect_equal(settings$dpi(), 200)
  expect_equal(settings$dpi_save(), 600)
  expect_equal(settings$figwidth(), 1000)
  expect_equal(settings$figheight(), 800)
  expect_equal(settings$font_family(), "Times")
  expect_equal(settings$font_size(), 16)
})

test_that("CCFlowSettings reset works", {
  settings <- CCFlowSettings$new()
  settings$dpi(999)
  settings$font_size(99)

  result <- settings$reset()

  expect_identical(result, settings)
  expect_equal(settings$dpi(), 100)
  expect_equal(settings$font_size(), 12)
})

test_that("CCFlowSettings print method works", {
  settings <- CCFlowSettings$new()

  expect_output(print(settings), "CCFlow Settings")
  expect_output(print(settings), "dpi: 100")
  expect_output(print(settings), "dpi_save: 300")
  expect_output(print(settings), "figwidth: 600")
  expect_output(print(settings), "figheight: 900")
  expect_output(print(settings), "font_family: Arial")
  expect_output(print(settings), "fallback_color:")
  expect_output(print(settings), "default_colormap:")
  expect_output(print(settings), "verbose:")
})

test_that("ccflow_settings returns singleton", {
  settings1 <- ccflow_settings()
  settings2 <- ccflow_settings()

  expect_identical(settings1, settings2)
})

test_that("settings have default values", {
  settings <- ccflow_settings()

  expect_equal(settings$dpi(), 100)
  expect_equal(settings$dpi_save(), 300)
  expect_type(settings$figwidth(), "integer")
  expect_type(settings$figheight(), "integer")
  expect_type(settings$font_family(), "character")
})

test_that("settings can be modified", {
  settings <- ccflow_settings()
  original_dpi <- settings$dpi()

  settings$dpi(200)
  expect_equal(settings$dpi(), 200)

  # Reset
  settings$dpi(original_dpi)
})

test_that("settings method chaining works", {
  settings <- ccflow_settings()
  original <- list(
    dpi = settings$dpi(),
    font_size = settings$font_size()
  )

  result <- settings$dpi(150)$font_size(14)

  expect_s3_class(result, "CCFlowSettings")
  expect_equal(settings$dpi(), 150)
  expect_equal(settings$font_size(), 14)

  # Reset
  settings$dpi(original$dpi)$font_size(original$font_size)
})

test_that("set_figure_params sets multiple params", {
  settings <- ccflow_settings()
  original <- list(
    dpi = settings$dpi(),
    figwidth = settings$figwidth()
  )

  settings$set_figure_params(dpi = 120, figwidth = 900)

  expect_equal(settings$dpi(), 120)
  expect_equal(settings$figwidth(), 900)

  # Reset
  settings$set_figure_params(dpi = original$dpi, figwidth = original$figwidth)
})

test_that("settings reset works", {
  settings <- ccflow_settings()
  settings$dpi(999)
  settings$reset()

  expect_equal(settings$dpi(), 100)  # Default value
})

test_that("settings print works", {
  settings <- ccflow_settings()

  expect_output(print(settings), "CCFlow Settings")
  expect_output(print(settings), "dpi:")
  expect_output(print(settings), "font_family:")
})

test_that("dpi_save getter and setter work", {
  settings <- ccflow_settings()
  original <- settings$dpi_save()

  result <- settings$dpi_save(600)
  expect_equal(settings$dpi_save(), 600)
  expect_s3_class(result, "CCFlowSettings")

  settings$dpi_save(original)
})

test_that("figwidth getter and setter work", {
  settings <- ccflow_settings()
  original <- settings$figwidth()

  result <- settings$figwidth(1000)
  expect_equal(settings$figwidth(), 1000)
  expect_s3_class(result, "CCFlowSettings")

  settings$figwidth(original)
})

test_that("figheight getter and setter work", {
  settings <- ccflow_settings()
  original <- settings$figheight()

  result <- settings$figheight(600)
  expect_equal(settings$figheight(), 600)
  expect_s3_class(result, "CCFlowSettings")

  settings$figheight(original)
})

test_that("font_family getter and setter work", {
  settings <- ccflow_settings()
  original <- settings$font_family()

  result <- settings$font_family("Helvetica")
  expect_equal(settings$font_family(), "Helvetica")
  expect_s3_class(result, "CCFlowSettings")

  settings$font_family(original)
})

test_that("fallback_color getter and setter work", {
  settings <- ccflow_settings()
  original <- settings$fallback_color()

  result <- settings$fallback_color("#FF0000")
  expect_equal(settings$fallback_color(), "#FF0000")
  expect_s3_class(result, "CCFlowSettings")

  settings$fallback_color(original)
})

test_that("default_colormap getter and setter work", {
  settings <- ccflow_settings()
  original <- settings$default_colormap()

  result <- settings$default_colormap("Paired")
  expect_equal(settings$default_colormap(), "Paired")
  expect_s3_class(result, "CCFlowSettings")

  settings$default_colormap(original)
})

test_that("verbose getter and setter work", {
  settings <- ccflow_settings()
  original <- settings$verbose()

  result <- settings$verbose(FALSE)
  expect_equal(settings$verbose(), FALSE)
  expect_s3_class(result, "CCFlowSettings")

  settings$verbose(original)
})

test_that("set_figure_params sets all parameters", {
  settings <- ccflow_settings()
  original <- list(
    dpi = settings$dpi(),
    dpi_save = settings$dpi_save(),
    figwidth = settings$figwidth(),
    figheight = settings$figheight(),
    font_family = settings$font_family(),
    font_size = settings$font_size()
  )

  settings$set_figure_params(
    dpi = 150,
    dpi_save = 450,
    figwidth = 850,
    figheight = 650,
    font_family = "Times",
    font_size = 16
  )

  expect_equal(settings$dpi(), 150)
  expect_equal(settings$dpi_save(), 450)
  expect_equal(settings$figwidth(), 850)
  expect_equal(settings$figheight(), 650)
  expect_equal(settings$font_family(), "Times")
  expect_equal(settings$font_size(), 16)

  # Reset
  settings$set_figure_params(
    dpi = original$dpi,
    dpi_save = original$dpi_save,
    figwidth = original$figwidth,
    figheight = original$figheight,
    font_family = original$font_family,
    font_size = original$font_size
  )
})
