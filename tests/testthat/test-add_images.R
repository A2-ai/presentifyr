test_that("add_images creates config JSON with correct slide groups, positions, and font settings", {
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(add_images, "run_python_script", mock_python_success)

  temp_config_file <- NULL
  mockery::stub(add_images, "tempfile", function(fileext = ".json") {
    temp_config_file <<- tempfile(fileext = fileext)
    return(temp_config_file)
  })

  add_images(
    files = c("/path/to/img1.png", "/path/to/img2.png"),
    output_pptx = output_pptx,
    slide_layout_name = "Two Content",
    slide_groups = list(c("/path/to/img1.png", "/path/to/img2.png")),
    slide_positions = list(c(1L, 2L)),
    font_settings = list(font_name = "Arial", font_size = 10)
  )

  config <- jsonlite::read_json(temp_config_file)

  expect_equal(config$slide_layout_name, "Two Content")
  expect_equal(config$slide_groups, list(list("/path/to/img1.png", "/path/to/img2.png")))
  expect_equal(config$slide_positions, list(list(1L, 2L)))
  expect_equal(config$font_settings$font_name, "Arial")
  expect_equal(config$font_settings$font_size, 10)
})

test_that("add_images populates image_keys from prfy_image_key for each unique file", {
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(add_images, "run_python_script", mock_python_success)

  temp_config_file <- NULL
  mockery::stub(add_images, "tempfile", function(fileext = ".json") {
    temp_config_file <<- tempfile(fileext = fileext)
    return(temp_config_file)
  })

  add_images(
    files = c("/path/to/img1.png", "/path/to/img2.png"),
    output_pptx = output_pptx,
    slide_groups = list(c("/path/to/img1.png"), c("/path/to/img2.png"))
  )

  config <- jsonlite::read_json(temp_config_file)

  ## image_keys should map absolute path → key for each unique file
  expect_true("image_keys" %in% names(config))
  expect_equal(length(config$image_keys), 2)
  ## Since paths are outside any project root, keys fall back to basename
  expect_equal(config$image_keys[["/path/to/img1.png"]], "img1.png")
  expect_equal(config$image_keys[["/path/to/img2.png"]], "img2.png")
})

test_that("add_images passes -b and -o flags to run_python_script", {
  output_pptx <- tempfile(fileext = ".pptx")
  base_pptx <- create_temp_pptx()

  captured_args <- NULL
  mockery::stub(add_images, "run_python_script", function(script_args, label) {
    captured_args <<- script_args
    list(stdout = "", stderr = "", status = 0)
  })

  add_images(
    files = c("/path/to/image.png"),
    output_pptx = output_pptx,
    base_pptx = base_pptx
  )

  expect_true("-b" %in% captured_args)
  expect_true(base_pptx %in% captured_args)
  expect_true("-o" %in% captured_args)
  expect_true(output_pptx %in% captured_args)
})

test_that("add_images omits -b flag when base_pptx is NULL", {
  output_pptx <- tempfile(fileext = ".pptx")

  captured_args <- NULL
  mockery::stub(add_images, "run_python_script", function(script_args, label) {
    captured_args <<- script_args
    list(stdout = "", stderr = "", status = 0)
  })

  add_images(
    files = c("/path/to/image.png"),
    output_pptx = output_pptx,
    base_pptx = NULL
  )

  expect_false("-b" %in% captured_args)
})

test_that("add_images embeds abbreviation_definitions in the config JSON", {
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(add_images, "run_python_script", mock_python_success)
  mockery::stub(add_images, "load_abbreviation_definitions", function() {
    list(CI = "confidence interval", HR = "hazard ratio")
  })

  temp_config_file <- NULL
  mockery::stub(add_images, "tempfile", function(fileext = ".json") {
    temp_config_file <<- tempfile(fileext = fileext)
    return(temp_config_file)
  })

  add_images(
    files = c("/path/to/img1.png"),
    output_pptx = output_pptx
  )

  config <- jsonlite::read_json(temp_config_file)

  expect_true("abbreviation_definitions" %in% names(config))
  expect_equal(config$abbreviation_definitions$CI, "confidence interval")
  expect_equal(config$abbreviation_definitions$HR, "hazard ratio")
})

test_that("add_images uses single-image mode when slide_groups is NULL", {
  output_pptx <- tempfile(fileext = ".pptx")

  temp_config_file <- NULL
  mockery::stub(add_images, "run_python_script", mock_python_success)
  mockery::stub(add_images, "tempfile", function(fileext = ".json") {
    temp_config_file <<- tempfile(fileext = fileext)
    return(temp_config_file)
  })

  add_images(
    files = c("/path/to/img1.png", "/path/to/img2.png"),
    output_pptx = output_pptx
  )

  config <- jsonlite::read_json(temp_config_file)

  ## Single-image mode: each file becomes its own slide group
  expect_equal(length(config$slide_groups), 2)
  ## Single-element vectors are auto-unboxed to scalars by write_json(auto_unbox=TRUE),
  ## so read_json returns them as length-1 character/integer vectors, not lists.
  expect_equal(config$slide_groups[[1]], "/path/to/img1.png")
  expect_equal(config$slide_groups[[2]], "/path/to/img2.png")
  expect_equal(config$slide_positions[[1]], 1L)
  expect_equal(config$slide_positions[[2]], 1L)
})
