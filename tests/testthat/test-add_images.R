test_that("add_images fails when Python script execution fails", {
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(add_images, "run_python_script", function(script_args, label) {
    e <- simpleError("Python script execution failed")
    e$status <- 1
    e$stdout <- ""
    e$stderr <- "Python script error occurred"
    stop(e)
  })

  expect_error(
    add_images(
      files = c("/path/to/image1.png"),
      output_pptx = output_pptx,
      base_pptx = NULL
    ),
    "Python script execution failed"
  )
})

test_that("add_images correctly creates config JSON with slide groups and positions", {
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(add_images, "run_python_script", function(script_args, label) {
    list(stdout = "Python script executed successfully", stderr = "", status = 0)
  })

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
  expect_true("image_keys" %in% names(config))
})

test_that("add_images forwards base_pptx argument to Python script", {
  output_pptx <- tempfile(fileext = ".pptx")
  base_pptx <- tempfile(fileext = ".pptx")
  file.create(base_pptx)

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
})

test_that("add_images does not include -b flag when base_pptx is NULL", {
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

test_that("add_images fails when virtual environment does not exist", {
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(add_images, "run_python_script", function(script_args, label) {
    stop("Create virtual environment with initialize_python")
  })

  expect_error(
    add_images(
      files = c("/path/to/image.png"),
      output_pptx = output_pptx
    ),
    "Create virtual environment"
  )
})

test_that("add_images uses single-image mode when slide_groups is NULL", {
  output_pptx <- tempfile(fileext = ".pptx")

  temp_config_file <- NULL
  mockery::stub(add_images, "run_python_script", function(script_args, label) {
    list(stdout = "", stderr = "", status = 0)
  })
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
