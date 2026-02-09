test_that("sync_images fails when input_pptx does not exist", {
  input_pptx <- tempfile(fileext = ".pptx")
  output_pptx <- tempfile(fileext = ".pptx")

  expect_error(sync_images(input_pptx, output_pptx), "The input .pptx file does not exist:")
})

test_that("sync_images fails when input_pptx file is not a .pptx", {
  input_pptx <- tempfile(fileext = ".txt")
  file.create(input_pptx)

  output_pptx <- tempfile(fileext = ".pptx")

  expect_error(sync_images(input_pptx, output_pptx), "Invalid file type. Expected a .pptx file.")
})

test_that("sync_images fails when no images are found", {
  input_pptx <- tempfile(fileext = ".pptx")
  file.create(input_pptx)
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "parse_directory_for_images", function(...) character(0))

  expect_error(sync_images(input_pptx, output_pptx), "No image files found. Execution halted.")
})

test_that("sync_images fails when no images are found (empty string)", {
  input_pptx <- tempfile(fileext = ".pptx")
  file.create(input_pptx)
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "parse_directory_for_images", function(...) "")

  expect_error(sync_images(input_pptx, output_pptx), "No image files found. Execution halted.")
})

test_that("sync_images correctly creates and writes JSON dictionary", {
  input_pptx <- tempfile(fileext = ".pptx")
  file.create(input_pptx)
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "dir.exists", function(path) TRUE)

  mock_images <- c("/path/to/image1.png", "/path/to/image2.png")
  mockery::stub(sync_images, "parse_directory_for_images", function(...) mock_images)
  mockery::stub(sync_images, "processx::run", function(...) {
    list(stdout = "Python script executed successfully", stderr = "", status = 0)
  })

  temp_json_file <- NULL
  mockery::stub(sync_images, "tempfile", function(fileext = ".json") {
    temp_json_file <<- tempfile(fileext = fileext)
    return(temp_json_file)
  })

  sync_images(input_pptx, output_pptx)

  written_json <- jsonlite::read_json(temp_json_file)
  expected_json <- as.list(stats::setNames(mock_images, basename(mock_images)))

  expect_equal(written_json, expected_json)
})

test_that("sync_images uses relative keys when under project root and falls back to basename", {
  root <- tempfile()
  dir.create(root)
  img_rel <- file.path(root, "figs", "plot.png")
  dir.create(dirname(img_rel), recursive = TRUE)
  file.create(img_rel)

  img_outside <- tempfile(fileext = ".png")

  input_pptx <- tempfile(fileext = ".pptx")
  file.create(input_pptx)
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "getOption", function(x, default) {
    if (identical(x, "project.dir")) return(root)
    base::getOption(x, default = default)
  })
  mockery::stub(sync_images, "dir.exists", function(...) TRUE)
  mockery::stub(sync_images, "parse_directory_for_images", function(...) c(img_rel, img_outside))
  mockery::stub(sync_images, "get_uv_path", function() "/mock/uv")
  mockery::stub(sync_images, "processx::run", function(...) list(status = 0))
  temp_json_file <- tempfile(fileext = ".json")
  mockery::stub(sync_images, "tempfile", function(fileext = ".json") temp_json_file)

  sync_images(input_pptx, output_pptx)

  written_json <- jsonlite::read_json(temp_json_file)
  expect_true("figs/plot.png" %in% names(written_json))
  # outside root should be skipped/unsupported; ensure only the in-root key is present
  expect_equal(names(written_json), "figs/plot.png")
})

test_that("sync_images fails when virtual environment does not exist", {
  input_pptx <- tempfile(fileext = ".pptx")
  file.create(input_pptx)
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "dir.exists", function(path) FALSE)

  mock_images <- c("/path/to/image1.png", "/path/to/image2.png")
  mockery::stub(sync_images, "parse_directory_for_images", function(...) mock_images)

  expect_error(sync_images(input_pptx, output_pptx), "Create virtual environment with initialize_python")
})


test_that("sync_images fails when Python script execution fails", {
  input_pptx <- tempfile(fileext = ".pptx")
  file.create(input_pptx)
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "dir.exists", function(path) TRUE)
  mockery::stub(sync_images, "get_uv_path", function() "/mock/path/to/uv")

  mock_images <- c("/path/to/image1.png", "/path/to/image2.png")
  mockery::stub(sync_images, "parse_directory_for_images", function(...) mock_images)

  mock_run_fail <- function(...) {
    e <- simpleError("Python script execution failed")
    e$status <- 1
    e$stdout <- ""
    e$stderr <- "Python script error occurred"
    stop(e)
  }
  mockery::stub(sync_images, "processx::run", mock_run_fail)

  expect_error(
    sync_images(input_pptx, output_pptx),
    "Sync images script failed. Status:  1 Stderr:  Python script error occurred"
  )
})
