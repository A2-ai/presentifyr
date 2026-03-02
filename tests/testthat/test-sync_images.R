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
  input_pptx <- create_temp_pptx()
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "parse_directory_for_images", function(...) character(0))

  expect_error(sync_images(input_pptx, output_pptx), "No image files found. Execution halted.")
})

test_that("sync_images fails when all image paths are empty strings", {
  input_pptx <- create_temp_pptx()
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "parse_directory_for_images", function(...) "")

  expect_error(sync_images(input_pptx, output_pptx), "No image files found. Execution halted.")
})

test_that("sync_images writes JSON with basename keys when images are outside project root", {
  input_pptx <- create_temp_pptx()
  output_pptx <- tempfile(fileext = ".pptx")

  mock_images <- c("/path/to/image1.png", "/path/to/image2.png")
  mockery::stub(sync_images, "parse_directory_for_images", function(...) mock_images)
  mockery::stub(sync_images, "run_python_script", mock_python_success)

  temp_json_file <- NULL
  mockery::stub(sync_images, "tempfile", function(fileext = ".json") {
    temp_json_file <<- tempfile(fileext = fileext)
    return(temp_json_file)
  })

  sync_images(input_pptx, output_pptx)

  written_json <- jsonlite::read_json(temp_json_file)

  ## Keys should be basenames since mock paths are outside any project root
  expect_equal(names(written_json), c("image1.png", "image2.png"))
  ## Values should be the full paths
  expect_equal(written_json[["image1.png"]], "/path/to/image1.png")
  expect_equal(written_json[["image2.png"]], "/path/to/image2.png")
})

test_that("sync_images writes JSON with relative keys when images are under project root", {
  root <- tempfile()
  dir.create(root)
  img_rel <- file.path(root, "figs", "plot.png")
  dir.create(dirname(img_rel), recursive = TRUE)
  file.create(img_rel)

  img_outside <- tempfile(fileext = ".png")

  input_pptx <- create_temp_pptx()
  output_pptx <- tempfile(fileext = ".pptx")

  mockery::stub(sync_images, "get_project_dir", function() root)
  mockery::stub(sync_images, "parse_directory_for_images", function(...) c(img_rel, img_outside))
  mockery::stub(sync_images, "run_python_script", mock_python_success)
  temp_json_file <- tempfile(fileext = ".json")
  mockery::stub(sync_images, "tempfile", function(fileext = ".json") temp_json_file)

  sync_images(input_pptx, output_pptx)

  written_json <- jsonlite::read_json(temp_json_file)
  ## In-root image gets a relative key
  expect_true("figs/plot.png" %in% names(written_json))
  ## Outside-root image falls back to basename key
  expect_true(basename(img_outside) %in% names(written_json))
})
