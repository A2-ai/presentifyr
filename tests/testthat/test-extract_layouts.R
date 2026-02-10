test_that("extract_layouts fails when base_pptx file does not exist", {
  base_pptx <- tempfile()

  output_dir <- tempfile()
  dir.create(output_dir)

  expect_error(extract_layouts(base_pptx, output_dir), "Base PowerPoint file not found")
})

test_that("extract_layouts fails when base_pptx file is not a .pptx", {
  base_pptx <- tempfile(fileext = ".txt")
  file.create(base_pptx)

  output_dir <- tempfile()
  dir.create(output_dir)

  expect_error(extract_layouts(base_pptx, output_dir), "Invalid file type. Expected a .pptx file.")
})

test_that("extract_layouts fails when virtual environment does not exist", {
  base_pptx <- tempfile(fileext = ".pptx")
  file.create(base_pptx)

  output_dir <- tempfile()
  dir.create(output_dir)

  mockery::stub(extract_layouts, "reportifyr::get_venv_uv_paths", function() {
    stop("Create virtual environment with initialize_python")
  })

  expect_error(extract_layouts(base_pptx, output_dir), "Create virtual environment")
})

test_that("extract_layouts fails when Python script execution fails", {
  base_pptx <- tempfile(fileext = ".pptx")
  file.create(base_pptx)
  output_dir <- tempfile()
  dir.create(output_dir)

  mockery::stub(extract_layouts, "reportifyr::get_venv_uv_paths", function() {
    list(uv = "/mock/path/to/uv", venv = "/mock/venv")
  })

  mock_run_fail <- function(...) {
    e <- simpleError("Python script execution failed")
    e$status <- 1
    e$stdout <- ""
    e$stderr <- "Python script error occurred"
    stop(e)
  }

  mockery::stub(extract_layouts, "processx::run", mock_run_fail)

  expect_error(
    extract_layouts(base_pptx, output_dir),
    "Extract layouts Python script failed. Status:  1 Stderr:  Python script error occurred"
  )
})

test_that("extract_layouts correctly parses layout indices from filenames", {
  base_pptx <- tempfile(fileext = ".pptx")
  file.create(base_pptx)
  output_dir <- tempfile()
  dir.create(output_dir)

  mockery::stub(extract_layouts, "reportifyr::get_venv_uv_paths", function() {
    list(uv = "/mock/path/to/uv", venv = "/mock/venv")
  })
  mockery::stub(extract_layouts, "processx::run", function(...) {
    list(stdout = "Python script executed successfully", stderr = "", status = 0)
  })

  mock_images <- file.path(output_dir, c("layout_3.png", "layout_1.png", "layout_10.png"))
  mockery::stub(extract_layouts, "list.files", function(...) mock_images)

  result <- extract_layouts(base_pptx, output_dir)

  layout_names <- sub("\\.png$", "", basename(mock_images))  # Extract names without extension

  sorted_order <- order(layout_names)
  layout_names <- layout_names[sorted_order]
  layout_images <- mock_images[sorted_order]

  expected_df <- data.frame(
    layout_name = layout_names,
    image_path = layout_images,
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected_df)
})

