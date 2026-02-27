test_that("extract_layouts fails when base_pptx file does not exist", {
  base_pptx <- tempfile()

  output_dir <- tempfile()
  dir.create(output_dir)

  expect_error(extract_layouts(base_pptx, output_dir), "The input .pptx file does not exist:")
})

test_that("extract_layouts fails when base_pptx file is not a .pptx", {
  base_pptx <- tempfile(fileext = ".txt")
  file.create(base_pptx)

  output_dir <- tempfile()
  dir.create(output_dir)

  expect_error(extract_layouts(base_pptx, output_dir), "Invalid file type. Expected a .pptx file.")
})

test_that("extract_layouts fails when virtual environment does not exist", {
  base_pptx <- create_temp_pptx()
  output_dir <- tempfile()
  dir.create(output_dir)

  mockery::stub(extract_layouts, "run_python_script", mock_venv_missing)

  expect_error(extract_layouts(base_pptx, output_dir), "Create virtual environment")
})

test_that("extract_layouts fails when Python script execution fails", {
  base_pptx <- create_temp_pptx()
  output_dir <- tempfile()
  dir.create(output_dir)

  mockery::stub(extract_layouts, "run_python_script", mock_python_failure)

  expect_error(
    extract_layouts(base_pptx, output_dir),
    "Python script execution failed"
  )
})

test_that("extract_layouts correctly parses layout indices from filenames", {
  base_pptx <- create_temp_pptx()
  output_dir <- tempfile()
  dir.create(output_dir)

  mockery::stub(extract_layouts, "run_python_script", mock_python_success)

  mock_images <- file.path(output_dir, c("layout_3.png", "layout_1.png", "layout_10.png"))
  mockery::stub(extract_layouts, "list.files", function(...) mock_images)

  result <- extract_layouts(base_pptx, output_dir)

  layout_names <- sub("\\.png$", "", basename(mock_images))  # Extract names without extension

  sorted_order <- order(layout_names)
  layout_names <- layout_names[sorted_order]
  layout_images <- mock_images[sorted_order]

  expected_df <- data.frame(
    layout_name = layout_names,
    original_name = layout_names,
    image_path = layout_images,
    placeholder_count = NA_integer_,
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected_df)
})
