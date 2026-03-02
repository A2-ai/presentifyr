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

test_that("extract_layouts returns sorted dataframe with correct columns", {
  base_pptx <- create_temp_pptx()
  output_dir <- tempfile()
  dir.create(output_dir)

  mockery::stub(extract_layouts, "run_python_script", mock_python_success)

  mock_images <- file.path(output_dir, c("layout_3.png", "layout_1.png", "layout_10.png"))
  mockery::stub(extract_layouts, "list.files", function(...) mock_images)

  result <- extract_layouts(base_pptx, output_dir)

  ## Correct column names
  expect_named(result, c("layout_name", "original_name", "image_path", "placeholder_count"))

  ## Sorted alphabetically by layout_name
  expect_equal(result$layout_name, c("layout_1", "layout_10", "layout_3"))

  ## Without metadata, placeholder_count is NA and original_name falls back to layout_name
  expect_true(all(is.na(result$placeholder_count)))
  expect_equal(result$original_name, result$layout_name)
})

test_that("extract_layouts reads placeholder_count and original_name from metadata JSON", {
  base_pptx <- create_temp_pptx()
  output_dir <- tempfile()
  dir.create(output_dir)

  mockery::stub(extract_layouts, "run_python_script", mock_python_success)

  ## Create mock layout images
  mock_images <- file.path(output_dir, c("Title_Slide.png", "Two_Content.png"))
  mockery::stub(extract_layouts, "list.files", function(...) mock_images)

  ## Create metadata JSON with placeholder counts and original names
  metadata <- list(
    Title_Slide = list(placeholder_count = 1L, layout_name = "Title Slide"),
    Two_Content = list(placeholder_count = 2L, layout_name = "Two Content")
  )
  jsonlite::write_json(metadata, file.path(output_dir, "layout_metadata.json"),
                       auto_unbox = TRUE)

  result <- extract_layouts(base_pptx, output_dir)

  ## Sorted: Title_Slide before Two_Content
  expect_equal(result$layout_name, c("Title_Slide", "Two_Content"))
  expect_equal(result$original_name, c("Title Slide", "Two Content"))
  expect_equal(result$placeholder_count, c(1L, 2L))
})
