test_that("load_image_metadata constructs correct metadata filename", {
  dir <- tempfile()
  dir.create(dir)
  img_path <- file.path(dir, "plot.png")

  ## Create the expected metadata file: plot_png_metadata.json
  meta_path <- file.path(dir, "plot_png_metadata.json")
  jsonlite::write_json(list(key = "value"), meta_path, auto_unbox = TRUE)

  result <- load_image_metadata(img_path)
  expect_equal(result$key, "value")
})

test_that("load_image_metadata handles multi-dot filenames", {
  dir <- tempfile()
  dir.create(dir)
  img_path <- file.path(dir, "my.plot.v2.png")

  ## Filename convention: everything before last dot is name, extension is ext
  ## tools::file_path_sans_ext("my.plot.v2.png") → "my.plot.v2"
  ## tools::file_ext("my.plot.v2.png") → "png"
  meta_path <- file.path(dir, "my.plot.v2_png_metadata.json")
  jsonlite::write_json(list(found = TRUE), meta_path, auto_unbox = TRUE)

  result <- load_image_metadata(img_path)
  expect_true(result$found)
})

test_that("load_image_metadata returns NULL when metadata file does not exist", {
  dir <- tempfile()
  dir.create(dir)
  img_path <- file.path(dir, "no_metadata.png")

  result <- load_image_metadata(img_path)
  expect_null(result)
})

test_that("load_image_metadata returns NULL for malformed JSON", {
  dir <- tempfile()
  dir.create(dir)
  img_path <- file.path(dir, "bad.png")

  meta_path <- file.path(dir, "bad_png_metadata.json")
  writeLines("{ not valid json !!!", meta_path)

  result <- load_image_metadata(img_path)
  expect_null(result)
})

test_that("load_image_metadata returns full nested structure", {
  dir <- tempfile()
  dir.create(dir)
  img_path <- file.path(dir, "chart.png")

  metadata <- list(
    source_meta = list(path = "scripts/make_chart.R", latest_time = "2026-01-15"),
    object_meta = list(
      footnotes = list(
        notes = list("Sample size was 100"),
        abbreviations = list("CI", "HR")
      )
    )
  )
  meta_path <- file.path(dir, "chart_png_metadata.json")
  jsonlite::write_json(metadata, meta_path, auto_unbox = TRUE)

  result <- load_image_metadata(img_path)
  expect_equal(result$source_meta$path, "scripts/make_chart.R")
  expect_equal(result$object_meta$footnotes$notes[[1]], "Sample size was 100")
  expect_equal(result$object_meta$footnotes$abbreviations, list("CI", "HR"))
})
