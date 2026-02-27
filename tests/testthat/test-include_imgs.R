test_that("include_imgs matches common image extensions", {
  pattern <- include_imgs()

  ## Hardcoded expectations — these must always be supported
  expect_true(grepl(pattern, "photo.png"))
  expect_true(grepl(pattern, "photo.jpg"))
  expect_true(grepl(pattern, "photo.jpeg"))
  expect_true(grepl(pattern, "chart.bmp"))
  expect_true(grepl(pattern, "anim.gif"))
  expect_true(grepl(pattern, "scan.tiff"))
  expect_true(grepl(pattern, "scan.tif"))
  expect_true(grepl(pattern, "icon.webp"))
})

test_that("include_imgs excludes pdf", {
  pattern <- include_imgs()

  ## pdf is explicitly excluded in the function via setdiff(..., "pdf")
  expect_false(grepl(pattern, "document.pdf"))
})

test_that("include_imgs does not match non-image extensions", {
  pattern <- include_imgs()

  expect_false(grepl(pattern, "data.csv"))
  expect_false(grepl(pattern, "notes.txt"))
  expect_false(grepl(pattern, "report.docx"))
  expect_false(grepl(pattern, "script.R"))
  expect_false(grepl(pattern, "config.json"))
})

test_that("include_imgs returns a valid regex anchored to end of string", {
  pattern <- include_imgs()

  expect_type(pattern, "character")
  expect_length(pattern, 1)
  ## Pattern should end with $ to anchor to end of filename
  expect_true(grepl("\\$$", pattern))
  ## Should not match mid-string (e.g., "png_backup.txt")
  expect_false(grepl(pattern, "png_backup.txt"))
})
