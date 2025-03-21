test_that("include_imgs returns expected regex pattern", {
  expected_extensions <- setdiff(pkglite::ext_binary(flat = FALSE)$figure, "pdf")
  expected_pattern <- paste0("\\.(", paste(expected_extensions, collapse = "|"), ")$")

  result <- include_imgs()

  expect_type(result, "character")
  expect_equal(result, expected_pattern)
})

test_that("include_imgs correctly matches valid image file extensions", {
  pattern <- include_imgs()

  expected_extensions <- setdiff(pkglite::ext_binary(flat = FALSE)$figure, "pdf")

  for (ext in expected_extensions) {
    test_filename <- paste0("image.", ext)
    expect_true(grepl(pattern, test_filename), info = paste("Failed for extension:", ext))
  }
})

test_that("include_imgs does not match invalid file extensions", {
  pattern <- include_imgs()

  non_matching <- c("txt", "csv", "docx")

  for (ext in non_matching) {
    test_filename <- paste0("file.", ext)
    expect_false(grepl(pattern, test_filename), info = paste("Incorrectly matched extension:", ext))
  }
})
