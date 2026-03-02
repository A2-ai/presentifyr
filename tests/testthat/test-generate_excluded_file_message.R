test_that("generate_excluded_file_message returns HTML with basenames of excluded files", {
  excluded_files <- c("path/to/file1.txt", "some/other/file2.pdf")

  result <- generate_excluded_file_message(excluded_files)

  ## Should contain basenames, not full paths
  expect_true(grepl("file1.txt", result))
  expect_true(grepl("file2.pdf", result))
  expect_false(grepl("path/to/", result))

  ## Should have list item markup
  expect_true(grepl("<li>file1.txt</li>", result, fixed = TRUE))
  expect_true(grepl("<li>file2.pdf</li>", result, fixed = TRUE))

  ## Should contain the warning icon
  expect_true(grepl("&#10071;", result, fixed = TRUE))
})

test_that("generate_excluded_file_message returns empty for no excluded files", {
  result <- generate_excluded_file_message(character(0))
  expect_equal(result, NULL)
})

test_that("generate_excluded_file_message works with a single file", {
  result <- generate_excluded_file_message("dir/only_file.csv")

  expect_true(grepl("<li>only_file.csv</li>", result, fixed = TRUE))
})
