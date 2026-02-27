test_that("list_files_and_dirs returns all files and non-empty dirs when pattern matches everything", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, "file1.txt"))
  file.create(file.path(path, "file2.png"))

  result <- list_files_and_dirs(path, pattern = ".", all.files = FALSE)
  expect_false(result$empty)
  expect_equal(basename(result$files), c("file1.txt", "file2.png"))
})

test_that("list_files_and_dirs filters files by pattern but always includes directories", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, "image.png"))
  file.create(file.path(path, "data.csv"))
  sub <- file.path(path, "subdir")
  dir.create(sub)
  file.create(file.path(sub, "nested.png"))

  result <- list_files_and_dirs(path, pattern = "\\.png$", all.files = FALSE)
  expect_false(result$empty)

  fnames <- basename(result$files)
  ## Image matches the pattern
  expect_true("image.png" %in% fnames)
  ## CSV does not match
  expect_false("data.csv" %in% fnames)
  ## Directory is always included (even though its name doesn't match the pattern)
  expect_true("subdir" %in% fnames)
})

test_that("list_files_and_dirs excludes hidden files when all.files = FALSE", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, ".hidden_file"))
  file.create(file.path(path, "visible_file.txt"))

  result <- list_files_and_dirs(path, pattern = ".*", all.files = FALSE)
  expect_false(result$empty)
  expect_equal(basename(result$files), "visible_file.txt")
})

test_that("list_files_and_dirs includes hidden files when all.files = TRUE", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, ".hidden_file"))
  file.create(file.path(path, "visible_file.txt"))

  result <- list_files_and_dirs(path, pattern = ".*", all.files = TRUE)
  expect_false(result$empty)
  expect_equal(sort(basename(result$files)), sort(c(".hidden_file", "visible_file.txt")))
})

test_that("list_files_and_dirs returns all files with empty=TRUE when no files match pattern", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, "file1.txt"))
  file.create(file.path(path, "file2.txt"))

  result <- list_files_and_dirs(path, pattern = include_imgs(), all.files = FALSE)
  expect_true(result$empty)
  expect_equal(basename(result$files), c("file1.txt", "file2.txt"))
})

test_that("list_files_and_dirs excludes empty subdirectories", {
  path <- tempfile()
  dir.create(path)

  dir.create(file.path(path, "empty_dir1"))
  dir.create(file.path(path, "empty_dir2"))

  result <- list_files_and_dirs(path, pattern = ".*", all.files = TRUE)
  expect_true(result$empty)
  expect_equal(as.character(result$files), character(0))
})

test_that("list_files_and_dirs includes non-empty dirs and excludes empty ones", {
  path <- tempfile()
  dir.create(path)

  dir.create(file.path(path, "dir1"))
  dir.create(file.path(path, "dir2"))
  file.create(file.path(path, "dir2/file2.png"))

  result <- list_files_and_dirs(path, pattern = include_imgs(), all.files = FALSE)
  expect_false(result$empty)
  ## Only dir2 appears (non-empty), dir1 is excluded (empty)
  expect_equal(basename(result$files), "dir2")
})
