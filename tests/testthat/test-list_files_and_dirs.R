test_that("list_files_and_dirs returns files and directories matching a given pattern", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, "file1.txt"))
  file.create(file.path(path, "file2.png"))
  dir.create(file.path(path, "renv"))

  result <- list_files_and_dirs(path, pattern = ".", all.files = FALSE)
  expect_false(result$empty)
  expect_equal(
    unlist(strsplit(trimws(basename(result$files)), "\\s+")),
    c("file1.txt", "file2.png")
  )
})

test_that("list_files_and_dirs correctly handles hidden files when all.files = FALSE", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, ".hidden_file"))
  file.create(file.path(path, "visible_file.txt"))

  result <- list_files_and_dirs(path, pattern = ".*", all.files = FALSE)
  expect_false(result$empty)
  expect_equal(basename(result$files), "visible_file.txt")
})

test_that("list_files_and_dirs correctly includes hidden files when all.files = TRUE", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, ".hidden_file"))
  file.create(file.path(path, "visible_file.txt"))

  result <- list_files_and_dirs(path, pattern = ".*", all.files = TRUE)
  expect_false(result$empty)
  expect_equal(sort(basename(result$files)), sort(c(".hidden_file", "visible_file.txt")))
})

test_that("list_files_and_dirs returns all files as a backup for shiny messaging, if no matches with pattern are found", {
  path <- tempfile()
  dir.create(path)

  file.create(file.path(path, "file1.txt"))
  file.create(file.path(path, "file2.txt"))

  result <- list_files_and_dirs(path, pattern = include_imgs(), all.files = FALSE)
  expect_true(result$empty)
  expect_equal(basename(result$files), c("file1.txt", "file2.txt"))
})

test_that("list_files_and_dirs excludes empty subdirectories correctly", {
  path <- tempfile()
  dir.create(path)

  dir.create(file.path(path, "empty_dir1"))
  dir.create(file.path(path, "empty_dir2"))

  result <- list_files_and_dirs(path, pattern = ".*", all.files = TRUE)
  expect_true(result$empty)
  expect_equal(as.character(result$files), character(0))
})

test_that("list_files_and_dirs includes non-empty or nested directories and excludes empty ones", {
  path <- tempfile()
  dir.create(path)

  dir.create(file.path(path, "dir1"))
  dir.create(file.path(path, "dir2"))
  file.create(file.path(path, "dir2/file2.png"))

  result <- list_files_and_dirs(path, pattern = include_imgs(), all.files = FALSE)
  expect_false(result$empty)
  expect_equal(basename(result$files), c("dir2"))
})
