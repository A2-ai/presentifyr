test_that("parse_directory_for_images fails when directory does not exist", {
  fake_directory <- tempfile()

  expect_error(parse_directory_for_images(fake_directory), "The specified directory does not exist.")
})

test_that("parse_directory_for_images returns empty when no images are found", {
  empty_dir <- tempfile()
  dir.create(empty_dir)

  result <- parse_directory_for_images(empty_dir)
  expect_length(result, 0)
})

test_that("parse_directory_for_images finds images in a directory", {
  img_dir <- tempfile()
  dir.create(img_dir)

  file.create(file.path(img_dir, "image1.png"))
  file.create(file.path(img_dir, "image2.jpg"))
  file.create(file.path(img_dir, "document.txt"))

  result <- parse_directory_for_images(img_dir)
  expect_equal(basename(result), c("image1.png", "image2.jpg"))
})

test_that("parse_directory_for_images finds images recursively", {
  parent_dir <- tempfile()
  dir.create(parent_dir)

  sub_dir <- file.path(parent_dir, "subfolder")
  dir.create(sub_dir)

  file.create(file.path(parent_dir, "image1.png"))
  file.create(file.path(sub_dir, "image2.jpg"))

  result <- parse_directory_for_images(parent_dir, recursive = TRUE)
  expect_equal(basename(result), c("image1.png", "image2.jpg"))
})

test_that("parse_directory_for_images excludes specified directories", {
  parent_dir <- tempfile()
  dir.create(parent_dir)

  exclude_dir <- file.path(parent_dir, "exclude_me")
  dir.create(exclude_dir)

  file.create(file.path(parent_dir, "image1.png"))
  file.create(file.path(exclude_dir, "image2.jpg"))

  result <- parse_directory_for_images(parent_dir, recursive = TRUE, exclude_dirs = "exclude_me")
  expect_equal(basename(result), c("image1.png"))
})
