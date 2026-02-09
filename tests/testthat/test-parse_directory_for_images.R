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

test_that("prfy_image_key prefers relative paths within root", {
  root <- tempfile()
  dir.create(root)
  f <- file.path(root, "subdir", "img.png")
  dir.create(dirname(f), recursive = TRUE)
  file.create(f)

  key <- prfy_image_key(f, root = root)
  expect_equal(key, "subdir/img.png")

  # outside root falls back to basename
  g <- tempfile(fileext = ".png")
  key2 <- prfy_image_key(g, root = root)
  expect_equal(key2, basename(g))
})

test_that("parse_directory_for_images uses default exclude dirs helper", {
  parent_dir <- tempfile()
  dir.create(parent_dir)

  renv_dir <- file.path(parent_dir, "renv")
  rv_dir <- file.path(parent_dir, "rv")
  dir.create(renv_dir)
  dir.create(rv_dir)
  dir.create(file.path(rv_dir, "library"))

  file.create(file.path(renv_dir, "imageA.png"))
  file.create(file.path(rv_dir, "library", "imageB.png"))
  file.create(file.path(parent_dir, "imageC.png"))

  res <- parse_directory_for_images(parent_dir, recursive = TRUE)
  expect_equal(basename(res), "imageC.png")
})

test_that("parse_directory_for_images does not descend into excluded dirs", {
  parent_dir <- tempfile()
  dir.create(parent_dir)

  heavy_dir <- file.path(parent_dir, "node_modules")
  dir.create(heavy_dir)
  nested <- file.path(heavy_dir, "deep")
  dir.create(nested, recursive = TRUE)
  file.create(file.path(nested, "should_not_be_seen.png"))

  keep_dir <- file.path(parent_dir, "figs")
  dir.create(keep_dir)
  file.create(file.path(keep_dir, "keep.png"))

  res <- parse_directory_for_images(parent_dir, recursive = TRUE)
  expect_equal(basename(res), "keep.png")
})
