test_that("prfy_image_key returns relative path when file is under root", {
  root <- tempfile()
  dir.create(root)
  f <- file.path(root, "subdir", "img.png")
  dir.create(dirname(f), recursive = TRUE)
  file.create(f)

  key <- prfy_image_key(f, root = root)
  expect_equal(key, "subdir/img.png")
})

test_that("prfy_image_key falls back to basename when file is outside root", {
  root <- tempfile()
  dir.create(root)
  g <- tempfile(fileext = ".png")

  key <- prfy_image_key(g, root = root)
  expect_equal(key, basename(g))
})

test_that("prfy_image_key normalizes backslashes to forward slashes", {
  root <- tempfile()
  dir.create(root)
  f <- file.path(root, "sub", "img.png")
  dir.create(dirname(f), recursive = TRUE)
  file.create(f)

  key <- prfy_image_key(f, root = root)
  expect_false(grepl("\\\\", key))
  expect_true(grepl("/", key))
})

test_that("prfy_image_key returns basename for file at root level", {
  root <- tempfile()
  dir.create(root)
  f <- file.path(root, "plot.png")
  file.create(f)

  key <- prfy_image_key(f, root = root)
  expect_equal(key, "plot.png")
})
