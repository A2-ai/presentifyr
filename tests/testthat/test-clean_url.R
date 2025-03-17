test_that("clean_url correctly cleans HTTPS URLs", {
  result <- clean_url("https://github.com/user/repo.git")
  expected <- paste0("https://github.com/user/", basename(getwd()))
  expect_equal(result, expected)
})

test_that("clean_url handles HTTPS URLs without .git extension", {
  result <- clean_url("https://github.com/user/repo")
  expected <- paste0("https://github.com/user/", basename(getwd()))
  expect_equal(result, expected)
})

test_that("clean_url converts SSH URLs to HTTPS-style paths", {
  result <- clean_url("git@github.com:user/repo.git")
  expect_equal(result, "github.com/user/repo")
})

test_that("clean_url stops with an error for invalid URLs", {
  expect_error(clean_url("ftp://github.com/user/repo.git"), "The provided URL is not a valid SSH Key.")
  expect_error(clean_url("user@server:path/to/repo.git"), "The provided URL is not a valid SSH Key.")
  expect_error(clean_url("https:/github.com/user/repo.git"), "The provided URL is not a valid SSH Key.")
})

