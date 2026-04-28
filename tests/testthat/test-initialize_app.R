test_that("initialize_app returns the success/errors structure", {
  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(...) NULL)

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_type(result, "list")
  expect_named(result, c("success", "errors"), ignore.order = TRUE)
  expect_type(result$success, "logical")
  expect_type(result$errors, "character")
})

test_that("initialize_app reports success when fyrstartr returns cleanly", {
  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(...) NULL)

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_true(result$success)
  expect_length(result$errors, 0)
})

test_that("initialize_app passes groups='presentifyr' to fyrstartr", {
  groups_seen <- NULL
  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(continue, groups, ...) {
    groups_seen <<- groups
    NULL
  })

  suppressMessages(initialize_app(verbose = FALSE))

  expect_equal(groups_seen, "presentifyr")
})

test_that("initialize_app captures fyrstartr error message in status$errors", {
  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(...) {
    stop("fyrstartr blew up")
  })

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_false(result$success)
  expect_true(any(grepl("fyrstartr blew up", result$errors)))
})

test_that("initialize_app surfaces e$stderr when present", {
  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(...) {
    e <- structure(
      class = c("processx_error", "error", "condition"),
      list(
        message = "System command 'uv_setup.sh' failed",
        stderr = "error: Group 'foo' is not defined in the project's `dependency-groups` table",
        call = NULL
      )
    )
    stop(e)
  })

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_false(result$success)
  expect_true(any(grepl("Group 'foo' is not defined", result$errors)))
})
