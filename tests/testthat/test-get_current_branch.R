test_that("get_current_branch retrieves and trims branch successfully", {
  mock_run <- function(...) {
    list(stdout = "main\n", stderr = "", status = 0)
  }

  mockery::stub(get_current_branch, "processx::run", mock_run)

  result <- get_current_branch()
  expect_equal(result, "main")
})

test_that("get_current_branch fails when git command returns non-zero status", {
  mock_run_fail <- function(...) {
    stop(structure(
      list(
        status = 1,
        stdout = "",
        stderr = "fatal: Not a git repository"
      ),
      class = "error"
    ))
  }

  mockery::stub(get_current_branch, "processx::run", mock_run_fail)

  expect_error(get_current_branch(), "Error retrieving current Git branch.")
})
