test_that("my_layout includes level in brackets and message text", {
  result <- my_layout("WARN", "something went wrong")

  expect_true(grepl("\\[WARN\\]", result))
  expect_true(grepl("something went wrong", result))
})

test_that("my_layout includes a timestamp prefix", {
  result <- my_layout("INFO", "test message")

  ## Should start with a date-like string (YYYY-MM-DD)
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}", result))
})

test_that("my_layout ends with a newline", {
  result <- my_layout("DEBUG", "msg")

  expect_true(grepl("\n$", result))
})
