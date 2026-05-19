test_that("toggle_logger creates a WARN-level logger by default", {
  withr::with_envvar(c("PRFY_VERBOSE" = NA), {
    toggle_logger()

    expected_threshold <- log4r::logger("WARN")$threshold
    expect_equal(.le$logger$threshold, expected_threshold)
  })
})

test_that("toggle_logger respects PRFY_VERBOSE=DEBUG", {
  withr::with_envvar(c("PRFY_VERBOSE" = "DEBUG"), {
    toggle_logger()

    expected_threshold <- log4r::logger("DEBUG")$threshold
    expect_equal(.le$logger$threshold, expected_threshold)
  })
})

test_that("toggle_logger respects PRFY_VERBOSE=ERROR", {
  withr::with_envvar(c("PRFY_VERBOSE" = "ERROR"), {
    toggle_logger()

    expected_threshold <- log4r::logger("ERROR")$threshold
    expect_equal(.le$logger$threshold, expected_threshold)
  })
})

test_that("toggle_logger assigns a logger object to .le$logger", {
  withr::with_envvar(c("PRFY_VERBOSE" = "INFO"), {
    toggle_logger()

    expect_true(exists("logger", envir = .le))
    expect_s3_class(.le$logger, "logger")
  })
})

test_that("toggle_logger rejects invalid verbosity levels with descriptive error", {
  withr::with_envvar(c("PRFY_VERBOSE" = "GARBAGE"), {
    expect_error(
      toggle_logger(),
      "Invalid verbosity level 'GARBAGE'"
    )
    ## Error message includes available options
    expect_error(
      toggle_logger(),
      "DEBUG, INFO, WARN, ERROR, FATAL"
    )
  })
})

test_that("toggle_logger rejects empty string verbosity", {
  withr::with_envvar(c("PRFY_VERBOSE" = ""), {
    skip_if(
      is.na(Sys.getenv("PRFY_VERBOSE", unset = NA_character_)),
      "Platform cannot represent an existing-but-empty environment variable"
    )

    expect_error(toggle_logger(), "Invalid verbosity level ''")
  })
})
