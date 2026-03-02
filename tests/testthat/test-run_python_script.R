test_that("run_python_script returns processx result on success", {
  mockery::stub(run_python_script, "reportifyr::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })
  mockery::stub(run_python_script, "processx::run", function(...) {
    list(stdout = "done", stderr = "", status = 0)
  })

  result <- run_python_script(c("script.py", "-o", "out.pptx"), label = "Test")

  expect_type(result, "list")
  expect_equal(result$status, 0)
  expect_equal(result$stdout, "done")
})

test_that("run_python_script prepends 'run' to script_args", {
  mockery::stub(run_python_script, "reportifyr::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })

  captured_args <- NULL
  mockery::stub(run_python_script, "processx::run", function(command, args, ...) {
    captured_args <<- args
    list(stdout = "", stderr = "", status = 0)
  })

  run_python_script(c("myscript.py", "-f", "val"), label = "Test")

  expect_equal(captured_args[1], "run")
  expect_equal(captured_args[2], "myscript.py")
  expect_equal(captured_args[3], "-f")
})

test_that("run_python_script sets VIRTUAL_ENV and PY_LOG_LEVEL in env", {
  mockery::stub(run_python_script, "reportifyr::get_venv_uv_paths", function() {
    list(venv = "/my/venv", uv = "/fake/uv")
  })

  captured_env <- NULL
  mockery::stub(run_python_script, "processx::run", function(command, args, env, ...) {
    captured_env <<- env
    list(stdout = "", stderr = "", status = 0)
  })

  withr::with_envvar(c("PRFY_VERBOSE" = "DEBUG"), {
    run_python_script(c("script.py"), label = "Test")
  })

  expect_equal(captured_env[["VIRTUAL_ENV"]], "/my/venv")
  expect_equal(captured_env[["PY_LOG_LEVEL"]], "DEBUG")
})

test_that("run_python_script error includes label but not internal paths", {
  mockery::stub(run_python_script, "reportifyr::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })
  mockery::stub(run_python_script, "processx::run", function(...) {
    e <- simpleError("process failed")
    e$status <- 1
    e$stdout <- "some output"
    e$stderr <- "/Users/secret/.venv/bin/python traceback here"
    stop(e)
  })

  expect_error(
    run_python_script(c("script.py"), label = "Add images"),
    "Add images failed"
  )
  ## Stderr and internal paths must NOT leak into the user-facing error
  expect_error(
    run_python_script(c("script.py"), label = "Add images"),
    "PRFY_VERBOSE=DEBUG"
  )
  tryCatch(
    run_python_script(c("script.py"), label = "Add images"),
    error = function(e) {
      expect_false(grepl("/Users/", e$message))
      expect_false(grepl("traceback", e$message))
    }
  )
})

test_that("run_python_script propagates error when venv paths cannot be resolved", {
  mockery::stub(run_python_script, "reportifyr::get_venv_uv_paths", function() {
    stop("Create virtual environment with initialize_python")
  })

  expect_error(
    run_python_script(c("script.py"), label = "Test"),
    "Create virtual environment"
  )
})
