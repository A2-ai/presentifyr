test_that("run_python_script returns pyro result on success", {
  mockery::stub(run_python_script, "pyro::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })
  mockery::stub(run_python_script, "pyro::run_python_script", function(...) {
    list(stdout = "done", stderr = "", status = 0)
  })

  result <- run_python_script(c("script.py", "-o", "out.pptx"), label = "Test")

  expect_type(result, "list")
  expect_equal(result$status, 0)
  expect_equal(result$stdout, "done")
})

test_that("run_python_script prepends 'run' to script_args", {
  mockery::stub(run_python_script, "pyro::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })

  captured_args <- NULL
  mockery::stub(run_python_script, "pyro::run_python_script", function(uv_path, args, ...) {
    captured_args <<- args
    list(stdout = "", stderr = "", status = 0)
  })

  run_python_script(c("myscript.py", "-f", "val"), label = "Test")

  expect_equal(captured_args[1], "run")
  expect_equal(captured_args[2], "myscript.py")
  expect_equal(captured_args[3], "-f")
})

test_that("run_python_script forwards venv/uv paths and a stderr_callback to pyro", {
  mockery::stub(run_python_script, "pyro::get_venv_uv_paths", function() {
    list(venv = "/my/venv", uv = "/my/uv")
  })

  captured <- list()
  mockery::stub(run_python_script, "pyro::run_python_script", function(uv_path, args, venv_path, script_name, pythonpath, stderr_callback, ...) {
    captured <<- list(
      uv_path = uv_path,
      venv_path = venv_path,
      script_name = script_name,
      pythonpath = pythonpath,
      stderr_callback = stderr_callback
    )
    list(stdout = "", stderr = "", status = 0)
  })

  run_python_script(c("script.py"), label = "Test")

  expect_equal(captured$uv_path, "/my/uv")
  expect_equal(captured$venv_path, "/my/venv")
  expect_equal(captured$script_name, "Test")
  expect_type(captured$pythonpath, "character")
  expect_type(captured$stderr_callback, "closure")
})

test_that("run_python_script's stderr callback filters console by PRFY_VERBOSE", {
  mockery::stub(run_python_script, "pyro::get_venv_uv_paths", function() {
    list(venv = "/my/venv", uv = "/my/uv")
  })

  cb_holder <- list(cb = NULL)
  mockery::stub(run_python_script, "pyro::run_python_script", function(stderr_callback, ...) {
    cb_holder$cb <<- stderr_callback
    list(stdout = "", stderr = "", status = 0)
  })

  withr::with_envvar(c(PRFY_VERBOSE = "WARN"), {
    run_python_script(c("script.py"), label = "Test")
  })

  ## DEBUG and INFO suppressed at WARN threshold; WARNING/ERROR shown
  out <- capture.output(cb_holder$cb("2026-01-01 [DEBUG] hidden\n", NULL))
  expect_false(any(grepl("hidden", out)))

  out <- capture.output(cb_holder$cb("2026-01-01 [WARNING] visible\n", NULL))
  expect_true(any(grepl("visible", out)))

  ## Lines without a [LEVEL] tag (raw tracebacks) always pass
  out <- capture.output(cb_holder$cb("Traceback (most recent call last):\n", NULL))
  expect_true(any(grepl("Traceback", out)))
})

test_that("run_python_script error includes label but not internal paths", {
  mockery::stub(run_python_script, "pyro::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })
  mockery::stub(run_python_script, "pyro::run_python_script", function(...) {
    stop("Add images failed.", call. = FALSE)
  })

  expect_error(
    run_python_script(c("script.py"), label = "Add images"),
    "Add images failed"
  )
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
  mockery::stub(run_python_script, "pyro::get_venv_uv_paths", function() {
    stop("Create virtual environment with initialize_python")
  })

  expect_error(
    run_python_script(c("script.py"), label = "Test"),
    "Create virtual environment"
  )
})
