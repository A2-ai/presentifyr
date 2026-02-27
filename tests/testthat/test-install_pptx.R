test_that("install_pptx returns TRUE without installing when pptx already present", {
  mockery::stub(install_pptx, "reportifyr::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })

  install_called <- FALSE
  call_count <- 0L
  mockery::stub(install_pptx, "processx::run", function(command, args, ...) {
    call_count <<- call_count + 1L
    if (any(grepl("import pptx", args))) {
      ## Import check succeeds → already installed
      list(status = 0, stdout = "1.0.2\n", stderr = "")
    } else {
      install_called <<- TRUE
      list(status = 0, stdout = "", stderr = "")
    }
  })

  result <- suppressMessages(install_pptx(verbose = FALSE))

  expect_true(result)
  expect_false(install_called)
  expect_equal(call_count, 1L)  ## Only the import check, no install
})

test_that("install_pptx installs and returns TRUE when pptx is missing", {
  mockery::stub(install_pptx, "reportifyr::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })

  captured_install_args <- NULL
  mockery::stub(install_pptx, "processx::run", function(command, args, ...) {
    if (any(grepl("import pptx", args))) {
      ## Import check fails → not installed
      list(status = 1, stdout = "", stderr = "ModuleNotFoundError")
    } else {
      captured_install_args <<- args
      list(status = 0, stdout = "", stderr = "")
    }
  })

  result <- suppressMessages(install_pptx(verbose = FALSE))

  expect_true(result)
  expect_true("pip" %in% captured_install_args)
  expect_true("install" %in% captured_install_args)
})

test_that("install_pptx returns FALSE when installation fails", {
  mockery::stub(install_pptx, "reportifyr::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })

  mockery::stub(install_pptx, "processx::run", function(command, args, ...) {
    if (any(grepl("import pptx", args))) {
      list(status = 1, stdout = "", stderr = "ModuleNotFoundError")
    } else {
      ## Install itself fails
      list(status = 1, stdout = "", stderr = "error installing")
    }
  })

  result <- suppressMessages(install_pptx(verbose = FALSE))

  expect_false(result)
})

test_that("install_pptx uses version from options when set", {
  mockery::stub(install_pptx, "reportifyr::get_venv_uv_paths", function() {
    list(venv = "/fake/venv", uv = "/fake/uv")
  })

  captured_install_args <- NULL
  mockery::stub(install_pptx, "processx::run", function(command, args, ...) {
    if (any(grepl("import pptx", args))) {
      list(status = 1, stdout = "", stderr = "ModuleNotFoundError")
    } else {
      captured_install_args <<- args
      list(status = 0, stdout = "", stderr = "")
    }
  })

  withr::with_options(list("python-pptx.version" = "0.6.23"), {
    suppressMessages(install_pptx(verbose = FALSE))
  })

  version_arg <- captured_install_args[grepl("python-pptx==", captured_install_args)]
  expect_equal(version_arg, "python-pptx==0.6.23")
})
