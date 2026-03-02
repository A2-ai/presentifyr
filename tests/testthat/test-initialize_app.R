test_that("initialize_app returns correct structure with reportifyr, python_pptx, errors", {
  project_dir <- tempfile()
  dir.create(project_dir)

  ## Simulate already-initialized state: init JSON + .venv
  file.create(file.path(project_dir, ".prfy_init.json"))
  dir.create(file.path(project_dir, ".venv"))

  ## Mock the venv/pptx check path so it doesn't need a real venv
  mockery::stub(initialize_app, "reportifyr::get_venv_uv_paths", function() {
    list(venv = file.path(project_dir, ".venv"), uv = "/fake/uv")
  })
  mockery::stub(initialize_app, "processx::run", function(...) {
    list(status = 0, stdout = "1.0.2\n", stderr = "")
  })

  result <- suppressMessages(initialize_app(project_dir = project_dir, verbose = FALSE))

  expect_type(result, "list")
  expect_named(result, c("reportifyr", "python_pptx", "errors"), ignore.order = TRUE)
  expect_type(result$reportifyr, "logical")
  expect_type(result$errors, "character")
})

test_that("initialize_app detects already-initialized reportifyr without calling initialize_report_project", {
  project_dir <- tempfile()
  dir.create(project_dir)

  ## Simulate already-initialized state
  file.create(file.path(project_dir, ".prfy_init.json"))
  dir.create(file.path(project_dir, ".venv"))

  init_called <- FALSE
  mockery::stub(initialize_app, "reportifyr::initialize_report_project", function(...) {
    init_called <<- TRUE
  })
  mockery::stub(initialize_app, "reportifyr::get_venv_uv_paths", function() {
    list(venv = file.path(project_dir, ".venv"), uv = "/fake/uv")
  })
  mockery::stub(initialize_app, "processx::run", function(...) {
    list(status = 0, stdout = "1.0.2\n", stderr = "")
  })

  result <- suppressMessages(initialize_app(project_dir = project_dir, verbose = FALSE))

  expect_true(result$reportifyr)
  expect_false(init_called)
})

test_that("initialize_app captures reportifyr failure in status$errors", {
  project_dir <- tempfile()
  dir.create(project_dir)
  ## No init file or .venv → triggers initialization attempt

  mockery::stub(initialize_app, "reportifyr::initialize_report_project", function(...) {
    stop("reportifyr init failed badly")
  })
  mockery::stub(initialize_app, "reportifyr::get_venv_uv_paths", function() {
    stop("no venv")
  })

  result <- suppressMessages(initialize_app(
    project_dir = project_dir,
    install_python_deps = FALSE,
    verbose = FALSE
  ))

  expect_false(result$reportifyr)
  expect_true(any(grepl("reportifyr init failed badly", result$errors)))
})

test_that("initialize_app sets python_pptx to NA when install_python_deps is FALSE", {
  project_dir <- tempfile()
  dir.create(project_dir)
  file.create(file.path(project_dir, ".prfy_init.json"))
  dir.create(file.path(project_dir, ".venv"))

  mockery::stub(initialize_app, "reportifyr::get_venv_uv_paths", function() {
    list(venv = file.path(project_dir, ".venv"), uv = "/fake/uv")
  })

  result <- suppressMessages(initialize_app(
    project_dir = project_dir,
    install_python_deps = FALSE,
    verbose = FALSE
  ))

  expect_true(is.na(result$python_pptx))
})

test_that("initialize_app captures python-pptx install failure in status$errors", {
  project_dir <- tempfile()
  dir.create(project_dir)
  file.create(file.path(project_dir, ".prfy_init.json"))
  dir.create(file.path(project_dir, ".venv"))

  mockery::stub(initialize_app, "reportifyr::get_venv_uv_paths", function() {
    list(venv = file.path(project_dir, ".venv"), uv = "/fake/uv")
  })
  ## First processx::run (import check) → pptx not installed
  ## install_pptx will also fail
  mockery::stub(initialize_app, "processx::run", function(...) {
    list(status = 1, stdout = "", stderr = "ModuleNotFoundError")
  })
  mockery::stub(initialize_app, "install_pptx", function(...) {
    stop("uv not found")
  })

  result <- suppressMessages(initialize_app(project_dir = project_dir, verbose = FALSE))

  expect_false(result$python_pptx)
  expect_true(any(grepl("uv not found", result$errors)))
})
