test_that("initialize_app returns the venv/python_pptx/errors structure", {
  venv_parent <- tempfile()
  dir.create(venv_parent)
  dir.create(file.path(venv_parent, ".venv"))
  withr::local_options(list(venv_dir = venv_parent))

  mockery::stub(initialize_app, "pptx_importable", function(...) TRUE)

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_type(result, "list")
  expect_named(
    result, c("venv", "python_pptx", "errors"), ignore.order = TRUE
  )
  expect_type(result$venv, "logical")
  expect_type(result$errors, "character")

  unlink(venv_parent, recursive = TRUE)
})

test_that("initialize_app skips fyrstartr when .venv exists and pptx imports", {
  venv_parent <- tempfile()
  dir.create(venv_parent)
  dir.create(file.path(venv_parent, ".venv"))
  withr::local_options(list(venv_dir = venv_parent))

  fyrstartr_called <- FALSE
  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(...) {
    fyrstartr_called <<- TRUE
  })
  mockery::stub(initialize_app, "pptx_importable", function(...) TRUE)

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_true(result$venv)
  expect_true(result$python_pptx)
  expect_false(fyrstartr_called)

  unlink(venv_parent, recursive = TRUE)
})

test_that("initialize_app calls fyrstartr(groups='presentifyr') when .venv is missing", {
  venv_parent <- tempfile()
  dir.create(venv_parent)
  withr::local_options(list(venv_dir = venv_parent))

  groups_seen <- NULL
  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(continue, groups, ...) {
    groups_seen <<- groups
    dir.create(file.path(venv_parent, ".venv"))
  })
  mockery::stub(initialize_app, "pptx_importable", function(...) TRUE)

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_true(result$venv)
  expect_equal(groups_seen, "presentifyr")

  unlink(venv_parent, recursive = TRUE)
})

test_that("initialize_app re-syncs presentifyr group when .venv exists but pptx is missing", {
  venv_parent <- tempfile()
  dir.create(venv_parent)
  dir.create(file.path(venv_parent, ".venv"))
  withr::local_options(list(venv_dir = venv_parent))

  call_count <- 0L
  groups_seen <- NULL
  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(continue, groups, ...) {
    call_count <<- call_count + 1L
    groups_seen <<- groups
  })
  ## First pptx_importable call returns FALSE (recovery branch fires);
  ## second call (post-resync) returns TRUE.
  importable_calls <- 0L
  mockery::stub(initialize_app, "pptx_importable", function(...) {
    importable_calls <<- importable_calls + 1L
    importable_calls > 1L
  })

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_true(result$venv)
  expect_true(result$python_pptx)
  expect_equal(call_count, 1L)
  expect_equal(groups_seen, "presentifyr")

  unlink(venv_parent, recursive = TRUE)
})

test_that("initialize_app captures fyrstartr failure in status$errors", {
  venv_parent <- tempfile()
  dir.create(venv_parent)
  withr::local_options(list(venv_dir = venv_parent))

  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(...) {
    stop("fyrstartr blew up")
  })
  mockery::stub(initialize_app, "pptx_importable", function(...) FALSE)

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_false(result$venv)
  expect_false(result$python_pptx)
  expect_true(any(grepl("fyrstartr blew up", result$errors)))

  unlink(venv_parent, recursive = TRUE)
})

test_that("initialize_app reports python_pptx FALSE when import check fails post-resync", {
  venv_parent <- tempfile()
  dir.create(venv_parent)
  dir.create(file.path(venv_parent, ".venv"))
  withr::local_options(list(venv_dir = venv_parent))

  mockery::stub(initialize_app, "fyrstartr::initialize_python", function(...) NULL)
  mockery::stub(initialize_app, "pptx_importable", function(...) FALSE)

  result <- suppressMessages(initialize_app(verbose = FALSE))

  expect_false(result$python_pptx)

  unlink(venv_parent, recursive = TRUE)
})
