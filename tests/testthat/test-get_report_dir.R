test_that("get_report_dir returns <project>/report when no init file exists", {
  dir <- tempfile()
  dir.create(dir)
  withr::with_options(list(project.dir = dir), {
    expect_equal(get_report_dir(), file.path(dir, "report"))
  })
})

test_that("get_report_dir reads report_dir_name from init file", {
  dir <- tempfile()
  dir.create(dir)
  jsonlite::write_json(
    list(config = list(report_dir_name = "my_reports")),
    file.path(dir, ".my_reports_init.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
  withr::with_options(list(project.dir = dir), {
    expect_equal(get_report_dir(), file.path(dir, "my_reports"))
  })
})

test_that("get_report_dir falls back to 'report' when init has no report_dir_name", {
  dir <- tempfile()
  dir.create(dir)
  jsonlite::write_json(
    list(config = list(outputs_dir_name = "OUT")),
    file.path(dir, ".report_init.json"),
    auto_unbox = TRUE
  )
  withr::with_options(list(project.dir = dir), {
    expect_equal(get_report_dir(), file.path(dir, "report"))
  })
})

test_that("get_report_dir falls back to 'report' for malformed init JSON", {
  dir <- tempfile()
  dir.create(dir)
  writeLines("{not valid json", file.path(dir, ".report_init.json"))
  withr::with_options(list(project.dir = dir), {
    expect_equal(get_report_dir(), file.path(dir, "report"))
  })
})

test_that("get_report_dir uses first match when multiple init files exist", {
  dir <- tempfile()
  dir.create(dir)
  jsonlite::write_json(
    list(config = list(report_dir_name = "a_dir")),
    file.path(dir, ".a_init.json"),
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(config = list(report_dir_name = "b_dir")),
    file.path(dir, ".b_init.json"),
    auto_unbox = TRUE
  )
  withr::with_options(list(project.dir = dir), {
    expect_equal(get_report_dir(), file.path(dir, "a_dir"))
  })
})
