test_that("load_abbreviation_definitions parses explicit yaml_path", {
  path <- tempfile(fileext = ".yaml")
  writeLines(
    c(
      "abbreviations:",
      "  CI: confidence interval",
      "  HR: hazard ratio"
    ),
    path
  )

  result <- load_abbreviation_definitions(path)
  expect_equal(result$CI, "confidence interval")
  expect_equal(result$HR, "hazard ratio")
})

test_that("load_abbreviation_definitions returns empty list when file missing", {
  result <- load_abbreviation_definitions("/nonexistent/path/foo.yaml")
  expect_equal(result, list())
})

test_that("load_abbreviation_definitions returns empty list for empty path", {
  result <- load_abbreviation_definitions("")
  expect_equal(result, list())
})

test_that("load_abbreviation_definitions returns empty list for malformed YAML", {
  path <- tempfile(fileext = ".yaml")
  writeLines("abbreviations:\n  CI: [unclosed", path)

  result <- load_abbreviation_definitions(path)
  expect_equal(result, list())
})

test_that("load_abbreviation_definitions returns empty list when abbreviations key is missing", {
  path <- tempfile(fileext = ".yaml")
  writeLines(c("something_else:", "  foo: bar"), path)

  result <- load_abbreviation_definitions(path)
  expect_equal(result, list())
})

test_that("load_abbreviation_definitions resolves path via report_dir option when yaml_path is NULL", {
  dir <- tempfile()
  dir.create(dir)
  report_dir <- file.path(dir, "custom_reports")
  dir.create(report_dir)

  writeLines(
    c("abbreviations:", "  CI: confidence interval"),
    file.path(report_dir, "standard_footnotes.yaml")
  )

  withr::with_options(
    list(project.dir = dir, presentifyr.report_dir_name = "custom_reports"),
    {
      result <- load_abbreviation_definitions()
    }
  )
  expect_equal(result$CI, "confidence interval")
})

test_that("load_abbreviation_definitions returns empty list when default path has no YAML", {
  dir <- tempfile()
  dir.create(dir)

  withr::with_options(list(project.dir = dir), {
    result <- load_abbreviation_definitions()
  })
  expect_equal(result, list())
})
