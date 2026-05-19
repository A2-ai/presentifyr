test_that("get_report_dir returns <project>/report when option is unset", {
  dir <- tempfile()
  dir.create(dir)
  withr::with_options(list(project.dir = dir, presentifyr.report_dir_name = NULL), {
    expect_equal(get_report_dir(), file.path(dir, "report"))
  })
})

test_that("get_report_dir honors options('presentifyr.report_dir_name')", {
  dir <- tempfile()
  dir.create(dir)
  withr::with_options(
    list(project.dir = dir, presentifyr.report_dir_name = "my_reports"),
    {
      expect_equal(get_report_dir(), file.path(dir, "my_reports"))
    }
  )
})
