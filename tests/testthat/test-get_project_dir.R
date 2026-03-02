test_that("get_project_dir returns option value when project.dir is set", {
  withr::with_options(list(project.dir = "/custom/project/path"), {
    expect_equal(get_project_dir(), "/custom/project/path")
  })
})

test_that("get_project_dir falls back to here::here() when option is NULL", {
  withr::with_options(list(project.dir = NULL), {
    expect_equal(get_project_dir(), here::here())
  })
})

test_that("get_project_dir returns option value as-is without normalization", {
  ## Trailing slash, double slashes — should be returned exactly as given
  withr::with_options(list(project.dir = "/some//path/with/trailing/"), {
    expect_equal(get_project_dir(), "/some//path/with/trailing/")
  })
})
