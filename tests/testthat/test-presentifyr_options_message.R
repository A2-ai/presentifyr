test_that("presentifyr_options_message shows project hint when project.dir is not set", {
  withr::with_options(list(project.dir = NULL, presentifyr.exclude_dirs = NULL), {
    msg <- presentifyr_options_message()

    expect_true(grepl("here::here\\(\\)", msg))
    expect_true(grepl("set options\\('project.dir'\\)", msg))
  })
})

test_that("presentifyr_options_message shows exclude dirs hint when not set", {
  withr::with_options(list(project.dir = NULL, presentifyr.exclude_dirs = NULL), {
    msg <- presentifyr_options_message()

    expect_true(grepl("default exclude dirs", msg))
    expect_true(grepl("set options\\('presentifyr.exclude_dirs'\\)", msg))
  })
})

test_that("presentifyr_options_message shows set values when options are configured", {
  withr::with_options(list(project.dir = "/my/project", presentifyr.exclude_dirs = c("vendor", "tmp")), {
    msg <- presentifyr_options_message()

    expect_true(grepl("project\\.dir: /my/project", msg))
    expect_true(grepl("presentifyr\\.exclude_dirs: vendor, tmp", msg))
    ## Should NOT show the hints when values are set
    expect_false(grepl("here::here\\(\\)", msg))
    expect_false(grepl("default exclude dirs", msg))
  })
})

test_that("presentifyr_options_message shows mixed state correctly", {
  withr::with_options(list(project.dir = "/custom/path", presentifyr.exclude_dirs = NULL), {
    msg <- presentifyr_options_message()

    ## project.dir is set → shows value
    expect_true(grepl("project\\.dir: /custom/path", msg))
    ## exclude_dirs not set → shows hint
    expect_true(grepl("default exclude dirs", msg))
  })
})
