test_that("default_exclude_dirs returns hardcoded list when nothing is configured", {
  withr::with_envvar(c("PRFY_EXCLUDE_DIRS" = NA), {
    withr::with_options(list(presentifyr.exclude_dirs = NULL), {
      result <- default_exclude_dirs()

      expected <- c(
        "renv", "rv", "rv/library", ".git", ".hg", ".svn",
        "node_modules", ".Rproj.user", ".venv", ".direnv",
        "__pycache__", "env", "site-library", ".cache"
      )
      expect_equal(result, expected)
    })
  })
})

test_that("PRFY_EXCLUDE_DIRS env var replaces defaults and splits on delimiters", {
  withr::with_options(list(presentifyr.exclude_dirs = NULL), {
    ## Semicolon
    withr::with_envvar(c("PRFY_EXCLUDE_DIRS" = "dirA;dirB"), {
      expect_equal(default_exclude_dirs(), c("dirA", "dirB"))
    })

    ## Colon
    withr::with_envvar(c("PRFY_EXCLUDE_DIRS" = "dirA:dirB"), {
      expect_equal(default_exclude_dirs(), c("dirA", "dirB"))
    })

    ## Comma
    withr::with_envvar(c("PRFY_EXCLUDE_DIRS" = "dirA,dirB"), {
      expect_equal(default_exclude_dirs(), c("dirA", "dirB"))
    })
  })
})

test_that("presentifyr.exclude_dirs option takes highest priority over env var", {
  withr::with_envvar(c("PRFY_EXCLUDE_DIRS" = "from_env"), {
    withr::with_options(list(presentifyr.exclude_dirs = c("from_option")), {
      result <- default_exclude_dirs()
      expect_equal(result, "from_option")
      expect_false("from_env" %in% result)
    })
  })
})

test_that("default_exclude_dirs filters out empty strings from delimited env var", {
  withr::with_options(list(presentifyr.exclude_dirs = NULL), {
    ## Consecutive delimiters produce empty strings that should be dropped
    withr::with_envvar(c("PRFY_EXCLUDE_DIRS" = "a,,b"), {
      expect_equal(default_exclude_dirs(), c("a", "b"))
    })
  })
})

test_that("character(0) option disables all exclusions", {
  withr::with_envvar(c("PRFY_EXCLUDE_DIRS" = NA), {
    withr::with_options(list(presentifyr.exclude_dirs = character(0)), {
      expect_equal(default_exclude_dirs(), character(0))
    })
  })
})
