## regroup_files ---------------------------------------------------------------

test_that("regroup_files redistributes files keeping slide sizes", {
  files <- c("a.png", "b.png", "c.png", "d.png")
  current_groups <- list(c("d.png", "c.png"), c("b.png", "a.png"))

  result <- regroup_files(files, current_groups)

  expect_equal(result$groups, list(c("a.png", "b.png"), c("c.png", "d.png")))
})

test_that("regroup_files resets positions to sequential", {
  files <- c("x.png", "y.png", "z.png")
  current_groups <- list(c("a.png", "b.png"), c("c.png"))

  result <- regroup_files(files, current_groups)

  expect_equal(result$positions, list(1:2, 1L))
})

test_that("regroup_files handles single-image slides", {
  files <- c("b.png", "a.png")
  current_groups <- list("a.png", "b.png")

  result <- regroup_files(files, current_groups)

  expect_equal(result$groups, list("b.png", "a.png"))
  expect_equal(result$positions, list(1L, 1L))
})

## split_before_index ----------------------------------------------------------

test_that("split_before_index splits slide at the given index", {
  groups <- list(c("a.png", "b.png", "c.png"))
  positions <- list(c(1L, 2L, 3L))

  ## Split before index 2 → "a" stays, "b","c" move to new slide
  result <- split_before_index(groups, positions, global_idx = 2)

  expect_equal(result$groups, list("a.png", c("b.png", "c.png")))
  expect_equal(result$positions, list(1L, 1:2))
})

test_that("split_before_index preserves slides before and after the split", {
  groups <- list(c("a.png"), c("b.png", "c.png", "d.png"), c("e.png"))
  positions <- list(1L, c(1L, 2L, 3L), 1L)

  ## Global index 3 = "c.png" (2nd in slide 2). Split slide 2 before it.
  result <- split_before_index(groups, positions, global_idx = 3)

  expect_equal(length(result$groups), 4)
  expect_equal(result$groups[[1]], "a.png")       # unchanged
  expect_equal(result$groups[[2]], "b.png")        # before split
  expect_equal(result$groups[[3]], c("c.png", "d.png"))  # after split
  expect_equal(result$groups[[4]], "e.png")        # unchanged
})

test_that("split_before_index resets positions for split slides", {
  groups <- list(c("a.png", "b.png", "c.png"))
  positions <- list(c(2L, 1L, 3L))  # custom positions

  result <- split_before_index(groups, positions, global_idx = 3)

  ## Split slides get sequential positions regardless of original
  expect_equal(result$positions[[1]], 1:2)
  expect_equal(result$positions[[2]], 1L)
})

## merge_slides ----------------------------------------------------------------

test_that("merge_slides combines two adjacent slides", {
  groups <- list(c("a.png"), c("b.png"), c("c.png"))
  positions <- list(1L, 1L, 1L)

  result <- merge_slides(groups, positions, slide_idx = 1)

  expect_equal(length(result$groups), 2)
  expect_equal(result$groups[[1]], c("a.png", "b.png"))
  expect_equal(result$groups[[2]], "c.png")
})

test_that("merge_slides resets positions to sequential for merged slide", {
  groups <- list(c("a.png"), c("b.png", "c.png"))
  positions <- list(1L, c(2L, 1L))

  result <- merge_slides(groups, positions, slide_idx = 1)

  expect_equal(result$positions[[1]], 1:3)
})

test_that("merge_slides is a no-op when slide_idx is the last slide", {
  groups <- list(c("a.png"), c("b.png"))
  positions <- list(1L, 1L)

  result <- merge_slides(groups, positions, slide_idx = 2)

  expect_equal(result$groups, groups)
  expect_equal(result$positions, positions)
})

test_that("merge_slides preserves slides outside the merge", {
  groups <- list(c("a.png"), c("b.png"), c("c.png"), c("d.png"))
  positions <- list(1L, 1L, 1L, 1L)

  result <- merge_slides(groups, positions, slide_idx = 2)

  expect_equal(length(result$groups), 3)
  expect_equal(result$groups[[1]], "a.png")
  expect_equal(result$groups[[2]], c("b.png", "c.png"))
  expect_equal(result$groups[[3]], "d.png")
})
