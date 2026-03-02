test_that("format_slide_notes with full metadata produces all three lines", {
  metadata <- list(
    source_meta = list(path = "scripts/run.R", latest_time = "2026-01-15 10:30"),
    object_meta = list(
      footnotes = list(
        notes = list("Population was adults", "Dose was 100mg."),
        abbreviations = list("CI", "HR", "AUC")
      )
    )
  )

  result <- format_slide_notes(metadata)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(lines[1], "Source: scripts/run.R 2026-01-15 10:30")
  ## "Population was adults" gets a period appended; "Dose was 100mg." already has one
  expect_equal(lines[2], "Notes: Population was adults. Dose was 100mg.")
  expect_equal(lines[3], "Abbreviations: CI, HR, AUC")
  ## Trailing blank line — result ends with "\n"
  expect_true(grepl("\n$", result))
})

test_that("format_slide_notes shows source path without time when time is missing", {
  metadata <- list(
    source_meta = list(path = "scripts/run.R"),
    object_meta = list(footnotes = list())
  )

  result <- format_slide_notes(metadata)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(lines[1], "Source: scripts/run.R")
})

test_that("format_slide_notes shows N/A for all fields when metadata is empty", {
  metadata <- list()

  result <- format_slide_notes(metadata)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(lines[1], "Source: N/A")
  expect_equal(lines[2], "Notes: N/A")
  expect_equal(lines[3], "Abbreviations: N/A")
})

test_that("format_slide_notes shows N/A for notes when notes list is all empty strings", {
  metadata <- list(
    source_meta = list(path = "file.R"),
    object_meta = list(
      footnotes = list(
        notes = list("", ""),
        abbreviations = list("BMI")
      )
    )
  )

  result <- format_slide_notes(metadata)

  expect_true(grepl("Notes: N/A", result))
  expect_true(grepl("Abbreviations: BMI", result))
})

test_that("format_slide_notes appends period to notes missing trailing period", {
  metadata <- list(
    source_meta = list(path = "x.R"),
    object_meta = list(
      footnotes = list(
        notes = list("No period here", "Has period."),
        abbreviations = list()
      )
    )
  )

  result <- format_slide_notes(metadata)

  expect_true(grepl("No period here\\. Has period\\.", result))
})
