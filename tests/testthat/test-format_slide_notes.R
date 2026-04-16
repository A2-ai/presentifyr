test_that("format_slide_notes decodes abbreviations when definitions provided", {
  metadata <- list(
    source_meta = list(
      path = "scripts/run.R", latest_time = "2026-01-15 10:30"
    ),
    object_meta = list(
      footnotes = list(
        notes = list("Population was adults", "Dose was 100mg."),
        abbreviations = list("CI", "HR", "AUC")
      )
    )
  )

  abbrev_defs <- list(
    CI = "confidence interval",
    HR = "hazard ratio",
    AUC = "area under the curve"
  )

  result <- format_slide_notes(metadata, abbrev_defs)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(
    lines[1], "Source: scripts/run.R 2026-01-15 10:30"
  )
  expect_equal(
    lines[2], "Notes: Population was adults. Dose was 100mg."
  )
  expect_equal(
    lines[3],
    paste0(
      "Abbreviations: CI: confidence interval, ",
      "HR: hazard ratio, AUC: area under the curve."
    )
  )
  expect_true(grepl("\n$", result))
})

test_that("format_slide_notes shows raw keys when no definitions provided", {
  metadata <- list(
    source_meta = list(
      path = "scripts/run.R", latest_time = "2026-01-15 10:30"
    ),
    object_meta = list(
      footnotes = list(
        notes = list("Population was adults"),
        abbreviations = list("CI", "HR")
      )
    )
  )

  result <- format_slide_notes(metadata)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(lines[3], "Abbreviations: CI, HR")
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

  abbrev_defs <- list(BMI = "body mass index")

  result <- format_slide_notes(metadata, abbrev_defs)

  expect_true(grepl("Notes: N/A", result))
  expect_true(grepl("Abbreviations: BMI: body mass index\\.", result))
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

test_that("format_slide_notes falls back to raw key for unknown abbreviation", {
  metadata <- list(
    source_meta = list(path = "x.R"),
    object_meta = list(
      footnotes = list(
        notes = list(),
        abbreviations = list("CI", "UNKNOWN")
      )
    )
  )

  abbrev_defs <- list(CI = "confidence interval")

  result <- format_slide_notes(metadata, abbrev_defs)

  expect_true(grepl("CI: confidence interval, UNKNOWN\\.", result))
})

test_that("decode_abbreviations strips trailing period from definitions", {
  defs <- list(AUC = "area under the curve.")

  result <- decode_abbreviations(list("AUC"), defs)

  expect_equal(result, "AUC: area under the curve.")
})
