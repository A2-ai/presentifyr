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

test_that("format_slide_notes errors when abbreviations present but no definitions", {
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

  expect_error(
    format_slide_notes(metadata),
    "Abbreviation 'CI' not found"
  )
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

test_that("format_slide_notes errors when abbreviation key is missing from definitions", {
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

  expect_error(
    format_slide_notes(metadata, abbrev_defs),
    "Abbreviation 'UNKNOWN' not found"
  )
})

test_that("decode_abbreviations strips trailing period from definitions", {
  defs <- list(AUC = "area under the curve.")

  result <- decode_abbreviations(list("AUC"), defs)

  expect_equal(result, "AUC: area under the curve.")
})

test_that("format_slide_notes prepends meta_type definition to Notes", {
  metadata <- list(
    source_meta = list(path = "scripts/run.R"),
    object_meta = list(
      meta_type = "efficacy",
      footnotes = list(
        notes = list("Population was adults."),
        abbreviations = list()
      )
    )
  )
  fig_fn <- list(efficacy = "Efficacy population.")

  result <- format_slide_notes(
    metadata, meta_type_definitions = fig_fn
  )

  expect_true(grepl(
    "Notes: Efficacy population\\. Population was adults\\.", result
  ))
})

test_that("format_slide_notes resolves meta_type from merged fig+table dict", {
  metadata <- list(
    source_meta = list(path = "x.R"),
    object_meta = list(
      meta_type = "demographics",
      footnotes = list(notes = list(), abbreviations = list())
    )
  )
  ## demographics lives in the table_footnotes side of the merged dict
  defs <- list(
    efficacy = "Efficacy population.",
    demographics = "Demographics table."
  )

  result <- format_slide_notes(metadata, meta_type_definitions = defs)

  expect_true(grepl("Notes: Demographics table\\.", result))
})

test_that("format_slide_notes does not double-period meta_type ending in '. '", {
  ## YAML folded scalars leave a trailing space after the final period.
  metadata <- list(
    source_meta = list(path = "x.R"),
    object_meta = list(
      meta_type = "conc_time_plot",
      footnotes = list(
        notes = list("Testing another note to see compatibility"),
        abbreviations = list()
      )
    )
  )
  defs <- list(conc_time_plot = "Lower LLQ = 2 ug/mL. ")

  result <- format_slide_notes(metadata, meta_type_definitions = defs)

  expect_false(grepl("\\. \\. ", result))
  expect_true(grepl(
    "Notes: Lower LLQ = 2 ug/mL\\. Testing another note", result
  ))
})

test_that("format_slide_notes appends period to meta_type text missing one", {
  metadata <- list(
    source_meta = list(path = "x.R"),
    object_meta = list(
      meta_type = "efficacy",
      footnotes = list(notes = list(), abbreviations = list())
    )
  )
  fig_fn <- list(efficacy = "Efficacy population")

  result <- format_slide_notes(metadata, meta_type_definitions = fig_fn)

  expect_true(grepl("Notes: Efficacy population\\.", result))
})

test_that("format_slide_notes ignores meta_type when value is 'NA'", {
  metadata <- list(
    source_meta = list(path = "x.R"),
    object_meta = list(
      meta_type = "NA",
      footnotes = list(notes = list(), abbreviations = list())
    )
  )

  result <- format_slide_notes(metadata, meta_type_definitions = list())

  expect_true(grepl("Notes: N/A", result))
})

test_that("format_slide_notes errors on meta_type missing from definitions", {
  metadata <- list(
    source_meta = list(path = "x.R"),
    object_meta = list(
      meta_type = "efficacy",
      footnotes = list(notes = list(), abbreviations = list())
    )
  )

  expect_error(
    format_slide_notes(
      metadata,
      meta_type_definitions = list(safety = "Safety.")
    ),
    "meta_type 'efficacy' not found"
  )
})

test_that("format_slide_notes errors when meta_type set but no fig footnotes", {
  metadata <- list(
    source_meta = list(path = "x.R"),
    object_meta = list(
      meta_type = "efficacy",
      footnotes = list(notes = list(), abbreviations = list())
    )
  )

  expect_error(
    format_slide_notes(metadata),
    "meta_type 'efficacy' not found"
  )
})

test_that("format_slide_notes renders shiny source from app_name + version", {
  metadata <- list(
    source_meta = list(
      type = "shiny",
      app_name = "myapp",
      app_version = "1.2.0"
    ),
    object_meta = list(
      creation_time = "2026-05-13 09:00:00",
      footnotes = list(notes = list(), abbreviations = list())
    )
  )

  result <- format_slide_notes(metadata)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(lines[1], "Source: myapp v1.2.0 2026-05-13 09:00:00")
})

test_that("format_slide_notes renders script source from path + latest_time", {
  metadata <- list(
    source_meta = list(
      type = "script",
      path = "scripts/01-pk.R",
      latest_time = "2026-04-20 12:00:00"
    ),
    object_meta = list(footnotes = list())
  )

  result <- format_slide_notes(metadata)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(lines[1], "Source: scripts/01-pk.R 2026-04-20 12:00:00")
})

test_that("format_slide_notes legacy fallback uses path+time when type absent", {
  metadata <- list(
    source_meta = list(
      path = "scripts/run.R",
      latest_time = "2026-01-15 10:30"
    ),
    object_meta = list(footnotes = list())
  )

  result <- format_slide_notes(metadata)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(lines[1], "Source: scripts/run.R 2026-01-15 10:30")
})

test_that("format_slide_notes shiny source -> N/A when app fields missing", {
  metadata <- list(
    source_meta = list(type = "shiny"),
    object_meta = list(footnotes = list())
  )

  result <- format_slide_notes(metadata)
  lines <- strsplit(result, "\n")[[1]]

  expect_equal(lines[1], "Source: N/A")
})
