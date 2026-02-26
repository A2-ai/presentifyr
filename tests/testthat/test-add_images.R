test_that("add_images fails if invalid layout name is specified", {
  tmp_dir <- tempdir()
  output_pptx <- file.path(tmp_dir, "bad_layout.pptx")

  img <- file.path(tmp_dir, "dummy.png")
  magick::image_write(magick::image_blank(100, 100, "blue"), img)

  expect_error(
    add_images(
      files = img,
      output_pptx = output_pptx,
      slide_layout_name = "NonExistentLayout",
      base_pptx = NULL
    ),
    regexp = "not found in template"
  )
})

test_that("add_images fails if no content placeholder 2 is found", {
  tmp_dir <- tempdir()
  output_pptx <- file.path(tmp_dir, "mocked_layout_test.pptx")

  img <- file.path(tmp_dir, "dummy.png")
  magick::image_write(magick::image_blank(100, 100, "blue"), img)

  mock_layout_properties <- function(doc, layout) {
    data.frame(
      master_name = c("FAKE MASTER","FAKE MASTER"),
      name        = c(layout, layout),
      type        = c("sldNum","title"),
      id          = c("6","2"),
      ph_label    = c("Slide Number Placeholder 5","Title 1"),
      ph          = c("<p:ph type=\"sldNum\" sz=\"quarter\" idx=\"12\"/>",
                      "<p:ph type=\"title\"/>"),
      offx        = c(9.4166667, 0.9166667),
      offy        = c(6.9513911, 0.3993077),
      cx          = c(3, 11.5),
      cy          = c(0.3993056, 1.4496533),
      rotation    = c(NA_real_, NA_real_),
      fld_id      = c("{C7AF375C-9754-4EA2-97C9-0ABC2B16F434}", NA),
      fld_type    = c("slidenum", NA),
      stringsAsFactors = FALSE
    )
  }

  mockery::stub(add_images, "officer::layout_properties", mock_layout_properties)

  expect_error(
    add_images(
      files = img,
      output_pptx = output_pptx,
      base_pptx = NULL
    ),
    "No usable placeholder found"
  )
})

test_that("add_images fails if magick cannot read the image", {
  tmp_dir <- tempdir()
  output_pptx <- file.path(tmp_dir, "unreadable_image.pptx")

  fake_img <- file.path(tmp_dir, "image.png")
  file.create(fake_img)

  expect_error(
    add_images(
      files = c(fake_img),
      output_pptx = output_pptx,
      slide_layout_name = NULL,
      base_pptx = NULL
    ),
    regexp = "[Ii]mproper.?[Ii]mage.?[Hh]eader"
  )
})
