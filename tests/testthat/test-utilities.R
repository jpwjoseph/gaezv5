# Tests for utility functions

test_that("list_gaez_crops works without filters", {
  result <- list_gaez_crops(theme = 4)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(all(result$gaez_theme == 4))
})

test_that("list_gaez_crops filters by crop group", {
  result <- list_gaez_crops(crop_group = "cereal", theme = 4)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(all(grepl("Cereal", result$gaez_crop_group, ignore.case = TRUE)))
})

test_that("list_gaez_crops validates theme", {
  expect_error(list_gaez_crops(theme = 7), "Theme must be")
})

test_that("list_gaez_crops returns NULL and lists available groups when no match found", {
  # Test with a crop group that doesn't exist
  expect_message(
    result <- list_gaez_crops(crop_group = "nonexistent", theme = 4),
    "No crops found matching crop_group"
  )
  expect_message(
    result <- list_gaez_crops(crop_group = "nonexistent", theme = 4),
    "Available crop groups for theme"
  )
  expect_message(
    result <- list_gaez_crops(crop_group = "nonexistent", theme = 4),
    "Please resubmit your query"
  )
  expect_null(result)
})

test_that("list_gaez_crops shows correct available groups for each theme", {
  # Test that the message contains expected crop groups for theme 4
  expect_message(
    result <- list_gaez_crops(crop_group = "xyz123", theme = 4),
    "Cereals"  # Should be in theme 4
  )
})

test_that("show_gaez_examples runs without error", {
  expect_output(show_gaez_examples(), "GAEZ v5 Function Examples")
  expect_invisible(show_gaez_examples())
})

test_that("data objects are loaded correctly", {
  expect_true(exists("gaez_variables"))
  expect_true(exists("gaez_crops"))
  expect_true(exists("gaez_scenarios"))
  expect_true(exists("gaez_url_structure"))

  expect_s3_class(gaez_variables, "tbl_df")
  expect_s3_class(gaez_crops, "tbl_df")
  expect_s3_class(gaez_scenarios, "tbl_df")
  expect_s3_class(gaez_url_structure, "tbl_df")
})

test_that("data objects have expected structure", {
  # gaez_variables
  expect_true(all(c(
    "theme_number",
    "theme_name",
    "variable_code",
    "data_code"
  ) %in% names(gaez_variables)))

  # gaez_crops
  expect_true(all(c(
    "gaez_crop_code",
    "name",
    "gaez_theme"
  ) %in% names(gaez_crops)))

  # Check theme numbers
  expect_true(all(gaez_variables$theme_number %in% 1:9))
  expect_true(all(gaez_crops$gaez_theme %in% 3:6))
})
