# Tests for lookup functions

test_that("lookup_gaez_variable works with exact code match", {
  options(gaez_testing_mode = TRUE)
  result <- lookup_gaez_variable("RES05-YX")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$variable_code, "RES05-YX")

  options(gaez_testing_mode = NULL)
})

test_that("lookup_gaez_variable works with partial name match", {
  options(gaez_testing_mode = TRUE)
  result <- lookup_gaez_variable("attainable yield")

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) >= 1)

  options(gaez_testing_mode = NULL)
})

test_that("lookup_gaez_variable fails with empty input", {
  expect_error(lookup_gaez_variable(""), "provide a variable")
  expect_error(lookup_gaez_variable(NULL), "provide a variable")
})

test_that("lookup_gaez_variable fails with no matches", {
  expect_error(
    lookup_gaez_variable("nonexistent_variable_xyz"),
    "No matches found"
  )
})

test_that("lookup_gaez_crop works with exact code match", {
  options(gaez_testing_mode = TRUE)
  result <- lookup_gaez_crop("MZE", theme = 4)

  expect_type(result, "character")
  expect_equal(result, "MZE")

  options(gaez_testing_mode = NULL)
})

test_that("lookup_gaez_crop works with name search", {
  options(gaez_testing_mode = TRUE)
  result <- lookup_gaez_crop("maize", theme = 4)

  expect_type(result, "character")
  expect_equal(result, "MZE")

  options(gaez_testing_mode = NULL)
})

test_that("lookup_gaez_crop fails with invalid theme", {
  expect_error(lookup_gaez_crop("maize", theme = 7), "Theme must be")
})

test_that("lookup_gaez_crop fails with empty input", {
  expect_error(lookup_gaez_crop("", theme = 4), "provide a crop name")
})
