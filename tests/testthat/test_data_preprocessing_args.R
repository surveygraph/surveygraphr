test_that("Output an error if `data` is not a dataframe", {
  expect_error(
    data_preprocess(list()), 
    regexp = "Input data must be a dataframe."
  )
})


test_that("Output an error if `data` dataframe is empty", {
  expect_error(
    data_preprocess(data.frame()), 
    regexp = "Input dataframe cannot be empty."
  )
})


test_that("`limits` must be a dataframe if it's not null", {
  expect_error(
    data_preprocess(data.frame(1), limits = list()),
    regexp = "limits must be a dataframe"
  )
})


test_that("limits must have two rows", {
  expect_error(
    data_preprocess(data.frame(1), limits = data.frame(1)),
    regexp = "limits must have two rows, the min and max value of each column"
  )
})


test_that("limits must have as many columns as the survey dataframe", {
  expect_error(
    data_preprocess(data.frame(1, 2), limits = data.frame(c(1, 2))),
    regexp = "limits must have as many columns as the survey dataframe"
  )
})


test_that("limits should be numeric or logical, set to c(NA, NA) otherwise", {
  expect_warning(
    data_preprocess(data.frame(1), limits = data.frame(c("a", "b"))),
    regexp = "setting limits columns that aren't numeric or logical to logical NA"
  )
})


test_that("if a limits column is numeric, both values must be finite", {
  expect_warning(
    data_preprocess(data.frame(1), limits = data.frame(c(1, NA))),
    regexp = "at least one numerical limits column invalid, setting to logical NA"
  )
})


test_that("if a limits column is logical, both values must be NA", {
  expect_warning(
    data_preprocess(data.frame(1), limits = data.frame(c(T, F))),
    regexp = "at least one logical limits column contains non NA entries, setting to NA"
  )
})


test_that("numerical columns of limits should be non-decreasing", {
  expect_warning(
    data_preprocess(data.frame(1), limits = data.frame(c(2, 1))),
    regexp = "each numerical column in limits should be non-decreasing, setting to NA"
  )
})


test_that("`dummycode` must be an atomic vector if it's not null", {
  expect_error(
    data_preprocess(data.frame(1), dummycode = list()),
    regexp = "`dummycode` must be an atomic vector"
  )
})


test_that("`dummycode` vector must be numeric or logical", {
  expect_error(
    data_preprocess(data.frame(1), dummycode = c("a")),
    regexp = "`dummycode` must be numeric or logical"
  )
})


test_that("`dummycode` must have as many entries as the survey dataframe as columns", {
  expect_error(
    data_preprocess(data.frame(1), dummycode = c(1, 2)),
    regexp = "`dummycode` length must equal number of columns in survey dataframe"
  )
})


test_that("If dummycode is logical, NA entries are set to FALSE", {
  expect_warning(
    data_preprocess(data.frame(1, 2), dummycode = c(T, NA)),
    regexp = "Setting NA entries of `dummycode` to FALSE"
  )
})


test_that("If `dummycode` is numeric, setting entries that aren't 1 or 0 to 0", {
  expect_warning(
    data_preprocess(data.frame(1, 2), dummycode = c(1, 2)),
    regexp = "Setting entries of `dummycode` that aren't 1 or 0 to 0"
  )
})


test_that("Numeric with `dummycode` and non-integers", {
  expect_warning(
    data_preprocess(data.frame(a = c(1.1)), dummycode = c(1)),
    regexp = "Dummycoding a numeric column that contains non-integer values."
  )
})


test_that("ignore limits scaling for character vectors", {
  expect_warning(
    data_preprocess(data.frame(a = "a"), limits = data.frame(c(1, 3))),
    regexp = "Ignoring `limits` flag for character vector."
  )
})


test_that("ignore limits flag for logical vectors", {
  expect_warning(
    data_preprocess(data.frame(a = c(T)), limits = data.frame(c(1, 3))),
    regexp = "ignoring limits flag for logical vector"
  )
})


test_that("ignore dummycode flag for logical vectors", {
  expect_warning(
    data_preprocess(data.frame(a = c(T)), dummycode = c(T)),
    regexp = "ignoring dummycode flag for logical vector"
  )
})


test_that("Set Inf and -Inf to NA", {
  expect_equal(
    data_preprocess(data.frame(a = c(NA, Inf, -Inf))),
    data.frame(a = as.numeric(c(NA, NA, NA)))
  )
})
