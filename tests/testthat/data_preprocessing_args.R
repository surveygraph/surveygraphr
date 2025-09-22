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


test_that("`likert` must be a dataframe if it's not null", {
	expect_error(
		data_preprocess(data.frame(1), likert = list()),
		regexp = "likert must be a dataframe"
	)
})


test_that("likert must have two rows", {
	expect_error(
		data_preprocess(data.frame(1), likert = data.frame(1)),
		regexp = "likert must have two rows, the min and max value of each column"
	)
})


test_that("likert must have as many columns as the survey dataframe", {
	expect_error(
		data_preprocess(data.frame(1, 2), likert = data.frame(c(1, 2))),
		regexp = "likert must have as many columns as the survey dataframe"
	)
})


test_that("likert should be numeric or logical, set to c(NA, NA) otherwise", {
	expect_warning(
		data_preprocess(data.frame(1), likert = data.frame(c("a", "b"))),
	  regexp = "setting likert columns that aren't numeric or logical to logical NA"
	)
})


test_that("if a likert column is numeric, both values must be finite", {
	expect_warning(
		data_preprocess(data.frame(1), likert = data.frame(c(1, NA))),
		regexp = "at least one numerical likert column invalid, setting to logical NA"
	)
})


test_that("if a likert column is logical, both values must be NA", {
	expect_warning(
		data_preprocess(data.frame(1), likert = data.frame(c(T, F))),
		regexp = "at least one logical likert column contains non NA entries, setting to NA"
	)
})


test_that("numerical columns of likert should be non-decreasing", {
	expect_warning(
		data_preprocess(data.frame(1), likert = data.frame(c(2, 1))),
		regexp = "each numerical column in likert should be non-decreasing, setting to NA"
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
		regexp = "dummycoding a numeric column that contains non-integer values"
	)
})


test_that("ignore likert scaling for character vectors", {
	expect_warning(
		data_preprocess(data.frame(a = c("a")), likert = data.frame(c(1, 3))),
		regexp = "ignoring likert flag for character vector"
	)
})


test_that("ignore likert flag for logical vectors", {
	expect_warning(
		data_preprocess(data.frame(a = c(T)), likert = data.frame(c(1, 3))),
		regexp = "ignoring likert flag for logical vector"
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
