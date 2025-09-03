# Here we test the expected behaviour of the data preprocessing module.
#
# The principle is that our C++ code should do the absolute minimum of testing
# itself. In particular, all it should do is check that all data provided by
# R are doubles, or NAs. If not, it should throw an error.
#
# Note to self, there's room for improvement in how we're approaching tests. That is, 
# there is a sensitivity to the order in which these tests are carried out, and 
# the order of the implementation in R/data-preprocess.R. It feels like this
# shouldn't be the case. That is, if we rearrange the tests, the errors might
# not match up with what's wrong... (is that right?) need to think about logic.

# TODO
# in general, specify in warnings which columns of data are relevant, with plurals
# test factors, dates, any other dataframe column type
# test full datasets
# search for more gotchas


test_that("output an error if data is not a dataframe", {
  expect_error(
		data_preprocess(list()), 
		regexp = "Input data must be a dataframe."
	)
})


test_that("output an error if dataframe is empty", {
  expect_error(
		data_preprocess(data.frame()), 
		regexp = "Input dataframe cannot be empty."
	)
})


test_that("set Inf and -Inf to NA", {
	expect_equal(
		data_preprocess(data.frame(a = c(NA, Inf, -Inf))),
		data.frame(a = as.numeric(c(NA, NA, NA)))
	)
})

# TODO, need to consider how data will be scaled if likert isn't provided, and
# a column doesn't reach it's max and min vals. eg 1 and 3 on a 3 point likert
# scale are far, but close on a 20 point likert scale

test_that("likert must be a dataframe if it's not null", {
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


test_that("dummycode must be an atomic vector if it's not null", {
	expect_error(
		data_preprocess(data.frame(1), dummycode = list()),
		regexp = "dummycode must be an atomic vector"
	)
})


test_that("dummycode vector must be numeric or logical", {
	expect_error(
		data_preprocess(data.frame(1), dummycode = c("a")),
		regexp = "dummycode must be numeric or logical"
	)
})


test_that("dummycode must have as many entries as the survey dataframe as columns", {
	expect_error(
		data_preprocess(data.frame(1), dummycode = c(1, 2)),
		regexp = "dummycode length must equal number of columns in survey dataframe"
	)
})


test_that("if dummycode is logical, NA entries are set to FALSE", {
	expect_warning(
		data_preprocess(data.frame(1, 2), dummycode = c(T, NA)),
		regexp = "setting NA entries of dummycode to FALSE"
	)
})


test_that("if dummycode is numeric, setting entries that aren't 1 or 0 to 0", {
	expect_warning(
		data_preprocess(data.frame(1, 2), dummycode = c(1, 2)),
		regexp = "setting entries of dummycode that aren't 1 or 0 to 0"
	)
})


test_that("likert scale on numerical data behaves as expected", {
	expect_equal(
	  data_preprocess(data.frame(a = c(1, 3)), likert = data.frame(c(1, 2))),
	  data.frame(a = c(1, NA))
	)
})


test_that("as above, but with multiple columns", {
	expect_equal(
	  data_preprocess(
			data.frame(a = c(1, 3), b = c(3, 4)),
			likert = data.frame(c(1, 2), c(1, 2))
		),
	  data.frame(a = c(1, NA), b = as.numeric(c(NA, NA)))
	)
})


test_that("numeric with dummycode", {
	expect_equal(
	  data_preprocess(data.frame(a = c(1, NA, 2, 3, 1)), dummycode = c(1)),
	  data.frame(a_1 = c(1, 0, 0, 0, 1), a_2 = c(0, 0, 1, 0, 0), a_3 = c(0, 0, 0, 1, 0))
	)
})


test_that("numeric with dummycode and non-integers", {
	expect_warning(
	  data_preprocess(data.frame(a = c(1.1)), dummycode = c(1)),
		regexp = "dummycoding a numeric column that contains non-integer values"
	)
})


test_that("numeric with likert and dummycode", {
	expect_equal(
	  data_preprocess(
			data.frame(a = c(1, 2, 3, -99)), 
			likert = data.frame(c(1, 3)), 
			dummycode = c(1)
		),
	  data.frame(a = c(1, 2, 3, NA), a_.99 = c(0, 0, 0, 1))
	)
})


test_that("ignore likert scaling for character vectors", {
	expect_warning(
		data_preprocess(data.frame(a = c("a")), likert = data.frame(c(1, 3))),
		regexp = "ignoring likert flag for character vector"
	)
})


test_that("character vector without dummycoding flag coerced to NA", {
	expect_warning(
		expect_equal(
			data_preprocess(data.frame(a = c("a"))),
			data.frame(data.frame(a = as.numeric(c(NA))))
		)
	)
})


test_that("character vector without dummycoding flag coerced to numeric", {
	expect_warning(
		expect_equal(
			data_preprocess(data.frame(a = c("1"))),
			data.frame(data.frame(a = c(1)))
		)
	)
})


test_that("character vector without dummycoding flag coerced to numeric", {
	expect_equal(
		data_preprocess(data.frame(a = c("m", NA, "f", "m", "f", "f")), dummycode = c(T)),
		data.frame(data.frame(a_m = c(1, 0, 0, 1, 0, 0), a_f = c(0, 0, 1, 0, 1, 1)))
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


test_that("coerce logical vector to numeric", {
	expect_equal(
		data_preprocess(data.frame(a = c(T, F, NA))),
		data.frame(a = c(1, 0, NA))
	)
})
