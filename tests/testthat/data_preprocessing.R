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

# TODO:
# in general, specify in warnings which columns of data are relevant, with plurals
# test factors, dates, any other dataframe column type
# test full datasets
# search for more gotchas




test_that("`data` normalisation behaves as expected", {
	expect_equal(
	  data_preprocess(data.frame(a = -1)),
	  data.frame(a = 0.5)
	)

	expect_equal(
	  data_preprocess(data.frame(a = 0)),
	  data.frame(a = 0.5)
	)

	expect_equal(
	  data_preprocess(data.frame(a = 1)),
	  data.frame(a = 0.5)
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(-2, -1))),
	  data.frame(a = c(0, 1))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(-1, 0))),
	  data.frame(a = c(0, 1))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(0, 1))),
	  data.frame(a = c(0, 1))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(-1, 1))),
	  data.frame(a = c(0, 1))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(1, 2))),
	  data.frame(a = c(0, 1))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(1, 1))),
	  data.frame(a = c(0.5, 0.5))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(2, 2))),
	  data.frame(a = c(0.5, 0.5))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(1, 2, 3))),
	  data.frame(a = c(0, 0.5, 1))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(1, 2, 3, 4))),
	  data.frame(a = c(0, 1 / 3, 2 / 3, 1))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(1, 2, 3, 4, 5))),
	  data.frame(a = c(0, 1 / 4, 2 / 4, 3 / 4, 1))
	)
})

test_that("Likert scale on numerical data behaves as expected", {
	expect_equal(
	  data_preprocess(data.frame(a = c(1, 3)), likert = data.frame(c(1, 2))),
	  data.frame(a = c(0, NA))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(2, 3)), likert = data.frame(c(1, 2))),
	  data.frame(a = c(1, NA))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(1, 2, 3)), likert = data.frame(c(1, 2))),
	  data.frame(a = c(0, 1, NA))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(4, 1, 2, 3)), likert = data.frame(c(1, 2))),
	  data.frame(a = c(NA, 0, 1, NA))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(1, 3), b = c(3, 4)), likert = data.frame(c(1, 2), c(1, 2))),
	  data.frame(a = c(0, NA), b = as.numeric(c(NA, NA)))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(1, 2), b = c(1, 2)), likert = data.frame(c(NA, NA), c(1, 5))),
	  data.frame(a = c(0, 1), b = c(0, 0.25))
	)
})


test_that("Numeric data with `dummycode`", {
	expect_equal(
	  data_preprocess(data.frame(a = c(1, NA, 2, 3, 1)), dummycode = c(1)),
	  data.frame(a_1 = c(1, 0, 0, 0, 1), a_2 = c(0, 0, 1, 0, 0), a_3 = c(0, 0, 0, 1, 0))
	)
})


test_that("Numeric data with `likert` and `dummycode`", {
	expect_equal(
	  data_preprocess(data.frame(a = c(1, 2, 3, -99)), likert = data.frame(c(1, 3)), dummycode = 1),
	  data.frame(a = c(0, 0.5, 1, NA), a_.99 = c(0, 0, 0, 1))
	)

	expect_equal(
	  data_preprocess(data.frame(a = c(50, 3, 1, 2, -99)), likert = data.frame(c(1, 3)), dummycode = 1),
	  data.frame(a = c(NA, 1, 0, 0.5, NA), a_50 = c(1, 0, 0, 0, 0), a_.99 = c(0, 0, 0, 0, 1))
	)
})


# TODO: after conversion to numeric, should be treated as numeric... normalised, likert etc
test_that("Character vector without `dummycode` is coerced to numeric", {
	expect_warning(
		expect_equal(
			data_preprocess(data.frame(a = c("1"))),
			data.frame(data.frame(a = c(1)))
		)
	)
})


test_that("Character vector without `dummycode` is coerced to NA", {
	expect_warning(
		expect_equal(
			data_preprocess(data.frame(a = c("a"))),
			data.frame(data.frame(a = as.numeric(c(NA))))
		)
	)
})


test_that("Character vector with `dummycode` is coerced to numeric", {
	expect_equal(
		data_preprocess(data.frame(a = c("m", NA, "f", "m", "f", "f")), dummycode = c(T)),
		data.frame(data.frame(a_m = c(1, 0, 0, 1, 0, 0), a_f = c(0, 0, 1, 0, 1, 1)))
	)
})


test_that("Logical vector is coerced to numeric", {
	expect_equal(
		data_preprocess(data.frame(a = c(T, F, NA))),
		data.frame(a = c(1, 0, NA))
	)
})
