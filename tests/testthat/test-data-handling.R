# Here we test the expected behaviour of the data handling module.
#
# The principle is that our C++ code should do the absolute minimum of testing
# itself, if any. It should throw an error if it can't compute edges naively.


test_that("output an error if data is not a dataframe", {
  expect_error(
		data_handling(list()), 
		regexp = "Input data must be a dataframe."
	)
})


test_that("output an error if dataframe is empty", {
  expect_error(
		data_handling(data.frame()), 
		regexp = "Input dataframe cannot be empty."
	)
})


test_that("set Inf and -Inf to NA", {
 	S1 <- data_handling(data.frame(c(NA, Inf, -Inf)))
  S2 <- data.frame(as.numeric(c(NA, NA, NA)))
	colnames(S1) <- NULL
	colnames(S2) <- NULL

	expect_equal(S1, S2)
})


test_that("dummycode should be null, or be coercible to 0 or 1", {
	expect_error(
		data_handling(data.frame(1), dummycode = 2), 
		regexp = "dummycode should equal 0 or 1, or be coercible to those values."
	)
})


test_that("likert must be a dataframe if it's not null", {
	expect_error(
		data_handling(data.frame(1), likert = list()),
		regexp = "likert must be a dataframe"
	)
})


test_that("likert must have two rows", {
	expect_error(
		data_handling(data.frame(1), likert = data.frame(1)),
		regexp = "likert must have two rows, the min and max value of each column"
	)
})


test_that("likert must have as many columns as the survey dataframe", {
	expect_error(
		data_handling(data.frame(1, 2), likert = data.frame(c(1, 2))),
		regexp = "likert must have as many columns as the survey dataframe"
	)
})


test_that("each column in likert should be numeric", {
	expect_warning(
		data_handling(data.frame(1), likert = data.frame(c(F, T))),
		regexp = "each column in likert should be numeric"
	)
})


test_that("likert should be ordered", {
	expect_warning(
		data_handling(data.frame(1), likert = data.frame(c(2, 1))),
		regexp = "each column in likert should be non-decreasing"
	)
})


# TODO
# likert comparisons fail on NAs, returns logicals
# one or both entries in likert should be able to be NA

test_that("numeric with likert", {
	df1 <- data_handling(data.frame(c(1, 2, 3)), likert = data.frame(c(1, 2)))
	df2 <- data.frame(c(1, 2, NA))
	colnames(df1) <- NULL
	colnames(df2) <- NULL

	expect_equal(df1, df2)
})


test_that("numeric with likert, multiple columns", {
	df1 <- data_handling(data.frame(c(1, 2, 3), c(2, 3, 4)), likert = data.frame(c(1, 2), c(1, 2)))
	df2 <- data.frame(c(1, 2, NA), c(2, NA, NA))
	colnames(df1) <- NULL
	colnames(df2) <- NULL

	expect_equal(df1, df2)
})


#test_that("numeric with dummycode", {
#	df1 <- data_handling(data.frame(c(1, 2, 3, 1)), dummycode = 1)
#	df2 <- data.frame(c(1, 0, 0, 1), c(0, 1, 0, 0), c(0, 0, 1, 0))
#	colnames(df1) <- NULL
#	colnames(df2) <- NULL
#
#	expect_equal(df1, df2)
#})


#test_that("numeric with likert", {
#	expect_equal(
#		expect_warning(data_handling(data.frame(c(1, 2, 3), likert = data.frame(c(2, 1))))),
#		data.frame(c(NA, NA, NA))
#	)
#})


#test_that("numeric with dummycode")
#test_that("numeric with dummycode, warning about decimals")
#test_that("numeric with likert and dummycode")

# need to test coercions here too
#test_that("logical with likert")
#test_that("logical with dummycode")
#test_that("logical with dummycode, warning about decimals")
#test_that("logical with likert and dummycode")


