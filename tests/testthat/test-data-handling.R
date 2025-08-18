# Here we test the expected behaviour of the data handling module.
#
# The principle is that our C++ code should do the absolute minimum of testing
# itself, if any. It should throw an error if it can't compute edges naively.
# 
# check that program...
# ===============================================
# outputs error if not dataframe
# outputs warning if dataframe is empty
# coerces non-numeric data to numeric using example dataframe
# sets Inf and -Inf to NA
# handles Likert scale specifications consistently
# handles dummy coding consistently


test_that("output an error if data is not a dataframe", {
  expect_error(data_handling(list()), "Input data must be a dataframe.")
})

test_that("output a warning if dataframe is empty", {
  expect_warning(data_handling(data.frame()), "Input dataframe is empty.")
})

test_that("coercion to numeric for a bunch of test cases", {
	S <- data.frame(
		item1 = c(1.2, 3.4, 5),
		item2 = c(TRUE, FALSE, TRUE),
		item3 = factor(c("low", "medium", "high")),
		item4 = c("1", "2.5", "-3"),
		item5 = c("3.14", "pi", "42"),
		item6 = c("NA", "NA", "NA"),
		item7 = as.Date(c("2002-03-02", "2008-12-14", "1955-02-23")),
		item8 = as.POSIXct(c("2002-03-02", "2008-12-14", "1955-02-23"), tz="UTC"),
		stringsAsFactors = FALSE
	)

	Sclean <- data.frame(
		item1 = c(1.2, 3.4, 5),
		item2 = c(1, 0, 1),
		item3 = as.numeric(c(NA, NA, NA)),
		item4 = c(1, 2.5, -3),
		item5 = c(3.14, NA, 42.0),
		item6 = as.numeric(c(NA, NA, NA)),
		item7 = c(11748, 14227, -5426),
		item8 = c(1015027200, 1229212800, -468806400)
	)

	expect_warning(S <- data_handling(S), regexp = "NAs introduced by coercion")
	expect_equal(Sclean, S);
})


test_that("set Inf and -Inf to NA", {
	S <- data.frame(c(NA, Inf, -Inf))

	Sclean <- data.frame(as.numeric(c(NA, NA, NA)))
	S <- data_handling(S)

	colnames(S) <- NULL
	colnames(Sclean) <- NULL

	expect_equal(Sclean, S)	
})


test_that("likert scale dataframe is consistent", {
  S <- data.frame(item_1 = c(1, -99),
                  item_2 = c(1, 3),
                  item_3 = c(1, 2))

	l <- data.frame(minval = c(1, 1, 1), maxval = c(3, 3, 3))

  expect_equal(
		make_projection(S, likert = l),
    data.frame(u = c(1), v = c(2), weight = c(0.25))
  )
})
