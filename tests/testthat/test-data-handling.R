# Here we test the expected behaviour of the data handling module.
#
# The principle is that our C++ code should do the absolute minimum of testing
# itself, if any. It should throw an error if it can't compute edges naively.
# 
# check if dataframe
# coerce to numeric
# are likert scale specifications correct, consistent?
# dummy coding
# NA versus NULL versus Inf versus numeric(0)
# check if empty, require one row, one column?


test_that("data needs to be in a data frame", {

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


# make this an example showing everything that happens upon coercion to numeric
# really just a sanity check, showing that base R as.numeric() works as expected
test_that("example dataframe showing coercion from string to NA works as expected", {

  S <- data.frame(c(1, "two", "3"), c(4, 5, "6"))

  Sclean <- data.frame(as.numeric(c(1, NA, 3)), as.numeric(c(4, 5, 6)))

  expect_warning(S <- data_handling(S))

  colnames(S) <- NULL
  colnames(Sclean) <- NULL

  expect_equal(S, Sclean) 
})


# TODO 
# test incorrect likert specification, e.g. min greater than max
# handling of NA
# likert incorrect dimensions, 
# different ways of constructing likert dataframe

# TODO in documentation, explain what's happening upon coercion to numeric
