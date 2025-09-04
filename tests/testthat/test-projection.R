# Tests for make_projection(). Note that arguments likert and dummycode are
# tested in test-data-preprocessing.R.
test_that("unused arguments", {
	expect_warning(
		make_projection(data.frame(1), argname1 = 1, argname2 = 1),
		regexp = "Unused arguments in ...: argname1, argname2"
	)
})


test_that("unrecognised option for layer argument", {
	expect_warning(
		make_projection(data.frame(1), layer = "agnet"),
		regexp = "`layer` option \"agnet\" unrecognised; defaulting to \"agent\"."
	)
})


test_that("unrecognised option for method argument", {
	expect_warning(
		make_projection(data.frame(1), method = "clc"),
		regexp = "`method` option \"clc\" unrecognised; defaulting to \"lcc\"."
	)
})


test_that("methodval not in range", {
	expect_warning(
		make_projection(data.frame(1), methodval = -1),
		regexp = "`methodval` must be between 0 and 1 for all methods; defaulting to 1."
	)
})


test_that("correct edge list", {
  S <- data.frame(
		group = c(0, 0, 1, 1),
    item_1 = c(1, 2, 5, 4),
    item_2 = c(1, 3, 2, 2),
    item_3 = c(1, 5, 3, 2)
	)

  fn <- function(x)
    make_projection(S, method = "similarity", methodval = x)

	expect_equal(
		fn(0),
		data.frame(
			u = c(1, 1, 1, 2, 2, 3),
			v = c(2, 3, 4, 3, 4, 4),
			weight = c(0.4375, 0.25, 0.375, 0.3125, 0.3125, 0.875)
		)
	)

	expect_equal(
		fn(0.25),
		data.frame(
			u = c(1, 1, 2, 2, 3),
			v = c(2, 4, 3, 4, 4),
			weight = c(0.4375, 0.375, 0.3125, 0.3125, 0.875)
		)
	)

	expect_equal(
		fn(0.9),
		data.frame(
			u = numeric(0),
			v = numeric(0),
			weight = numeric(0)
		)
	)
})
