# Tests for make_projection(). Note that arguments likert and dummycode are
# tested in test-data-preprocessing.R.
test_that("unused arguments", {
	expect_warning(
		make_projection(data.frame(1), argname1 = 1, argname2 = 1),
		regexp = "Unused arguments in ...: argname1, argname2"
	)
})


test_that("`layer` argument not a character", {
	expect_warning(
		make_projection(data.frame(1), layer = 2),
		regexp = "`layer` must be a character string; defaulting to \"agent\"."
	)
})


test_that("`layer` argument has unrecognised option", {
	expect_warning(
		make_projection(data.frame(1), layer = "agnet"),
		regexp = "`layer` option \"agnet\" unrecognised; defaulting to \"agent\"."
	)
})


test_that("`method` argument not a character", {
	expect_warning(
		make_projection(data.frame(1), method = 0),
		regexp = "`method` must be a character string; defaulting to \"lcc\"."
	)
})


test_that("`method` argument has unrecognised option", {
	expect_warning(
		make_projection(data.frame(1), method = "clc"),
		regexp = "`method` option \"clc\" unrecognised; defaulting to \"lcc\"."
	)
})


test_that("`methodval` not in range, lcc", {
	expect_warning(
		make_projection(data.frame(1), method = "lcc", methodval = -1),
		regexp = "`methodval` must be between 0 and 1; defaulting to 1 for lcc method."
	)
})


test_that("`methodval` not in range, avgdegree", {
	expect_warning(
		make_projection(data.frame(1), method = "avgdegree", methodval = -1),
		regexp = "`methodval` must be between 0 and 1; defaulting to 0 for avgdegree method."
	)
})


test_that("`methodval` not in range, similarity", {
	expect_warning(
		make_projection(data.frame(1), method = "similarity", methodval = -1),
		regexp = "`methodval` must be between 0 and 1; defaulting to 1 for similarity method."
	)
})


test_that("`mincompare` not integer, agent layer", {
	expect_warning(
		make_projection(data.frame(1:2), layer = "agent", mincompare = "hello"),
		regexp = "Expecting an integer for `mincompare`; defaulting to ceiling(ncol(data) / 2) for agent layer.",
		fixed = T
	)	
})


test_that("`mincompare` not integer, symbolic layer", {
	expect_warning(
		make_projection(data.frame(1:2), layer = "symbolic", mincompare = "hello"),
		regexp = "Expecting an integer for `mincompare`; defaulting to ceiling(nrow(data) / 2) for symbolic layer.",
		fixed = T
	)	
})


test_that("`mincompare` out of range, agent layer", {
	expect_warning(
		make_projection(data.frame(1:2), layer = "agent", mincompare = -1),
		regexp = "Expecting `mincompare` between 1 and ncol(data) for agent layer; defaulting to ceiling(ncol(data) / 2).",
		fixed = T
	)	
})


test_that("`mincompare` out of range, symbolic layer", {
	expect_warning(
		make_projection(data.frame(1:2), layer = "symbolic", mincompare = -1),
		regexp = "Expecting `mincompare` between 1 and nrow(data) for symbolic layer; defaulting to ceiling(nrow(data) / 2).",
		fixed = T
	)	
})


test_that("`metric` argument has unrecognised option", {
	expect_warning(
		make_projection(data.frame(1), metric = "Mnahattan"),
		regexp = "`metric` option \"Mnahattan\" unrecognised; defaulting to \"Manhattan\"."
	)
})


test_that("`metric` argument not a character", {
	expect_warning(
		make_projection(data.frame(1), metric = 0),
		regexp = "`metric` must be a character string; defaulting to \"Manhattan\"."
	)
})


test_that("`mincompare` behaves as expected", {
	
	fn <- function(n){make_projection(data.frame(c(1, 2), c(3, 4), c(5, 6), c(NA, 7), c(8, NA), c(NA, NA)), mincompare = n)}

	fn <- function(n){
		make_projection(
			data.frame(
				item1 = c(1, 2),
				item2 = c(3, 4),
				item3 = c(5, 6),
				item4 = c(NA, 7),
				item5 = c(8, NA),
				item6 = c(NA, NA)
			),
			mincompare = n
		)
	}

	expect_equal(nrow(fn(3)), 1)
	expect_equal(nrow(fn(4)), 0)
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
