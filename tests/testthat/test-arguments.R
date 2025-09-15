# TODO: errors and warnings should point to relevant documentation

# epsilon value for numerical thresholds
eps <- 1e-6

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
	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = 1 + eps),
		regexp = "Expecting `methodval` between 0 and 1 inclusive for `lcc` method."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = -eps),
		regexp = "Expecting `methodval` between 0 and 1 inclusive for `lcc` method."
	)
})


test_that("`methodval` not in range, avgdegree", {
	expect_error(
		make_projection(data.frame(1), method = "avgdegree", methodval = 1 + eps),
		regexp = "Expecting `methodval` between 0 and 1 inclusive for `avgdegree` method."
	)

	expect_error(
		make_projection(data.frame(1), method = "avgdegree", methodval = -eps),
		regexp = "Expecting `methodval` between 0 and 1 inclusive for `avgdegree` method."
	)
})


test_that("`mincompare` not integer", {
	expect_error(
		make_projection(data.frame(1:2), mincompare = "hello"),
		regexp = "Expecting an integer for `mincompare`."
	)	

	expect_error(
		make_projection(data.frame(1:2), mincompare = 1.2),
		regexp = "Expecting an integer for `mincompare`."
	)	
})


test_that("`mincompare` out of range", {
	expect_error(
		make_projection(data.frame(1:2), mincompare = 0),
		regexp = "Expecting `mincompare` between 1 and ncol(data) for agent layer.",
		fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), mincompare = 2),
		regexp = "Expecting `mincompare` between 1 and ncol(data) for agent layer.",
		fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), layer = "symbolic", mincompare = 0),
		regexp = "Expecting `mincompare` between 1 and nrow(data) for symbolic layer.",
		fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), layer = "symbolic", mincompare = 3),
		regexp = "Expecting `mincompare` between 1 and nrow(data) for symbolic layer.",
		fixed = T
	)	
})


test_that("`metric` argument not a character", {
	expect_error(
		make_projection(data.frame(1), metric = 0),
		regexp = "Expecting a character string for `metric`."
	)
})


test_that("`metric` argument has unrecognised option", {
	expect_error(
		make_projection(data.frame(1), metric = "Mnahattan"),
		regexp = "`metric` option \"Mnahattan\" unrecognised; expecting \"Manhattan\" or \"Euclidean\"."
	)
})
