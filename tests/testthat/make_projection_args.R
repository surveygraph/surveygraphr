# TODO: errors and warnings should point to relevant documentation

# epsilon value for numerical thresholds
eps <- 1e-6

test_that("unused arguments", {
	expect_warning(
		make_projection(data.frame(1), argname1 = 1, argname2 = 1),
		regexp = "Unused arguments in ...: argname1, argname2"
	)
})


test_that("`layer` argument not a character, or is a character and NA", {
	expect_error(
		make_projection(data.frame(1), layer = 2),
		regexp = "`layer` argument must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), layer = as.character(NA)),
		regexp = "`layer` argument cannot be NA."
	)
})


test_that("`layer` argument has unrecognised option", {
	expect_error(
		make_projection(data.frame(1), layer = "agnet"),
		regexp = "`layer` option \"agnet\" unrecognised."
	)
})


test_that("`method` argument not a character, or is a character and NA", {
	expect_error(
		make_projection(data.frame(1), method = 0),
		regexp = "`method` argument must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), method = as.character(NA)),
		regexp = "`method` argument cannot be NA."
	)
})


test_that("`method` argument has unrecognised option", {
	expect_error(
		make_projection(data.frame(1), method = "clc"),
		regexp = "`method` option \"clc\" unrecognised."
	)
})


test_that("`methodval` argument not numeric, or is numeric and NA", {
	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = TRUE),
		regexp = "`methodval` argument must be a numerical value."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = as.numeric(NA)),
		regexp = "`methodval` argument cannot be NA."
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


test_that("`mincompare` not integer, or integer and NA", {
	expect_error(
		make_projection(data.frame(1:2), mincompare = "hello"),
		regexp = "`mincompare` argument must be an integer."
	)	

	expect_error(
		make_projection(data.frame(1:2), mincompare = as.integer(NA)),
		regexp = "`mincompare` argument cannot be NA."
	)	

	expect_error(
		make_projection(data.frame(1:2), mincompare = 1.2),
		regexp = "`mincompare` argument must be an integer."
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


test_that("`metric` argument not a character, or is a character but NA", {
	expect_error(
		make_projection(data.frame(1), metric = 0),
		regexp = "`metric` argument must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), metric = as.character(NA)),
		regexp = "`metric` argument cannot be NA."
	)
})


test_that("`metric` argument has unrecognised option", {
	expect_error(
		make_projection(data.frame(1), metric = "Mnahattan"),
		regexp = "`metric` option \"Mnahattan\" unrecognised; expecting \"Manhattan\" or \"Euclidean\"."
	)
})


test_that("`bootreps` and `bootval` arguments set together", {
	expect_error(
		make_projection(data.frame(1), bootreps = 1),
		regexp = "`bootval` argument must be set if `bootreps` is set."
	)

	expect_error(
		make_projection(data.frame(1), bootval = 0),
		regexp = "`bootreps` argument must be set if `bootval` is set."
	)

	expect_error(
		make_projection(data.frame(1), bootseed = 1),
		regexp = "`bootrep` and `bootval` arguments must be set if `bootseed` is set."
	)
})


test_that("`bootreps` argument", {
	expect_error(
		make_projection(data.frame(1), bootreps = TRUE, bootval = 0),
		regexp = "`bootreps` argument must be a positive integer."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = as.integer(NA), bootval = 0),
		regexp = "`bootreps` argument cannot be NA."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = -1, bootval = 0),
		regexp = "`bootreps` argument must be a positive integer."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 0, bootval = 0),
		regexp = "`bootreps` argument must be a positive integer."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1.1, bootval = 0),
		regexp = "`bootreps` argument must be a positive integer."
	)
})


test_that("`bootval` argument", {
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = TRUE),
		regexp = "`bootval` argument must be between 0 and 1, inclusive."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = as.numeric(NA)),
		regexp = "`bootval` argument cannot be NA."
	)
  
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = -eps),
		regexp = "`bootval` argument must be between 0 and 1, inclusive."
	)
  
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = 1 + eps),
		regexp = "`bootval` argument must be between 0 and 1, inclusive."
	)
})


test_that("`bootseed` argument", {
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = 0.5, bootseed = TRUE),
		regexp = "`bootseed` argument must be 0 or 1."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = 0.5, bootseed = as.integer(NA)),
		regexp = "`bootseed` argument cannot be NA."
	)
  
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = 0.5, bootseed = 2),
		regexp = "`bootseed` argument must be 0 or 1."
	)
})
