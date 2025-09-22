# TODO: errors and warnings should point to relevant documentation

# epsilon value for numerical thresholds
eps <- 1e-6

test_that("unused arguments", {
	expect_warning(
		make_projection(data.frame(1), argname1 = 1, argname2 = 1),
		regexp = "Unused arguments in ...: argname1, argname2"
	)
})


test_that("`layer` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), layer = TRUE),
		regexp = "`layer` argument must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), layer = character()),
		regexp = "`layer` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), layer = c("agent", "symbolic")),
		regexp = "`layer` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), layer = as.character(NA)),
		regexp = "`layer` argument cannot be NA."
	)

	expect_error(
		make_projection(data.frame(1), layer = "agnet"),
		regexp = "`layer` option \"agnet\" unrecognised."
	)
})


test_that("`method` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), method = TRUE),
		regexp = "`method` argument must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), method = character()),
		regexp = "`method` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), method = c("lcc", "avgdegree", "similarity")),
		regexp = "`method` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), method = as.character(NA)),
		regexp = "`method` argument cannot be NA."
	)

	expect_error(
		make_projection(data.frame(1), method = "clc"),
		regexp = "`method` option \"clc\" unrecognised."
	)
})


test_that("`methodval` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = TRUE),
		regexp = "`methodval` argument must be a numerical value."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = numeric()),
		regexp = "`methodval` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = c(0, 1)),
		regexp = "`methodval` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = as.numeric(NA)),
		regexp = "`methodval` argument cannot be NA."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = 1 + eps),
		regexp = "`methodval` must be between 0 and 1 inclusive for `lcc` method."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = -eps),
		regexp = "`methodval` must be between 0 and 1 inclusive for `lcc` method."
	)

	expect_error(
		make_projection(data.frame(1), method = "avgdegree", methodval = 1 + eps),
		regexp = "`methodval` must be between 0 and 1 inclusive for `avgdegree` method."
	)

	expect_error(
		make_projection(data.frame(1), method = "avgdegree", methodval = -eps),
		regexp = "`methodval` must be between 0 and 1 inclusive for `avgdegree` method."
	)
})


test_that("`comparisons` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1:2), comparisons = TRUE),
		regexp = "`comparisons` argument must be an integer."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = integer()),
		regexp = "`comparisons` argument must be of length 1."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = c(1, 2)),
		regexp = "`comparisons` argument must be of length 1."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = as.integer(NA)),
		regexp = "`comparisons` argument cannot be NA."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = 1.2),
		regexp = "`comparisons` argument must be an integer."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = 0),
		regexp = "`comparisons` must be between 1 and ncol(data) for agent layer.",
		fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = 2),
		regexp = "`comparisons` must be between 1 and ncol(data) for agent layer.",
		fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), layer = "symbolic", comparisons = 0),
		regexp = "`comparisons` must be between 1 and nrow(data) for symbolic layer.",
		fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), layer = "symbolic", comparisons = 3),
		regexp = "`comparisons` must be between 1 and nrow(data) for symbolic layer.",
		fixed = T
	)	
})


test_that("`metric` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), metric = TRUE),
		regexp = "`metric` argument must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), metric = character()),
		regexp = "`metric` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), metric = c("Manhattan", "Euclidean")),
		regexp = "`metric` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), metric = as.character(NA)),
		regexp = "`metric` argument cannot be NA."
	)

	expect_error(
		make_projection(data.frame(1), metric = "Mnahattan"),
		regexp = "`metric` option \"Mnahattan\" unrecognised; expecting \"Manhattan\" or \"Euclidean\"."
	)
})


test_that("Bootstrapping arguments are set together.", {
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
		regexp = "`bootval` argument must be set if `bootseed` is set."
	)

	expect_error(
		make_projection(data.frame(1), bootseed = 1, bootreps = 3),
		regexp = "`bootval` argument must be set if `bootseed` is set."
	)

	expect_error(
		make_projection(data.frame(1), bootseed = 1, bootval = 0, bootreps = 3),
		regexp = "`bootreps` argument must be NULL if `bootseed` is set."
	)
})


test_that("`bootreps` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), bootreps = TRUE, bootval = 0),
		regexp = "`bootreps` argument must be a positive integer."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = numeric(), bootval = 0),
		regexp = "`bootreps` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = c(1, 2), bootval = 0),
		regexp = "`bootreps` argument must be of length 1."
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


test_that("`bootval` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = TRUE),
		regexp = "`bootval` argument must be between 0 and 1, inclusive."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = numeric()),
		regexp = "`bootval` argument must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = c(0, 1)),
		regexp = "`bootval` argument must be of length 1."
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


test_that("`bootseed` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = TRUE),
		regexp = "`bootseed` argument must be an integer vector."
	)

	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = c(0, 1, NA, 3)),
		regexp = "`bootseed` argument must not contain NAs."
	)
  
	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = integer()),
		regexp = "`bootseed` argument, if provided, must contain at least one integer."
	)
  
	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = c(0, 1, 2.1)),
		regexp = "`bootseed` argument must only contain integers."
	)
})
