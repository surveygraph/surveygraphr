# TODO: errors and warnings should point to relevant documentation
eps <- 1e-6


test_that("Unused arguments.", {
	expect_warning(
		make_projection(data.frame(1), argname1 = 1, argname2 = 1),
		regexp = "Unused arguments in ...: argname1, argname2"
	)
})


# Test for deprecation messages for old argument names in make_projection().
test_that("Deprecated argument names for make_projection()", {
	expect_warning(
    make_projection(data.frame(1), threshold_method = "raw_similarity"),
		regexp = "`threshold_method` is deprecated and will be removed in future versions; use `method`."
	)

	expect_warning(
    make_projection(data.frame(1), method_value = 0.5),
		regexp = "`method_value` is deprecated and will be removed future versions; use `methodval`."
	)

	expect_warning(
    make_projection(data.frame(1), similarity_metric = "Manhattan"),
		regexp = "`similarity_metric` is deprecated and will be removed in future versions; use `metric`."
	)

  # TODO: add this back in
	#expect_warning(
  #  make_projection(data.frame(1), centre = 1),
	#	regexp = "`likert` is deprecated and will be removed future versions; use `range`."
	#)
})


test_that("`layer` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), layer = TRUE),
		regexp = "`layer` must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), layer = character()),
		regexp = "`layer` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), layer = c("agent", "symbolic")),
		regexp = "`layer` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), layer = as.character(NA)),
		regexp = "`layer` cannot be NA."
	)

	expect_error(
		make_projection(data.frame(1), layer = "agnet"),
		regexp = "`layer` option \"agnet\" unrecognised."
	)
})


test_that("`method` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), method = TRUE),
		regexp = "`method` must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), method = character()),
		regexp = "`method` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), method = c("lcc", "avgdegree", "similarity")),
		regexp = "`method` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), method = as.character(NA)),
		regexp = "`method` cannot be NA."
	)

	expect_error(
		make_projection(data.frame(1), method = "clc"),
		regexp = "`method` option \"clc\" unrecognised."
	)
})


test_that("`methodval` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = TRUE),
		regexp = "`methodval` must be a numerical value."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = numeric()),
		regexp = "`methodval` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = c(0, 1)),
		regexp = "`methodval` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = as.numeric(NA)),
		regexp = "`methodval` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = NaN),
		regexp = "`methodval` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), method = "lcc", methodval = Inf),
		regexp = "`methodval` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
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
		regexp = "`comparisons` must be an integer."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = integer()),
		regexp = "`comparisons` must be of length 1."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = c(1, 2)),
		regexp = "`comparisons` must be of length 1."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = as.integer(NA)),
		regexp = "`comparisons` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = NaN),
		regexp = "`comparisons` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = Inf),
		regexp = "`comparisons` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = 1.2),
		regexp = "`comparisons` must be an integer."
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = 0),
		regexp = "`comparisons` must be between 1 and ncol(data) for agent layer.", fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), comparisons = 2),
		regexp = "`comparisons` must be between 1 and ncol(data) for agent layer.", fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), layer = "symbolic", comparisons = 0),
		regexp = "`comparisons` must be between 1 and nrow(data) for symbolic layer.", fixed = T
	)	

	expect_error(
		make_projection(data.frame(1:2), layer = "symbolic", comparisons = 3),
		regexp = "`comparisons` must be between 1 and nrow(data) for symbolic layer.", fixed = T
	)	
})


test_that("`metric` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), metric = TRUE),
		regexp = "`metric` must be a character string."
	)

	expect_error(
		make_projection(data.frame(1), metric = character()),
		regexp = "`metric` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), metric = c("Manhattan", "Euclidean")),
		regexp = "`metric` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), metric = as.character(NA)),
		regexp = "`metric` cannot be NA."
	)

	expect_error(
		make_projection(data.frame(1), metric = "Mnahattan"),
		regexp = "`metric` option \"Mnahattan\" unrecognised; expecting \"Manhattan\" or \"Euclidean\"."
	)
})


test_that("Bootstrapping arguments are set together.", {
	expect_error(
		make_projection(data.frame(1), bootreps = 1),
		regexp = "`bootval` must be set if `bootreps` is set."
	)

	expect_error(
		make_projection(data.frame(1), bootval = 0),
		regexp = "`bootreps` must be set if `bootval` is set."
	)

	expect_error(
		make_projection(data.frame(1), bootseed = 1),
		regexp = "`bootval` must be set if `bootseed` is set."
	)

	expect_error(
		make_projection(data.frame(1), bootseed = 1, bootreps = 3),
		regexp = "`bootval` must be set if `bootseed` is set."
	)

	expect_error(
		make_projection(data.frame(1), bootseed = 1, bootval = 0, bootreps = 3),
		regexp = "`bootreps` must be NULL if `bootseed` is set."
	)
})


test_that("`bootreps` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), bootreps = TRUE, bootval = 0),
		regexp = "`bootreps` must be a positive integer."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = numeric(), bootval = 0),
		regexp = "`bootreps` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = c(1, 2), bootval = 0),
		regexp = "`bootreps` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = as.integer(NA), bootval = 0),
		regexp = "`bootreps` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), bootreps = NaN, bootval = 0),
		regexp = "`bootreps` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), bootreps = Inf, bootval = 0),
		regexp = "`bootreps` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), bootreps = -1, bootval = 0),
		regexp = "`bootreps` must be a positive integer."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 0, bootval = 0),
		regexp = "`bootreps` must be a positive integer."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1.1, bootval = 0),
		regexp = "`bootreps` must be a positive integer."
	)
})


test_that("`bootval` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = TRUE),
		regexp = "`bootval` must be between 0 and 1, inclusive."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = numeric()),
		regexp = "`bootval` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = c(0, 1)),
		regexp = "`bootval` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = as.numeric(NA)),
		regexp = "`bootval` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = NaN),
		regexp = "`bootval` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = Inf),
		regexp = "`bootval` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)
  
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = -eps),
		regexp = "`bootval` must be between 0 and 1, inclusive."
	)
  
	expect_error(
		make_projection(data.frame(1), bootreps = 1, bootval = 1 + eps),
		regexp = "`bootval` must be between 0 and 1, inclusive."
	)
})


test_that("`bootseed` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = TRUE),
		regexp = "`bootseed` must be an integer vector."
	)

	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = c(1, NA)),
		regexp = "`bootseed` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = c(1, NaN)),
		regexp = "`bootseed` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = c(1, Inf)),
		regexp = "`bootseed` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)
  
	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = integer()),
		regexp = "`bootseed`, if provided, must contain at least one integer."
	)
  
	expect_error(
		make_projection(data.frame(1), bootval = 0.5, bootseed = c(0, 1, 2.1)),
		regexp = "`bootseed` must only contain integers."
	)
})


test_that("`centre` argument is correctly supplied.", {
	expect_error(
		make_projection(data.frame(1), centre = logical()),
		regexp = "`centre` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), centre = c(TRUE, FALSE)),
		regexp = "`centre` must be of length 1."
	)

	expect_error(
		make_projection(data.frame(1), centre = as.logical(NA)),
		regexp = "`centre` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), centre = NaN),
		regexp = "`centre` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), centre = Inf),
		regexp = "`centre` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_error(
		make_projection(data.frame(1), centre = "hello"),
		regexp = "`centre` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)

	expect_warning(
		make_projection(data.frame(1), centre = 0),
		regexp = "`centre` will be coerced to logical; setting 0 to FALSE."
	)

	expect_warning(
		make_projection(data.frame(1), centre = -1),
		regexp = "`centre` will be coerced to logical; setting -1 to TRUE."
	)

	expect_warning(
		make_projection(data.frame(1), centre = 1),
		regexp = "`centre` will be coerced to logical; setting 1 to TRUE."
	)

	expect_warning(
		make_projection(data.frame(1), centre = 2),
		regexp = "`centre` will be coerced to logical; setting 2 to TRUE."
	)
})
