eps <- 1e-6


test_that("unused arguments", {
	expect_warning(
		make_threshold_profile(data.frame(1), argname1 = 1, argname2 = 1),
		regexp = "Unused arguments in ...: argname1, argname2"
	)
})


test_that("`layer` argument is correctly supplied.", {
	expect_error(
		make_threshold_profile(data.frame(1), layer = TRUE),
		regexp = "`layer` must be a character string."
	)

	expect_error(
		make_threshold_profile(data.frame(1), layer = character()),
		regexp = "`layer` must be of length 1."
	)

	expect_error(
		make_threshold_profile(data.frame(1), layer = c("agent", "symbolic")),
		regexp = "`layer` must be of length 1."
	)

	expect_error(
		make_threshold_profile(data.frame(1), layer = as.character(NA)),
		regexp = "`layer` cannot be NA."
	)

	expect_error(
		make_threshold_profile(data.frame(1), layer = "agnet"),
		regexp = "`layer` option \"agnet\" unrecognised."
	)
})


test_that("`comparisons` argument is correctly supplied.", {
	expect_error(
		make_threshold_profile(data.frame(1), comparisons = TRUE),
		regexp = "`comparisons` must be an integer."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), comparisons = integer()),
		regexp = "`comparisons` must be of length 1."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), comparisons = c(3, 4)),
		regexp = "`comparisons` must be of length 1."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), comparisons = as.integer(NA)),
		regexp = "`comparisons` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), comparisons = NaN),
		regexp = "`comparisons` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), comparisons = Inf),
		regexp = "`comparisons` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), comparisons = 1.2),
		regexp = "`comparisons` must be an integer."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), comparisons = 0),
		regexp = "`comparisons` must be between 1 and ncol(data) for agent layer.",
		fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), comparisons = 2),
		regexp = "`comparisons` must be between 1 and ncol(data) for agent layer.",
		fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), layer = "symbolic", comparisons = 0),
		regexp = "`comparisons` must be between 1 and nrow(data) for symbolic layer.",
		fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), layer = "symbolic", comparisons = 3),
		regexp = "`comparisons` must be between 1 and nrow(data) for symbolic layer.",
		fixed = T
	)	
})


test_that("`metric` argument is correctly supplied.", {
	expect_error(
		make_threshold_profile(data.frame(1), metric = TRUE),
		regexp = "`metric` must be a character string."
	)

	expect_error(
		make_threshold_profile(data.frame(1), metric = character()),
		regexp = "`metric` must be of length 1."
	)

	expect_error(
		make_threshold_profile(data.frame(1), metric = c("Manhattan", "Euclidean")),
		regexp = "`metric` must be of length 1."
	)

	expect_error(
		make_threshold_profile(data.frame(1), metric = as.character(NA)),
		regexp = "`metric` cannot be NA."
	)

	expect_error(
		make_threshold_profile(data.frame(1), metric = "Mnahattan"),
		regexp = "`metric` option \"Mnahattan\" unrecognised; expecting \"Manhattan\" or \"Euclidean\"."
	)
})


test_that("`count` argument is correctly supplied.", {
	expect_error(
		make_threshold_profile(data.frame(1), count = TRUE),
		regexp = "`count` must be an integer."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = integer()),
		regexp = "`count` must be of length 1."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = c(3, 4)),
		regexp = "`count` must be of length 1."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = as.integer(NA)),
		regexp = "`count` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = NaN),
		regexp = "`count` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = Inf),
		regexp = "`count` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = 1.2),
		regexp = "`count` must be an integer."
	)	

	expect_warning(
		make_threshold_profile(data.frame(1), count = 2),
		regexp = "Setting `count` to 3."
	)	
})
