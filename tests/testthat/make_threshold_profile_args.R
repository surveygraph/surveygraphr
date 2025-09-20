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
		regexp = "`layer` argument must be a character string."
	)

	expect_error(
		make_threshold_profile(data.frame(1), layer = character()),
		regexp = "`layer` argument must be of length 1."
	)

	expect_error(
		make_threshold_profile(data.frame(1), layer = c("agent", "symbolic")),
		regexp = "`layer` argument must be of length 1."
	)

	expect_error(
		make_threshold_profile(data.frame(1), layer = as.character(NA)),
		regexp = "`layer` argument cannot be NA."
	)

	expect_error(
		make_threshold_profile(data.frame(1), layer = "agnet"),
		regexp = "`layer` option \"agnet\" unrecognised."
	)
})


test_that("`mincompare` argument is correctly supplied.", {
	expect_error(
		make_threshold_profile(data.frame(1), mincompare = TRUE),
		regexp = "`mincompare` argument must be an integer."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), mincompare = integer()),
		regexp = "`mincompare` argument must be of length 1."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), mincompare = c(3, 4)),
		regexp = "`mincompare` argument must be of length 1."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), mincompare = as.integer(NA)),
		regexp = "`mincompare` argument cannot be NA."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), mincompare = 1.2),
		regexp = "`mincompare` argument must be an integer."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), mincompare = 0),
		regexp = "`mincompare` must be between 1 and ncol(data) for agent layer.",
		fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), mincompare = 2),
		regexp = "`mincompare` must be between 1 and ncol(data) for agent layer.",
		fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), layer = "symbolic", mincompare = 0),
		regexp = "`mincompare` must be between 1 and nrow(data) for symbolic layer.",
		fixed = T
	)	

	expect_error(
		make_threshold_profile(data.frame(1), layer = "symbolic", mincompare = 3),
		regexp = "`mincompare` must be between 1 and nrow(data) for symbolic layer.",
		fixed = T
	)	
})


test_that("`metric` argument is correctly supplied.", {
	expect_error(
		make_threshold_profile(data.frame(1), metric = TRUE),
		regexp = "`metric` argument must be a character string."
	)

	expect_error(
		make_threshold_profile(data.frame(1), metric = character()),
		regexp = "`metric` argument must be of length 1."
	)

	expect_error(
		make_threshold_profile(data.frame(1), metric = c("Manhattan", "Euclidean")),
		regexp = "`metric` argument must be of length 1."
	)

	expect_error(
		make_threshold_profile(data.frame(1), metric = as.character(NA)),
		regexp = "`metric` argument cannot be NA."
	)

	expect_error(
		make_threshold_profile(data.frame(1), metric = "Mnahattan"),
		regexp = "`metric` option \"Mnahattan\" unrecognised; expecting \"Manhattan\" or \"Euclidean\"."
	)
})


test_that("`count` argument is correctly supplied.", {
	expect_error(
		make_threshold_profile(data.frame(1), count = TRUE),
		regexp = "`count` argument must be an integer."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = integer()),
		regexp = "`count` argument must be of length 1."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = c(3, 4)),
		regexp = "`count` argument must be of length 1."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = as.integer(NA)),
		regexp = "`count` argument cannot be NA."
	)	

	expect_error(
		make_threshold_profile(data.frame(1), count = 1.2),
		regexp = "`count` argument must be an integer."
	)	

	expect_warning(
		make_threshold_profile(data.frame(1), count = 2),
		regexp = "Setting `count` to 3."
	)	
})
