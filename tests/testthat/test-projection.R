# Tests for make_projection(). Note that arguments likert and dummycode are
# tested in test-data-preprocessing.R.

# epsilon
eps = 0.001

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


# TODO: warning should point to documentation for unclear ideas like methodval interpretation
test_that("`methodval` not in range, lcc", {
	expect_warning(
		make_projection(data.frame(1), method = "lcc", methodval = 2),
		regexp = "`methodval` must be between 0 and 1 inclusive for `lcc` method, setting to 1."
	)

	expect_warning(
		make_projection(data.frame(1), method = "lcc", methodval = -1),
		regexp = "`methodval` must be between 0 and 1 inclusive for `lcc` method, setting to 0."
	)
})


test_that("`methodval` not in range, avgdegree", {
	expect_warning(
		make_projection(data.frame(1), method = "avgdegree", methodval = 2),
		regexp = "`methodval` must be between 0 and 1 inclusive for `avgdegree` method, setting to 1."
	)

	expect_warning(
		make_projection(data.frame(1), method = "avgdegree", methodval = -1),
		regexp = "`methodval` must be between 0 and 1 inclusive for `avgdegree` method, setting to 0."
	)
})


test_that("`methodval` not in range, similarity", {
	expect_warning(
		make_projection(data.frame(1), method = "similarity", methodval = 2),
		regexp = "Note that for `similarity` method, all values of `methodval` greater than 1 are equivalent."
	)

	expect_warning(
		make_projection(data.frame(1), method = "similarity", methodval = -1),
		regexp = "Note that for `similarity` method all values of `methodval` less than 0 are equivalent."
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
		make_projection(data.frame(1:2), mincompare = 0),
		regexp = "Expecting `mincompare` between 1 and ncol(data) for agent layer; defaulting to ceiling(ncol(data) / 2).",
		fixed = T
	)	

	expect_warning(
		make_projection(data.frame(1:2), mincompare = 2),
		regexp = "Expecting `mincompare` between 1 and ncol(data) for agent layer; defaulting to ceiling(ncol(data) / 2).",
		fixed = T
	)	
})


test_that("`mincompare` out of range, symbolic layer", {
	expect_warning(
		make_projection(data.frame(1:2), layer = "symbolic", mincompare = 0),
		regexp = "Expecting `mincompare` between 1 and nrow(data) for symbolic layer; defaulting to ceiling(nrow(data) / 2).",
		fixed = T
	)	

	expect_warning(
		make_projection(data.frame(1:2), layer = "symbolic", mincompare = 3),
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


# ====================================================================================================
# Checking behaviour of lcc method on 1-, 2-, 3- and 4-cliques
# ====================================================================================================
test_that("`lcc` method on a 1-clique, agent layer", {
	expect_equal(
		make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "l", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "l", methodval = 1),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})

test_that("`lcc` method on a 1-clique, symbolic layer", {
	expect_equal(
		make_projection(data.frame(a = 1:3), layer = "s", method = "l", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 1:3), layer = "s", method = "l", methodval = 1),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})


test_that("`lcc` method on a 2-clique, agent layer", {
	expect_equal(
		make_projection(data.frame(a = c(0, 1)), method = "l", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 1)), method = "l", methodval = eps),
		data.frame(u = 1, v = 2, weight = 0)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 1)), method = "l", methodval = 1 - eps),
		data.frame(u = 1, v = 2, weight = 0)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 1)), method = "l", methodval = 1),
		data.frame(u = 1, v = 2, weight = 0)
	)
})


test_that("`lcc` method on a 2-clique, symbolic layer", {
	expect_equal(
		make_projection(data.frame(a = 0, b = 1), layer = "s", method = "l", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 1), layer = "s", method = "l", methodval = eps),
		data.frame(u = 1, v = 2, weight = 0)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 1), layer = "s", method = "l", methodval = 1 - eps),
		data.frame(u = 1, v = 2, weight = 0)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 1), layer = "s", method = "l", methodval = 1),
		data.frame(u = 1, v = 2, weight = 0)
	)
})


test_that("`lcc` method on a 3-clique, agent layer.", {
	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "l", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "l", methodval = eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "l", methodval = 1 / 2 - eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "l", methodval = 1 / 2),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "l", methodval = 1 / 2 + eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "l", methodval = 1 - eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "l", methodval = 1),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)
})


test_that("`lcc` method on a 3-clique, symbolic layer.", {
	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "l", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "l", methodval = eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "l", methodval = 1 / 2 - eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "l", methodval = 1 / 2),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "l", methodval = 1 / 2 + eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "l", methodval = 1 - eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "l", methodval = 1),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)
})


test_that("`lcc` method on a 4-clique, agent layer.", {
	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 1 / 3 - eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 1 / 3),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 1 / 3 + eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 2 / 3 - eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 2 / 3),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 2 / 3 + eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 1 - eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = 1),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)
})


test_that("`lcc` method on a 4-clique, symbolic layer.", {
	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 1 / 3 - eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 1 / 3),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 1 / 3 + eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 2 / 3 - eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 2 / 3),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 2 / 3 + eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 1 - eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = 1),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)
})


# ====================================================================================================
# Checking behaviour of avgdegree method on 1-, 2-, 3- and 4-cliques
# ====================================================================================================
test_that("`avgdegree` method on a 1-clique, agent layer", {
	expect_equal(
		make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "a", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "a", methodval = 1),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})

test_that("`avgdegree` method on a 1-clique, symbolic layer", {
	expect_equal(
		make_projection(data.frame(a = 1:3), layer = "s", method = "a", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 1:3), layer = "s", method = "a", methodval = 1),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})


test_that("`avgdegree` method on a 2-clique, agent layer", {
	expect_equal(
		make_projection(data.frame(a = c(0, 1)), method = "a", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 1)), method = "a", methodval = eps),
		data.frame(u = 1, v = 2, weight = 0)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 1)), method = "a", methodval = 1 - eps),
		data.frame(u = 1, v = 2, weight = 0)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 1)), method = "a", methodval = 1),
		data.frame(u = 1, v = 2, weight = 0)
	)
})


test_that("`avgdegree` method on a 2-clique, symbolic layer", {
	expect_equal(
		make_projection(data.frame(a = 0, b = 1), layer = "s", method = "a", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 1), layer = "s", method = "a", methodval = eps),
		data.frame(u = 1, v = 2, weight = 0)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 1), layer = "s", method = "a", methodval = 1 - eps),
		data.frame(u = 1, v = 2, weight = 0)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 1), layer = "s", method = "a", methodval = 1),
		data.frame(u = 1, v = 2, weight = 0)
	)
})


test_that("`avgdegree` method on a 3-clique, agent layer.", {
	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 1 / 3 - eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 1 / 3),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 1 / 3 + eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 2 / 3 - eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 2 / 3 + eps),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 1 - eps),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 1),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)
})


test_that("`avgdegree` method on a 3-clique, symbolic layer.", {
	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 1 / 3 - eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 1 / 3),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 1 / 3 + eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 2 / 3 - eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 2 / 3),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 2 / 3 + eps),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 1 - eps),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = 1),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)
})


test_that("`avgdegree` method on a 4-clique, agent layer.", {
	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 1 / 6 - eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 1 / 6),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 1 / 6 + eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 2 / 6 - eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 2 / 6),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 2 / 6 + eps),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 3 / 6 - eps),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 3 / 6),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 3 / 6 + eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 4 / 6 - eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 4 / 6),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 4 / 6 + eps),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 5 / 6 - eps),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 5 / 6),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 5 / 6 + eps),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 1 - eps),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = 1),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)
})


test_that("`avgdegree` method on a 4-clique, symbolic layer.", {
	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 1 / 6 - eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 1 / 6),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 1 / 6 + eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 2 / 6 - eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 2 / 6),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 2 / 6 + eps),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 3 / 6 - eps),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 3 / 6),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 3 / 6 + eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 4 / 6 - eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 4 / 6),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 4 / 6 + eps),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 5 / 6 - eps),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 5 / 6),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 5 / 6 + eps),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 1 - eps),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = 1),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)
})


# ====================================================================================================
# Checking behaviour of similarity method on 1-, 2-, 3- and 4-cliques
# ====================================================================================================
test_that("`similarity` method on a 1-clique, agent layer", {
	expect_equal(
		make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "s", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "s", methodval = 1),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})

test_that("`similarity` method on a 1-clique, symbolic layer", {
	expect_equal(
		make_projection(data.frame(a = 1:3), layer = "s", method = "s", methodval = 0),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)

	expect_equal(
		make_projection(data.frame(a = 1:3), layer = "s", method = "s", methodval = 1),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})


test_that("`similarity` method on a 2-clique, agent layer", {
	expect_equal(
		make_projection(data.frame(a = c(1, 1)), method = "s", methodval = 0),
		data.frame(u = 1, v = 2, weight = 1)
	)

	expect_equal(
		make_projection(data.frame(a = c(1, 1)), method = "s", methodval = eps),
		data.frame(u = 1, v = 2, weight = 1)
	)

	expect_equal(
		make_projection(data.frame(a = c(1, 1)), method = "s", methodval = 1 - eps),
		data.frame(u = 1, v = 2, weight = 1)
	)

	expect_equal(
		make_projection(data.frame(a = c(1, 1)), method = "s", methodval = 1),
		data.frame(u = 1, v = 2, weight = 1)
	)

	expect_equal(
		make_projection(data.frame(a = c(1, 1)), method = "s", methodval = 1 + eps),
		data.frame(u = 1, v = 2, weight = 1)
	)
})
