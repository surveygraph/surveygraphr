# Main tests for make_projection(). Note that arguments likert and dummycode are
# tested in test-data-preprocessing.R.

# epsilon for testing small changes to parameter arguments. Note that there is
# an internal tolerance of 1e-8 when comparing similarity weight to the threshold,
# see graph_build.cc
eps = 1e-6

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
		make_projection(data.frame(a = c(1, 1)), method = "s", methodval = 1 - eps),
		data.frame(u = 1, v = 2, weight = 1)
	)

	expect_equal(
		make_projection(data.frame(a = c(1, 1)), method = "s", methodval = 1),
		data.frame(u = 1, v = 2, weight = 1)
	)

	expect_equal(
		make_projection(data.frame(a = c(1, 1)), method = "s", methodval = 1 + eps),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})


test_that("`similarity` method on a 2-clique, symbolic layer", {
	expect_equal(
		make_projection(data.frame(a = 1, b = 1), layer = "s", method = "s", methodval = 1 - eps),
		data.frame(u = 1, v = 2, weight = 1)
	)

	expect_equal(
		make_projection(data.frame(a = 1, b = 1), layer = "s", method = "s", methodval = 1),
		data.frame(u = 1, v = 2, weight = 1)
	)

	expect_equal(
		make_projection(data.frame(a = 1, b = 1), layer = "s", method = "s", methodval = 1 + eps),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})


test_that("`similarity` method on a 3-clique, agent layer.", {
	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = -eps),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = 0),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = 0.4 - eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = 0.4),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = 0.4 + eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = 0.6 - eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = 0.6),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = 0.6 + eps),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})


test_that("`similarity` method on a 3-clique, symbolic layer.", {
	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = -eps),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = 0),
		data.frame(u = c(1, 1, 2), v = c(2, 3, 3), weight = c(0.6, 0, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = 0.4 - eps),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = 0.4),
		data.frame(u = c(1, 2), v = c(2, 3), weight = c(0.6, 0.4))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = 0.4 + eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = 0.6 - eps),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = 0.6),
		data.frame(u = 1, v = 2, weight = 0.6)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = 0.6 + eps),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})


test_that("`similarity` method on a 4-clique, agent layer.", {
	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = -eps),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = eps),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.1 - eps),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.1),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.1 + eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.4 - eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.4),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.4 + eps),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.6 - eps),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.6),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.6 + eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.7 - eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.7),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.7 + eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.9 - eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.9),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = 0.9 + eps),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})

test_that("`similarity` method on a 4-clique, agent layer.", {
	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = -eps),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0),
		data.frame(u = c(1, 1, 1, 2, 2, 3), v = c(2, 3, 4, 3, 4, 4), weight = c(0.4, 0.1, 0, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = eps),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.1 - eps),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.1),
		data.frame(u = c(1, 1, 2, 2, 3), v = c(2, 3, 3, 4, 4), weight = c(0.4, 0.1, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.1 + eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.4 - eps),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.4),
		data.frame(u = c(1, 2, 2, 3), v = c(2, 3, 4, 4), weight = c(0.4, 0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.4 + eps),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.6 - eps),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.6),
		data.frame(u = c(2, 2, 3), v = c(3, 4, 4), weight = c(0.7, 0.6, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.6 + eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.7 - eps),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.7),
		data.frame(u = c(2, 3), v = c(3, 4), weight = c(0.7, 0.9))
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.7 + eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.9 - eps),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.9),
		data.frame(u = 3, v = 4, weight = 0.9)
	)

	expect_equal(
		make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = 0.9 + eps),
		data.frame(u = numeric(), v = numeric(), weight = numeric())
	)
})
