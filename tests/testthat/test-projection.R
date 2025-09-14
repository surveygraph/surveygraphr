# Main tests for make_projection(). Note that arguments likert and dummycode are
# tested in test-data-preprocessing.R.

# epsilon for testing small changes to parameter arguments. Note that there is
# an internal tolerance of 1e-8 when comparing similarity weight to the threshold,
# see graph_build.cc
eps = 1e-6

nulldf <- data.frame(u = numeric(), v = numeric(), weight = numeric())

strtodf <- function(s){
  a <- as.vector(strsplit(s, " ")[[1]])
  l <- length(a) / 3
  df <- data.frame(
    u = as.integer(a[1:l]),
    v = as.integer(a[(l + 1):(2 * l)]),
    weight = as.numeric(a[(2 * l + 1):(3 * l)])
  )
}


test_that("`lcc` method on a 1-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`lcc` method on a 1-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = 1:3), layer = "s", method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`lcc` method on a 2-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = c(0, 1)), method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0"))
  expect_equal(proj(1 - eps), strtodf("1 2 0"))
  expect_equal(proj(1), strtodf("1 2 0"))
})


test_that("`lcc` method on a 2-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 1), layer = "s", method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0"))
  expect_equal(proj(1 - eps), strtodf("1 2 0"))
  expect_equal(proj(1), strtodf("1 2 0"))
})


test_that("`lcc` method on a 3-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = c(0, 0.4, 1)), method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 2 - eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 2), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 2 + eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(1 - eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(1), strtodf("1 2 2 3 0.6 0.4"))
})


test_that("`lcc` method on a 3-clique, symbolic layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 2 - eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 2), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 2 + eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(1 - eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(1), strtodf("1 2 2 3 0.6 0.4"))
})


test_that("`lcc` method on a 4-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 3 - eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 3), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 3 + eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 3 - eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 3), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 3 + eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(1 - eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(1), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
})


test_that("`lcc` method on a 4-clique, symbolic layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 3 - eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 3), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 3 + eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 3 - eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 3), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 3 + eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(1 - eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(1), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
})


test_that("`avgdegree` method on a 1-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`avgdegree` method on a 1-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = 1:3), layer = "s", method = "a", methodval = x)
  
  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`avgdegree` method on a 2-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = c(0, 1)), method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0"))
  expect_equal(proj(1 - eps), strtodf("1 2 0"))
  expect_equal(proj(1), strtodf("1 2 0"))
})


test_that("`avgdegree` method on a 2-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 1), layer = "s", method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0"))
  expect_equal(proj(1 - eps), strtodf("1 2 0"))
  expect_equal(proj(1), strtodf("1 2 0"))
})


test_that("`avgdegree` method on a 3-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3 - eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3 + eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3 - eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3 + eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(1 - eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(1), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
})


test_that("`avgdegree` method on a 3-clique, symbolic layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3 - eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3 + eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3 - eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3 + eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(1 - eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(1), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
})


test_that("`avgdegree` method on a 4-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6 - eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6 + eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6 - eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6 + eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6 - eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6 + eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6 - eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6 + eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6 - eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6 + eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(1 - eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(1), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
})


test_that("`avgdegree` method on a 4-clique, symbolic layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6 - eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6 + eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6 - eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6 + eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6 - eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6 + eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6 - eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6 + eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6 - eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6 + eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(1 - eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(1), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
})


test_that("`similarity` method on a 1-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = 1, b = 1, c = 1), layer = "a", method = "s", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`similarity` method on a 1-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = 1:3), layer = "s", method = "s", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`similarity` method on a 2-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = c(1, 1)), method = "s", methodval = x)

  expect_equal(proj(1 - eps), strtodf("1 2 1"))
  expect_equal(proj(1), strtodf("1 2 1"))
  expect_equal(proj(1 + eps), nulldf)
})


test_that("`similarity` method on a 2-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = 1, b = 1), layer = "s", method = "s", methodval = x)

  expect_equal(proj(1 - eps), strtodf("1 2 1"))
  expect_equal(proj(1), strtodf("1 2 1"))
  expect_equal(proj(1 + eps), nulldf)
})


test_that("`similarity` method on a 3-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = c(0, 0.4, 1)), method = "s", methodval = x)

  expect_equal(proj(-eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(0), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(0.4 - eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(0.4), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(0.4 + eps), strtodf("1 2 0.6"))
  expect_equal(proj(0.6 - eps), strtodf("1 2 0.6"))
  expect_equal(proj(0.6), strtodf("1 2 0.6"))
  expect_equal(proj(0.6 + eps), nulldf)
})


test_that("`similarity` method on a 3-clique, symbolic layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", method = "s", methodval = x)

  expect_equal(proj(-eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(0), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(0.4 - eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(0.4), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(0.4 + eps), strtodf("1 2 0.6"))
  expect_equal(proj(0.6 - eps), strtodf("1 2 0.6"))
  expect_equal(proj(0.6), strtodf("1 2 0.6"))
  expect_equal(proj(0.6 + eps), nulldf)
})


test_that("`similarity` method on a 4-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "s", methodval = x)

  expect_equal(proj(-eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(0), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(0.1 - eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(0.1), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(0.1 + eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(0.4 - eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(0.4), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(0.4 + eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(0.6 - eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(0.6), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(0.6 + eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(0.7 - eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(0.7), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(0.7 + eps), strtodf("3 4 0.9"))
  expect_equal(proj(0.9 - eps), strtodf("3 4 0.9"))
  expect_equal(proj(0.9), strtodf("3 4 0.9"))
  expect_equal(proj(0.9 + eps), nulldf)
})


test_that("`similarity` method on a 4-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", method = "s", methodval = x)

  expect_equal(proj(-eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(0), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(0.1 - eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(0.1), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(0.1 + eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(0.4 - eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(0.4), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(0.4 + eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(0.6 - eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(0.6), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(0.6 + eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(0.7 - eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(0.7), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(0.7 + eps), strtodf("3 4 0.9"))
  expect_equal(proj(0.9 - eps), strtodf("3 4 0.9"))
  expect_equal(proj(0.9), strtodf("3 4 0.9"))
  expect_equal(proj(0.9 + eps), nulldf)
})

# TODO, test the following
# normalising step for rows and columns...
# likert scale values
# minimum comparisons
# different metrics
# bootstrapping

# TODO, in different files, test
# make_threshold_profile()
# make_synthetic()
