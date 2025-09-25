# Main tests for make_projection(). Note that arguments likert and dummycode are
# tested in test-data-preprocessing.R.

# epsilon for testing small changes to parameter arguments. Note that there is
# an internal tolerance of 1e-8 when comparing similarity weight to the threshold,
# see graph_build.cc
eps = 1e-6

nulldf <- data.frame(u = numeric(), v = numeric(), weight = numeric())

#ln <- data.frame(replicate(n, c(0, 1)))
l1 <- data.frame(c(0, 1))
l2 <- data.frame(c(0, 1), c(0, 1))
l3 <- data.frame(c(0, 1), c(0, 1), c(0, 1))
l4 <- data.frame(c(0, 1), c(0, 1), c(0, 1), c(0, 1))

strtodf <- function(s){
  a <- as.vector(strsplit(s, " ")[[1]])
  l <- length(a) / 3
  df <- data.frame(
    u = as.integer(a[1:l]),
    v = as.integer(a[(l + 1):(2 * l)]),
    weight = as.numeric(a[(2 * l + 1):(3 * l)])
  )
}


# TODO: add more tests where the weight 1 appears?




test_that("`similarity` method on a 1-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = 1, b = 1, c = 1), method = "s", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`similarity` method on a 1-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = c(1, 1, 1)), layer = "s", likert = l1, method = "s", methodval = x)

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
  proj <- function(x) make_projection(data.frame(a = 1, b = 1), layer = "s", likert = l2, method = "s", methodval = x)

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
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", likert = l3, method = "s", methodval = x)

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


test_that("`similarity` method on a 4-clique, symbolic layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", likert = l4, method = "s", methodval = x)

  #,,,,,,,,,,,,,,, u = c(), v = c(), weight = c()
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


