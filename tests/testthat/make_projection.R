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


#files
#======================================
#data_preprocessing_args.R
#data_preprocessing.R
#
#make_projection_args.R
#make_projection_method_lcc.R
#make_projection_method_avgdegree.R
#make_projection_method_similarity.R
#make_projection_mincompare.R
#make_projection_metric.R
#make_projection_bootstrap.R
#
#make_synthetic_args.R
#make_synthetic.R
#
#make_threshold_profile_args.R
#make_threshold_profile.R


test_that("`lcc` method on a 1-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = 1, b = 1, c = 1), method = "l", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`lcc` method on a 1-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = c(1, 1, 1)), layer = "s", likert = l1, method = "l", methodval = x)

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
  proj <- function(x) make_projection(data.frame(a = 0, b = 1), layer = "s", likert = l2, method = "l", methodval = x)

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
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", likert = l3, method = "l", methodval = x)

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
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", likert = l4, method = "l", methodval = x)

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
  proj <- function(x) make_projection(data.frame(a = 1, b = 1, c = 1), method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`avgdegree` method on a 1-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = c(1, 1, 1)), layer = "s", likert = l1, method = "a", methodval = x)
  
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
  proj <- function(x) make_projection(data.frame(a = 0, b = 1), layer = "s", likert = l2, method = "a", methodval = x)

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
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", likert = l3, method = "a", methodval = x)

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
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", likert = l4, method = "a", methodval = x)

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


test_that("`mincompare` gives expected behaviour", {
  proj <- function(n){ 
    make_projection(
      data.frame(
        c(0,  1), 
        c(0,  1), 
        c(0,  1), 
        c(0,  1),
        c(0,  1),
        c(0,  NA),
        c(NA, 1),
        c(NA, NA)
      ),
      mincompare = n
    )
  }

  expect_equal(proj(1), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(2), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(3), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(4), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(5), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(6), data.frame(u = numeric(), v = numeric(), weight = numeric()))
  expect_equal(proj(7), data.frame(u = numeric(), v = numeric(), weight = numeric()))
  expect_equal(proj(8), data.frame(u = numeric(), v = numeric(), weight = numeric()))
})


test_that("`metric` gives expected behaviour", {
  proj <- function(x, y, s){
    make_projection(
      data.frame(a = c(0, x), b = c(0, y)), 
      likert = data.frame(c(0, 1), c(0, 1)), 
      metric = s
    )
  }

  expect_equal(proj(0,    0,    "Manhattan"), data.frame(u = 1, v = 2, weight = 1.0000000000), tolerance = 1e-6)
  expect_equal(proj(0.25, 0,    "Manhattan"), data.frame(u = 1, v = 2, weight = 0.8750000000), tolerance = 1e-6)
  expect_equal(proj(0.25, 0.25, "Manhattan"), data.frame(u = 1, v = 2, weight = 0.7500000000), tolerance = 1e-6)
  expect_equal(proj(0.5,  0,    "Manhattan"), data.frame(u = 1, v = 2, weight = 0.7500000000), tolerance = 1e-6)
  expect_equal(proj(0.5,  0.25, "Manhattan"), data.frame(u = 1, v = 2, weight = 0.6250000000), tolerance = 1e-6)
  expect_equal(proj(0.5,  0.5,  "Manhattan"), data.frame(u = 1, v = 2, weight = 0.5000000000), tolerance = 1e-6)
  expect_equal(proj(0.75, 0,    "Manhattan"), data.frame(u = 1, v = 2, weight = 0.6250000000), tolerance = 1e-6)
  expect_equal(proj(0.75, 0.25, "Manhattan"), data.frame(u = 1, v = 2, weight = 0.5000000000), tolerance = 1e-6)
  expect_equal(proj(0.75, 0.5,  "Manhattan"), data.frame(u = 1, v = 2, weight = 0.3750000000), tolerance = 1e-6)
  expect_equal(proj(0.75, 0.75, "Manhattan"), data.frame(u = 1, v = 2, weight = 0.2500000000), tolerance = 1e-6)
  expect_equal(proj(1,    1,    "Manhattan"), data.frame(u = 1, v = 2, weight = 0.0000000000), tolerance = 1e-6)

  expect_equal(proj(0,    0,    "Euclidean"), data.frame(u = 1, v = 2, weight = 1.0000000000), tolerance = 1e-6)
  expect_equal(proj(0.25, 0,    "Euclidean"), data.frame(u = 1, v = 2, weight = 0.8232233047), tolerance = 1e-6)
  expect_equal(proj(0.25, 0.25, "Euclidean"), data.frame(u = 1, v = 2, weight = 0.7500000000), tolerance = 1e-6)
  expect_equal(proj(0.5,  0,    "Euclidean"), data.frame(u = 1, v = 2, weight = 0.6464466094), tolerance = 1e-6)
  expect_equal(proj(0.5,  0.25, "Euclidean"), data.frame(u = 1, v = 2, weight = 0.6047152925), tolerance = 1e-6)
  expect_equal(proj(0.5,  0.5,  "Euclidean"), data.frame(u = 1, v = 2, weight = 0.5000000000), tolerance = 1e-6)
  expect_equal(proj(0.75, 0,    "Euclidean"), data.frame(u = 1, v = 2, weight = 0.4696699141), tolerance = 1e-6)
  expect_equal(proj(0.75, 0.25, "Euclidean"), data.frame(u = 1, v = 2, weight = 0.4409830056), tolerance = 1e-6)
  expect_equal(proj(0.75, 0.5,  "Euclidean"), data.frame(u = 1, v = 2, weight = 0.3626225608), tolerance = 1e-6)
  expect_equal(proj(0.75, 0.75, "Euclidean"), data.frame(u = 1, v = 2, weight = 0.2500000000), tolerance = 1e-6)
  expect_equal(proj(1,    1,    "Euclidean"), data.frame(u = 1, v = 2, weight = 0.0000000000), tolerance = 1e-6)
})


# TODO, test the following
# bootstrapping
