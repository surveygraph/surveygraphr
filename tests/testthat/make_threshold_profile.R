test_that("`mincompare` gives expected behaviour", {
  proj <- function(n) make_projection(data.frame(c(0, 0.25, 0.25, 1)), count = n)

  expect_equal(proj(1), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(2), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(3), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(4), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(5), data.frame(u = 1, v = 2, weight = 0))
  expect_equal(proj(6), data.frame(u = numeric(), v = numeric(), weight = numeric()))
  expect_equal(proj(7), data.frame(u = numeric(), v = numeric(), weight = numeric()))
  expect_equal(proj(8), data.frame(u = numeric(), v = numeric(), weight = numeric()))
})
