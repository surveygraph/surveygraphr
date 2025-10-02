test_that("`metric` gives expected behaviour", {
  proj <- function(x, y, s){
    make_projection(
      data.frame(a = c(0, x), b = c(0, y)),
      limits = data.frame(c(0, 1), c(0, 1)),
      #likert = data.frame(c(0, 1), c(0, 1)),
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
