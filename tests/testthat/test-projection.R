test_that("correct edge list", {
  S <- data.frame(group = c("0", "0", "1", "1"), item_1 = c(1, 2, 5, 4), item_2 = c(1, 3, 2, 2))

  e1 <- make_projection(S, "agent", threshold_method = "raw_similarity", method_value = -1.00)
  e2 <- make_projection(S, "agent", threshold_method = "raw_similarity", method_value = -0.50)
  e3 <- make_projection(S, "agent", threshold_method = "raw_similarity", method_value = -0.25)
  e4 <- make_projection(S, "agent", threshold_method = "raw_similarity", method_value =  0.00)
  e5 <- make_projection(S, "agent", threshold_method = "raw_similarity", method_value =  0.75)

  expect_equal(
    e1,
    data.frame(
      u = c(1, 1, 1, 2, 2, 3),
      v = c(2, 3, 4, 3, 4, 4),
      weight = c(-0.25, -0.5, -0.25, -0.25, 0, 0.75)
    )
  )

  expect_equal(
    e2,
    data.frame(
      u = c(1, 1, 2, 2, 3),
      v = c(2, 4, 3, 4, 4),
      weight = c(-0.25, -0.25, -0.25, 0, 0.75)
    )
  )

  expect_equal(
    e3,
    data.frame(
      u = c(2, 3),
      v = c(4, 4),
      weight = c(0, 0.75)
    )
  )

  expect_equal(
    e4,
    data.frame(
      u = c(3),
      v = c(4),
      weight = c(0.75)
    )
  )

  expect_equal(
    e5,
    data.frame(
      u = as.integer(c()),
      v = as.integer(c()),
      weight = as.numeric(c())
    )
  )
})
