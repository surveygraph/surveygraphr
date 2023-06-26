test_that("correct edge list", {
  S <- data.frame(group = c("0", "0", "1"), item_1 = c(1, 2, 5), item_2 = c(1, 3, 2))
  e <- make_projection(S, "agent", threshold_method = "raw_similarity", method_value = -1)
  f <- data.frame(u = as.integer(c(1, 1, 2)), v = as.integer(c(2, 3, 3)), weight = c(-0.25, -0.5, -0.25))

  expect_equal(e, f) 
})
