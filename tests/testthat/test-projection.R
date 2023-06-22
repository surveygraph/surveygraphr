test_that("check names in synthetic survey dataframes", {
  S <- make_synthetic_data(1, 3)
  x <- names(S)
  expect_equal(x, c("group", "item_1", "item_2", "item_3"))
})

test_that("correct number of columns", {
  S <- make_synthetic_data(1, 3)
  x <- length(S)
  expect_equal(x, 4)
})

test_that("correct number of rows", {
  S <- make_synthetic_data(1, 3)
  x <- length(S$group)
  expect_equal(x, 1)
})

#test_that("correct edge list", {
#  S <- data.frame(group = c("0", "0", "1"), item_1 = c(1, 2, 5), item_2 = c(1, 3, 4))
#  e <- make_projection(S, "agent")
#})
