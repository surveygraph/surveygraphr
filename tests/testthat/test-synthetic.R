# use a seed to actually test

test_that("correctly names columns", {
  S <- make_synthetic_data(1, 3)
  x <- names(S)
  expect_equal(x, c("group", "item_1", "item_2", "item_3"))
})
