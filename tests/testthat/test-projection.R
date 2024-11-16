test_that("correct likert", {
  S <- data.frame(item_1 = c(1, -99),
                  item_2 = c(1, 3),
                  item_3 = c(1, 2))

	l <- data.frame(minval = c(1, 1, 1), maxval = c(3, 3, 3))

  expect_equal(
		make_projection(S, likert = l),
    data.frame(u = c(1), v = c(2), weight = c(0.25))
  )

	# TODO test incorrect likert specification, e.g. min greater than max
	l <- data.frame(minval = c(4, 4, 4), maxval = c(3, 3, 3))

	# TODO test likert = NA
	# TODO test likert incorrect dimensions
	
})


test_that("correct edge list", {
  S <- data.frame(group = c("0", "0", "1", "1"),
                  item_1 = c(1, 2, 5, 4),
                  item_2 = c(1, 3, 2, 2),
                  item_3 = c(1, 5, 3, 2))

  fn <- function(m){
    make_projection(
      S,
      threshold_method = "raw_similarity",
      method_value = m
    )
  }

  expect_equal(
    fn(0),
    data.frame(
      u = c(1, 1, 1, 2, 2, 3),
      v = c(2, 3, 4, 3, 4, 4),
      weight = c(0.4375, 0.25, 0.375, 0.3125, 0.3125, 0.875)
    )
  )

  expect_equal(
    fn(0.25),
    data.frame(
      u = c(1, 1, 2, 2, 3),
      v = c(2, 4, 3, 4, 4),
      weight = c(0.4375, 0.375, 0.3125, 0.3125, 0.875)
    )
  )

  expect_equal(
    fn(0.32),
    data.frame(
      u = c(1, 1, 3),
      v = c(2, 4, 4),
      weight = c(0.4375, 0.375, 0.875)
    )
  )

  expect_equal(
    fn(0.4),
    data.frame(
      u = c(1, 3),
      v = c(2, 4),
      weight = c(0.4375, 0.875)
    )
  )

  expect_equal(
    fn(0.5),
    data.frame(
      u = c(3),
      v = c(4),
      weight = c(0.875)
    )
  )

  expect_equal(
    fn(0.9),
		data.frame(
			u = numeric(0),
			v = numeric(0),
			weight = numeric(0),
			stringsAsFactors = FALSE
		)
  )
})
