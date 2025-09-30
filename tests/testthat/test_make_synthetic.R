test_that("Limiting one dimension to zero.", {
  synth <- function(n, m) make_synthetic_data(n, m)
  mat <- function(n){
	  df <- as.data.frame(matrix(nrow = n, ncol = 0))
	  names(df) <- character()
	  df
  }

  expect_equal(synth(0, 0), data.frame())
  expect_equal(synth(0, 1), data.frame(group = numeric()))
  expect_equal(synth(0, 2), data.frame(group = numeric(), item_1 = numeric()))
  expect_equal(synth(0, 3), data.frame(group = numeric(), item_1 = numeric(), item_2 = numeric()))

  #expect_equal(synth(1, 0), as.data.frame(matrix(nrow = 1, ncol = 0)))
  #expect_equal(synth(2, 0), as.data.frame(matrix(nrow = 2, ncol = 0)))
  #expect_equal(synth(3, 0), as.data.frame(matrix(nrow = 3, ncol = 0)))

  expect_equal(synth(1, 0), mat(1))
  expect_equal(synth(2, 0), mat(2))
  expect_equal(synth(3, 0), mat(3))
})


test_that("`minority` argument behaves as expected.", {
  txt <- "
  1  4  0  7  0  7  0  7  0  7  0  7  0  4  0  4  0  4  0  4  1  4
  1  8  1  8  0  3  0  3  0  3  0  3  0  8  1  8  1  8  1  8  1  8
  1  9  1  9  1  9  0  2  0  2  0  2  0  9  0  9  0  9  0  9  1  9
  1  2  1  2  1  2  1  2  0  9  0  9  0  2  0  2  1  2  1  2  1  2
  1  7  1  7  1  7  1  7  1  7  0  4  1  7  1  7  1  7  1  7  1  7
  1  5  1  5  1  5  1  5  1  5  1  5  0  5  0  5  0  5  1  5  1  5
  1  5  1  5  1  5  1  5  1  5  1  5  0  5  0  5  0  5  0  5  0  5
  1  4  1  4  1  4  1  4  1  4  1  4  0  4  0  4  0  4  0  4  0  4
  1  4  1  4  1  4  1  4  1  4  1  4  1  4  1  4  1  4  1  4  1  4
  1  5  1  5  1  5  1  5  1  5  1  5  1  5  1  5  1  5  1  5  1  5"
	data <- read.table(text = txt, header = FALSE)
  data <- lapply(data, as.numeric)

  synth <- function(x, y)
    make_synthetic_data(10, 2, seed = 0, minority = x, correlation = y)

  expect_equal(synth(0.0, 1), data.frame(group = data[[1]],  item_1 = data[[2]]))
  expect_equal(synth(0.1, 1), data.frame(group = data[[3]],  item_1 = data[[4]]))
  expect_equal(synth(0.2, 1), data.frame(group = data[[5]],  item_1 = data[[6]]))
  expect_equal(synth(0.3, 1), data.frame(group = data[[7]],  item_1 = data[[8]]))
  expect_equal(synth(0.4, 1), data.frame(group = data[[9]],  item_1 = data[[10]]))
  expect_equal(synth(0.5, 1), data.frame(group = data[[11]], item_1 = data[[12]]))

  expect_equal(synth(0, 0.0), data.frame(group = data[[13]], item_1 = data[[14]]))
  expect_equal(synth(0, 0.2), data.frame(group = data[[15]], item_1 = data[[16]]))
  expect_equal(synth(0, 0.4), data.frame(group = data[[17]], item_1 = data[[18]]))
  expect_equal(synth(0, 0.6), data.frame(group = data[[19]], item_1 = data[[20]]))
  expect_equal(synth(0, 0.8), data.frame(group = data[[21]], item_1 = data[[22]]))
})


test_that("`polarisation` argument behaves as expected.", {
  synth <- function(x, y) 
    make_synthetic_data(10, 2, seed = 0, correlation = x, polarisation = y)

  txt <- "
  0  7  4  3  3  3  2  5   5   2   1   1
  0  3  8  8  7  7  7  1   1   1   1   1
  0  2  8  5  5  4  4  1   2   1   1   1
  0  9  2  3  2  2  2  4   3   2   1   1
  0  4  6  6  6  6  5  4   3   3   1   1
  1  5  6  5  8  9  9  4   8   8   8   10
  1  5  7  5  8  8  9  10  10  10  10  10
  1  4  8  7  7  8  8  9   9   10  10  10
  1  4  7  4  7  7  8  10  10  10  10  10
  1  5  7  6  9  9  9  8   8   8   10  10"
	data <- read.table(text = txt, header = FALSE)
  data <- lapply(data, as.numeric)

  expect_equal(synth(1.0, 0.0), data.frame(group = data[[1]],  item_1 = data[[2]]))
  expect_equal(synth(1.0, 0.1), data.frame(group = data[[1]],  item_1 = data[[3]]))
  expect_equal(synth(1.0, 0.2), data.frame(group = data[[1]],  item_1 = data[[4]]))
  expect_equal(synth(1.0, 0.3), data.frame(group = data[[1]],  item_1 = data[[5]]))
  expect_equal(synth(1.0, 0.4), data.frame(group = data[[1]],  item_1 = data[[6]]))
  expect_equal(synth(1.0, 0.5), data.frame(group = data[[1]],  item_1 = data[[7]]))
  expect_equal(synth(1.0, 0.6), data.frame(group = data[[1]],  item_1 = data[[8]]))
  expect_equal(synth(1.0, 0.7), data.frame(group = data[[1]],  item_1 = data[[9]]))
  expect_equal(synth(1.0, 0.8), data.frame(group = data[[1]],  item_1 = data[[10]]))
  expect_equal(synth(1.0, 0.9), data.frame(group = data[[1]],  item_1 = data[[11]]))
  expect_equal(synth(1.0, 1.0), data.frame(group = data[[1]],  item_1 = data[[12]]))

  txt <- "
  1   1  1  1  1  0  0
  1   0  0  0  0  0  0
  1   0  0  0  0  0  0
  1   1  0  0  0  0  0
  1   1  1  1  1  1  0
  10  1  1  1  1  1  1
  10  0  0  0  0  1  1
  10  0  0  0  0  0  1
  10  0  0  1  1  1  1
  10  0  0  1  1  1  1"
	data <- read.table(text = txt, header = FALSE)
  data <- lapply(data, as.numeric)

  expect_equal(synth(0.0, 1.0), data.frame(group = data[[2]], item_1 = data[[1]]))
  expect_equal(synth(0.2, 1.0), data.frame(group = data[[3]], item_1 = data[[1]]))
  expect_equal(synth(0.4, 1.0), data.frame(group = data[[4]], item_1 = data[[1]]))
  expect_equal(synth(0.6, 1.0), data.frame(group = data[[5]], item_1 = data[[1]]))
  expect_equal(synth(0.8, 1.0), data.frame(group = data[[6]], item_1 = data[[1]]))
  expect_equal(synth(1.0, 1.0), data.frame(group = data[[7]], item_1 = data[[1]]))
})


test_that("Accept US spelling of `polarisation`.", {
  expect_equal(
    make_synthetic_data(10, 10, seed = 0, polarisation = 0.6),
    make_synthetic_data(10, 10, seed = 0, polarization = 0.6)
  )
})


test_that("`likert` argument behaves as expected.", {
  synth <- function(n) 
    make_synthetic_data(2, 2, correlation = 1, polarisation = 1, likert = n)

  expect_equal(synth(1), data.frame(group = c(0, 1), item_1 = c(1, 1))) 
  expect_equal(synth(2), data.frame(group = c(0, 1), item_1 = c(1, 2))) 
  expect_equal(synth(3), data.frame(group = c(0, 1), item_1 = c(1, 3))) 
  expect_equal(synth(4), data.frame(group = c(0, 1), item_1 = c(1, 4))) 
  expect_equal(synth(5), data.frame(group = c(0, 1), item_1 = c(1, 5))) 
  expect_equal(synth(6), data.frame(group = c(0, 1), item_1 = c(1, 6))) 
  expect_equal(synth(7), data.frame(group = c(0, 1), item_1 = c(1, 7))) 
  expect_equal(synth(8), data.frame(group = c(0, 1), item_1 = c(1, 8))) 
  expect_equal(synth(9), data.frame(group = c(0, 1), item_1 = c(1, 9))) 
  expect_equal(synth(10), data.frame(group = c(0, 1), item_1 = c(1, 10))) 
})
