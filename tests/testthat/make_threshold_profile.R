test_that("Correct when survey consists of a single row, default `count`.", {
  proj <- function() make_threshold_profile(data = data.frame(1))	

  df <- data.frame(
    threshold = seq(0, 1, by = 0.05), 
    lcc = rep(1, 21), 
    edges = rep(0, 21), 
    components = rep(1, 21), 
    isolated = rep(1, 21)
  )

  expect_equal(proj(), df)
})


test_that("Correct when survey consists of a single row, varying `count`.", {
  proj <- function(n) make_threshold_profile(data = data.frame(1), count = n)	

  df <- data.frame(
      threshold = c(0, 0.5, 1),
      lcc = c(1, 1, 1), 
      edges = c(0, 0, 0), 
      components = c(1, 1, 1), 
      isolated = c(1, 1, 1)
  )

  expect_warning(expect_equal(proj(-1), df))
  expect_warning(expect_equal(proj(0), df))
  expect_warning(expect_equal(proj(1), df))
  expect_warning(expect_equal(proj(2), df))
})


test_that("Correct when survey consists of a single row, varying `count`.", {
  proj <- function(n) make_threshold_profile(data = data.frame(1), count = n)	

  df <- function(n){
    data.frame(
      threshold = seq(0, 1, by = 1 / (n - 1)),
      lcc = rep(1, n), 
      edges = rep(0, n), 
      components = rep(1, n), 
      isolated = rep(1, n)
    )
  }

  expect_equal(proj(3), df(3))
  expect_equal(proj(4), df(4))
  expect_equal(proj(5), df(5))
  expect_equal(proj(6), df(6))
  expect_equal(proj(7), df(7))
  expect_equal(proj(8), df(8))
  expect_equal(proj(9), df(9))
  expect_equal(proj(10), df(10))
})


test_that("Behaves as expected on more general data.", {
  txt <- "
  threshold lcc edges components isolated
  0.00      7   21    1          0
  0.05      7   20    1          0
  0.10      7   20    1          0
  0.15      7   19    1          0
  0.20      7   19    1          0
  0.25      7   19    1          0
  0.30      7   16    1          0
  0.35      7   16    1          0
  0.40      7   13    1          0
  0.45      7   12    1          0
  0.50      7   12    1          0
  0.55      7   12    1          0
  0.60      7   11    1          0
  0.65      7   11    1          0
  0.70      5    8    2          0
  0.75      4    6    3          1
  0.80      3    4    4          2
  0.85      3    4    4          2
  0.90      3    4    4          2
  0.95      3    3    5          4
  1.00      2    1    6          5"
  profile <- read.table(text = txt, header = TRUE)

  proja <- function(){
    make_threshold_profile(
	    data = data.frame(c(0, 0.25, 0.25, 0.26, 0.56, 0.9, 1.0)),
      count = 21
    )	
  }

  projs <- function(){
    make_threshold_profile(
	    data = data.frame(a = 0, b = 0.25, c = 0.25, d = 0.26, e = 0.56, f = 0.9, g = 1.0),
      likert = data.frame(replicate(7, c(0, 1))),
      layer = "s",
      count = 21
    )	
  }

  expect_equal(proja(), profile)
  expect_equal(projs(), profile)
})
