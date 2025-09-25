eps = 1e-6

projs <- function(s, x){
  make_projection(
    data.frame(t(s)), 
    likert = data.frame(replicate(length(s), c(0, 1))), 
    layer = "symbolic", 
    method = "lcc",
    methodval = x
  )
}

proja <- function(s, x){
  make_projection(
    data.frame(s), 
    layer = "agent", 
    method = "lcc",
    methodval = x
  )
}

fn <- function(x, y, z){
  df <- data.frame(u = x, v = y, weight = as.numeric(z))
	df <- df[!is.na(df$weight),]
	if(nrow(df) > 0) 
    rownames(df) <- 1:nrow(df)
  df
}


test_that("`lcc` method on a 1-clique.", {
  survey <- 1

  weights <- "
  u  v  complete  lcc1"
	data <- read.table(text = weights, colClasses = "numeric", header = TRUE)

  expect_equal(proja(survey, 0),           fn(data$u, data$v, data$lcc1))
  expect_equal(proja(survey, 1),           fn(data$u, data$v, data$lcc1))

  expect_equal(projs(survey, 0),           fn(data$u, data$v, data$lcc1))
  expect_equal(projs(survey, 1),           fn(data$u, data$v, data$lcc1))
})


test_that("`lcc` method on a 2-clique.", {
  survey <- c(0, 1)

  weights <- "
  u  v  complete  lcc1  lcc2
  1  2  0         NA    0"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proja(survey, 0),           fn(data$u, data$v, data$lcc1))
  expect_equal(proja(survey, eps),         fn(data$u, data$v, data$lcc2))
  expect_equal(proja(survey, 1 - eps),     fn(data$u, data$v, data$lcc2))
  expect_equal(proja(survey, 1),           fn(data$u, data$v, data$lcc2))

  expect_equal(projs(survey, 0),           fn(data$u, data$v, data$lcc1))
  expect_equal(projs(survey, eps),         fn(data$u, data$v, data$lcc2))
  expect_equal(projs(survey, 1 - eps),     fn(data$u, data$v, data$lcc2))
  expect_equal(projs(survey, 1),           fn(data$u, data$v, data$lcc2))
})


test_that("`lcc` method on a 3-clique.", {
  survey <- c(0, 0.4, 1)

  weights <- "
  u  v  lcc1  lcc2  lcc3  complete
  1  2  NA    0.6   0.6   0.6      
  1  3  NA    NA    NA    0.0     
  2  3  NA    NA    0.4   0.4"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proja(survey, 0),           fn(data$u, data$v, data$lcc1))
  expect_equal(proja(survey, eps),         fn(data$u, data$v, data$lcc2))
  expect_equal(proja(survey, 1 / 2 - eps), fn(data$u, data$v, data$lcc2))
  expect_equal(proja(survey, 1 / 2),       fn(data$u, data$v, data$lcc2))
  expect_equal(proja(survey, 1 / 2 + eps), fn(data$u, data$v, data$lcc3))
  expect_equal(proja(survey, 1 - eps),     fn(data$u, data$v, data$lcc3))
  expect_equal(proja(survey, 1),           fn(data$u, data$v, data$lcc3))

  expect_equal(projs(survey, 0),           fn(data$u, data$v, data$lcc1))
  expect_equal(projs(survey, eps),         fn(data$u, data$v, data$lcc2))
  expect_equal(projs(survey, 1 / 2 - eps), fn(data$u, data$v, data$lcc2))
  expect_equal(projs(survey, 1 / 2),       fn(data$u, data$v, data$lcc2))
  expect_equal(projs(survey, 1 / 2 + eps), fn(data$u, data$v, data$lcc3))
  expect_equal(projs(survey, 1 - eps),     fn(data$u, data$v, data$lcc3))
  expect_equal(projs(survey, 1),           fn(data$u, data$v, data$lcc3))
})


test_that("`lcc` method on a 4-clique.", {
  survey <- c(0, 0.6, 0.9, 1)

  weights <- "
  u  v  lcc1  lcc2  lcc3  lcc4  complete
  1  2  NA    NA    NA    0.4   0.4     
  1  3  NA    NA    NA    NA    0.1     
  1  4  NA    NA    NA    NA    0       
  2  3  NA    NA    0.7   0.7   0.7     
  2  4  NA    NA    NA    0.6   0.6     
  3  4  NA    0.9   0.9   0.9   0.9"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proja(survey, 0),           fn(data$u, data$v, data$lcc1))
  expect_equal(proja(survey, eps),         fn(data$u, data$v, data$lcc2))
  expect_equal(proja(survey, 1 / 3 - eps), fn(data$u, data$v, data$lcc2))
  expect_equal(proja(survey, 1 / 3),       fn(data$u, data$v, data$lcc2))
  expect_equal(proja(survey, 1 / 3 + eps), fn(data$u, data$v, data$lcc3))
  expect_equal(proja(survey, 2 / 3 - eps), fn(data$u, data$v, data$lcc3))
  expect_equal(proja(survey, 2 / 3),       fn(data$u, data$v, data$lcc3))
  expect_equal(proja(survey, 2 / 3 + eps), fn(data$u, data$v, data$lcc4))
  expect_equal(proja(survey, 1 - eps),     fn(data$u, data$v, data$lcc4))
  expect_equal(proja(survey, 1),           fn(data$u, data$v, data$lcc4))

  expect_equal(projs(survey, 0),           fn(data$u, data$v, data$lcc1))
  expect_equal(projs(survey, eps),         fn(data$u, data$v, data$lcc2))
  expect_equal(projs(survey, 1 / 3 - eps), fn(data$u, data$v, data$lcc2))
  expect_equal(projs(survey, 1 / 3),       fn(data$u, data$v, data$lcc2))
  expect_equal(projs(survey, 1 / 3 + eps), fn(data$u, data$v, data$lcc3))
  expect_equal(projs(survey, 2 / 3 - eps), fn(data$u, data$v, data$lcc3))
  expect_equal(projs(survey, 2 / 3),       fn(data$u, data$v, data$lcc3))
  expect_equal(projs(survey, 2 / 3 + eps), fn(data$u, data$v, data$lcc4))
  expect_equal(projs(survey, 1 - eps),     fn(data$u, data$v, data$lcc4))
  expect_equal(projs(survey, 1),           fn(data$u, data$v, data$lcc4))
})
