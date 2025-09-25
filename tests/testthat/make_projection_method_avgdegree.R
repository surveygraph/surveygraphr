eps = 1e-6

nulldf <- data.frame(u = numeric(), v = numeric(), weight = numeric())

#ln <- data.frame(replicate(n, c(0, 1)))
l1 <- data.frame(c(0, 1))
l2 <- data.frame(c(0, 1), c(0, 1))
l3 <- data.frame(c(0, 1), c(0, 1), c(0, 1))
l4 <- data.frame(c(0, 1), c(0, 1), c(0, 1), c(0, 1))

strtodf <- function(s){
  a <- as.vector(strsplit(s, " ")[[1]])
  l <- length(a) / 3
  df <- data.frame(
    u = as.integer(a[1:l]),
    v = as.integer(a[(l + 1):(2 * l)]),
    weight = as.numeric(a[(2 * l + 1):(3 * l)])
  )
}

test_that("`avgdegree` method on a 1-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = 1, b = 1, c = 1), method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`avgdegree` method on a 1-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = c(1, 1, 1)), layer = "s", likert = l1, method = "a", methodval = x)
  
  expect_equal(proj(0), nulldf)
  expect_equal(proj(1), nulldf)
})


test_that("`avgdegree` method on a 2-clique, agent layer", {
  proj <- function(x) make_projection(data.frame(a = c(0, 1)), method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0"))
  expect_equal(proj(1 - eps), strtodf("1 2 0"))
  expect_equal(proj(1), strtodf("1 2 0"))
})


test_that("`avgdegree` method on a 2-clique, symbolic layer", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 1), layer = "s", likert = l2, method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0"))
  expect_equal(proj(1 - eps), strtodf("1 2 0"))
  expect_equal(proj(1), strtodf("1 2 0"))
})

# e <- make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = 1e-6)

test_that("`avgdegree` method on a 3-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = c(0, 0.4, 1)), method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3 - eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3 + eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3 - eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3 + eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(1 - eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(1), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
})


test_that("`avgdegree` method on a 3-clique, symbolic layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.4, c = 1), layer = "s", likert = l3, method = "a", methodval = x)

  expect_equal(proj(0),           nulldf)
  expect_equal(proj(eps),         strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3 - eps), strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3),       strtodf("1 2 0.6"))
  expect_equal(proj(1 / 3 + eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3 - eps), strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3),       strtodf("1 2 2 3 0.6 0.4"))
  expect_equal(proj(2 / 3 + eps), strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(1 - eps),     strtodf("1 1 2 2 3 3 0.6 0 0.4"))
  expect_equal(proj(1),           strtodf("1 1 2 2 3 3 0.6 0 0.4"))
})


test_that("`avgdegree` method on a 4-clique, agent layer.", {
  proj <- function(x) make_projection(data.frame(a = c(0, 0.6, 0.9, 1)), method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6 - eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6 + eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6 - eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6 + eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6 - eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6 + eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6 - eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6 + eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6 - eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6 + eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(1 - eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(1), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
})


test_that("`avgdegree` method on a 4-clique, symbolic layer.", {
  proj <- function(x) make_projection(data.frame(a = 0, b = 0.6, c = 0.9, d = 1), layer = "s", likert = l4, method = "a", methodval = x)

  expect_equal(proj(0), nulldf)
  expect_equal(proj(eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6 - eps), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6), strtodf("3 4 0.9"))
  expect_equal(proj(1 / 6 + eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6 - eps), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6), strtodf("2 3 3 4 0.7 0.9"))
  expect_equal(proj(2 / 6 + eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6 - eps), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6), strtodf("2 2 3 3 4 4 0.7 0.6 0.9"))
  expect_equal(proj(3 / 6 + eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6 - eps), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6), strtodf("1 2 2 3 2 3 4 4 0.4 0.7 0.6 0.9"))
  expect_equal(proj(4 / 6 + eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6 - eps), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6), strtodf("1 1 2 2 3 2 3 3 4 4 0.4 0.1 0.7 0.6 0.9"))
  expect_equal(proj(5 / 6 + eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(1 - eps), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
  expect_equal(proj(1), strtodf("1 1 1 2 2 3 2 3 4 3 4 4 0.4 0.1 0 0.7 0.6 0.9"))
})


proja <- function(s, x){
  make_projection(
    data.frame(s), 
    layer = "agent", 
    method = "avgdegree",
    methodval = x
  )
}

projs <- function(s, x){
  make_projection(
    data.frame(t(s)), 
    likert = data.frame(replicate(length(s), c(0, 1))), 
    layer = "symbolic", 
    method = "avgdegree",
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


test_that("`avgdegree` method on a 3-clique.", {
  survey <- c(0, 0.4, 1)

  weights <- "
  u  v  e0  e1   e2   e3 
  1  2  NA  0.6  0.6  0.6
  1  3  NA  NA   NA   0.0
  2  3  NA  NA   0.4  0.4"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proja(survey, 0),           fn(data$u, data$v, data$e0))
  expect_equal(proja(survey, eps),         fn(data$u, data$v, data$e1))
  expect_equal(proja(survey, 1 / 3 - eps), fn(data$u, data$v, data$e1))
  expect_equal(proja(survey, 1 / 3),       fn(data$u, data$v, data$e1))
  expect_equal(proja(survey, 1 / 3 + eps), fn(data$u, data$v, data$e2))
  expect_equal(proja(survey, 2 / 3 - eps), fn(data$u, data$v, data$e5))
  expect_equal(proja(survey, 2 / 3),       fn(data$u, data$v, data$e5))
  expect_equal(proja(survey, 2 / 3 + eps), fn(data$u, data$v, data$e6))
  expect_equal(proja(survey, 1 - eps),     fn(data$u, data$v, data$e6))
  expect_equal(proja(survey, 1),           fn(data$u, data$v, data$e6))

  expect_equal(projs(survey, 0),           fn(data$u, data$v, data$e0))
  expect_equal(projs(survey, eps),         fn(data$u, data$v, data$e1))
  expect_equal(projs(survey, 1 / 6 - eps), fn(data$u, data$v, data$e1))
  expect_equal(projs(survey, 1 / 6),       fn(data$u, data$v, data$e1))
  expect_equal(projs(survey, 1 / 6 + eps), fn(data$u, data$v, data$e2))
  expect_equal(projs(survey, 2 / 6 - eps), fn(data$u, data$v, data$e2))
  expect_equal(projs(survey, 2 / 6),       fn(data$u, data$v, data$e2))
  expect_equal(projs(survey, 2 / 6 + eps), fn(data$u, data$v, data$e3))
  expect_equal(projs(survey, 3 / 6 - eps), fn(data$u, data$v, data$e3))
  expect_equal(projs(survey, 3 / 6),       fn(data$u, data$v, data$e3))
  expect_equal(projs(survey, 3 / 6 + eps), fn(data$u, data$v, data$e4))
  expect_equal(projs(survey, 4 / 6 - eps), fn(data$u, data$v, data$e4))
  expect_equal(projs(survey, 4 / 6),       fn(data$u, data$v, data$e4))
  expect_equal(projs(survey, 4 / 6 + eps), fn(data$u, data$v, data$e5))
  expect_equal(projs(survey, 5 / 6 - eps), fn(data$u, data$v, data$e5))
  expect_equal(projs(survey, 5 / 6),       fn(data$u, data$v, data$e5))
  expect_equal(projs(survey, 5 / 6 + eps), fn(data$u, data$v, data$e6))
  expect_equal(projs(survey, 1 - eps),     fn(data$u, data$v, data$e6))
  expect_equal(projs(survey, 1),           fn(data$u, data$v, data$e6))
})


test_that("`avgdegree` method on a 4-clique.", {
  survey <- c(0, 0.6, 0.9, 1)

  weights <- "
  u  v  e0  e1   e2   e3   e4   e5   e6
  1  2  NA  NA   NA   NA   0.4  0.4  0.4     
  1  3  NA  NA   NA   NA   NA   0.1  0.1     
  1  4  NA  NA   NA   NA   NA   NA   0.0
  2  3  NA  NA   0.7  0.7  0.7  0.7  0.7     
  2  4  NA  NA   NA   0.6  0.6  0.6  0.6     
  3  4  NA  0.9  0.9  0.9  0.9  0.9  0.9"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proja(survey, 0),           fn(data$u, data$v, data$e0))
  expect_equal(proja(survey, eps),         fn(data$u, data$v, data$e1))
  expect_equal(proja(survey, 1 / 6 - eps), fn(data$u, data$v, data$e1))
  expect_equal(proja(survey, 1 / 6),       fn(data$u, data$v, data$e1))
  expect_equal(proja(survey, 1 / 6 + eps), fn(data$u, data$v, data$e2))
  expect_equal(proja(survey, 2 / 6 - eps), fn(data$u, data$v, data$e2))
  expect_equal(proja(survey, 2 / 6),       fn(data$u, data$v, data$e2))
  expect_equal(proja(survey, 2 / 6 + eps), fn(data$u, data$v, data$e3))
  expect_equal(proja(survey, 3 / 6 - eps), fn(data$u, data$v, data$e3))
  expect_equal(proja(survey, 3 / 6),       fn(data$u, data$v, data$e3))
  expect_equal(proja(survey, 3 / 6 + eps), fn(data$u, data$v, data$e4))
  expect_equal(proja(survey, 4 / 6 - eps), fn(data$u, data$v, data$e4))
  expect_equal(proja(survey, 4 / 6),       fn(data$u, data$v, data$e4))
  expect_equal(proja(survey, 4 / 6 + eps), fn(data$u, data$v, data$e5))
  expect_equal(proja(survey, 5 / 6 - eps), fn(data$u, data$v, data$e5))
  expect_equal(proja(survey, 5 / 6),       fn(data$u, data$v, data$e5))
  expect_equal(proja(survey, 5 / 6 + eps), fn(data$u, data$v, data$e6))
  expect_equal(proja(survey, 1 - eps),     fn(data$u, data$v, data$e6))
  expect_equal(proja(survey, 1),           fn(data$u, data$v, data$e6))

  expect_equal(projs(survey, 0),           fn(data$u, data$v, data$e0))
  expect_equal(projs(survey, eps),         fn(data$u, data$v, data$e1))
  expect_equal(projs(survey, 1 / 6 - eps), fn(data$u, data$v, data$e1))
  expect_equal(projs(survey, 1 / 6),       fn(data$u, data$v, data$e1))
  expect_equal(projs(survey, 1 / 6 + eps), fn(data$u, data$v, data$e2))
  expect_equal(projs(survey, 2 / 6 - eps), fn(data$u, data$v, data$e2))
  expect_equal(projs(survey, 2 / 6),       fn(data$u, data$v, data$e2))
  expect_equal(projs(survey, 2 / 6 + eps), fn(data$u, data$v, data$e3))
  expect_equal(projs(survey, 3 / 6 - eps), fn(data$u, data$v, data$e3))
  expect_equal(projs(survey, 3 / 6),       fn(data$u, data$v, data$e3))
  expect_equal(projs(survey, 3 / 6 + eps), fn(data$u, data$v, data$e4))
  expect_equal(projs(survey, 4 / 6 - eps), fn(data$u, data$v, data$e4))
  expect_equal(projs(survey, 4 / 6),       fn(data$u, data$v, data$e4))
  expect_equal(projs(survey, 4 / 6 + eps), fn(data$u, data$v, data$e5))
  expect_equal(projs(survey, 5 / 6 - eps), fn(data$u, data$v, data$e5))
  expect_equal(projs(survey, 5 / 6),       fn(data$u, data$v, data$e5))
  expect_equal(projs(survey, 5 / 6 + eps), fn(data$u, data$v, data$e6))
  expect_equal(projs(survey, 1 - eps),     fn(data$u, data$v, data$e6))
  expect_equal(projs(survey, 1),           fn(data$u, data$v, data$e6))
})
