eps = 1e-6

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
    limits = data.frame(replicate(length(s), c(0, 1))),
    #likert = data.frame(replicate(length(s), c(0, 1))),
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


test_that("`avgdegree` method on a 1-clique.", {
  survey <- 1

  weights <- "u  v  e0"
  df <- read.table(text = weights, header = TRUE, colClasses = "numeric")

  expect_equal(proja(survey, 0),           fn(df$u, df$v, df$e0))
  expect_equal(proja(survey, 1),           fn(df$u, df$v, df$e0))

  expect_equal(projs(survey, 0),           fn(df$u, df$v, df$e0))
  expect_equal(projs(survey, 1),           fn(df$u, df$v, df$e0))
})


test_that("`avgdegree` method on a 2-clique.", {
  survey <- c(0, 1)

  weights <- "
  u  v  e0  e1
  1  2  NA  0.0"
  df <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proja(survey, 0),           fn(df$u, df$v, df$e0))
  expect_equal(proja(survey, eps),         fn(df$u, df$v, df$e1))
  expect_equal(proja(survey, 1 - eps),     fn(df$u, df$v, df$e1))
  expect_equal(proja(survey, 1),           fn(df$u, df$v, df$e1))

  expect_equal(projs(survey, 0),           fn(df$u, df$v, df$e0))
  expect_equal(projs(survey, eps),         fn(df$u, df$v, df$e1))
  expect_equal(projs(survey, 1 - eps),     fn(df$u, df$v, df$e1))
  expect_equal(projs(survey, 1),           fn(df$u, df$v, df$e1))
})


test_that("`avgdegree` method on a 3-clique.", {
  survey <- c(0, 0.4, 1)

  weights <- "
  u  v  e0  e1   e2   e3
  1  2  NA  0.6  0.6  0.6
  1  3  NA  NA   NA   0.0
  2  3  NA  NA   0.4  0.4"
  df <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proja(survey, 0),           fn(df$u, df$v, df$e0))
  expect_equal(proja(survey, eps),         fn(df$u, df$v, df$e1))
  expect_equal(proja(survey, 1 / 3 - eps), fn(df$u, df$v, df$e1))
  expect_equal(proja(survey, 1 / 3),       fn(df$u, df$v, df$e1))
  expect_equal(proja(survey, 1 / 3 + eps), fn(df$u, df$v, df$e2))
  expect_equal(proja(survey, 2 / 3 - eps), fn(df$u, df$v, df$e2))
  expect_equal(proja(survey, 2 / 3),       fn(df$u, df$v, df$e2))
  expect_equal(proja(survey, 2 / 3 + eps), fn(df$u, df$v, df$e3))
  expect_equal(proja(survey, 1 - eps),     fn(df$u, df$v, df$e3))
  expect_equal(proja(survey, 1),           fn(df$u, df$v, df$e3))

  expect_equal(projs(survey, 0),           fn(df$u, df$v, df$e0))
  expect_equal(projs(survey, eps),         fn(df$u, df$v, df$e1))
  expect_equal(projs(survey, 1 / 3 - eps), fn(df$u, df$v, df$e1))
  expect_equal(projs(survey, 1 / 3),       fn(df$u, df$v, df$e1))
  expect_equal(projs(survey, 1 / 3 + eps), fn(df$u, df$v, df$e2))
  expect_equal(projs(survey, 2 / 3 - eps), fn(df$u, df$v, df$e2))
  expect_equal(projs(survey, 2 / 3),       fn(df$u, df$v, df$e2))
  expect_equal(projs(survey, 2 / 3 + eps), fn(df$u, df$v, df$e3))
  expect_equal(projs(survey, 1 - eps),     fn(df$u, df$v, df$e3))
  expect_equal(projs(survey, 1),           fn(df$u, df$v, df$e3))
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
  df <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proja(survey, 0),           fn(df$u, df$v, df$e0))
  expect_equal(proja(survey, eps),         fn(df$u, df$v, df$e1))
  expect_equal(proja(survey, 1 / 6 - eps), fn(df$u, df$v, df$e1))
  expect_equal(proja(survey, 1 / 6),       fn(df$u, df$v, df$e1))
  expect_equal(proja(survey, 1 / 6 + eps), fn(df$u, df$v, df$e2))
  expect_equal(proja(survey, 2 / 6 - eps), fn(df$u, df$v, df$e2))
  expect_equal(proja(survey, 2 / 6),       fn(df$u, df$v, df$e2))
  expect_equal(proja(survey, 2 / 6 + eps), fn(df$u, df$v, df$e3))
  expect_equal(proja(survey, 3 / 6 - eps), fn(df$u, df$v, df$e3))
  expect_equal(proja(survey, 3 / 6),       fn(df$u, df$v, df$e3))
  expect_equal(proja(survey, 3 / 6 + eps), fn(df$u, df$v, df$e4))
  expect_equal(proja(survey, 4 / 6 - eps), fn(df$u, df$v, df$e4))
  expect_equal(proja(survey, 4 / 6),       fn(df$u, df$v, df$e4))
  expect_equal(proja(survey, 4 / 6 + eps), fn(df$u, df$v, df$e5))
  expect_equal(proja(survey, 5 / 6 - eps), fn(df$u, df$v, df$e5))
  expect_equal(proja(survey, 5 / 6),       fn(df$u, df$v, df$e5))
  expect_equal(proja(survey, 5 / 6 + eps), fn(df$u, df$v, df$e6))
  expect_equal(proja(survey, 1 - eps),     fn(df$u, df$v, df$e6))
  expect_equal(proja(survey, 1),           fn(df$u, df$v, df$e6))

  expect_equal(projs(survey, 0),           fn(df$u, df$v, df$e0))
  expect_equal(projs(survey, eps),         fn(df$u, df$v, df$e1))
  expect_equal(projs(survey, 1 / 6 - eps), fn(df$u, df$v, df$e1))
  expect_equal(projs(survey, 1 / 6),       fn(df$u, df$v, df$e1))
  expect_equal(projs(survey, 1 / 6 + eps), fn(df$u, df$v, df$e2))
  expect_equal(projs(survey, 2 / 6 - eps), fn(df$u, df$v, df$e2))
  expect_equal(projs(survey, 2 / 6),       fn(df$u, df$v, df$e2))
  expect_equal(projs(survey, 2 / 6 + eps), fn(df$u, df$v, df$e3))
  expect_equal(projs(survey, 3 / 6 - eps), fn(df$u, df$v, df$e3))
  expect_equal(projs(survey, 3 / 6),       fn(df$u, df$v, df$e3))
  expect_equal(projs(survey, 3 / 6 + eps), fn(df$u, df$v, df$e4))
  expect_equal(projs(survey, 4 / 6 - eps), fn(df$u, df$v, df$e4))
  expect_equal(projs(survey, 4 / 6),       fn(df$u, df$v, df$e4))
  expect_equal(projs(survey, 4 / 6 + eps), fn(df$u, df$v, df$e5))
  expect_equal(projs(survey, 5 / 6 - eps), fn(df$u, df$v, df$e5))
  expect_equal(projs(survey, 5 / 6),       fn(df$u, df$v, df$e5))
  expect_equal(projs(survey, 5 / 6 + eps), fn(df$u, df$v, df$e6))
  expect_equal(projs(survey, 1 - eps),     fn(df$u, df$v, df$e6))
  expect_equal(projs(survey, 1),           fn(df$u, df$v, df$e6))
})
