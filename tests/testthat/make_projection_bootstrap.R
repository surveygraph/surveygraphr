# Consider the following array, representative of survey data,
#
# 1.000000 0.750000 0.000000 1.000000 0.500000
# 0.000000 0.500000 0.750000 0.250000 0.000000
# 0.500000 1.000000 1.000000 0.500000 1.000000
# 0.250000 0.000000 0.250000 0.250000 1.000000
# 0.750000 0.500000 0.500000 0.000000 0.750000
#
# With probability p = 0.6, we retain a given entry of the survey, and with 
# probability 1 - p, we set it to NA. To do this, we generate pseudo-random
# variables using the Mersenne twister mt19937, and sample from
# uniform_real_distribution<double>(0, 1). In order to do so reproducibly, we
# seed the generator with values 0, 1 and 2. This produces the following
# sequences of 25 numbers, respectively.
#
# 0.592845 0.844266 0.857946 0.847252 0.623564
# 0.384382 0.297535 0.056713 0.272656 0.477665
# 0.812169 0.479977 0.392785 0.836079 0.337396
# 0.648172 0.368242 0.957155 0.140351 0.870087
# 0.473608 0.800911 0.520477 0.678880 0.720633
#
# 0.997185 0.932557 0.128124 0.999041 0.236089
# 0.396581 0.387911 0.669746 0.935539 0.846311
# 0.313274 0.524548 0.443453 0.229577 0.534414
# 0.913962 0.457205 0.430699 0.939128 0.778389
# 0.715971 0.802758 0.092801 0.518153 0.865020
#
# 0.185082 0.931541 0.947731 0.484749 0.320536
# 0.154427 0.698863 0.119951 0.485176 0.632738
# 0.818227 0.683026 0.498561 0.586797 0.719754
# 0.258498 0.546207 0.407308 0.176985 0.969632
# 0.297018 0.287869 0.116193 0.181727 0.494290
#
# Applying the threshold p = 0.6 to the original array, we produce the
# following data.
#
# 1.000000 NA       NA       NA       NA 
# 0.000000 0.500000 0.750000 0.250000 0.000000
# NA       1.000000 1.000000 NA       1.000000
# NA       0.000000 NA       0.250000 NA 
# 0.750000 NA       0.500000 NA       NA 
# 
# NA       NA       0.000000 NA       0.500000
# 0.000000 0.500000 NA       NA       NA 
# 0.500000 1.000000 1.000000 0.500000 1.000000
# NA       0.000000 0.250000 NA       NA 
# NA       NA       0.500000 0.000000 NA 
# 
# 1.000000 NA       NA       1.000000 0.500000
# 0.000000 NA       0.750000 0.250000 NA 
# NA       NA       1.000000 0.500000 NA 
# 0.250000 0.000000 0.250000 0.250000 NA 
# 0.750000 0.500000 0.500000 0.000000 0.750000
#
# Computing similarity weights on these arrays, requiring at least one valid 
# comparison yields the following weights, with the corresponding number of 
# valid comparisons.
#
# u v w        w0       c0 w1       c1 w2       c2
# 1 2 0.350000 0.000000 1  NaN      0  0.125000 2
# 1 3 0.450000 NaN      0  0.250000 2  0.500000 3
# 1 4 0.400000 NaN      0  0.750000 1  0.250000 4
# 1 5 0.550000 0.750000 1  0.500000 1  0.500000 5
# 2 3 0.500000 0.416666 3  0.500000 2  0.750000 3
# 2 4 0.550000 0.750000 2  0.500000 1  0.750000 4
# 2 5 0.600000 0.500000 2  NaN      0  0.583333 5
# 3 4 0.550000 0.000000 1  0.125000 2  0.500000 4
# 3 5 0.600000 0.500000 1  0.500000 2  0.500000 5
# 4 5 0.650000 NaN      0  0.750000 1  0.625000 5
#
# Finally, when bootstrapping in make_projection(), we return a weighted edge
# list, where the weight is the mean of the weights in each realisation in which
# it appeared with a finite value. We include an additional column with the
# frequency with which the edge occured, as a fraction.


survey <- data.frame(
  c(1.000000, 0.000000, 0.500000, 0.250000, 0.750000),
  c(0.750000, 0.500000, 1.000000, 0.000000, 0.500000),
  c(0.000000, 0.750000, 1.000000, 0.250000, 0.500000),
  c(1.000000, 0.250000, 0.500000, 0.250000, 0.000000),
  c(0.500000, 0.000000, 1.000000, 1.000000, 0.750000)
)

eps <- 1e-5

datafn <- function(x){
	df <- data.frame(
		u = as.integer(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)),
		v = as.integer(c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5)),
		weight = as.numeric(x)
	)
	df <- df[!is.na(df$weight),]
	if(nrow(df) > 0) rownames(df) <- 1:nrow(df)
	df
}

datafreqfn <- function(x, y){
	df <- data.frame(
		u = as.integer(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)),
		v = as.integer(c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5)),
		weight = as.numeric(x),
		freq = as.numeric(y)
	)
	df <- df[!is.na(df$weight),]
	if(nrow(df) > 0) rownames(df) <- 1:nrow(df)
	df
}

test_that("`bootseed` behaves as expected", {
  proj <- function(x){
    make_projection(
      survey,
      method = "s",
      methodval = -1,
      comparisons = 1,
      bootval = 0.6,
      bootseed = x
    ) 
  }

	weights <- "
  w0       w1       w2       w3       f3       w4       f4       w5       f5       w6       f6
  0.000000 NA       0.125000 0.000000 0.500000 0.062500 1.000000 0.125000 0.500000 0.062500 0.666667
  NA       0.250000 0.500000 0.250000 0.500000 0.500000 0.500000 0.375000 1.000000 0.375000 0.666667
  NA       0.750000 0.250000 0.750000 0.500000 0.250000 0.500000 0.500000 1.000000 0.500000 0.666667
  0.750000 0.500000 0.500000 0.625000 1.000000 0.625000 1.000000 0.500000 1.000000 0.583333 1.000000
  0.416667 0.500000 0.750000 0.458333 1.000000 0.583333 1.000000 0.625000 1.000000 0.555556 1.000000
  0.750000 0.500000 0.750000 0.625000 1.000000 0.750000 1.000000 0.625000 1.000000 0.666667 1.000000
  0.500000 NA       0.583333 0.500000 0.500000 0.541667 1.000000 0.583333 0.500000 0.541667 0.666667
  0.000000 0.125000 0.500000 0.062500 1.000000 0.250000 1.000000 0.312500 1.000000 0.208333 1.000000
  0.500000 0.500000 0.500000 0.500000 1.000000 0.500000 1.000000 0.500000 1.000000 0.500000 1.000000
  NA       0.750000 0.625000 0.750000 0.500000 0.625000 0.500000 0.687500 1.000000 0.687500 0.666667"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

  expect_equal(proj(0), datafn(data$w0), tolerance = eps)
  expect_equal(proj(1), datafn(data$w1), tolerance = eps)
  expect_equal(proj(2), datafn(data$w2), tolerance = eps)
  expect_equal(proj(c(0, 1)), datafreqfn(data$w3, data$f3), tolerance = eps)
  expect_equal(proj(c(0, 2)), datafreqfn(data$w4, data$f4), tolerance = eps)
  expect_equal(proj(c(1, 2)), datafreqfn(data$w5, data$f5), tolerance = eps)
  expect_equal(proj(c(0, 1, 2)), datafreqfn(data$w6, data$f6), tolerance = eps)
})


test_that("`bootval` behaving as expected", {
  proj <- function(x){
    make_projection(
      survey,
      method = "s",
      methodval = -1,
      comparisons = 1,
      bootval = x,
      bootseed = 0
    ) 
  }

	weights <- "
	w0       w1       w2       w3       w4       w5       w6       w7       w8       w9       w10     
	NA       NA       NA       NA       NA       NA       0.000000 0.250000 0.250000 0.350000 0.350000
	NA       NA       NA       NA       NA       NA       NA       0.500000 0.500000 0.450000 0.450000
	NA       NA       NA       NA       NA       NA       NA       0.250000 0.250000 0.312500 0.400000
	NA       NA       NA       NA       NA       NA       0.750000 0.750000 0.750000 0.550000 0.550000
	NA       NA       NA       NA       0.750000 0.416667 0.416667 0.416667 0.416667 0.500000 0.500000
	NA       NA       NA       1.000000 0.750000 0.750000 0.750000 0.750000 0.750000 0.562500 0.550000
	NA       NA       NA       NA       NA       0.250000 0.500000 0.583333 0.500000 0.600000 0.600000
	NA       NA       NA       NA       NA       0.000000 0.000000 0.000000 0.000000 0.625000 0.550000
	NA       NA       NA       NA       NA       NA       0.500000 0.500000 0.625000 0.600000 0.600000
	NA       NA       NA       NA       NA       NA       NA       0.625000 0.625000 0.625000 0.650000"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

	expect_equal(proj(0.0), datafn(data$w0), tolerance = eps)
	expect_equal(proj(0.1), datafn(data$w1), tolerance = eps)
	expect_equal(proj(0.2), datafn(data$w2), tolerance = eps)
	expect_equal(proj(0.3), datafn(data$w3), tolerance = eps)
	expect_equal(proj(0.4), datafn(data$w4), tolerance = eps)
	expect_equal(proj(0.5), datafn(data$w5), tolerance = eps)
	expect_equal(proj(0.6), datafn(data$w6), tolerance = eps)
	expect_equal(proj(0.7), datafn(data$w7), tolerance = eps)
	expect_equal(proj(0.8), datafn(data$w8), tolerance = eps)
	expect_equal(proj(0.9), datafn(data$w9), tolerance = eps)
	expect_equal(proj(1.0), datafn(data$w10), tolerance = eps)
})


test_that("`comparisons` working as expected when data is bootstrapped", {
  proj <- function(x, y){
    make_projection(
      survey,
      method = "s",
      methodval = -1,
      comparisons = y,
      bootval = 0.75,
      bootseed = x
    ) 
  }

	weights <- "
	w01      w02      w03      w04      w05      w11      w12      w13      w14      w15      w21      w22      w23      w24      w25
	0.250000 0.250000 NA       NA       NA       0.250000 NA       NA       NA       NA       0.250000 0.250000 0.250000 NA       NA
	0.500000 NA       NA       NA       NA       0.250000 0.250000 NA       NA       NA       0.500000 0.500000 NA       NA       NA
	0.250000 NA       NA       NA       NA       0.750000 NA       NA       NA       NA       0.250000 0.250000 NA       NA       NA
	0.750000 0.750000 NA       NA       NA       0.500000 NA       NA       NA       NA       0.500000 0.500000 0.500000 NA       NA
	0.416667 0.416667 0.416667 NA       NA       0.583333 0.583333 0.583333 NA       NA       0.500000 0.500000 0.500000 0.500000 NA
	0.750000 0.750000 0.750000 NA       NA       0.500000 0.500000 NA       NA       NA       0.687500 0.687500 0.687500 0.687500 NA
	0.500000 0.500000 0.500000 0.500000 NA       0.500000 0.500000 NA       NA       NA       0.600000 0.600000 0.600000 0.600000 0.600000
	0.000000 NA       NA       NA       NA       0.125000 0.125000 NA       NA       NA       0.333333 0.333333 0.333333 NA       NA
	0.625000 0.625000 NA       NA       NA       0.583333 0.583333 0.583333 NA       NA       0.562500 0.562500 0.562500 0.562500 NA
	0.625000 0.625000 NA       NA       NA       0.750000 NA       NA       NA       NA       0.625000 0.625000 0.625000 0.625000 NA"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

 	expect_equal(proj(0, 1), datafn(data$w01), tolerance = eps)
 	expect_equal(proj(0, 2), datafn(data$w02), tolerance = eps)
 	expect_equal(proj(0, 3), datafn(data$w03), tolerance = eps)
 	expect_equal(proj(0, 4), datafn(data$w04), tolerance = eps)
 	expect_equal(proj(0, 5), datafn(data$w05), tolerance = eps)
 	expect_equal(proj(1, 1), datafn(data$w11), tolerance = eps)
 	expect_equal(proj(1, 2), datafn(data$w12), tolerance = eps)
 	expect_equal(proj(1, 3), datafn(data$w13), tolerance = eps)
 	expect_equal(proj(1, 4), datafn(data$w14), tolerance = eps)
 	expect_equal(proj(1, 5), datafn(data$w15), tolerance = eps)
 	expect_equal(proj(2, 1), datafn(data$w21), tolerance = eps)
 	expect_equal(proj(2, 2), datafn(data$w22), tolerance = eps)
 	expect_equal(proj(2, 3), datafn(data$w23), tolerance = eps)
 	expect_equal(proj(2, 4), datafn(data$w24), tolerance = eps)
 	expect_equal(proj(2, 5), datafn(data$w25), tolerance = eps)
})


test_that("A couple of bootstrapping sanity checks.", {
  proj <- function(x){
    make_projection(
      survey,
      method = "s",
      methodval = -1,
      comparisons = 1,
			bootreps = 100,
      bootval = x
    ) 
  }

	weights <- "
	w0       f0       w1       f1 
	NA       NA       0.350000 1.000000
	NA       NA       0.450000 1.000000
	NA       NA       0.400000 1.000000
	NA       NA       0.550000 1.000000
	NA       NA       0.500000 1.000000
	NA       NA       0.550000 1.000000
	NA       NA       0.600000 1.000000
	NA       NA       0.550000 1.000000
	NA       NA       0.600000 1.000000
	NA       NA       0.650000 1.000000"
	data <- read.table(text = weights, header = TRUE, na.strings = "NA")

	expect_equal(proj(0), datafreqfn(data$w0, data$f0), tolerance = eps)
	expect_equal(proj(1), datafreqfn(data$w1, data$f1), tolerance = eps)
})
