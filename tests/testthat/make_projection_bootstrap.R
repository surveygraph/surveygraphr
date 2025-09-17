# Seeding the Mersenne twister mt19937 with 0, 1 and 2, and sampling from
# uniform_real_distribution<double>(0, 1) 25 times each produces the following
# arrays, respectively.
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
# We now consider the following array, representative of survey data,
#
# 1.000000 0.750000 0.000000 1.000000 0.500000
# 0.000000 0.500000 0.750000 0.250000 0.000000
# 0.500000 1.000000 1.000000 0.500000 1.000000

# 0.250000 0.000000 0.250000 0.250000 1.000000
# 0.750000 0.500000 0.500000 0.000000 0.750000
#
# With probability p = 0.6, we retain a given entry of the survey, and with 
# probability 1 - p, we set it to nan. We use the random values above to do
# this, for three independent resampling instances. This produces the 
# following data.
#
# 1.000000 nan      nan      nan      nan
# 0.000000 0.500000 0.750000 0.250000 0.000000
# nan      1.000000 1.000000 nan      1.000000
# nan      0.000000 nan      0.250000 nan
# 0.750000 nan      0.500000 nan      nan
# 
# nan      nan      0.000000 nan      0.500000
# 0.000000 0.500000 nan      nan      nan
# 0.500000 1.000000 1.000000 0.500000 1.000000
# nan      0.000000 0.250000 nan      nan
# nan      nan      0.500000 0.000000 nan
# 
# 1.000000 nan      nan      1.000000 0.500000
# 0.000000 nan      0.750000 0.250000 nan
# nan      nan      1.000000 0.500000 nan
# 0.250000 0.000000 0.250000 0.250000 nan
# 0.750000 0.500000 0.500000 0.000000 0.750000
#
# Computing similarity weights on these arrays, requiring at least one valid 
# comparison yields the following weights, with the corresponding number of 
# actual valid comparisons.
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

df <- data.frame(
  c(1.000000, 0.000000, 0.500000, 0.250000, 0.750000),
  c(0.750000, 0.500000, 1.000000, 0.000000, 0.500000),
  c(0.000000, 0.750000, 1.000000, 0.250000, 0.500000),
  c(1.000000, 0.250000, 0.500000, 0.250000, 0.000000),
  c(0.500000, 0.000000, 1.000000, 1.000000, 0.750000)
)

test_that("`bootreps` behave as expected", {

  proj <- function(a, b, c, d){
    make_projection(
      df,
      method = "s",
      methodval = -1,
      bootreps = a,
      bootval = b,
      bootseed = 1,
      mincompare = 1
    ) 
  }

  e1 <- data.frame(
    u = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4),
    v = c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5),
    weight = c(0.350000, 0.450000, 0.400000, 0.550000, 0.500000, 0.550000, 0.600000, 0.550000, 0.600000, 0.650000)
  )

  e2 <- data.frame(
    u = c(1, 1, 2, 2, 2, 3, 3),
    v = c(2, 5, 3, 4, 5, 4, 5),
    weight = c(0.000000, 0.750000, 0.416666, 0.750000, 0.500000, 0.000000, 0.50000)
  )

  e3 <- data.frame(
    u = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4),
    v = c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5),
    weight = c(0.0000000, 0.2500000, 0.7500000, 0.6250000, 0.4583333, 0.6250000, 0.5000000, 0.0625000, 0.5000000, 0.7500000),
		freq = c(0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0, 0.5)
  )


  e4 <- data.frame(
    u = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4),
    v = c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5),
    weight = c(0.0625000, 0.3750000, 0.5000000, 0.5833333, 0.5555556, 0.6666667, 0.5416667, 0.2083333, 0.5000000, 0.6875000),
    freq = c(0.6666667, 0.6666667, 0.6666667, 1.0000000, 1.0000000, 1.0000000, 0.6666667, 1.0000000, 1.0000000, 0.6666667)
  )

  expect_equal(
    proj(1, 1),
    e1,
    tolerance = 1e-5
  )

  expect_equal(
    proj(1, 0.6),
    e2,
    tolerance = 1e-5
  )

  expect_equal(
    proj(2, 0.6),
    e3,
    tolerance = 1e-5
  )

  expect_equal(
    proj(3, 0.6),
    e4,
    tolerance = 1e-5
  )
})




