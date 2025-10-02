# TODO: errors and warnings should point to relevant documentation

# epsilon value for numerical thresholds
eps <- 1e-6


test_that("Unused arguments.", {
  expect_warning(
    make_synthetic_data(1, 1, argname1 = 1, argname2 = 1),
    regexp = "Unused arguments in ...: argname1, argname2."
  )
})


test_that("`nrow` argument is correctly supplied.", {
  expect_error(
    make_synthetic_data(TRUE, 0),
    regexp = "`nrow` must be a non-negative integer."
  )

  expect_error(
    make_synthetic_data(numeric(), 0),
    regexp = "`nrow` must be of length 1."
  )

  expect_error(
    make_synthetic_data(c(1, 2), 0),
    regexp = "`nrow` must be of length 1."
  )

  expect_error(
    make_synthetic_data(as.integer(NA), 0),
    regexp = "`nrow` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(NaN, 0),
    regexp = "`nrow` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(Inf, 0),
    regexp = "`nrow` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(-1, 0),
    regexp = "`nrow` must be a non-negative integer."
  )

  expect_error(
    make_synthetic_data(1.1, 0),
    regexp = "`nrow` must be a non-negative integer."
  )
})


test_that("`ncol` argument is correctly supplied.", {
  expect_error(
    make_synthetic_data(0, TRUE),
    regexp = "`ncol` must be a non-negative integer."
  )

  expect_error(
    make_synthetic_data(0,numeric()),
    regexp = "`ncol` must be of length 1."
  )

  expect_error(
    make_synthetic_data(0, c(1, 2)),
    regexp = "`ncol` must be of length 1."
  )

  expect_error(
    make_synthetic_data(0, as.integer(NA)),
    regexp = "`ncol` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, NaN),
    regexp = "`ncol` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, Inf),
    regexp = "`ncol` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, -1),
    regexp = "`ncol` must be a non-negative integer."
  )

  expect_error(
    make_synthetic_data(0, 1.1),
    regexp = "`ncol` must be a non-negative integer."
  )
})


test_that("`minority` argument is correctly supplied.", {
  expect_error(
    make_synthetic_data(1, 1, minority = TRUE),
    regexp = "`minority` must be between 0 and 0.5, inclusive."
  )

  expect_error(
    make_synthetic_data(1, 1, minority = numeric()),
    regexp = "`minority` must be of length 1."
  )

  expect_error(
    make_synthetic_data(1, 1, minority = c(0, 1)),
    regexp = "`minority` must be of length 1."
  )

  expect_error(
    make_synthetic_data(1, 1, minority = as.numeric(NA)),
    regexp = "`minority` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, minority = NaN),
    regexp = "`minority` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, minority = Inf),
    regexp = "`minority` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, minority = -eps),
    regexp = "`minority` must be between 0 and 0.5, inclusive."
  )

  expect_error(
    make_synthetic_data(1, 1, minority = 1 + eps),
    regexp = "`minority` must be between 0 and 0.5, inclusive."
  )

  expect_warning(
    make_synthetic_data(1, 1, minority = 0.5 + eps),
    regexp = "`minority` must be between 0 and 0.5, inclusive; taking 1 - minority."
  )
})


test_that("`correlation` argument is correctly supplied.", {
  expect_error(
    make_synthetic_data(1, 1, correlation = TRUE),
    regexp = "`correlation` must be between 0 and 1, inclusive."
  )

  expect_error(
    make_synthetic_data(1, 1, correlation = numeric()),
    regexp = "`correlation` must be of length 1."
  )

  expect_error(
    make_synthetic_data(1, 1, correlation = c(0, 1)),
    regexp = "`correlation` must be of length 1."
  )

  expect_error(
    make_synthetic_data(1, 1, correlation = as.numeric(NA)),
    regexp = "`correlation` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, correlation = NaN),
    regexp = "`correlation` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, correlation = Inf),
    regexp = "`correlation` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, correlation = -eps),
    regexp = "`correlation` must be between 0 and 1, inclusive."
  )

  expect_error(
    make_synthetic_data(1, 1, correlation = 1 + eps),
    regexp = "`correlation` must be between 0 and 1, inclusive."
  )
})


test_that("`polarisation` argument is correctly supplied.", {
  expect_error(
    make_synthetic_data(1, 1, polarisation = TRUE),
    regexp = "`polarisation` must be between 0 and 1, inclusive."
  )

  expect_error(
    make_synthetic_data(1, 1, polarisation = numeric()),
    regexp = "`polarisation` must be of length 1."
  )

  expect_error(
    make_synthetic_data(1, 1, polarisation = c(0, 1)),
    regexp = "`polarisation` must be of length 1."
  )

  expect_error(
    make_synthetic_data(1, 1, polarisation = as.numeric(NA)),
    regexp = "`polarisation` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, polarisation = NaN),
    regexp = "`polarisation` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, polarisation = Inf),
    regexp = "`polarisation` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(1, 1, polarisation = -eps),
    regexp = "`polarisation` must be between 0 and 1, inclusive."
  )

  expect_error(
    make_synthetic_data(1, 1, polarisation = 1 + eps),
    regexp = "`polarisation` must be between 0 and 1, inclusive."
  )
})


test_that("`likert` argument is correctly supplied.", {
  expect_error(
    make_synthetic_data(0, 0, likert = TRUE),
    regexp = "`likert` must be a positive integer."
  )

  expect_error(
    make_synthetic_data(0, 0, likert = numeric()),
    regexp = "`likert` must be of length 1."
  )

  expect_error(
    make_synthetic_data(0, 0, likert = c(1, 2)),
    regexp = "`likert` must be of length 1."
  )

  expect_error(
    make_synthetic_data(0, 0, likert = as.integer(NA)),
    regexp = "`likert` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, 0, likert = NaN),
    regexp = "`likert` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, 0, likert = Inf),
    regexp = "`likert` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, 0, likert = -1),
    regexp = "`likert` must be a positive integer."
  )

  expect_error(
    make_synthetic_data(0, 0, likert = 0),
    regexp = "`likert` must be a positive integer."
  )

  expect_error(
    make_synthetic_data(0, 0, likert = 1.1),
    regexp = "`likert` must be a positive integer."
  )

  expect_warning(
    make_synthetic_data(0, 0, scale = 1),
    regexp = "`scale` is deprecated and will be removed in future versions; use `likert`."
  )
})


test_that("`seed` argument is correctly supplied.", {
  expect_error(
    make_synthetic_data(0, 0, seed = TRUE),
    regexp = "`seed` must be an integer."
  )

  expect_error(
    make_synthetic_data(0, 0, seed = numeric()),
    regexp = "`seed` must be of length 1."
  )

  expect_error(
    make_synthetic_data(0, 0, seed = c(1, 2)),
    regexp = "`seed` must be of length 1."
  )

  expect_error(
    make_synthetic_data(0, 0, seed = as.integer(NA)),
    regexp = "`seed` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, 0, seed = NaN),
    regexp = "`seed` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, 0, seed = Inf),
    regexp = "`seed` must be finite (not NA, NaN, Inf or -Inf).", fixed = T
  )

  expect_error(
    make_synthetic_data(0, 0, seed = 1.1),
    regexp = "`seed` must be an integer."
  )
})
