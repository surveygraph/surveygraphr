# Test for deprecation messages for old argument names in make_projection().
test_that("Deprecated argument names for make_projection()", {
	expect_warning(
    make_projection(data.frame(1), threshold_method = "raw_similarity"),
		regexp = "Argument `threshold_method` is deprecated and will be removed in future versions; use `method` instead."
	)

	expect_warning(
    make_projection(data.frame(1), method_value = 0.5),
		regexp = "Argument `method_value` is deprecated and will be removed future versions; use `methodval` instead."
	)

	expect_warning(
    make_projection(data.frame(1), similarity_metric = "Manhattan"),
		regexp = "Argument `similarity_metric` is deprecated and will be removed in future versions; use `methodval` instead."
	)

	expect_warning(
    make_projection(data.frame(1), centre = 1),
		regexp = "Argument `centre` is deprecated; outputting edge weights in range 0 to 1."
	)
})

# TODO: also need to output deprecation warnings for old option names
# target_lcc
# target_ad
# raw_similarity
