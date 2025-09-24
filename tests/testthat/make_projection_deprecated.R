# Test for deprecation messages for old argument names in make_projection().
test_that("Deprecated argument names for make_projection()", {
	expect_warning(
    make_projection(data.frame(1), threshold_method = "raw_similarity"),
		regexp = "`threshold_method` argument is deprecated and will be removed in future versions; use `method`."
	)

	expect_warning(
    make_projection(data.frame(1), method_value = 0.5),
		regexp = "`method_value` argument is deprecated and will be removed future versions; use `methodval`."
	)

	expect_warning(
    make_projection(data.frame(1), similarity_metric = "Manhattan"),
		regexp = "`similarity_metric` argument is deprecated and will be removed in future versions; use `methodval`."
	)

	expect_warning(
    make_projection(data.frame(1), centre = 1),
		regexp = "`centre` argument is deprecated; outputting edge weights in range 0 to 1."
	)
})

# TODO: also need to output deprecation warnings for old option names
# target_lcc
# target_ad
# raw_similarity
