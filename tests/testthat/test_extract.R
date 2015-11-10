test_that("non-numeric values throw an error, but NA's are ignored", {
	expect_equal(check_numeric_values("testcol", c(1, 2, 3)), c(1, 2, 3))
	expect_equal(check_numeric_values("testcol", c(1, 2, NA)), c(1, 2, NA))
	expect_equal(check_numeric_values("testcol", c(NA, NA, NA)), c(NA, NA, NA))
	expect_error(check_numeric_values("testcol", c(1, 2, "X")))
})