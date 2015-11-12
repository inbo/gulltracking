# -------------------------------------------- # 
#      Fixture data
# -------------------------------------------- # 
fixture_tracking_data <- tracks_validated # from package data
fixture_bird_data <- birds_validated # from package data
fixture_data_table <- data.table(
	device_info_serial=c(2, 2, 2, 2, 5, 5, 2, 5),
	date_time=c(2, 3, 4, 1, 2, 6, 4, 3)
)


# -------------------------------------------- # 
#      Tests
# -------------------------------------------- # 
test_that("tracking data and bird metadata can be joined in 1 data table", {
  joined <- join_tracks_and_metadata(fixture_tracking_data, fixture_bird_data)
	expect_equal(length(joined), length(colnames(fixture_bird_data)) +
								length(colnames(fixture_tracking_data)) - 1)
	expect_equal(length(joined$device_info_serial),
							 length(fixture_tracking_data$device_info_serial))
})

test_that("joining stops when tracking records cannot be joined with bird metadata", {
	error_data <- copy(fixture_tracking_data)
	l <- length(error_data$device_info_serial)
	error_data[, device_info_serial:=c(rep(847, l-1), 9999)]
	expect_error(join_tracks_and_metadata(error_data, fixture_bird_data))
})

test_that("deleting test records works", {
	joined <- join_tracks_and_metadata(fixture_tracking_data, fixture_bird_data)
	result <- delete_test_records(joined)
	expect_equal(length(result$date_time), 18) # manually checked this. Only 18 records should be returned
})

test_that("enrich can calculate diffs in date_time", {
	fixt_expected_diff_col <- c(NA, 1, 1, 1, 0, NA, 1, 3)
	result <- add_time_since_previous_fix(fixture_data_table)
	expect_equal(result$time_diff, fixt_expected_diff_col)
})
