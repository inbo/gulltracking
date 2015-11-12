library(lubridate)

# -------------------------------------------- # 
#      Fixture data
# -------------------------------------------- # 
fixture_tracking_data <- tracks_validated # from package data
fixture_bird_data <- birds_validated # from package data
fixture_data_table <- data.table(
	device_info_serial=c(2, 2, 2, 2, 5, 5, 2, 5),
	date_time=ymd_hms(c("2014-01-01 10:00:00", "2014-01-01 10:02:00",
											"2014-01-01 10:04:00", "2014-01-01 10:06:00",
											"2014-01-01 10:08:00", "2014-01-01 10:10:00",
											"2014-01-01 10:12:00", "2014-01-01 10:14:00")
								)
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
	fixt_expected_diff_col <- c(NA, 120, 120, 120, 360, NA, 120, 240)
	setkey(fixture_data_table, device_info_serial, date_time)
	add_time_since_previous_fix(fixture_data_table)
	expect_equal(as.numeric(fixture_data_table$time_diff), fixt_expected_diff_col)
})

test_that("distances between consecutive points are calculated for each device", {
	data <- data.table(
		device_info_serial=c(1, 1, 2, 2, 3, 3),
		latitude=c(1, 2, 1, 2, 3, 3),
		longitude=c(1, 2, 1, 2, 3, 3)
	)
	expected_distances = c(NA, 157401.5610458, NA, 157401.5610458, NA, 0)
	add_dist_travelled(data)
	expect_equal(data$distance, expected_distances)
})

test_that("speed is calculated based on time diffs and distances", {
	test_data <- data.table(
		time_diff=as.difftime(c(1200, 1800, 3000), units="secs"),
		distance=c(2000, 3000, 5000)
	)
	expected_speed <- c(6, 6, 6)
	add_speed(test_data)
	expect_equal(test_data$speed_km_h, expected_speed)
	
})
