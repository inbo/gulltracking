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

test_that("year, month and hour columns are added", {
	data <- copy(fixture_data_table)
	add_year_month_hour(data)
	expected_years <- rep(2014, 8)
	expected_months <- rep(1, 8)
	expected_hours <- rep(10, 8)
	add_year_month_hour(data)
	expect_equal(data$inbo_year, expected_years)
	expect_equal(data$inbo_month, expected_months)
	expect_equal(data$inbo_hour, expected_hours)
})

test_that("enrich can calculate diffs in date_time", {
	fixt_expected_diff_col <- c(NA, 120, 120, 120, 360, NA, 120, 240)
	setkey(fixture_data_table, device_info_serial, date_time)
	add_time_since_previous_fix(fixture_data_table)
	expect_equal(as.numeric(fixture_data_table$inbo_time_diff), fixt_expected_diff_col)
})

test_that("distances between consecutive points are calculated for each device", {
	data <- data.table(
		device_info_serial=c(1, 1, 2, 2, 3, 3),
		latitude=c(1, 2, 1, 2, 3, 3),
		longitude=c(1, 2, 1, 2, 3, 3)
	)
	expected_distances = c(NA, 157401.5610458, NA, 157401.5610458, NA, 0)
	add_dist_travelled(data)
	expect_equal(data$inbo_distance_diff, expected_distances)
})

test_that("speed is calculated based on time diffs and distances", {
	test_data <- data.table(
		inbo_time_diff=as.difftime(c(1200, 1800, 3000), units="secs"),
		inbo_distance_diff=c(2000, 3000, 5000)
	)
	expected_speed <- c(10/6, 10/6, 10/6)
	add_speed(test_data)
	expect_equal(test_data$inbo_speed_2d, expected_speed)
})

test_that("distance to colony is calculated", {
	data <- data.table(
		latitude=c(1, 2, 1, 2, 1, 1),
		longitude=c(1, 2, 1, 2, 1, 1),
		colony_latitude=c(2, 1, 2, 1, 2, 2),
		colony_longitude=c(2, 1, 2, 1, 2, 2)
	)
	expected_distances <- c(157401.5610458, 157401.5610458, 157401.5610458,
													157401.5610458, 157401.5610458, 157401.5610458
	)
	add_dist_to_colony(data)
	expect_equal(data$inbo_distance_to_colony, expected_distances)
})

test_that("flag_outliers returns FALSE if everything is ok", {
	fixture_tracking_data[, inbo_speed_2d:=c(0, 1, 2, 33, rep(33, 96))]
	fixture_tracking_data[, altitude:=c(0, 1, 2, 10000, rep(938, 96))]
	fixture_tracking_data[, h_accuracy:=c(0, 1, 2, 1000, rep(333, 96))]
	flag_outliers(fixture_tracking_data)
	expect_equal(sum(fixture_tracking_data$inbo_outlier), 0)
})

test_that("flag_outliers flags records if they fail certain checks", {
	# speed_2d should be < 33.33333
	error_data <- copy(fixture_tracking_data)
	error_data[, inbo_speed_2d:=c(0, 1, 2, 34, rep(33, 96))]
	flag_outliers(error_data)
	expect_equal(sum(error_data$inbo_outlier), 1)
	
	# speed_2d should be >= 0
	error_data <- copy(fixture_tracking_data)
	error_data[, inbo_speed_2d:=c(0, 1, 2, -1, rep(33, 96))]
	flag_outliers(error_data)
	expect_equal(sum(error_data$inbo_outlier), 1)
	
	# altitude should be < 10000
	error_data <- copy(fixture_tracking_data)
	error_data[, altitude:=c(0, 1, 2, 10001, rep(80, 96))]
	flag_outliers(error_data)
	expect_equal(sum(error_data$inbo_outlier), 1)
	
	# h_accuracy should be < 1000
	error_data <- copy(fixture_tracking_data)
	error_data[, h_accuracy:=c(0, 1, 2, 1001, rep(80, 96))]
	flag_outliers(error_data)
	expect_equal(sum(error_data$inbo_outlier), 1)
})
