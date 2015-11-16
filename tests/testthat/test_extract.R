# -------------------------------------------- # 
#      Fixture data
# -------------------------------------------- # 
fixture_tracking_data <- data.table(
	device_info_serial=c("847", "322", "12"),
	date_time=c("2013-05-31 16:31:31", "2013-05-31 16:32:34", "2013-05-31 16:33:37"),
	latitude=c("51.3416", "51.3413", "51.3244"),
	longitude=c("3.1738", "3.1734", "3.17487"),
	altitude=c("10", "11", "2"),
	pressure=c("1930", "3839", "222"),
	temperature=c("12", "10.4", "23"),
	satellites_used=c("3", "4", "4"),
	gps_fixtime=c("31.323", "232.424", "32.3239"),
	positiondop=c("4.424", "5.324", "8.32183"),
	h_accuracy=c("8.42", "0.3723", "32.33"),
	v_accuracy=c("8.42", "0.3723", "32.33"),
	x_speed=c("8.42", "0.3723", "32.33"),
	y_speed=c("8.42", "0.3723", "32.33"),
	z_speed=c("8.42", "0.3723", "32.33"),
	speed_accuracy=c("8.42", "0.3723", "32.33"),
	location=c("84KSE89SNF84HQ", "84KSE89SNF84HQ", "84KSE89SNF84HQ"),
	userflag=c("0", "1", "1"),
	speed_3d=c("8.42", "0.3723", "32.33"),
	speed_2d=c("8.42", "0.3723", "32.33"),
	direction=c("8.42", "0.3723", "32.33"),
	altitude_agl=c("8.42", "0.3723", "32.33")
)

fixture_bird_data <- data.table(
	device_info_serial=c("783", "328", "2"),
	ring_code=c("32J", "2DZ", "238H"),
	color_ring_code=c("HDUE", "JDUS", "DUS"),
	species=c("Larus fuscus", "Larus fuscus", "Larus argentatus"),
	sex=c("male", "female", "female"),
	weight_in_g=c("738", "481", "1953"),
	tracking_start_date_time=c("2013-05-27 18:00:00",
														 "2013-05-27 18:00:00",
														 "2013-05-27 20:00:00"),
	tracking_end_date_time=c("2013-05-27 18:00:00",
													 "2013-05-27 18:00:00",
													 "2013-05-27 20:00:00"),
	colony_latitude=c("51.3493", "51.334", "51.356"),
	colony_longitude=c("2.593", "3.2113", "2.904"),
	remarks=c("", "", "nothing to say"),
	cartodb_id=c("doesn't matter", "", ""),
	created_at=c("2013-05-27 18:00:00",
							 "2013-05-27 18:00:00",
							 "2013-05-27 20:00:00"),
	updated_at=c("2013-05-27 18:00:00",
							 "2013-05-27 18:00:00",
							 "2013-05-27 20:00:00"),
	the_geom=c("IQE9JF93Q3JFIQ", "3IQH83F3NQI", "EIHQ83NQICE"),
	bird_name=c("Wilma", "Hilbran", "Joke"),
	colony_name=c("Vismijn", "APM", "APM"),
	colony_location=c("Oostende", "Zeebrugge", "Zeebrugge")
)

# -------------------------------------------- # 
#      Tests
# -------------------------------------------- # 
test_that("non-numeric values throw an error, but NA's are ignored", {
	expect_equal(check_numeric_values("testcol", c(1, 2, 3)), c(1, 2, 3))
	expect_equal(check_numeric_values("testcol", c(1, 2, NA)), c(1, 2, NA))
	expect_equal(check_numeric_values("testcol", c(NA, NA, NA)), c(NA, NA, NA))
	expect_error(check_numeric_values("testcol", c(1, 2, "X")))
})

test_that("bird tracking validation returns data if no errors are found", {
  expect_is(validate_tracks_data(fixture_tracking_data), "data.table")
})

test_that("bird tracking validation stops if an error is found", {
	# skip("too much output") # DELETE LATER
	error_data <- copy(fixture_tracking_data)
	error_data$date_time <- c("2013-05-31 16:31:31",
														"2013-05-31 16:32:34",
														"31/05/2013 16:33:37") # uh oh!
	expect_error(validate_tracks_data(error_data))
	error_data <- copy(fixture_tracking_data)
	error_data$altitude <- c("4", "5", "a")
	expect_error(validate_tracks_data(error_data))
})

test_that("bird metadata validation returns data if no errors are found", {
	expect_is(validate_bird_data(fixture_bird_data), "data.table")
})

test_that("bird metadata validation stops if an error is found", {
	error_data <- copy(fixture_bird_data)
	error_data$device_info_serial <- c("a", "b", "1")
	expect_error(validate_bird_data(error_data))
	error_data <- copy(fixture_bird_data)
	error_data$created_at <- c("2013-05-31 16:31:31",
														 "2013-05-31 16:32:34",
														 "2014/04/11 16:33:37")
	expect_error(validate_bird_data(error_data))
	error_data <- copy(fixture_bird_data)
	error_data$sex <- c("male", "female", "unknown")
	expect_error(validate_bird_data(error_data))
	error_data <- copy(fixture_bird_data)
	error_data$species <- c("Larus fuscus", "Larus fuscus", "unknown")
	expect_error(validate_bird_data(error_data))
})