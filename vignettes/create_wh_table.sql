CREATE TABLE tracking_warehouse (
  id integer primary key,
  project_leader varchar(50),
  device_info_serial varchar(20),
  bird_name varchar(50),
  ring_code varchar(20),
  colour_ring_code varchar(20),
  species_code varchar(20),
  scientific_name varchar(100),
  catch_weight integer,
  sex varchar(10),
  catch_location varchar(50),
  colony_latitude double precision,
  colony_longitude double precision,
  tracking_started_at timestamp with time zone,
  tracking_ended_at timestamp with time zone,
  is_active boolean,
  date_time timestamp with time zone,
  latitude double precision,
  longitude double precision,
  altitude double precision,
  pressure double precision,
  temperature double precision,
  satellites_used integer,
  gps_fixtime real,
  positiondop real,
  h_accuracy real,
  v_accuracy real,
  x_speed real,
  y_speed real,
  z_speed real,
  speed_accuracy real,
  userflag boolean,
  speed_3d double precision,
  speed_2d double precision,
  direction double precision,
  altitude_agl integer,
  calc_time_diff integer,
  calc_distance_diff real,
  calc_speed_2d double precision,
  calc_distance_to_colony real,
  calc_sunlight boolean,
  calc_outlier boolean,
  calc_corine_category integer
);

-- create indices
CREATE INDEX device_info_serial_index on tracking_warehouse (device_info_serial);
CREATE INDEX bird_name_index on tracking_warehouse (bird_name);
CREATE INDEX ring_code_index on tracking_warehouse (ring_code);
CREATE INDEX colour_ring_code_index on tracking_warehouse (colour_ring_code);
CREATE INDEX species_code_index on tracking_warehouse (species_code);
CREATE INDEX scientific_name_index on tracking_warehouse (scientific_name);
CREATE INDEX catch_weight_index on tracking_warehouse (catch_weight);
CREATE INDEX sex_index on tracking_warehouse (sex);
CREATE INDEX catch_location_index on tracking_warehouse (catch_location);
CREATE INDEX colony_latitude_index on tracking_warehouse (colony_latitude);
CREATE INDEX colony_longitude_index on tracking_warehouse (colony_longitude);
CREATE INDEX tracking_started_at_index on tracking_warehouse (tracking_started_at);
CREATE INDEX tracking_ended_at_index on tracking_warehouse (tracking_ended_at);
CREATE INDEX is_active_index on tracking_warehouse (is_active);
CREATE INDEX date_time_index on tracking_warehouse (date_time);
CREATE INDEX latitude_index on tracking_warehouse (latitude);
CREATE INDEX longitude_index on tracking_warehouse (longitude);
CREATE INDEX altitude_index on tracking_warehouse (altitude);
CREATE INDEX pressure_index on tracking_warehouse (pressure);
CREATE INDEX temperature_index on tracking_warehouse (temperature);
CREATE INDEX satellites_used_index on tracking_warehouse (satellites_used);
CREATE INDEX gps_fixtime_index on tracking_warehouse (gps_fixtime);
CREATE INDEX positiondop_index on tracking_warehouse (positiondop);
CREATE INDEX h_accuracy_index on tracking_warehouse (h_accuracy);
CREATE INDEX v_accuracy_index on tracking_warehouse (v_accuracy);
CREATE INDEX x_speed_index on tracking_warehouse (x_speed);
CREATE INDEX y_speed_index on tracking_warehouse (y_speed);
CREATE INDEX z_speed_index on tracking_warehouse (z_speed);
CREATE INDEX speed_accuracy_index on tracking_warehouse (speed_accuracy);
CREATE INDEX userflag_index on tracking_warehouse (userflag);
CREATE INDEX speed_3d_index on tracking_warehouse (speed_3d);
CREATE INDEX speed_2d_index on tracking_warehouse (speed_2d);
CREATE INDEX direction_index on tracking_warehouse (direction);
CREATE INDEX altitude_agl_index on tracking_warehouse (altitude_agl);
CREATE INDEX calc_time_diff_index on tracking_warehouse (calc_time_diff);
CREATE INDEX calc_distance_diff_index on tracking_warehouse (calc_distance_diff);
CREATE INDEX calc_speed_2d_index on tracking_warehouse (calc_speed_2d);
CREATE INDEX calc_distance_to_colony_index on tracking_warehouse (calc_distance_to_colony);
CREATE INDEX calc_sunlight_index on tracking_warehouse (calc_sunlight);
CREATE INDEX calc_outlier_index on tracking_warehouse (calc_outlier);
CREATE INDEX calc_corine_category_index on tracking_warehouse (calc_corine_category);