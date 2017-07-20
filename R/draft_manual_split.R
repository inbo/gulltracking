
library(data.table)
library(lubridate)
library(maptools)
library(sp)
library(raster)
library(geosphere)

#source("./R/enrich.R")
#source("./R/extract.R")
#source("./R/data.R")
library(BirdTrackingEtl)

load("./data/sex_choices.RData")
load("./data/species_choices.RData")

birds_filename <- "./draft/bird_tracking_devices.csv"
corine_filename <- "./draft/corine_g100_06.tif"
corine_legend_filename <- "./draft/corine_legend.csv"

birds <- load_bird_file(birds_filename)
birds <- validate_bird_data(birds)
corine <- read_raster_data(corine_filename, data.CRS = "+init=epsg:3035")
corine_legend <- read_raster_legend(corine_legend_filename)

# We need to provide a chunk based approach for this section
tracks_filename <- "./draft/logs_subset.csv"
tracks <- load_tracks_file(tracks_filename)
tracks <- validate_tracks_data(tracks)
dt_original <- enrich_data(tracks, birds, corine, corine_legend)
write.table(dt_original, file = "./draft/processed_bird_logs_original.csv",
            sep = ",", row.names = FALSE, fileEncoding = "UTF-8")

dt_original <- fread("./draft/processed_bird_logs_original.csv")
setkey(dt_original, device_info_serial, date_time)
# check about join operation
#birds <- load_bird_file(birds_filename)
#birds <- validate_bird_data(birds)
#dt_subcheck_join <- join_tracks_and_metadata(tracks, birds)

#-------------------------------------------------------------------------------
# Chunk based approach -> fixed block
#-------------------------------------------------------------------------------

library('LaF')

# preps (cfr. earlier)
birds_filename <- "./draft/bird_tracking_devices.csv"
tracks_filename <- "./draft/logs_subset.csv"
corine_filename <- "./draft/corine_g100_06.tif"
corine_legend_filename <- "./draft/corine_legend.csv"
corine <- read_raster_data(corine_filename, data.CRS = "+init=epsg:3035")
corine_legend <- read_raster_legend(corine_legend_filename)

output_file_name <- "./draft/processed_bird_logs.csv"
file.remove(output_file_name)

# tracks-alternative
model <- detect_dm_csv(tracks_filename, header = TRUE,
                       dec = ".", sep = ",", na.strings = "",
                       nrows = 10000)
df.logs <- laf_open(model)
begin(df.logs) # point to start of file

# define chunksize
blocksize <- 10001
first_time <- TRUE
while (TRUE) {
    tracks_chunk <- next_block(df.logs, nrows = blocksize)
    print(current_line(df.logs))

    # Actually handle the data
    birds <- load_bird_file(birds_filename) # reload, as changed in memory
    birds <- validate_bird_data(birds)
    tracks_chunk <- as.data.table(tracks_chunk)
    # make sure "" string are replaced by NA
    for (col in names(tracks_chunk)) set(tracks_chunk,
                                         i = which(tracks_chunk[[col]] == ""),
                                         j = col, value = NA)

    tracks_chunk <- validate_tracks_data(tracks_chunk)
    dt <- enrich_data(tracks_chunk, birds, corine, corine_legend)

    # write/append to the processed_logs file from element 2:end
    # formatting of fileds
    dt[, tracking_started_at := format(tracking_started_at, "%F %H:%M:%S",
                                       tz = "UTC")]
    dt[, tracking_ended_at := format(tracking_ended_at, "%F %H:%M:%S",
                                     tz = "UTC")]
    dt[, date_time := format(date_time, "%F %H:%M:%S", tz = "UTC")]

    # saving to file (without first line)
    print(nrow(dt))
    if (first_time == TRUE) {
        write.table(dt, file = output_file_name, sep = ",",
                    row.names = FALSE, fileEncoding = "UTF-8", append = FALSE)
        first_time <- FALSE
        }
    else {# FOUT!!!! Want geen rekening met de juiste switch!
        write.table(dt[-1], file = output_file_name, sep = ",",
                    row.names = FALSE, fileEncoding = "UTF-8", append = TRUE,
                    col.names = FALSE)
    }

    if (nrow(tracks_chunk) < blocksize) {
        break}
    # step one line back, in order to enable diff calculation for next run
    goto(df.logs, current_line(df.logs) - 1)
    print(current_line(df.logs))
}

# read
dt_chunk <- fread(output_file_name)
setkey(dt_chunk, device_info_serial, date_time)

#-------------------------------------------------------------------------------
# Chunk based approach -> read blocks
#-------------------------------------------------------------------------------
# read file with the chunk size of each device

# TODO



#-------------------------------------------------------------------------------
# SQLITE in between version
#-------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(magrittr)
library(RSQLite)

#' Save a single CSV-table into a single table sqlite database
#'
#' @param csv_file name of the CSV file to convert
#' @param sqlite_file name of the newly created sqlite file
#' @param table_name name of the table to store the data table in the sqlite
#'      dbase
#' @param pre_process_size the number of lines to check the data types of the
#'      individual columns (default 1000)
#' @param chunk_size the number of lines to read for each chunk (default 50000)
#'
csv_to_sqlite <- function(csv_file, sqlite_file, table_name,
                          pre_process_size = 1000, chunk_size = 50000) {
    con <- dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

    # read an extract of the data to extract the colnames and types
    # to figure out the date ande datetime columns
    df <- read_delim(csv_file, ",", n_max = pre_process_size)
    datetime_cols <- "date_time"
    # write this first batch of lines to SQLITE table, converting dates to string representation
    df[ , datetime_cols] <- as.character.POSIXt(df[ , datetime_cols])
    dbWriteTable(con, table_name, as.data.frame(df),
                 overwrite = TRUE)

    # subfunction that appends new sections to the table
    append_to_sqlite <- function(x, pos) {
        x <- as.data.frame(x)
        x[ , datetime_cols] <- as.character.POSIXt(x[ , datetime_cols])
        dbWriteTable(con, table_name, x, append = TRUE)
    }

    # readr chunk functionality
    read_delim_chunked(csv_file, append_to_sqlite, delim = ",",
                       skip = pre_process_size, col_names = colnames(df),
                       col_types = spec(df), chunk_size = chunk_size,
                       progress = FALSE)
    dbDisconnect(con)
}


csv_to_sqlite("./draft/logs_subset.csv", "./draft/logs_subset.sqlite", "logs",
                          pre_process_size = 1000, chunk_size = 50000)




