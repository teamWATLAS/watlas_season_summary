#===============================================================================
# Tag fates checked in detail
#===============================================================================

# This script is there to check specific tag fates in detail
# (fill tag_fate and start stationary column in all_tags)

# Summary
# Functions
# Check 2023 data

# packages
library(tools4watlas)
library(ggplot2)
library(scales)
library(viridis)
library(foreach)
library(mapview)

# load Excel file with metadata
all_tags <- readxl::read_excel(
  paste0(atl_file_path("watlas_teams"), "tags/tags_watlas_all.xlsx"),
  sheet = "tags_watlas_all"
) |>
  data.table()

# make release_ts UTC
all_tags[, release_ts_UTC := as.POSIXct(release_ts, tz = "UTC")]

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------

atl_get_data_admin <- function(tag_id,
                               tracking_time_start,
                               tracking_time_end,
                               type = c("localizations", "detections")) {
  type <- match.arg(type)

  # extract year from tracking_time_start
  year_id <- format(as.Date(tracking_time_start), "%Y")

  if (type == "localizations") {
    # localizations from SQLite
    db_fp <- atl_file_path("sqlite_db")
    sqlite_db <- paste0(db_fp, "watlas-", year_id, ".sqlite")
    con <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_db)

    data <- atl_get_data(
      tag_id,
      tracking_time_start = tracking_time_start,
      tracking_time_end = tracking_time_end,
      timezone = "UTC",
      use_connection = con
    )

    RSQLite::dbDisconnect(con)
  } else if (type == "detections") {
    # detections from MySQL
    from_unix <- as.numeric(as.POSIXct(tracking_time_start, tz = "UTC")) * 1000
    to_unix <- as.numeric(as.POSIXct(tracking_time_end, tz = "UTC")) * 1000

    sql_query <- glue::glue("
      SELECT BS, TAG, TIME, SNR, RSSI 
      FROM atlas{year_id}.DETECTIONS 
      WHERE TAG = {atl_full_tag_id(tag_id)} 
        AND TIME BETWEEN '{from_unix}' AND '{to_unix}'
    ")

    con <- RMySQL::dbConnect(
      RMySQL::MySQL(),
      user = Sys.getenv("username"),
      password = Sys.getenv("password"),
      dbname = paste0("atlas", year_id),
      host = "abtdb1.nioz.nl"
    )

    data <- DBI::dbGetQuery(con, sql_query)
    RMySQL::dbDisconnect(con)

    # add datetime
    setDT(data)
    data[, datetime := as.POSIXct(
      TIME / 1000,
      origin = "1970-01-01",
      tz = "UTC"
    )]
  }

  return(data)
}

atl_mapview <- function(data) {
  # ensure data is a data.table
  data <- as.data.table(data)

  # calculate time from last in hours
  data[, time_from_last := (time - max(time)) / 60 / 60, by = tag]

  # round all numeric columns to 1 decimal place
  data[, names(data) := lapply(
    .SD, function(x) if (is.numeric(x)) round(x, 1) else x
  )]

  # make data spatial (points)
  d_sf <- atl_as_sf(
    data,
    additional_cols = c("datetime", "time_from_last", "nbs")
  )

  # make track lines
  d_sf_lines <- atl_as_sf(
    data,
    additional_cols = c("time_from_last"),
    option = "lines"
  )

  # interactive map
  map <- mapview(d_sf_lines, zcol = "time_from_last", legend = FALSE) +
    mapview(d_sf, zcol = "time_from_last")

  map
}

#-------------------------------------------------------------------------------
# Check 2023 data
#-------------------------------------------------------------------------------

### bar-tailed godwits

tag_id <- "2910"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "detections"
)
# no detections

tag_id <- "3247"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "detections"
)
data[, .(start = min(datetime), end = max(datetime), .N)]

data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

tag <- "3251"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

### curlew

tag <- "3101"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data)
atl_mapview(data[datetime > as.POSIXct("2023-08-24 07:30:20", tz = "UTC")])
# last real data: "2023-08-24 07:42:26"

tag <- "3103"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data[datetime > as.POSIXct("2023-09-19 13:14:56", tz = "UTC")])

atl_mapview(
  data[datetime > as.POSIXct("2023-09-13 13:14:56", tz = "UTC") &
         datetime < as.POSIXct("2023-09-18 13:14:56", tz = "UTC")]
)
# last real data: "2023-09-13 16:39:23"

tag <- "3105"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data)
atl_mapview(data[datetime > as.POSIXct("2023-08-27 16:00:56", tz = "UTC")])
atl_mapview(data[datetime > as.POSIXct("2023-08-27 16:02:05", tz = "UTC")])
# last real data before "2023-08-27 16:02:05"

### dunlin

tag <- "3200"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data)
atl_mapview(data[datetime > as.POSIXct("2023-08-18 08:51:05", tz = "UTC")])
# last real data before "2023-08-18 08:51:05"

tag <- "3202"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data)
atl_mapview(data[datetime > as.POSIXct("2023-10-18 11:30:00", tz = "UTC")])
# last real data before "2023-08-17 12:39:24"

tag <- "3203"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data)
atl_mapview(data[datetime > as.POSIXct("2023-08-18 08:38:19", tz = "UTC")])
# last real data before "2023-08-17 12:39:24"

### oystercatcher

tag <- "3153"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data)
atl_mapview(data[datetime > as.POSIXct("2023-08-22 09:43:57", tz = "UTC")])
# last real data before "2023-08-22 09:43:57"

tag <- "3159"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data[datetime > as.POSIXct("2023-12-23 22:23:14", tz = "UTC")])
atl_mapview(data[datetime > as.POSIXct("2023-12-20 00:23:14", tz = "UTC")])
# last real data before "2023-08-22 09:43:57"


### red knot

tag <- "3098"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2024-01-01 00:00:00",
  tracking_time_end   = "2024-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# last useful data "2023-10-23 00:57:35"


tag_id <- "3167"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-01-01 00:00:00",
  tracking_time_end   = "2024-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# last real data before "2023-12-07 17:41:23"

tag_id <- "3170"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data[datetime > as.POSIXct("2023-12-05 22:23:14", tz = "UTC")])
atl_mapview(data[datetime > as.POSIXct("2023-12-23 22:23:14", tz = "UTC")])
atl_mapview(data[datetime > as.POSIXct("2023-12-20 00:23:14", tz = "UTC")])
# last real data before "2023-12-06 06:39:46"

tag_id <- "3248"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data[datetime > as.POSIXct("2023-10-18 09:30:15", tz = "UTC")])
# last real data before "2023-10-18 09:30:15"

tag_id <- "3026"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data)
# last real data before "15-09-2023 19:22:45"

### sanderling

tag_id <- "2611"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data[datetime > as.POSIXct("2023-06-22 14:15:00", tz = "UTC")])
# never moved


### turnstone

tag_id <- "3193"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]
all_tags[tag == tag_id, .(tag, release_ts_UTC)]
# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
# plot interactive map
atl_mapview(data[datetime > as.POSIXct("2023-10-23 23:41:53", tz = "UTC")])
# last data 2023-10-23 23:42:38

#-------------------------------------------------------------------------------
# Check 2023 data
#-------------------------------------------------------------------------------
