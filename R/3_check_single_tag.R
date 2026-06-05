#===============================================================================
# Check last data of one tag
#===============================================================================

# This script is there to check manually more in detail the data of one tag.

# Summary
# 1. Load data from one tag
# 2. Check data from one tag

# packages
library(tools4watlas)
library(ggplot2)
library(scales)
library(viridis)
library(foreach)
library(mapview)

#-------------------------------------------------------------------------------
# 0. Functions
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
      tracking_time_end   = tracking_time_end,
      timezone = "UTC",
      use_connection = con
    )
    
    RSQLite::dbDisconnect(con)
    
  } else if (type == "detections") {
    # detections from MySQL
    from_unix <- as.numeric(as.POSIXct(tracking_time_start, tz = "UTC")) * 1000
    to_unix   <- as.numeric(as.POSIXct(tracking_time_end, tz = "UTC")) * 1000
    
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



data <- atl_get_data_admin(
  tag_id = "3101",
  tracking_time_start = "2023-03-01 00:00:00",
  tracking_time_end = "2023-09-30 23:59:59"
)


data <- atl_get_data_admin(
  tag_id = "3101",
  tracking_time_start = "2023-03-01 00:00:00",
  tracking_time_end = "2023-09-30 23:59:59",
  type = "detections"
)


#-------------------------------------------------------------------------------
# 2. Check data from one tag
#-------------------------------------------------------------------------------

# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

# plot all by gap
atl_check_tag(
  data,
  option = "gap", scale_trans = "log",
  highlight_first = TRUE, highlight_last = TRUE
)

# plot last n positions
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE, last_n = 1000
)

# subset last n positions
data_subset <- data[max(1, .N - 999):.N]

# calculate time from last in hours
data_subset[, time_from_last := (time - max(time)) / 60 / 60, by = tag]

# round all numeric columns to 1 decimal place
data_subset[, (names(data_subset)) := lapply(
  .SD, function(x) if (is.numeric(x)) round(x, 1) else x
)]

# make data spatial
d_sf <- atl_as_sf(
  data_subset,
  additional_cols = c("datetime", "time_from_last", "nbs")
)

# add track
d_sf_lines <- atl_as_sf(
  data_subset,
  additional_cols = c("time_from_last"),
  option = "lines"
)

# plot interactive map
mapview(d_sf_lines, zcol = "time_from_last", legend = FALSE) +
  mapview(d_sf, zcol = "time_from_last")






#-------------------------------------------------------------------------------
# Check 2023 data
#-------------------------------------------------------------------------------

### bar-tailed godwits

tag <- "2910"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "detections"
)
# no detections


tag <- "3247"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "detections"
)
data[, .(start = min(datetime), end = max(datetime), .N)]


tag <- "3247"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)
data[, .(start = min(datetime), end = max(datetime), .N)]


# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)
data[, .(start = min(datetime), end = max(datetime), .N)]






tag <- "3247"
data <- atl_get_data_admin(
  tag_id = tag,
  tracking_time_start = "2023-01-01 00:00:00",
  tracking_time_end   = "2023-12-31 23:59:59",
  type = "localizations"
)


# extract detections
from_unix <- as.numeric(as.POSIXct(from), tz = "UTC") * 1000
to_unix <- as.numeric(as.POSIXct(to), tz = "UTC") * 1000

# SQL query
sql_query <- glue::glue("
  SELECT BS, TAG, TIME, SNR, RSSI 
  FROM atlas{year_id}.DETECTIONS 
  WHERE TAG = {atl_full_tag_id(tag_id)} 
    AND TIME BETWEEN '{from_unix}' AND '{to_unix}'
")

# connect to database
con <- RMySQL::dbConnect(
  RMySQL::MySQL(),
  user = Sys.getenv("username"),
  password = Sys.getenv("password"),
  dbname = paste0("atlas", year_id),
  host = "abtdb1.nioz.nl"
)

d <- DBI::dbGetQuery(con, sql_query)

# Close connection
RSQLite::dbDisconnect(con)


















