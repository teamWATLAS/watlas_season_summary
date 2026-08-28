#===============================================================================
# Tag fates checked in detail
#===============================================================================

# This script is there to check specific tag fates in detail
# (fill tag_fate and start stationary column in all_tags)

# Maps creates in \\zeus\cos\birds\watlas\season_summary_raw can be used to
# classify data. For birds for which more checks need to be done, add script
# below.

# DESCRIPTION
# tag_fate (status of last localization): 
# broken = broken at start or it never worked on bird; 
# stationary = tag not moving thus bird is dead or lost tag; 
# departed = bird flew out of tracking area;  
# disappeared = last localization within tracking area; 
# "" = not checked yet

# start_stationary:
# datetimestamp (in UTC) when tag is not moving anymore. 
# If exact time is unknown, the time is midnight (00:00:00). 

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


# ## Evy's file path
# all_tags <- readxl::read_excel("C:/Users/egobbens/OneDrive - NIOZ/Documenten/NIOZ/WATLAS/tags_watlas_all.xlsx",
#   sheet = "tags_watlas_all"
# ) |>
#   data.table()


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
    # db_fp <- "D:/localizations/"
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
# Check 2024 data
#-------------------------------------------------------------------------------

 ### BAR-TAILED GODWITS ###

tag_id <- "3349"
  data <- atl_get_data_admin(
    tag_id = tag_id,
    tracking_time_start = "2024-09-07 00:00:00",
    tracking_time_end   = "2024-09-09 23:59:59",
    type = "localizations"
  )
  
  
  # plot all by datetime
  atl_check_tag(
    data,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE
  )

#departed  

  tag_id <- "3413"
  data <- atl_get_data_admin(
    tag_id = tag_id,
    tracking_time_start = "2024-11-05 08:00:00",
    tracking_time_end   = "2024-11-05 23:59:59",
    type = "localizations"
  )
  
  
  # plot all by datetime
  atl_check_tag(
    data,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE
  )

#disappeared on roost, tag does not look stationary 

### CURLEW ### 
  
  
  tag_id <- "3402"
  data <- atl_get_data_admin(
    tag_id = tag_id,
    tracking_time_start = "2024-10-06 22:00:00",
    tracking_time_end   = "2024-10-07 23:59:59",
    type = "localizations"
  )
  
  
  # plot all by datetime
  atl_check_tag(
    data,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE
  )
  atl_mapview(data[datetime > as.POSIXct("2024-10-07 10:00:00", tz = "UTC")])

  #disappeared on roost, tag does not look stationary, and checked high-tide which coincided with time last position

  tag_id <- "3462"
  data <- atl_get_data_admin(
    tag_id = tag_id,
    tracking_time_start = "2024-09-21 00:00:00",
    tracking_time_end   = "2024-09-21 23:59:59",
    type = "localizations"
  )
  
  
  # plot all by datetime
  atl_check_tag(
    data,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE
  )
  atl_mapview(data[datetime > as.POSIXct("2024-09-21 10:00:00", tz = "UTC")])  

  #disappeared after roost, coincides with high tide
  
  tag_id <- "3465"
  data <- atl_get_data_admin(
    tag_id = tag_id,
    tracking_time_start = "2024-10-01 00:00:00",
    tracking_time_end   = "2024-10-03 23:59:59",
    type = "localizations"
  )
  
  
  # plot all by datetime
  atl_check_tag(
    data,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE
  )
  atl_mapview(data[datetime > as.POSIXct("2024-10-03 10:00:00", tz = "UTC")]) 
 
  #disappeared  
  
  
  tag_id <- "3563"
  data <- atl_get_data_admin(
    tag_id = tag_id,
    tracking_time_start = "2024-10-22 00:00:00",
    tracking_time_end   = "2024-10-23 23:59:59",
    type = "localizations"
  )
  
  
  # plot all by datetime
  atl_check_tag(
    data,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE
  )
  atl_mapview(data[datetime > as.POSIXct("2024-10-22 00:00:00", tz = "UTC")]) 
  
# stationary from 2024-10-22T08:01:38, flew from Griend 3 hours before high tide (2024-10-22T08:01:38), after that stayed for ~24 hours with weird tracks. 
  
  ### CURLEW SANDPIPER ###   
  
  
  tag_id <- "3685"
  data <- atl_get_data_admin(
    tag_id = tag_id,
    tracking_time_start = "2024-08-20 00:00:00",
    tracking_time_end   = "2024-08-21 23:59:59",
    type = "localizations"
  )
  
  
  # plot all by datetime
  atl_check_tag(
    data,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE
  )
  atl_mapview(data[datetime > as.POSIXct("2024-10-22 00:00:00", tz = "UTC")]) 

  
    
 ### DUNLIN ### 

tag_id <- "3718"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-10-01 00:00:00",
  tracking_time_end   = "2024-10-03 23:59:59",
  type = "localizations"
)

atl_mapview(data[datetime > as.POSIXct("2024-10-01 00:00:00", tz = "UTC")])

#disappeared during high tide, was already in the same spot the day before during high tide
          
          tag_id <- "3727"
          data <- atl_get_data_admin(
            tag_id = tag_id,
            tracking_time_start = "2024-09-04 00:00:00",
            tracking_time_end   = "2024-09-08 23:59:59",
            type = "localizations"
          )
          
          atl_mapview(data[datetime > as.POSIXct("2024-09-04 00:00:00", tz = "UTC")])
          
# stationary from 2024-09-04 17:30:21
    
    tag_id <- "3755"
    data <- atl_get_data_admin(
      tag_id = tag_id,
      tracking_time_start = "2024-09-20 00:00:00",
      tracking_time_end   = "2024-09-24 23:59:59",
      type = "localizations"
    )
    
    atl_check_tag(
      data,
      option = "datetime",
      highlight_first = TRUE, highlight_last = TRUE
    )
    
    atl_mapview(data[datetime > as.POSIXct("2024-09-22 08:00:00", tz = "UTC")])
##last position on beach: 2024-09-24 06:14:02, weird positions on the middle of the island (possible predation event?)
##stationary 	2024-09-22 09:57:27

 ### GREY PLOVER ###

tag_id <- "3540"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-09-09 00:00:00",
  tracking_time_end   = "2024-09-10 23:59:59",
  type = "localizations"
)

atl_mapview(data[datetime > as.POSIXct("2024-09-09 00:00:00", tz = "UTC")])

#last real data: 2024-09-09 12:16:35.153, when it walked to the island 


  ### OYSTERCATCHER ### 


tag_id <- "3334"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-09-27 00:00:00",
  tracking_time_end   = "2024-09-29 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2024-09-29 00:00:00", tz = "UTC")])

#disappeared during low-tide where it was seen the previous tide as well 




tag_id <- "3405"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-09-01 00:00:00",
  tracking_time_end   = "2024-09-01 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2024-09-01 00:00:00", tz = "UTC")])
# disappeared, was in same spot the previous tide

 ### REDSHANK ### 


tag_id <- "3432"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-08-04 00:00:00",
  tracking_time_end   = "2024-08-16 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2024-08-04 00:00:00", tz = "UTC")])
# stayed around the island during low-tide, do we want to include these? 
#stationary from: 2024-08-06 09:48:02


tag_id <- "3426"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-08-06 00:00:00",
  tracking_time_end   = "2024-08-06 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2024-08-06 00:00:00", tz = "UTC")])

#stationary from 2024-08-06 09:51:45

###########CHECK WITH HANNES######################## 


 ### SANDERLING ### 


tag_id <- "3627"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-09-09 00:00:00",
  tracking_time_end   = "2024-09-10 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2024-09-10 00:00:00", tz = "UTC")])
## disappeared, on 9-9 on roost with many positions, then one position on mudflat with low-tide


tag_id <- "3662"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-10-06 00:00:00",
  tracking_time_end   = "2024-10-08 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2024-10-06 00:00:00", tz = "UTC")])

#disappeared on roost 


 ### TURNSTONE ### 

tag_id <- "3611"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-08-01 00:00:00",
  tracking_time_end   = "2024-09-10 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2024-08-01 00:00:00", tz = "UTC")])

#floating around during high-tide? But how can position jump so far from 13:40 to 13:43? 



tag_id <- "3615"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2024-08-01 00:00:00",
  tracking_time_end   = "2024-09-10 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2024-08-01 00:00:00", tz = "UTC")])

#same as above 3611 


#-------------------------------------------------------------------------------
# Check 2025 data
#-------------------------------------------------------------------------------

### BAR-TAILED GODWIT ### 


tag_id <- "3812"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-05-01 00:00:00",
  tracking_time_end   = "2025-05-20 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-05-01 00:00:00", tz = "UTC")])

##looks stationary, because last position near coast during low-tide, but from when? 



tag_id <- "3865"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-10-03 00:00:00",
  tracking_time_end   = "2025-10-04 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-10-03 12:00:00", tz = "UTC")])
#stationary from: 2025-10-03 19:01:55, after that it was on the east side of Griend for >24 hours


tag_id <- "3867"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-08-25 00:00:00",
  tracking_time_end   = "2025-09-08 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-08-30 00:00:00", tz = "UTC")])

## stationary from 2025-08-30 17:34


tag_id <- "4043"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-08-22 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

#missing data 


 ### CURLEW ### 


tag_id <- "3897"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-09-01 00:00:00",
  tracking_time_end   = "2025-09-02 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-01 00:00:00", tz = "UTC")])

#stationary from 	2025-09-01 16:36:06.555, stayed there for another 10 hours in the same spot


tag_id <- "3951"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-08-01 00:00:00",
  tracking_time_end   = "2025-08-28 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-08-01 00:00:00", tz = "UTC")])

#stationary from: 2025-08-27 08:38:04

tag_id <- "3954"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-12-29 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-01 00:00:00", tz = "UTC")])

#disappeared during high tide

tag_id <- "4056"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-12-30 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-01 00:00:00", tz = "UTC")])

#disappeared on the island, not sure it is stationary 

tag_id <- "4057"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-08-22 00:00:00",
  tracking_time_end   = "2025-08-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-08-22 00:00:00", tz = "UTC")])

#disappeared, was in similar position previous tide but spent high tide in between on the island


 ### DUNLIN ### 

tag_id <- "3840"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-22 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)
atl_mapview(data[datetime > as.POSIXct("2025-07-22 00:00:00", tz = "UTC")])

#stationary from: 2025-08-26 19:14:59, tag stopped working ~4 hours after release

tag_id <- "3989"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-22 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-01 00:00:00", tz = "UTC")])

#disappeared ~1 hour after release


tag_id <- "3990"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-22 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-01 00:00:00", tz = "UTC")]) 

#disappeared ~1 hour after release


tag_id <- "3997"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-22 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-19 00:00:00", tz = "UTC")]) 

#stationary from: 2025-09-22 09:05:12

tag_id <- "4005"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-22 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")]) 

#stationary from:2025-08-26 20:07:30

tag_id <- "4194"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-09-27 00:00:00",
  tracking_time_end   = "2025-09-28 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-01 00:00:00", tz = "UTC")])

#disappeared during high tide


tag_id <- "4252"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-22 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-01 00:00:00", tz = "UTC")])

#start stationary: 2025-09-24 00:04:13.



tag_id <- "4271"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-10-27 00:00:00",
  tracking_time_end   = "2025-10-28 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-01 00:00:00", tz = "UTC")])

#stationary from: 2025-10-27 12:09:13.603, last position seems odd as it is during low tide

 ### GREY PLOVER ### 

tag_id <- "4053"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-12-20 00:00:00",
  tracking_time_end   = "2025-12-22 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-12-21 00:00:00", tz = "UTC")])

#disappeared during low tide on the mudflat 



 ### OYSTERCATCHER ### 


tag_id <- "3758"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-09-08 00:00:00",
  tracking_time_end   = "2025-09-12 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-09-08 00:00:00", tz = "UTC")])

#disappeared


tag_id <- "3797"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-08-26 00:00:00",
  tracking_time_end   = "2025-08-28 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-08-26 00:00:00", tz = "UTC")])

#disappeared on roost



tag_id <- "3919"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-11-10 00:00:00",
  tracking_time_end   = "2025-11-11 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-11-10 00:00:00", tz = "UTC")])
#disappeared during low tide on the mudflat



tag_id <- "3921"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-11-30 00:00:00",
  tracking_time_end   = "2025-12-01 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-11-25 00:00:00", tz = "UTC")])

#stationary from; 2025-11-30 23:24:20

 ### REDSHANK ###

tag_id <- "3891"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-01 00:00:00",
  tracking_time_end   = "2025-12-01 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from start 


tag_id <- "3957"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-09-02 00:00:00",
  tracking_time_end   = "2025-09-03 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from: 2025-09-02 11:33:59



tag_id <- "3958"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-02 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from: 2025-08-29 14:36:40


### SANDERLING ### 


tag_id <- "3971"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-09-18 00:00:00",
  tracking_time_end   = "2025-09-22 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from: 2025-09-18 17:39:21


tag_id <- "3983"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-07-01 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from: 30/07/2025 05:47:00

tag_id <- "4175"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-10-12 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from: 2025-10-14 04:25:49

tag_id <- "4183"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-10-14 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from: 2025-10-14 04:09:14


tag_id <- "4227"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-11-06 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from: 2025-11-08 10:30:56

tag_id <- "3848"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-09-09 00:00:00",
  tracking_time_end   = "2025-12-31 23:59:59",
  type = "localizations"
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#disappeared during high tide roost where it was also seen the day before

tag_id <- "4145"
data <- atl_get_data_admin(
  tag_id = tag_id,
  tracking_time_start = "2025-11-25 00:00:00",
  tracking_time_end   = "2025-11-27 23:59:59",
  type = "localizations"
)

atl_mapview(data[datetime > as.POSIXct("2025-07-01 00:00:00", tz = "UTC")])

#stationary from 2025-11-26 19:33:09