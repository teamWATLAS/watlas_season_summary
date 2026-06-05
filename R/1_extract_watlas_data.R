#===============================================================================
# Extract WATLAS data from database
#===============================================================================

# This script extracts WATLAS data from the database and saves it as a CSV file.
# Afterwards check the data (0c_check_raw_data).

# It extracts all data from one year and saves then separately for each tag
# as csv.

# Summary
# 1. Load WATLAS data
# 2. Save split by tag

# packages
library(tools4watlas)
library(foreach)
library(lubridate)

# specify year and file path's
year_id <- 2025
year2_id <- NULL # year_id + 1 # set to NULL if not existing yet

# file path to WATLAS teams data folder
watlas_fp <- atl_file_path("watlas_teams")

# file path to sqlite databases
db_fp <- atl_file_path("sqlite_db")
db_fp <- paste0(
  "C:/Users/jkrietsch/OneDrive - NIOZ/",
  "Documents/watlas_data/localizations/"
)

# check run time
st <- Sys.time()

#-------------------------------------------------------------------------------
# 1. Load WATLAS data
#-------------------------------------------------------------------------------

# load Excel file with metadata
all_tags <- readxl::read_excel(
  paste0(watlas_fp, "tags/tags_watlas_all.xlsx"),
  sheet = "tags_watlas_all"
) |>
  data.table()

# check species names
all_tags[, .N, species]
all_tags[year == year_id, .N, species]

# subset desired tags using data.table
tags <- all_tags[year == year_id & !is.na(species)]$tag

# check N
tags |> length()

# time period for which data should be extracted form the database
from <- paste0(year_id, "-01-01 00:00:00")
to <- paste0(year_id, "-12-31 23:59:59")

# database connection
sqlite_db <- paste0(db_fp, "watlas-", year_id, ".sqlite")
con <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_db)

# load data from database
data <- atl_get_data(
  tags,
  tracking_time_start = from,
  tracking_time_end = to,
  timezone = "CET",
  use_connection = con
)

# close connection
RSQLite::dbDisconnect(con)

### add data from next year if available

if (!is.null(year2_id)) {

  # time period for which data should be extracted form the database
  from <- paste0(year2_id, "-01-01 00:00:00")
  to <- paste0(year2_id, "-12-31 23:59:59")

  # database connection
  sqlite_db <- paste0(db_fp, "watlas-", year2_id, ".sqlite")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_db)

  # load data from database
  data2 <- atl_get_data(
    tags,
    tracking_time_start = from,
    tracking_time_end = to,
    timezone = "CET",
    use_connection = con
  )

  # close connection
  RSQLite::dbDisconnect(con)

  # combine data
  data <- rbind(data, data2)

}

# correct tag format
all_tags[, tag := sprintf("%04d", as.integer(tag))]

# correct time zone to CET and change to UTC
all_tags[, release_ts := force_tz(as_datetime(release_ts), tzone = "CET")]
all_tags[, release_ts := with_tz(release_ts, tzone = "UTC")]

# join release_ts and species with data
all_tags[, tag := as.character(tag)]
data[all_tags, on = "tag", `:=`(release_ts = i.release_ts, species = i.species)]

# make species first column
setcolorder(data, c("species", setdiff(names(data), c("species"))))

# exclude positions before the release
data <- data[datetime > release_ts]
data[, release_ts := NULL]

# order data.table
setorder(data, species, tag, time)

#-------------------------------------------------------------------------------
# 2. Save split by tag
#-------------------------------------------------------------------------------

# unique ID (here by tag)
id <- unique(data$tag)

# split data (only necessary if dataset is to big to send to all cores)
foreach(i = id) %do% {

  # subset data
  data_subset <- data[tag == i]

  # save data
  fwrite(
    data_subset,
    paste0(
      "./data/", year_id, "/watlas_", year_id,
      "_raw_tag_", i, ".csv"
    ),
    yaml = TRUE
  )

}

# total run time
round(Sys.time() - st, 2)