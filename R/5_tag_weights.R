#===============================================================================
# Check the weight of the used tags
#===============================================================================

# This script is there to check how heavy the tags where we used

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

# load tag data
data <- fread(
  paste0(
    atl_file_path("watlas_teams"),
    "tags/tag_weights/atlastag-2026-Jun-12.csv"
  )
)

#-------------------------------------------------------------------------------
# Get average tag weight by type
#-------------------------------------------------------------------------------

# short tag ID
data[, tag := id - 31001000000]

# merge with tag weight
data[all_tags, on = "tag", tag_weight := i.tag_weight]

# tag type
data[, tag_type := paste0(pcb, "_", battery)]

# check weight by type
dw <- data[, .(
  mean_weight = round(mean(tag_weight, na.rm = TRUE), 1),
  min_weight  = round(min(tag_weight, na.rm = TRUE), 1),
  max_weight  = round(max(tag_weight, na.rm = TRUE), 1),
  n_weight    = sum(!is.na(tag_weight))
), by = tag_type]
dw

# save file
fwrite(dw, paste0(
  atl_file_path("watlas_teams"),
  "tags/tag_weights/average_weight_by_tag_type.csv"
))


# add to table
data[, tag_weight_by_type := round(mean(tag_weight, na.rm = TRUE), 1), tag_type]

#-------------------------------------------------------------------------------
# Merge back to all_tags
#-------------------------------------------------------------------------------

# add tag type and weight
all_tags[data, on = "tag", `:=`(
  tag_weight_by_type = i.tag_weight_by_type,
  tag_type = i.tag_type
)]

# save file
fwrite(all_tags[, .(tag, species, tag_type, tag_weight_by_type)], paste0(
  atl_file_path("watlas_teams"),
  "tags/tag_weights/tag_type_and_mean_weight_all.csv"
))


# summary by species and year
ds <- all_tags[!is.na(species), .(
  N = .N,
  mean_weight = unique(tag_weight_by_type)
), by = .(year, species, tag_type)]

setorder(ds, year, species)
ds

# save file
fwrite(ds, paste0(
  atl_file_path("watlas_teams"),
  "tags/tag_weights/tag_type_and_mean_weight_summary.csv"
))
