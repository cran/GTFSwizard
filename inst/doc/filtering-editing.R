## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(GTFSwizard)
gtfs <- for_rail_gtfs

## -----------------------------------------------------------------------------
route_ids <- gtfs$routes$route_id[1:2]
route_feed <- filter_route(gtfs, route_ids)

service_id <- gtfs$trips$service_id[1]
service_feed <- filter_service(gtfs, service_id)

time_feed <- filter_time(gtfs, from = "06:00:00", to = "09:00:00")

## -----------------------------------------------------------------------------
stop_ids <- gtfs$stops$stop_id[1:5]
partial_feed <- filter_stop(gtfs, stop_ids)

## -----------------------------------------------------------------------------
typical_feed <- filter_servicepattern(gtfs)

## -----------------------------------------------------------------------------
active_date <- gtfs$dates_services$date[
  lengths(gtfs$dates_services$service_id) > 0
][1]
date_feed <- filter_date(gtfs, active_date)

## -----------------------------------------------------------------------------
area <- c(-39, -4, -38, -3) # xmin, ymin, xmax, ymax in WGS84
area_feed <- filter_area(gtfs, area)
nrow(area_feed$trips)

## -----------------------------------------------------------------------------
grouped <- selection(gtfs, route_id, direction_id)
attr(grouped, "selection")$groups

selected <- selection(
  gtfs,
  route_id,
  route_id %in% route_ids
)

selected <- unselection(selected)

## -----------------------------------------------------------------------------
trip_ids <- gtfs$trips$trip_id[1:2]
stop_ids <- gtfs$stops$stop_id[1:3]

delayed <- delay_trip(gtfs, trip = trip_ids, duration = 300)
faster <- edit_speed(gtfs, trips = trip_ids, stops = stop_ids, factor = 1.2)
fixed_dwell <- set_dwelltime(
  gtfs, duration = 30, trips = trip_ids, stops = stop_ids
)
scaled_dwell <- edit_dwelltime(
  gtfs, trips = trip_ids, stops = stop_ids, factor = 1.5
)

## -----------------------------------------------------------------------------
trip_id <- gtfs$trips$trip_id[1]
split_equal <- split_trip(gtfs, trip = trip_id, split = 1)

trip_stops <- gtfs$stop_times$stop_id[gtfs$stop_times$trip_id == trip_id]
if (length(trip_stops) > 2) {
  split_at_stop <- split_trip(gtfs, trip = trip_id, stops = trip_stops[2])
}

## -----------------------------------------------------------------------------
merged <- merge_gtfs(
  filter_route(gtfs, gtfs$routes$route_id[1]),
  filter_route(gtfs, gtfs$routes$route_id[2])
)

## -----------------------------------------------------------------------------
output <- tempfile(fileext = ".zip")
write_gtfs(delayed, output)
unlink(output)

