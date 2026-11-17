## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(GTFSwizard)
data(bcn_bus_gtfs, package = "GTFSwizard")
head(bcn_bus_gtfs$routes)

## -----------------------------------------------------------------------------
barcelona_snapshot <- filter_date(
  bcn_bus_gtfs, seq(as.Date("2024-06-21"), as.Date("2024-06-26"), by = "day")
)
nrow(barcelona_snapshot$trips)

## -----------------------------------------------------------------------------
gtfs <- for_rail_gtfs
summary(gtfs)

## -----------------------------------------------------------------------------
head(gtfs$routes)
head(gtfs$stops)

## ----eval=FALSE---------------------------------------------------------------
# gtfs <- read_gtfs("path/to/feed.zip")
# explore_gtfs() # choose a zip file and open the dashboard

## -----------------------------------------------------------------------------
converted <- as_wizardgtfs(unclass(for_rail_gtfs))
inherits(converted, "wizardgtfs")

## -----------------------------------------------------------------------------
created <- create_gtfs(
  agency = data.frame(
    agency_id = "A",
    agency_name = "Demo Transit",
    agency_url = "https://example.com",
    agency_timezone = "America/Fortaleza"
  ),
  routes = data.frame(
    route_id = "R1", agency_id = "A", route_short_name = "1",
    route_long_name = "Central", route_type = 3
  ),
  trips = data.frame(
    route_id = "R1", service_id = "WK", trip_id = "T1"
  ),
  stop_times = data.frame(
    trip_id = "T1",
    arrival_time = c("08:00:00", "08:10:00"),
    departure_time = c("08:00:00", "08:10:00"),
    stop_id = c("S1", "S2"),
    stop_sequence = 1:2
  ),
  stops = data.frame(
    stop_id = c("S1", "S2"),
    stop_name = c("First", "Second"),
    stop_lat = c(-3.73, -3.74),
    stop_lon = c(-38.52, -38.53)
  ),
  calendar = data.frame(
    service_id = "WK",
    monday = 1, tuesday = 1, wednesday = 1, thursday = 1,
    friday = 1, saturday = 0, sunday = 0,
    start_date = "20260101", end_date = "20261231"
  )
)

created

## ----fig.width=7, fig.height=5------------------------------------------------
plot(gtfs)

## -----------------------------------------------------------------------------
output <- tempfile(fileext = ".zip")
write_gtfs(created, output)
file.exists(output)
unlink(output)

