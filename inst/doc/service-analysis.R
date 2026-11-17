## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(GTFSwizard)
gtfs <- for_rail_gtfs

## -----------------------------------------------------------------------------
get_servicepattern(gtfs)

## ----fig.width=7, fig.height=4.5----------------------------------------------
plot_calendar(gtfs, fill = "service_pattern", facet_by_year = TRUE)

## -----------------------------------------------------------------------------
head(get_frequency(gtfs, method = "by_route"))
head(get_headways(gtfs, method = "by_route"))

## ----fig.width=7, fig.height=4.5----------------------------------------------
plot_frequency(gtfs)
plot_headways(gtfs)

## -----------------------------------------------------------------------------
head(get_durations(gtfs, method = "by_trip"))
head(get_distances(gtfs, method = "by_trip"))
head(get_speeds(gtfs, method = "by_route"))
head(get_dwelltimes(gtfs, method = "by_route"))
get_fleet(gtfs, method = "peak")

## -----------------------------------------------------------------------------
faster <- edit_speed(gtfs, factor = 1.1)
compare_scenarios(gtfs, faster, dates = "2021-12-13", by = "network")

## ----eval=requireNamespace("tidytransit", quietly=TRUE) && requireNamespace("hms", quietly=TRUE) && requireNamespace("data.table", quietly=TRUE)----
head(tidy_raptor(gtfs, dates = "2021-12-13", stop_ids = "66",
                 min_departure = "06:20:00", max_arrival = "09:40:00",
                 keep = "shortest"))

## -----------------------------------------------------------------------------
stops <- get_stops_sf(gtfs$stops)
shapes <- get_shapes_sf(gtfs$shapes)
nrow(stops)
nrow(shapes)

## -----------------------------------------------------------------------------
head(get_hubs(gtfs))
get_corridor(gtfs, i = 0.2, min_length = 100)

