## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE---------------------------------------------------------------
# library(GTFSwizard)
# explore_gtfs(for_rail_gtfs)
# # A multi-agency historical AMB Mobilitat bus feed from Barcelona:
# explore_gtfs(bcn_bus_gtfs)

## ----eval=FALSE---------------------------------------------------------------
# explore_gtfs()

## ----eval=FALSE---------------------------------------------------------------
# explore_gtfs(for_rail_gtfs, plotly = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# route_ids <- for_rail_gtfs$routes$route_id[1:2]
# scenario <- for_rail_gtfs |>
#   filter_route(route_ids) |>
#   filter_time("06:00:00", "10:00:00") |>
#   edit_speed(factor = 1.1)
# 
# write_gtfs(scenario, "morning-scenario.zip")

