## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
feed <- GTFSwizard::for_rail_gtfs
fixed <- GTFSwizard::repair_gtfs(feed, repairs = "overnight")
head(GTFSwizard::repair_report(fixed))

## ----eval=FALSE---------------------------------------------------------------
# install.packages(c("jsonlite", "openssl"))
# library(GTFSwizard)
# result <- validate_gtfs("path/to/feed.zip", download_validator = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# result <- validate_gtfs("path/to/feed.zip", validator_jar = "path/to/validator.jar")
# # Or configure the path once per session:
# Sys.setenv(GTFS_VALIDATOR_JAR = "path/to/validator.jar")
# result <- validate_gtfs("path/to/feed.zip")

## ----eval=FALSE---------------------------------------------------------------
# result$has_errors
# result$notices
# result$system_notices

## ----eval=FALSE---------------------------------------------------------------
# result <- validate_gtfs("path/to/feed.zip", download_validator = TRUE,
#                         output_dir = "validation-report")
# result$html_path
# result$report_path

## ----eval=FALSE---------------------------------------------------------------
# scenario <- delay_trip(for_rail_gtfs, trip = for_rail_gtfs$trips$trip_id[1],
#                        duration = 60)
# result <- validate_gtfs(scenario, download_validator = TRUE)

