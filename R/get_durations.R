#' Calculate Trip Durations in GTFS Data
#'
#' The `get_durations` function calculates trip durations within a `wizardgtfs` object using different methods. Depending on the selected `method`, it can provide average durations per route, durations for individual trips, or detailed segment durations between stops.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by.route"}{Calculates the average duration for each route.}
#'     \item{"by.trip"}{Calculates the total duration for each trip.}
#'     \item{"detailed"`}{Calculates detailed durations for each stop-to-stop segment within a trip.}
#'   }
#' @param trips A character vector of trip IDs to consider. When set to `all`, includes all trips.
#'
#' @return A data frame containing trip durations based on the specified method:
#'   \describe{
#'     \item{If `method = "by.route"`}{It includes dwell times. Returns a summary data frame with columns: `route_id`, `trips`, `average.duration`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.trip"`}{It includes dwell times. Returns a data frame with columns: `route_id`, `trip_id`, `duration`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "detailed"`}{It does not include dwell times. Returns a data frame with columns: `route_id`, `trip_id`, `hour`, `from_stop_id`, `to_stop_id`, `duration`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' This function calls specific sub-functions based on the selected method:
#'
#' - "by.route": Calculates average durations for each route.
#'
#' - "by.trip": Calculates the total duration of each trip.
#'
#' - "detailed": Calculates detailed durations between consecutive stops within each trip, excluding dwell times.
#'
#' If an invalid `method` is specified, the function defaults to `"by.route"` and provides a warning.
#'
#' @examples
#' # Calculate average route durations
#' durations_by_route <- get_durations(gtfs = for_rail_gtfs, method = "by.route", trips = 'all')
#'
#' # Calculate trip durations
#' durations_by_trip <- get_durations(gtfs = for_rail_gtfs, method = "by.trip", trips = 'all')
#'
#' # Calculate detailed durations between stops
#' detailed_durations <- get_durations(gtfs = for_rail_gtfs, method = "detailed", trips = 'all')
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @importFrom dplyr mutate group_by reframe select left_join filter ungroup
#' @importFrom stringr str_split str_extract
#' @export
get_durations <- function(gtfs, method = 'by.route', trips = 'all'){

  if(!any(trips == 'all')) {gtfs <- GTFSwizard::filter_trip(gtfs, trip = trips)}

  if (method == 'by.route') {
    durations <- get_durations_byroute(gtfs)
  }

  if (method == 'by.trip') {
    durations <- get_durations_bytrip(gtfs)
  }

  if (method == 'detailed') {
    durations <- get_durations_detailed(gtfs)
  }

  if (!method %in% c('by.route', 'detailed', 'by.trip')) {
    durations <- get_durations_byroute(gtfs)
    warning('"method" should be one of "by.route", "by.trip" or "detailed". Returning "method = by.route"".')
  }

  return(durations)

}

get_durations_byroute <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  durations <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::group_by(trip_id) %>%
    dplyr::reframe(starts = arrival_time[1] %>%
                     stringr::str_split(":") %>%
                     lapply(FUN = as.numeric) %>%
                     lapply(FUN = function(x){
                       x[1]*60*60+x[2]*60+x[3]
                     }) %>%
                     unlist() %>%
                     na.omit(),
                   ends = arrival_time[n()] %>%
                     stringr::str_split(":") %>%
                     lapply(FUN = as.numeric) %>%
                     lapply(FUN = function(x){
                       x[1]*60*60+x[2]*60+x[3]
                     }) %>%
                     unlist() %>%
                     na.omit(),
                   duration = ends - starts) %>%
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>%
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>%
    dplyr::reframe(average.duration = mean(duration),
                   trips = n()) %>%
    dplyr::select(route_id, trips, average.duration, service_pattern, pattern_frequency)

  return(durations)

}

get_durations_bytrip <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  durations <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::group_by(trip_id) %>%
    dplyr::reframe(starts = arrival_time[1] %>%
                     stringr::str_split(":") %>%
                     lapply(FUN = as.numeric) %>%
                     lapply(FUN = function(x){
                       x[1]*60*60+x[2]*60+x[3]
                     }) %>%
                     unlist() %>%
                     na.omit(),
                   ends = arrival_time[n()] %>%
                     stringr::str_split(":") %>%
                     lapply(FUN = as.numeric) %>%
                     lapply(FUN = function(x){
                       x[1]*60*60+x[2]*60+x[3]
                     }) %>%
                     unlist() %>%
                     na.omit(),
                   duration = ends - starts) %>%
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>%
    dplyr::select(route_id, trip_id, duration, service_pattern, pattern_frequency)

  return(durations)

}

get_durations_detailed <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  durations <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+"),
                  arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){x[1]*60*60+x[2]*60+x[3]}) %>%
                    unlist() %>%
                    na.omit(),
                  departure_time = departure_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit()
    ) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::mutate(from_stop_id = stop_id,
                  to_stop_id = lead(stop_id),
                  lead_arrival_time = lead(arrival_time),
                  duration =  lead_arrival_time - departure_time) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>%
    dplyr::mutate(arrival_time = as.character(hms::as_hms(lead_arrival_time))) %>%
    dplyr::select(route_id, trip_id, arrival_time, hour, from_stop_id, to_stop_id, duration, service_pattern, pattern_frequency) %>%
    stats::na.omit()

  return(durations)

}

