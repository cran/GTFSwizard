#' Explore GTFS Data in an Interactive Shiny Dashboard
#'
#' Opens a lightweight Shiny dashboard for exploring a GTFS feed. The dashboard
#' shows summary cards, route and stop maps, service calendar, and key
#' operational charts. Route, service, service-pattern, stop, date, and time
#' filters update the dashboard without changing the original object.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. When omitted
#'   or `NULL` in an interactive session, a file-selection window opens so the
#'   user can choose a GTFS `.zip` archive.
#'
#' @return A Shiny app object.
#'
#' @examples
#' if (interactive()) {
#'   explore_gtfs()
#'   explore_gtfs(GTFSwizard::for_rail_gtfs)
#' }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_shapes()], [GTFSwizard::plot_calendar()]
#'
#' @export
explore_gtfs <- function(gtfs = NULL){
  require_pkg("shiny", "`explore_gtfs()`")
  require_pkg("leaflet", "`explore_gtfs()`")
  if(is.null(gtfs)){
    if(!interactive()){
      gw_stop("`gtfs` is required in non-interactive sessions.")
    }
    selected_file <- tryCatch(
      file.choose(),
      error = function(error){
        gw_stop("no GTFS file was selected.")
      }
    )
    gtfs <- GTFSwizard::read_gtfs(selected_file)
  }
  UseMethod('explore_gtfs')
}

#' @exportS3Method GTFSwizard::explore_gtfs list
explore_gtfs.list <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  explore_gtfs.wizardgtfs(gtfs)
}

#' @exportS3Method GTFSwizard::explore_gtfs wizardgtfs
explore_gtfs.wizardgtfs <- function(gtfs){
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))

  all_routes <- sort(unique(as.character(gtfs$routes$route_id)))
  patterns_table <- GTFSwizard::get_servicepattern(gtfs)
  all_patterns <- as.character(patterns_table$service_pattern)
  all_services <- sort(unique(as.character(gtfs$trips$service_id)))
  stop_labels <- if("stop_name" %in% names(gtfs$stops)){
    paste(gtfs$stops$stop_id, "-", gtfs$stops$stop_name)
  } else {
    as.character(gtfs$stops$stop_id)
  }
  stop_choices <- stats::setNames(
    as.character(gtfs$stops$stop_id), stop_labels
  )
  date_range <- range(as.Date(gtfs$dates_services$date))
  stop_time_seconds <- gtfs_time_to_seconds(c(
    gtfs$stop_times$arrival_time, gtfs$stop_times$departure_time
  ))
  hour_limits <- range(floor(stop_time_seconds[is.finite(stop_time_seconds)] / 3600))
  if(!all(is.finite(hour_limits))){
    hour_limits <- c(0, 24)
  }
  hour_limits[1] <- max(0, hour_limits[1])
  hour_limits[2] <- max(hour_limits[1] + 1, min(48, hour_limits[2]))

  css <- "
    body { background: #f6f8fb; }
    .gw-title { margin: 10px 18px 0; color: #1f2d3a; font-weight: 700; }
    .gw-page { padding: 18px; }
    .gw-card {
      background: #fff; border: 1px solid #d9e1ea; border-radius: 6px;
      padding: 14px; margin-bottom: 14px;
    }
    .gw-stat { font-size: 28px; font-weight: 700; line-height: 1; color: #17456b; }
    .gw-label { color: #5f6f7d; font-size: 12px; text-transform: uppercase; }
    .gw-map { border: 1px solid #d9e1ea; border-radius: 6px; overflow: hidden; }
    .gw-table-scroll { overflow-x: auto; max-height: 360px; overflow-y: auto; }
    .form-group { margin-bottom: 10px; }
    @media (max-width: 767px) {
      .gw-page { padding: 12px; }
      .gw-stats .col-sm-2 { float: left; width: 50%; }
      .gw-stat { font-size: 24px; }
    }
  "

  stat_card <- function(label, value){
    shiny::div(class = "gw-card", shiny::div(class = "gw-stat", value), shiny::div(class = "gw-label", label))
  }
  value_or <- function(x, y){
    if(is.null(x) || !length(x) || anyNA(x)) y else x
  }
  route_plot_height <- function(n, base = 520L){
    paste0(max(base, 220L + as.integer(n) * 18L), "px")
  }

  ui <- shiny::fluidPage(
    class = "gw-page",
    shiny::tags$head(shiny::tags$style(css)),
    shiny::h2(class = "gw-title", "GTFSwizard Explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectizeInput(
          "selected_routes",
          "Routes",
          choices = all_routes,
          selected = character(),
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All routes")
        ),
        shiny::checkboxGroupInput(
          "selected_patterns",
          "Service patterns",
          choices = all_patterns,
          selected = all_patterns
        ),
        shiny::selectizeInput(
          "selected_services",
          "Services",
          choices = all_services,
          selected = all_services,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        shiny::selectizeInput(
          "selected_stops",
          "Stops",
          choices = NULL,
          selected = character(),
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All stops")
        ),
        shiny::dateRangeInput(
          "selected_dates",
          "Service dates",
          start = date_range[1],
          end = date_range[2],
          min = date_range[1],
          max = date_range[2]
        ),
        shiny::sliderInput(
          "selected_hours",
          "Scheduled hours",
          min = hour_limits[1],
          max = hour_limits[2],
          value = hour_limits,
          step = 1
        ),
        shiny::div(
          class = "gw-card",
          shiny::h4("Routes"),
          shiny::div(
            class = "gw-table-scroll",
            shiny::tableOutput("route_table")
          )
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::uiOutput("summary_cards"),
        shiny::tabsetPanel(
          id = "dashboard_view",
          shiny::tabPanel(
            "Network",
            shiny::div(
              class = "gw-map",
              leaflet::leafletOutput("network_map", height = "66vh")
            )
          ),
          shiny::tabPanel(
            "Service",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Frequency",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("frequency_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Headways",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("headway_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Route frequency",
                shiny::div(
                  class = "gw-card",
                  shiny::numericInput(
                    "route_frequency_top_n",
                    "Routes to show",
                    value = 25L,
                    min = 1L,
                    max = length(all_routes),
                    step = 1L
                  ),
                  shiny::uiOutput("route_frequency_plot_ui")
                )
              ),
              shiny::tabPanel(
                "Service span",
                shiny::div(
                  class = "gw-card",
                  shiny::numericInput(
                    "service_span_top_n",
                    "Routes to show",
                    value = 25L,
                    min = 1L,
                    max = length(all_routes),
                    step = 1L
                  ),
                  shiny::uiOutput("service_span_plot_ui")
                )
              ),
              shiny::tabPanel(
                "Weekday and hour",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("service_heatmap_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Fleet",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("fleet_plot", height = "520px")
                )
              )
            )
          ),
          shiny::tabPanel(
            "Performance",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Speed",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("speed_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Dwell time",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("dwell_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Trip duration",
                shiny::div(
                  class = "gw-card",
                  shiny::numericInput(
                    "route_duration_top_n",
                    "Routes to show",
                    value = 15L,
                    min = 1L,
                    max = length(all_routes),
                    step = 1L
                  ),
                  shiny::uiOutput("route_duration_plot_ui")
                )
              ),
              shiny::tabPanel(
                "Service supply",
                shiny::div(
                  class = "gw-card",
                  shiny::numericInput(
                    "service_supply_top_n",
                    "Routes to show",
                    value = 15L,
                    min = 1L,
                    max = length(all_routes),
                    step = 1L
                  ),
                  shiny::uiOutput("service_supply_plot_ui")
                )
              )
            )
          ),
          shiny::tabPanel(
            "Calendar",
            shiny::div(
              class = "gw-card",
              shiny::plotOutput("calendar_plot", height = "520px")
            )
          ),
          shiny::tabPanel(
            "Corridors and hubs",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Corridors",
                shiny::div(
                  class = "gw-card",
                  shiny::fluidRow(
                    shiny::column(
                      4,
                      shiny::numericInput(
                        "corridor_i",
                        "Corridor share",
                        value = 0.01,
                        min = 0.001,
                        max = 1,
                        step = 0.005
                      )
                    ),
                    shiny::column(
                      4,
                      shiny::numericInput(
                        "corridor_min_length",
                        "Minimum length (m)",
                        value = 1500,
                        min = 0,
                        step = 100
                      )
                    )
                  ),
                  shiny::plotOutput("corridor_plot", height = "620px")
                )
              ),
              shiny::tabPanel(
                "Hubs",
                shiny::div(
                  class = "gw-card",
                  shiny::fluidRow(
                    shiny::column(
                      4,
                      shiny::numericInput(
                        "hubs_i",
                        "Hub share",
                        value = 0.05,
                        min = 0.001,
                        max = 1,
                        step = 0.005
                      )
                    )
                  ),
                  shiny::plotOutput("hubs_plot", height = "620px")
                )
              )
            )
          ),
          shiny::tabPanel(
            "Agency",
            shiny::div(
              class = "gw-card",
              shiny::h4("Agency"),
              shiny::div(
                class = "gw-table-scroll",
                shiny::tableOutput("agency_table")
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session){
    shiny::updateSelectizeInput(
      session, "selected_stops", choices = stop_choices, server = TRUE
    )

    filtered_gtfs <- shiny::reactive({
      g <- gtfs
      routes <- input$selected_routes
      if(!is.null(routes) && length(routes)){
        g <- GTFSwizard::filter_route(g, routes)
      }
      services <- input$selected_services
      if(is.null(services) || !length(services)){
        services <- all_services
      }
      patterns <- input$selected_patterns
      if(is.null(patterns) || !length(patterns)){
        patterns <- all_patterns
      }
      pattern_services <- patterns_table$service_id[
        patterns_table$service_pattern %in% patterns
      ]
      selected_services <- intersect(services, pattern_services)
      available_services <- intersect(selected_services, unique(g$trips$service_id))
      if(length(available_services)){
        g <- GTFSwizard::filter_service(g, available_services)
      } else {
        g <- prune_gtfs(g, character())
      }
      dates <- input$selected_dates
      if(!is.null(dates) && length(dates) == 2L && !anyNA(dates)){
        selected_dates <- seq(as.Date(dates[1]), as.Date(dates[2]), by = "day")
        if(!identical(range(selected_dates), date_range)){
          selected_dates <- selected_dates[
            selected_dates %in% as.Date(g$dates_services$date)
          ]
          g <- if(length(selected_dates)){
            GTFSwizard::filter_date(g, selected_dates)
          } else {
            prune_gtfs(g, character())
          }
        }
      }
      stops <- input$selected_stops
      if(!is.null(stops) && length(stops)){
        available_stops <- intersect(stops, unique(g$stops$stop_id))
        g <- if(length(available_stops)){
          GTFSwizard::filter_stop(g, available_stops)
        } else {
          prune_gtfs(g, character())
        }
      }
      hours <- input$selected_hours
      if(!is.null(hours) && length(hours) == 2L &&
         !identical(as.numeric(hours), as.numeric(hour_limits))){
        g <- GTFSwizard::filter_time(
          g,
          from = seconds_to_gtfs_time(hours[1] * 3600),
          to = seconds_to_gtfs_time(hours[2] * 3600)
        )
      }
      g
    })

    dashboard_gtfs <- function(){
      g <- filtered_gtfs()
      shiny::validate(
        shiny::need(nrow(g$trips) > 0, "No trips match the selected filters."),
        shiny::need(nrow(g$stop_times) > 0, "No stop calls match the selected filters.")
      )
      g
    }

    output$summary_cards <- shiny::renderUI({
      g <- filtered_gtfs()
      agency_count <- if("agency_id" %in% names(g$agency)){
        length(unique(g$agency$agency_id))
      }else{
        nrow(g$agency)
      }
      shiny::fluidRow(
        class = "gw-stats",
        shiny::column(2, stat_card("Routes", nrow(g$routes))),
        shiny::column(2, stat_card("Trips", nrow(g$trips))),
        shiny::column(2, stat_card("Stops", nrow(g$stops))),
        shiny::column(2, stat_card("Shapes", length(unique(g$shapes$shape_id)))),
        shiny::column(2, stat_card("Service days", length(unique(g$dates_services$date)))),
        shiny::column(2, stat_card("Agencies", agency_count))
      )
    })

    output$agency_table <- shiny::renderTable({
      filtered_gtfs()$agency
    }, striped = TRUE, spacing = "xs")

    output$network_map <- leaflet::renderLeaflet({
      g <- dashboard_gtfs()
      shapes_sf <- GTFSwizard::get_shapes_sf(g$shapes)
      stops_sf <- GTFSwizard::get_stops_sf(g$stops)
      stop_counts <- g$stop_times %>%
        dplyr::group_by(stop_id) %>%
        dplyr::summarise(trips = dplyr::n(), .groups = "drop")
      stops_sf <- stops_sf %>%
        dplyr::left_join(stop_counts, by = "stop_id") %>%
        dplyr::mutate(trips = tidyr::replace_na(trips, 0))
      route_shapes <- g$trips |>
        dplyr::select(route_id, shape_id) |>
        dplyr::filter(!is.na(shape_id), nzchar(as.character(shape_id))) |>
        dplyr::distinct() |>
        dplyr::left_join(shapes_sf, by = "shape_id") |>
        sf::st_as_sf()
      routes <- sort(unique(route_shapes$route_id))
      route_colors <- if(length(routes) <= 20L){
        stats::setNames(gtfswizard_palette(length(routes)), routes)
      } else {
        stats::setNames(rep(gtfswizard_colors()[["blue"]], length(routes)), routes)
      }

      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Light") %>%
        leaflet::addTiles(group = "OSM") %>%
        leaflet::addLayersControl(baseGroups = c("Light", "OSM")) %>%
        leaflet::addPolylines(
          data = route_shapes,
          color = unname(route_colors[as.character(route_shapes$route_id)]),
          weight = 4,
          opacity = 0.88,
          popup = ~paste0("Route: ", route_id)
        ) %>%
        leaflet::addCircleMarkers(
          data = stops_sf,
          radius = 2,
          stroke = FALSE,
          fillOpacity = 0.7,
          color = "#20a39e",
          popup = ~paste0(stop_name, "<br>Trips: ", trips)
        )
    })

    output$frequency_plot <- shiny::renderPlot({
      GTFSwizard::plot_frequency(dashboard_gtfs())
    })

    output$fleet_plot <- shiny::renderPlot({
      fleet <- GTFSwizard::get_fleet(dashboard_gtfs(), method = "by.hour") %>%
        dplyr::mutate(hour = as.numeric(hour)) %>%
        dplyr::filter(is.finite(hour), is.finite(fleet))

      ggplot2::ggplot(fleet, ggplot2::aes(hour, fleet, color = service_pattern, group = service_pattern)) +
        ggplot2::geom_line(linewidth = 1) +
        hour_scale(fleet$hour) +
        ggplot2::labs(
          title = "Scheduled Fleet by Hour",
          subtitle = "Each line is one service pattern",
          x = "Scheduled clock hour",
          y = "Simultaneously active scheduled trips (vehicles)",
          color = "Service pattern"
        ) +
        theme_gtfswizard()
    })

    output$speed_plot <- shiny::renderPlot({
      speed <- GTFSwizard::get_speeds(dashboard_gtfs(), method = "by.route") %>%
        dplyr::filter(
          is.finite(average.speed), is.finite(trips),
          is.finite(pattern_frequency)
        )
      ggplot2::ggplot(speed, ggplot2::aes(average.speed, weight = trips * pattern_frequency)) +
        ggplot2::geom_histogram(fill = gtfswizard_colors()[["blue"]], color = "white", bins = 25) +
        ggplot2::labs(
          title = "Scheduled Route Speed Distribution",
          subtitle = "Each observation is a route-service pattern summary",
          x = "Average scheduled speed (km/h)",
          y = "Weighted route-pattern observations"
        ) +
        theme_gtfswizard()
    })

    output$headway_plot <- shiny::renderPlot({
      GTFSwizard::plot_headways(dashboard_gtfs())
    })

    output$service_heatmap_plot <- shiny::renderPlot({
      GTFSwizard::plot_serviceheatmap(dashboard_gtfs())
    })

    output$service_span_plot <- shiny::renderPlot({
      GTFSwizard::plot_servicespan(
        dashboard_gtfs(),
        top_n = as.integer(value_or(input$service_span_top_n, 25L))
      )
    })
    output$service_span_plot_ui <- shiny::renderUI({
      n <- value_or(input$service_span_top_n, 25L)
      shiny::plotOutput("service_span_plot", height = route_plot_height(n, 720L))
    })

    output$dwell_plot <- shiny::renderPlot({
      dwell_time <- GTFSwizard::get_dwelltimes(dashboard_gtfs(), method = "by.hour") %>%
        dplyr::filter(
          is.finite(average.dwelltime), is.finite(trips),
          is.finite(pattern_frequency)
        )
      ggplot2::ggplot(dwell_time, ggplot2::aes(average.dwelltime, weight = trips * pattern_frequency)) +
        ggplot2::geom_histogram(fill = gtfswizard_colors()[["coral"]], color = "white", bins = 25) +
        ggplot2::labs(
          title = "Scheduled Dwell Time Distribution",
          subtitle = "Each observation is a route-hour-service pattern summary",
          x = "Average scheduled dwell time (seconds)",
          y = "Weighted route-hour observations"
        ) +
        theme_gtfswizard()
    })

    output$route_duration_plot <- shiny::renderPlot({
      GTFSwizard::plot_routeduration(
        dashboard_gtfs(),
        top_n = as.integer(value_or(input$route_duration_top_n, 15L))
      )
    })
    output$route_duration_plot_ui <- shiny::renderUI({
      n <- value_or(input$route_duration_top_n, 15L)
      shiny::plotOutput("route_duration_plot", height = route_plot_height(n, 680L))
    })

    output$service_supply_plot <- shiny::renderPlot({
      GTFSwizard::plot_servicesupply(
        dashboard_gtfs(),
        top_n = as.integer(value_or(input$service_supply_top_n, 15L))
      )
    })
    output$service_supply_plot_ui <- shiny::renderUI({
      n <- value_or(input$service_supply_top_n, 15L)
      shiny::plotOutput("service_supply_plot", height = route_plot_height(n, 680L))
    })

    output$calendar_plot <- shiny::renderPlot({
      GTFSwizard::plot_calendar(dashboard_gtfs(), facet_by_year = TRUE)
    })

    output$corridor_plot <- shiny::renderPlot({
      tryCatch(
        GTFSwizard::plot_corridor(
          dashboard_gtfs(),
          i = input$corridor_i,
          min.length = input$corridor_min_length
        ),
        error = function(error){
          if(grepl("no consecutive stop pairs", conditionMessage(error), fixed = TRUE)){
            shiny::validate(shiny::need(FALSE, "No consecutive stop pairs match the selected filters."))
          }
          stop(error)
        }
      )
    })

    output$hubs_plot <- shiny::renderPlot({
      GTFSwizard::plot_hubs(dashboard_gtfs(), i = input$hubs_i)
    })

    output$route_table <- shiny::renderTable({
      g <- filtered_gtfs()
      routes_summary <- g$routes %>%
        dplyr::left_join(
          g$trips %>%
            dplyr::group_by(route_id) %>%
            dplyr::summarise(trips = dplyr::n(), .groups = "drop"),
          by = "route_id"
        ) %>%
        dplyr::mutate(trips = tidyr::replace_na(trips, 0))
      routes_summary %>%
        dplyr::select(dplyr::any_of(c(
          "route_id", "route_short_name", "route_long_name", "trips"
        )))
    }, striped = TRUE, spacing = "xs")

    output$route_frequency_plot <- shiny::renderPlot({
      g <- dashboard_gtfs()
      GTFSwizard::plot_routefrequency(
        g,
        top_n = as.integer(value_or(input$route_frequency_top_n, 25L))
      )
    })
    output$route_frequency_plot_ui <- shiny::renderUI({
      n <- min(nrow(dashboard_gtfs()$routes), value_or(input$route_frequency_top_n, 25L))
      shiny::plotOutput("route_frequency_plot", height = route_plot_height(n, 520L))
    })
  }

  shiny::shinyApp(ui, server)
}
