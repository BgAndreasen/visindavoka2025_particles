# app.R
# Minimal Shiny app for LADiM Parquet files
# - Filter by domain, release_time, and release_depth
# - Click map to snap to nearest release point and animate the particle
# Requires: shiny, leaflet, sf (with GeoParquet support), arrow, dplyr

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(arrow)
library(sfarrow)

options(arrow.use_threads = TRUE)

# -----------------------------
# File paths (adjust if needed)
# -----------------------------
PARQ_DIR     <- "parquet"
RELEASE_FILE <- file.path(PARQ_DIR, "release_points.parquet")
POINTS_FILE  <- file.path(PARQ_DIR, "points.parquet")

# -----------------------------
# Data: load release points (small)
# -----------------------------
# Prefer sf::st_read() (GeoParquet). If your GDAL lacks Parquet support,
# you can load lon/lat via arrow and st_as_sf() — see fallback below.
release_sf <- sfarrow::st_read_parquet(RELEASE_FILE, quiet = TRUE) # expects EPSG:4326
# Ensure columns we rely on exist:
stopifnot(all(c("pid","release_time","release_depth","domain") %in% names(release_sf)))

# Precompute UI choices and ranges
domains <- sort(unique(release_sf$domain))
rt_min  <- min(release_sf$release_time, na.rm = TRUE)
rt_max  <- max(release_sf$release_time, na.rm = TRUE)
depth_min <- floor(min(release_sf$release_depth, na.rm = TRUE))
depth_max <- ceiling(max(release_sf$release_depth, na.rm = TRUE))

# Build an Arrow dataset for points (lazy, columnar)
points_ds <- open_dataset(POINTS_FILE, format = "parquet")  # single-file dataset is OK

# -----------------------------
# Shiny UI
# -----------------------------
ui <- fluidPage(
  titlePanel("LADiM trajectories – filter, snap, animate"),
  sidebarLayout(
    sidebarPanel(
      selectInput("domain", "Domain", choices = domains, selected = domains[1]),
      dateRangeInput("rt_range", "Release time (UTC)",
                     start = as.Date(rt_min), end = as.Date(rt_max),
                     min = as.Date(rt_min), max = as.Date(rt_max)),
      sliderInput("depth", "Release depth (m):",
                  min = depth_min, max = depth_max, value = c(depth_min, depth_max), step = 1),
      tags$hr(),
      sliderInput("speed", "Animation speed (ms per step):", min = 10, max = 500, value = 100, step = 10),
      checkboxInput("show_path", "Show full path while animating", value = TRUE),
      helpText("Click on the map to snap to the nearest release point within the filters.")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {

  # reactive: filtered release points
  r_release <- reactive({
    req(input$domain, input$rt_range, input$depth)
    rp <- release_sf |>
      filter(
        domain == input$domain,
        as.Date(release_time) >= input$rt_range[1],
        as.Date(release_time) <= input$rt_range[2],
        release_depth >= input$depth[1],
        release_depth <= input$depth[2]
      )
    rp
  })

  # Base map
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 0, lat = 0, zoom = 2)
  })

  # Draw filtered release points
  observe({
    rp <- r_release()
    pal_rel <- colorNumeric("Blues", domain = rp$release_depth, na.color = "#999999")
    leafletProxy("map") |>
      clearMarkers() |>
      clearShapes() |>
      addCircleMarkers(
        data = rp,
        radius = 5,
        stroke = TRUE, weight = 1, fillOpacity = 0.9,
        color = ~pal_rel(release_depth),
        popup = ~paste0("pid: ", pid,
                        "<br>release_time: ", release_time,
                        "<br>release_depth: ", round(release_depth, 2))
      ) |>
      addLegend("bottomright", pal = pal_rel, values = rp$release_depth, title = "Release depth (m)")
  })

  # State for animation
  sel_pid   <- reactiveVal(NULL)
  anim_df   <- reactiveVal(NULL)
  anim_idx  <- reactiveVal(1)
  anim_timer <- reactiveVal(NULL)

  # Helper: color palette for age (continuous)
  age_pal <- reactive({
    # If no anim_df yet, default range
    rng <- c(0, 1)
    if (!is.null(anim_df())) {
      rng <- range(anim_df()$age_hours, na.rm = TRUE)
      if (!is.finite(diff(rng)) || diff(rng) == 0) rng <- c(rng[1], rng[1] + 1e-6)
    }
    colorNumeric("Viridis", domain = rng, na.color = "#CCCCCC")
  })

  # On map click: choose nearest filtered release point, then fetch that pid's points lazily
  observeEvent(input$map_click, {
    click <- input$map_click
    rp <- r_release()
    if (nrow(rp) == 0) return(NULL)

    # Find nearest release point to clicked location
    click_pt <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
    nearest_idx <- sf::st_nearest_feature(click_pt, rp)
    target <- rp[nearest_idx, , drop = FALSE]
    sel_pid(target$pid[[1]])

    # Fetch that particle's time series from the Arrow dataset (only needed columns)
    # NOTE: we DO NOT load the entire points parquet.
    pid_val <- sel_pid()
    # For safety, also filter by domain/time window to leverage row-group selection
    tmin <- as.POSIXct(paste(input$rt_range[1], "00:00:00"), tz = "UTC")
    tmax <- as.POSIXct(paste(input$rt_range[2], "23:59:59"), tz = "UTC")

    df <- points_ds |>
      filter(
        pid == pid_val,
        domain == input$domain,
        time >= tmin, time <= tmax
      ) |>
      select(pid, time, step, lon, lat, age_hours) |>
      arrange(step) |>
      collect()

    if (nrow(df) == 0) return(NULL)

    anim_df(df)
    anim_idx(1)

    # Center/zoom map on the selected track
    leafletProxy("map") |>
      flyTo(lng = df$lon[1], lat = df$lat[1], zoom = 9)

    # Draw full path if requested
    proxy <- leafletProxy("map") |> clearGroup("selected_path") |> clearGroup("anim_marker")
    if (isTRUE(input$show_path)) {
      proxy <- proxy |>
        addPolylines(lng = df$lon, lat = df$lat,
                     group = "selected_path", weight = 3, opacity = 0.7)
    }

    # Place initial marker
    pal <- age_pal()
    proxy |>
      addCircleMarkers(lng = df$lon[1], lat = df$lat[1],
                       color = pal(df$age_hours[1]),
                       radius = 6, weight = 2, fillOpacity = 1,
                       group = "anim_marker")
  })

  # Animation loop: advances the marker over the selected particle
  observe({
    df <- anim_df()
    req(df)
    idx <- anim_idx()

    # schedule next tick
    invalidateLater(input$speed, session)

    # advance
    idx <- idx + 1
    if (idx > nrow(df)) idx <- 1
    anim_idx(idx)

    pal <- age_pal()
    leafletProxy("map") |>
      clearGroup("anim_marker") |>
      addCircleMarkers(
        lng = df$lon[idx], lat = df$lat[idx],
        color = pal(df$age_hours[idx]),
        radius = 6, weight = 2, fillOpacity = 1,
        group = "anim_marker",
        popup = paste0(
          "pid: ", df$pid[idx],
          "<br>time: ", df$time[idx],
          "<br>age(h): ", round(df$age_hours[idx], 2),
          "<br>step: ", df$step[idx]
        )
      )
  })

}

shinyApp(ui, server)
