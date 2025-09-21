# app_parquet_2025-09-16.R
# Shiny app that uses GeoParquet (LineStrings + release points) instead of geojson.gz

# ---- Packages --------------------------------------------------------------
library(shiny)
library(leaflet)
library(sf)
library(dplyr) 
library(arrow)
library(htmlwidgets)
library(bslib)
library(bsicons)
library(sfarrow)

# ---- Config ---------------------------------------------------------------
# Set these to your GeoParquet files produced by your builder script
PARQUET_TRAJ <- "parquet/trajectories.parquet" # LineStrings with columns: pid, poly_id, particle_type, release_time, geometry
PARQUET_REL  <- "parquet/release_points.parquet"    # Points with columns: pid, poly_id, particle_type, release_time, release_depth, geometry
PARQUET_POINTS <- "parquet/points.parquet"
DEFAULT_DOMAIN <- "farcoast"                 # Domain label (added if Parquet lacks a domain column)
JS_MovingMarker <- "js/Leaflet.MovingMarker.js"

# ---- Load Parquet once ----------------------------------------------------
traj_sf <- st_read_parquet(PARQUET_TRAJ)
rel_sf  <- st_read_parquet(PARQUET_REL)
pts_ds  <- open_dataset(PARQUET_POINTS, format = "parquet")

# normalize columns
ttraj <- traj_sf |>
  rename(ptype = particle_type, seed_id = poly_id)
trel  <- rel_sf  |>
  rename(ptype = particle_type, seed_id = poly_id)

fmt_start <- function(t) format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H-%MZ")
ttraj$start <- fmt_start(ttraj$release_time)
trel$start  <- fmt_start(trel$release_time)

# ---- Seeds & index --------------------------------------------------------
seeds <- trel |>
  mutate(lon = st_coordinates(geometry)[,1], lat = st_coordinates(geometry)[,2]) |>
  group_by(domain, ptype, seed_id, start, release_depth) |>
  summarise(lon = first(lon), lat = first(lat), .groups = "drop") |>
  as.data.frame()

traj_index <- ttraj |>
  st_drop_geometry() |>
  select(domain, start, release_depth, ptype, seed_id, pid) |>
  distinct()

# ---- Helpers --------------------------------------------------------------
nearest_seed <- function(lon, lat, seeds_subset) {
  if (is.null(seeds_subset) || !nrow(seeds_subset)) return(NULL)
  d <- geosphere::distHaversine(cbind(seeds_subset$lon, seeds_subset$lat), c(lon, lat))
  i <- which.min(d)
  list(seed_id = seeds_subset$seed_id[i], lon = seeds_subset$lon[i], lat = seeds_subset$lat[i])
}

traj_from_points <- function(domain, start, depth, ptype, seed_id){
  
  depth_numeric = as.numeric(depth)
  start_dt <- as.POSIXct(start, format="%Y-%m-%dT%H-%MZ", tz="UTC")
  
  rel_sub <- trel |>
    filter(
      .data$domain == !!domain, 
      .data$start == !!start, 
      .data$release_depth == !!depth_numeric,
      .data$ptype == !!ptype, 
      .data$seed_id == !!seed_id)
  if (!nrow(rel_sub)) return(NULL)

  pid_pick <- rel_sub$pid[[1]]

  tbl <- pts_ds |>
    filter(.data$domain == !!domain,
           .data$ptype  == !!ptype,
           .data$poly_id == !!seed_id,
           .data$pid    == !!pid_pick) |>
    select(lon, lat, time, age_min) |>
    arrange(time) |>
    collect()
  if (!nrow(tbl)) return(NULL)

  coords <- as.matrix(tbl[, c("lon","lat")])
  seg_ms <- diff(tbl$age_min) * 60 * 1000

  list(coords = coords, age_min = tbl$age_min, seg_ms = as.numeric(seg_ms), pid = pid_pick)
}


# Return n×2 matrix [lon, lat] for a chosen (domain, start, ptype, seed_id)
traj_coords_from_geoparquet <- function(domain, start, depth, ptype, seed_id) {
  sub <- traj_sf |>
    dplyr::filter(.data$domain == domain,
                  .data$start  == start,
                  .data$release_depth == depth,
                  .data$particle_type == ptype,
                  .data$poly_id == seed_id)
  if (nrow(sub) == 0) return(NULL)
  # pick first pid per seed: smallest pid
  sub <- sub[order(sub$pid), ][1, , drop = FALSE]
  coords <- sf::st_coordinates(sub$geometry)
  if (is.null(coords) || nrow(coords) < 2) return(NULL)
  as.matrix(coords[, c("X","Y"), drop = FALSE])
}

# ---- Module: UI -----------------------------------------------------------
mod_traj_panel_ui <- function(id, title_label, icon_src, message, bg1 = "#FFE66D", bg2 = "#FFD93D", color = "#ffffff"){
  ns <- NS(id)
  nav_panel(
    title = div(tags$img(src = icon_src, height = "30px"), title_label),
    value = id,
    sidebarLayout(
      sidebarPanel(
        div(
          style = glue::glue("text-align: center; padding: 10px; background: linear-gradient(135deg, {bg1}, {bg2}); min-height: 100px; border-radius: 15px; margin: 10px;"),
          div(class = "bounce-animation", style = "margin-bottom: 10px;", 
            tags$img(src = icon_src, height = "100px;")),
          div(style = glue::glue("font-size: 36px; font-weight: bold; color: {color}; margin-bottom: 10px;"), title_label),
          div(style = glue::glue("font-size: 20px; color: {color};"), message)
        ),
        uiOutput(ns("domain_ui")),
        uiOutput(ns("start_ui")),
        uiOutput(ns("depth_ui")),
        sliderInput(ns("mps"), "Sim minutes per real second", min = 5, max = 120, value = 60, step = 10),
        div(style = "display:flex; gap:10px; align-items:center; margin:20px 0; flex-wrap:wrap;",
            actionButton(ns("resume_btn"), label = div(bs_icon("play-fill"),  "Play"),  class = "btn btn-success"),
            actionButton(ns("pause_btn"),  label = div(bs_icon("pause-fill"), "Pause"), class = "btn btn-warning"),
            actionButton(ns("clear"),      label = div(bs_icon("trash3-fill"),"Clear"), class = "btn btn-danger")
        ),
        helpText("Click the map to launch a particle. Each click adds another moving particle and keeps the trail.")
      ),
      mainPanel(
        leafletOutput(ns("map"), height = 640)
      )
    )
  )
}

# ---- Module: Server -------------------------------------------------------
mod_traj_panel_server <- function(id, ptype){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # # Limit Domain choices to those that actually have this ptype
    # observe({
    #   doms <- sort(unique(traj_index$domain[ traj_index$ptype == ptype ]))
    #   if (length(doms) == 0) {
    #     showNotification("No domains available for this particle type.", type = "warning", duration = 5)
    #     updateSelectInput(session, "domain", choices = character(0), selected = character(0))
    #     updateSelectInput(session, "start",  choices = character(0), selected = character(0))
    #     return()
    #   }
    #   updateSelectInput(session, "domain", choices = doms, selected = doms[1])
    # })

    # domain filter
    observeEvent(ptype, {
      ptype_val <- ptype
      doms <- seeds |> filter(ptype == ptype_val) |> distinct(domain) |> pull(domain)
      output$domain_ui <- renderUI({
        selectInput(ns("domain"), "Domain", choices = doms, selected = if (length(doms)) head(doms,1) else NULL)
      })
    }, ignoreInit = FALSE)

    # starts filter
    observeEvent(input$domain, {
      req(input$domain)
      starts <- sort(unique(traj_index$start[
        traj_index$domain == input$domain & traj_index$ptype == ptype
      ]))
      output$start_ui <- renderUI({
        selectInput(ns("start"), "Start time", choices = starts, selected = if (length(starts)) head(starts,1) else NULL)
      })
    }, ignoreInit = FALSE)

    # depth filter
    observeEvent(c(input$domain, input$start), {
      req(input$domain, input$start)
      depths <- sort(unique(traj_index$release_depth[
        traj_index$domain == input$domain & 
          traj_index$ptype == ptype & 
          traj_index$start == input$start
      ]))
      output$depth_ui <- renderUI({
        selectInput(ns("depth"), "Release Depth", choices = depths, selected = if (length(depths)) head(depths,1) else NULL)
      })
    }, ignoreInit = FALSE)

    # Leaflet map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = mean(seeds$lon, na.rm=TRUE), lat = mean(seeds$lat, na.rm=TRUE), zoom = 10) |>
        htmlwidgets::onRender("function(el,x){ _registerMapInline(this); }")
    })

    # Clear controls
    observeEvent(input$clear, {
      leafletProxy(ns("map")) |> clearGroup("start")
      session$sendCustomMessage("clearDucks", list())
      session$sendCustomMessage("clearTrails", list())
    })
    observeEvent(input$pause_btn,  { session$sendCustomMessage("ducksCtrl", list(cmd="pause")) })
    observeEvent(input$resume_btn, { session$sendCustomMessage("ducksCtrl", list(cmd="resume")) })
    observeEvent(list(input$domain, input$start, input$depth), {
      leafletProxy(ns("map")) |> clearGroup("start")
      session$sendCustomMessage("clearDucks", list())
      session$sendCustomMessage("clearTrails", list())
    }, ignoreInit = TRUE)

    # Map click handler: restrict to seeds that exist for (domain, start, ptype)
    observeEvent(input$map_click, {
      req(input$domain, input$start, input$depth)
      click <- input$map_click

      seed_ids_here <- unique(traj_index$seed_id[
        traj_index$domain==input$domain &
          traj_index$start==input$start & 
          traj_index$ptype==ptype & 
          traj_index$release_depth==input$depth
      ])

      seeds_here <- seeds |>
        filter(domain == input$domain, ptype == ptype, seed_id %in% seed_ids_here) |>
        select(seed_id, lon, lat)

      snap <- nearest_seed(click$lng, click$lat, seeds_here)
      if (is.null(snap)) { showNotification("No seeds available for this domain/start and particle type.", type = "warning"); return() }

      res <- traj_from_points(domain = input$domain, start = input$start, depth = input$depth, ptype = ptype, seed_id = snap$seed_id)
      if (is.null(res) || is.null(res$coords) || nrow(res$coords) < 2) { showNotification("No trajectory points (Parquet).", type = "warning"); return() }
      coords <- res$coords
      
      if (is.null(coords) || nrow(coords) < 2) { showNotification("No trajectory points (GeoParquet).", type = "warning"); return() }

      leafletProxy(ns("map")) |>
        addCircleMarkers(lng = snap$lon, lat = snap$lat, radius = 2, color = NA, fill = "#333", fillOpacity = 0.5, group = "start")

      thin_factor <- if (nrow(coords) > 1000L) 10L else 1L
      if (thin_factor > 1L) {
        idx <- unique(c(1L, seq(1L, nrow(coords), by = thin_factor), nrow(coords)))
        coords <- coords[idx,, drop=FALSE]
      }

      latlng <- lapply(seq_len(nrow(coords)), function(i) unname(c(coords[i,2], coords[i,1])))

      session$sendCustomMessage("addTrail", list(
        coords = latlng,
        keepLast = 6,
        trimOldest = TRUE
      ))

      # derive animation times from real model age, scaled by mps
      base_times <- if (!is.null(res$seg_ms) && length(res$seg_ms) == (nrow(coords)-1)) res$seg_ms else rep(1000, nrow(coords)-1)
      times_v <- as.numeric(base_times / input$mps)
      age_min <- res$age_min
      pngPath <- "img/duck.png"

      session$sendCustomMessage("addDuck", list(
        coords = latlng,
        times = as.numeric(times_v),
        iconPng = pngPath,
        mps = as.numeric(input$mps)
      ))
    })
  })
}

# ---- UI -------------------------------------------------------------------
ui <- page_navbar(
  title = "🌊 Bitlaspjaðing",
  id = "navbar",
  tags$head(
    tags$script(src = JS_MovingMarker),

        # --- CSS ---------------------------------------------------------------
    tags$style(HTML("
      .bounce-animation { animation: bounce 2s infinite; }
      @keyframes bounce { 0%,20%,60%,100% { transform: translateY(0);} 40% { transform: translateY(-20px);} 80% { transform: translateY(-10px);} }
      .duckEmoji{ font-size:26px; line-height:26px; filter:drop-shadow(0 0 2px #fff);} 
      .duckInactive{ filter:grayscale(1) drop-shadow(0 0 2px #fff); opacity:0.6; }
    ")),

    # --- JS registry so multiple maps can coexist -------------------------
    tags$script(HTML("
    function _ensureGroups(map){
      if (!map._ducks)       { map._ducks = L.layerGroup().addTo(map); }
      if (!map._duckMarkers) { map._duckMarkers = []; }
      if (!map._trails)      { map._trails = L.layerGroup().addTo(map); }
      if (!map._trailLines)  { map._trailLines = []; }
    }
    function _registerMapInline(map){
      window.LEAFLET_MAP = map;
      _ensureGroups(map);
      var hasMM = !!(L && ((L.Marker && L.Marker.movingMarker) || L.movingMarker));
      console.log('MovingMarker present:', hasMM);
    }
    
    Shiny.addCustomMessageHandler('ducksCtrl', function(msg){
      var map = window.LEAFLET_MAP; if(!map||!map._duckMarkers) return;
      map._duckMarkers.forEach(function(l){
        try{
          if(msg.cmd==='pause'&&l.pause) l.pause();
          if(msg.cmd==='resume'&&l.start) l.start(); 
        } catch(e){} });
    });

    

    // --- Add a trail (and fade older ones) ---------------------------------
    Shiny.addCustomMessageHandler('addTrail', function(msg){
      var map = window.LEAFLET_MAP; if (!map) return;
      _ensureGroups(map);
      //var keepLast   = (msg && msg.keepLast)   || 6;      // how many trails to keep visible
      //var trimOldest = (msg && msg.trimOldest) || true;   // drop oldest beyond keepLast
      var keepLast   = (msg && typeof msg.keepLast   !== 'undefined') ? msg.keepLast   : 6;
      var trimOldest = (msg && typeof msg.trimOldest !== 'undefined') ? msg.trimOldest : true;


      var pts = (msg.coords||[]).map(function(p){ return L.latLng(+p[0], +p[1]); });
      if (pts.length < 2) return;

      var poly = L.polyline(pts, {color:'#38ebbb', weight:4, opacity:0.95}).addTo(map._trails);
      map._trailLines.push(poly);

      // fade/restyle all, newest first (index n-1)
      var n = map._trailLines.length;
      for (var i=0; i<n; i++){
        var line = map._trailLines[i];
        var age  = n - 1 - i;                          // 0=newest
        var t    = Math.min(age / Math.max(keepLast-1,1), 1);  // 0..1
        var col  = (age === 0) ? '#38ebbb' : '#8d9e99';        // newest blue, older gray
        var op   = 0.95 * (1 - 0.8*t);                 // 0.95 -> 0.19
        var wt   = 4 - 2*t;                            // 4 -> 2
        try { line.setStyle({color: col, opacity: op, weight: wt}); } catch(e){}
      }

      // optionally remove oldest beyond keepLast
      if (trimOldest && n > keepLast) {
        var excess = n - keepLast;
        for (var k=0; k<excess; k++){
          try { map._trails.removeLayer(map._trailLines[0]); } catch(e){}
          map._trailLines.shift();
        }
      }
    });

    // --- Add a duck that moves, then becomes grayscale when finished -------
    Shiny.addCustomMessageHandler('addDuck', function(msg){
      try {
        var map = window.LEAFLET_MAP; if (!map) return;
        _ensureGroups(map);

        var pts = (msg.coords||[]).map(function(p){ return L.latLng(+p[0], +p[1]); });
        if (pts.length < 2) return;

        var times = (msg.times||[]).map(function(x){ return Math.max(1,+x); });
        if (times.length !== pts.length - 1) times = Array(pts.length-1).fill(200);

        var MM = (L.Marker && L.Marker.movingMarker) ? L.Marker.movingMarker
                 : (L.movingMarker ? L.movingMarker : null);

        var opts = {};
        if (msg.iconPng) {
          opts.icon = L.icon({iconUrl: msg.iconPng, iconSize:[28,28], iconAnchor:[14,14]});
        } else {
          opts.icon = L.divIcon({html:'<div class=\"duckEmoji\">🦆</div>', className:'', iconSize:[26,26]});
        }

        if (!MM) {
          // fallback: static marker
          var m = L.marker(pts[0], opts).addTo(map._ducks);
          map._duckMarkers.push(m);
          return;
        }

        var mm = MM(pts, times, opts).addTo(map._ducks);
        if (typeof mm.start === 'function') mm.start();
        map._duckMarkers.push(mm);

        // When finished, gray it out (but keep on map)
        mm.on('end', function(){
          var el = mm._icon;
          if (el) { el.classList.add('duckInactive'); }
        });

      } catch(e){ console.error('addDuck error:', e); }
    });

    // --- Clear ducks & trails on button press ------------------------------
    Shiny.addCustomMessageHandler('clearDucks', function(msg){
      var map = window.LEAFLET_MAP; if (!map) return;
      if (map._duckMarkers) {
        map._duckMarkers.forEach(function(l){
          try { if (l.stop) l.stop(); if (l.pause) l.pause(); if (l._raf) cancelAnimationFrame(l._raf); } catch(e){}
          try { map.removeLayer(l); } catch(e){}
        });
        map._duckMarkers = [];
      }
      try { if (map._ducks) map.removeLayer(map._ducks); } catch(e){}
      map._ducks = null;
      _ensureGroups(map);
    });

    Shiny.addCustomMessageHandler('clearTrails', function(msg){
      var map = window.LEAFLET_MAP; if (!map) return;
      if (map._trails) { map._trails.clearLayers(); }
      map._trailLines = [];
    });

  "))
  ),

  mod_traj_panel_ui("duck", "Dunna", "img/duck.png", 
    message = "- gvagg gvagg! eg bara flóti...",
    color =  "#8B4513"
  ),
  mod_traj_panel_ui("poop", "Lortur",  "img/Pile of Poo.png", 
    message = "- blobb blobb! eg søkki á botn...",
    bg1 =  "#5b3e31",
    bg2 =  "#765341",
    color = "#d8cbc4"
  ),
  nav_panel(
    title = div(tags$img(src = "img/info_icon.png", height = "30px"), "Info"),
    value = "info"
  )
)

# ---- Server ---------------------------------------------------------------
server <- function(input, output, session){
  mod_traj_panel_server("duck", ptype = "duck")
  mod_traj_panel_server("poop", ptype = "poop")
}

shinyApp(ui, server)
