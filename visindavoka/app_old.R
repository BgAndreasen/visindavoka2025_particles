# app.R
# Packages: install.packages(c("shiny","leaflet","sf","geojsonsf","geosphere","htmlwidgets"))

library(shiny)
library(leaflet)
library(sf)
library(geojsonsf)
library(geosphere)
library(htmlwidgets)
library(bslib)
library(bsicons)

# --- Config ----------------------------------------------------------------
GEOJSON_ROOT <- "geojson"          # folder with domain/start/seed_ptype.geojson.gz
SEEDS_CSV    <- "data/seeds.csv"   # columns: seed_id, lon, lat, domain
JS_MovingMarker <- "js/Leaflet.MovingMarker.js"

# --- Helpers ---------------------------------------------------------------
read_geojson_gz <- function(fpath) {
  con <- gzfile(fpath, open = "rt"); on.exit(close(con), add = TRUE)
  txt <- paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "")
  geojsonsf::geojson_sf(txt)  # sf object
}

list_starts <- function(domain) {
  d <- file.path(GEOJSON_ROOT, domain)
  if (!dir.exists(d)) return(character(0))
  sort(list.dirs(d, full.names = FALSE, recursive = FALSE))
}

nearest_seed <- function(lon, lat, domain, seeds) {
  rows <- seeds[seeds$domain == domain, ]
  if (!nrow(rows)) return(NULL)
  d <- geosphere::distHaversine(cbind(rows$lon, rows$lat), c(lon, lat))
  i <- which.min(d)
  list(seed_id = rows$seed_id[i], lon = rows$lon[i], lat = rows$lat[i])
}

simplify_traj_m <- function(traj_sf, tol_m = 30, crs_m = 5316) {
  g_m  <- st_transform(traj_sf, crs_m)
  g_sm <- st_simplify(g_m, dTolerance = tol_m, preserveTopology = TRUE)
  st_transform(g_sm, 4326)
}

# --- Data ------------------------------------------------------------------
seeds <- read.csv(SEEDS_CSV, stringsAsFactors = FALSE)
stopifnot(all(c("seed_id","lon","lat","domain") %in% names(seeds)))
domains <- sort(unique(seeds$domain))

# --- UI --------------------------------------------------------------------
ui <- page_navbar(
  title = "🌊 Bitlaspjaðing",
  id = "navbar",
  tags$head(
    tags$script(src = JS_MovingMarker),
    tags$style(HTML("
    
    .bounce-animation {
        animation: bounce 2s infinite;
      }
    
    @keyframes bounce {
        0%, 20%, 60%, 100% {
          transform: translateY(0);
        }
        40% {
          transform: translateY(-20px);
        }
        80% {
          transform: translateY(-10px);
        }
      }

    .duckEmoji{font-size:26px;line-height:26px;filter:drop-shadow(0 0 2px #fff);}
    .duckInactive{filter:grayscale(1) drop-shadow(0 0 2px #fff); opacity:0.6;}
                    ")),
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
      map._duckMarkers.forEach(function(l){ try{ if(msg.cmd==='pause'&&l.pause) l.pause();
                                            if(msg.cmd==='resume'&&l.start) l.start(); }catch(e){} });
    });

    // --- Add a trail (and fade older ones) ---------------------------------
    Shiny.addCustomMessageHandler('addTrail', function(msg){
      var map = window.LEAFLET_MAP; if (!map) return;
      _ensureGroups(map);
      var keepLast   = (msg && msg.keepLast)   || 6;      // how many trails to keep visible
      var trimOldest = (msg && msg.trimOldest) || true;   // drop oldest beyond keepLast

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
  
  #titlePanel("Vísindavøka – Moving Ducks"),
  nav_panel(
    title = div(tags$img(src = "img/duck.png", height = "30px"), "Dunnir"),
    value = "duck",
  sidebarLayout(
    sidebarPanel(
      div(
      style = "text-align: center; padding: 10px; background: linear-gradient(135deg, #FFE66D, #FFD93D); min-height: 100px; border-radius: 15px; margin: 10px;",
      div(class = "bounce-animation", style = "font-size: 100px; margin-bottom: 10px;", tags$img(src = "img/duck.png", height = "100px")),
      div(style = "font-size: 36px; font-weight: bold; color: #8B4513; margin-bottom: 10px;", "Plast dunna"),
      div(style = "font-size: 20px; color: #8B4513;", "Gvagg gvagg! eg bara flóti...")
    ),
      selectInput("domain", "Domain", choices = domains, selected = domains[1]),
      uiOutput("start_ui"),
      #radioButtons("ptype", "Particle type",
      #             choices = c("duck","lice","fecal"), selected = "lice", inline = TRUE),
                   #sliderInput("speed_kmh", "Duck speed (km/h)", min = 0.2, max = 8, value = 1.0, step = 0.1),
      #
      sliderInput("mps", "Sim minutes per real second", min = 5, max = 120, value = 60, step = 10),
      
      # inline control buttons
      div(
        style = "display: flex; gap: 10px; align-items: center; margin: 20px 0; flex-wrap: wrap;",
      
      actionButton(
        "resume_btn", 
        label = div(bs_icon("play-fill"), "Play"),
        style = "background: #28a745; color: white; border: none; padding: 8px 16px; border-radius: 6px; display: flex; align-items: center; gap: 5px;"
      ),
      
      actionButton(
        "pause_btn", 
        label = div(bs_icon("pause-fill"), "Pause"),
        style = "background: #ffc107; color: #212529; border: none; padding: 8px 16px; border-radius: 6px; display: flex; align-items: center; gap: 5px;"
      ),
      
      actionButton(
        "clear", 
        label = div(bs_icon("trash3-fill"), "Clear"),
        style = "background: #dc3545; color: white; border: none; padding: 8px 16px; border-radius: 6px; display: flex; align-items: center; gap: 5px;"
      )
    ),
      
      #actionButton("pause","Pause"),
      #actionButton("resume","Resume"),
      #actionButton("clear", "Clear paths & ducks"),
      helpText("Click the map to launch a duck. Each click adds another moving duck and keeps the trail.")
    ),
    mainPanel(
      leafletOutput("map", height = 640)
    )
  )
),
nav_panel(
  title = div(tags$img(src = "img/Pile of Poo.png", height = "30px"), " Skarn"),
  value = "poop"
),
nav_panel(
   title = div(tags$img(src = "img/info_icon.png", height = "30px"), "Info"),
   value = "info"
)

)

# --- Server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # what tab is selected?
  current_tab <- reactive({
      input$navbar
    })

  # Populate start times when domain changes
  observeEvent(input$domain, {
    starts <- list_starts(input$domain)
    output$start_ui <- renderUI({
      selectInput("start", "Start time",
                  choices = starts, selected = if (length(starts)) head(starts, 1) else NULL)
    })
  }, ignoreInit = FALSE)
  
  output$map <- renderLeaflet({
    ctr <- c(mean(seeds$lat, na.rm = TRUE), mean(seeds$lon, na.rm = TRUE))
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      #addScaleBar(position = "bottomleft") |>
      setView(lng = mean(seeds$lon), lat = mean(seeds$lat), zoom = 10) |>
      htmlwidgets::onRender("function(el,x){ _registerMapInline(this); }")
  })
  
  # Clear both trails (group 'traj') and all moving markers
  observeEvent(input$clear, {
    leafletProxy("map") |> clearGroup("start")
    session$sendCustomMessage("clearDucks", list())
    session$sendCustomMessage("clearTrails", list())
  })
  
  # clear markers and trails on domain or start change
  observeEvent(list(input$domain, input$start), {
    leafletProxy("map") |> clearGroup("start")
    session$sendCustomMessage("clearDucks",  list())
    session$sendCustomMessage("clearTrails", list())
  }, ignoreInit = TRUE)
  
  # Controls
  observeEvent(input$pause_btn,  { session$sendCustomMessage("ducksCtrl", list(cmd="pause")) })
  observeEvent(input$resume_btn, { session$sendCustomMessage("ducksCtrl", list(cmd="resume")) })
  
  # On click: snap to nearest seed, draw trail, launch moving duck
  observeEvent(input$map_click, {
    req(input$domain, input$start, input$navbar)
    click <- input$map_click
    snap  <- nearest_seed(click$lng, click$lat, input$domain, seeds)
    if (is.null(snap)) {
      showNotification("No seeds for this domain.", type = "warning"); return()
    }
    
    fpath <- file.path(GEOJSON_ROOT, input$domain, input$start,
                       sprintf("%s_%s.geojson.gz", snap$seed_id, input$navbar))
    if (!file.exists(fpath)) {
      showNotification(sprintf("No trajectory for seed %s at %s (%s).",
                               snap$seed_id, input$start, input$navbar),
                       type = "warning", duration = 5)
      return()
    }
    
    traj <- tryCatch(read_geojson_gz(fpath), error = function(e) NULL)
    if (is.null(traj)) {
      showNotification("Failed to read trajectory file.", type = "error"); return()
    }
    
    # 1) Draw full trail (kept on map; many clicks allowed)
    leafletProxy("map") |> 
      addCircleMarkers(lng = snap$lon, lat = snap$lat, radius = 2,
                       color = NA, 
                       fill = "#333", fillOpacity = 0.5, group = "start")#|>
      #addPolylines(data = traj, color = "blue", weight = 4, opacity = 0.5, group = "traj",
      #             label = paste0("Seed ", snap$seed_id, " • ", input$start, " • ", input$ptype))
    
    # coords is n x 2 [lon, lat] from the LINESTRING
    coords <- unclass(sf::st_coordinates(traj))[, 1:2, drop = FALSE]
    if (nrow(coords) < 2) return()
    
    # ---- thin if long ----
    thin_factor <- 1L
    if (nrow(coords) > 1000L) {
      thin_factor <- 10L                      # your "keep every 5th"
      idx <- unique(c(1L,
                      seq(1L, nrow(coords), by = thin_factor),
                      nrow(coords)))         # <- ensure last point kept
      coords <- coords[idx, , drop = FALSE]
    }
    
    # 1) add the trail (JS will fade older ones)
    latlng <- lapply(seq_len(nrow(coords)), function(i) unname(c(coords[i, 2], coords[i, 1])))  # [lat,lng]
    session$sendCustomMessage("addTrail", list(
      coords    = latlng,
      keepLast  = 6,     # how many trails to keep visible
      trimOldest= TRUE   # remove older beyond keepLast
    ))
    
    # 2) launch the duck with constant playback speed (points/second slider)
    ms_per_seg <- (1000 / input$mps) * thin_factor
    times_v    <- unname(rep(ms_per_seg, nrow(coords) - 1))
    
    pngPath    <- if (file.exists("www/img/duck.png")) "img/duck.png" else NULL
    
    session$sendCustomMessage("addDuck", list(
      coords  = latlng,
      times   = as.numeric(times_v),
      iconPng = pngPath
    ))
    
    # # --- constant time per segment based on slider ---
    # # Each GeoJSON vertex = one 60 s model step; user chooses how fast to play steps.
    # ms_per_seg <- 1000 / input$mps        # pps = points/second -> ms per segment
    # nseg <- nrow(coords) - 1
    # times_v <- rep(ms_per_seg, nseg)
    # times_v <- unname(as.numeric(times_v)) # ensure no names (avoid jsonlite warning)
    # 
    # # MovingMarker wants [lat, lng]
    # latlng <- lapply(seq_len(nrow(coords)), function(i) unname(c(coords[i, 2], coords[i, 1])))
    # 
    # pngPath <- if (file.exists("www/img/duck_324830.png")) "img/duck_324830.png" else NULL
    # session$sendCustomMessage("addDuck", list(
    #   coords  = latlng,
    #   times   = times_v,
    #   iconPng = pngPath
    # ))
  })
}

shinyApp(ui, server)
