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

options(arrow.use_threads = TRUE)

# ---- Load Parquet once ----------------------------------------------------
pts_ds  <- open_dataset(PARQUET_POINTS, format = "parquet")

# Read the SMALL release_points.parquet as sf (only once) to get lon/lat
trel <- sfarrow::st_read_parquet(PARQUET_REL) |>
  dplyr::rename(ptype = particle_type, seed_id = poly_id)


fmt_start <- function(t) format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H-%MZ")
trel$start <- fmt_start(trel$release_time)

# ---- Seeds & index --------------------------------------------------------

# Make a flat data.frame: one row per seed, with lon/lat & selection keys
seeds <- trel |>
  mutate(
    lon = sf::st_coordinates(geometry)[,1],
    lat = sf::st_coordinates(geometry)[,2]
  ) |>
  st_set_geometry(NULL) |>
  # keep only what the app needs
  select(domain, ptype, seed_id, pid, start, release_depth, sink_vel, lon, lat) |>
  distinct()

# Compact index for UI dropdowns and click-snapping logic
traj_index <- seeds |>
  select(domain, start, release_depth, sink_vel, ptype, seed_id, pid) |>
  distinct()

# ---- Helpers --------------------------------------------------------------
nearest_seed <- function(lon, lat, seeds_subset) {
  if (is.null(seeds_subset) || !nrow(seeds_subset)) return(NULL)
  d <- geosphere::distHaversine(cbind(seeds_subset$lon, seeds_subset$lat), c(lon, lat))
  i <- which.min(d)
  list(seed_id = seeds_subset$seed_id[i], lon = seeds_subset$lon[i], lat = seeds_subset$lat[i])
}

traj_from_points <- function(domain, start, depth, sinkvel, ptype, seed_id){

  # choose one pid that matches the UI state + the snapped seed
  if (ptype == "duck") {
    rel_sub <- seeds |>
      filter(
        .data$domain == !!domain,
        .data$start  == !!start,
        .data$ptype  == !!ptype,
        .data$seed_id == !!seed_id,
        .data$release_depth == !!as.numeric(depth)
      )
  } else if (ptype == "poop") {
    rel_sub <- seeds |>
      filter(
        .data$domain == !!domain,
        .data$start  == !!start,
        .data$ptype  == !!ptype,
        .data$seed_id == !!seed_id,
        .data$sink_vel == !!as.numeric(sinkvel)
      )
  } else {
    rel_sub <- seeds[0,]
  }

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
        uiOutput(ns("sink_ui")),
        sliderInput(ns("mps"), "Model minutes pr. real second", min = 15, max = 120, value = 30, step = 15),
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
    observeEvent(c(input$domain, input$start, ptype), {
      req(input$domain, input$start)

      if(ptype == "duck"){
      depths <- sort(unique(traj_index$release_depth[
        traj_index$domain == input$domain & 
          traj_index$ptype == ptype & 
          traj_index$start == input$start
      ]))
      output$depth_ui <- renderUI({
        selectInput(ns("depth"), "Release Depth", choices = depths, selected = if (length(depths)) head(depths,1) else NULL)
      })
      } 
    }, ignoreInit = FALSE)

    # sinking velocity filter
    observeEvent(c(input$domain, input$start, ptype), {
      req(input$domain, input$start)

      if(ptype == "poop"){
      sinks <- sort(unique(seeds$sink_vel[
        seeds$domain == input$domain & 
          seeds$ptype == ptype & 
          seeds$start == input$start
      ]))
      output$sink_ui <- renderUI({
        selectInput(ns("sink"), "Sinking velocity (cm/s)", choices = sinks, selected = if (length(sinks)) head(sinks,1) else NULL)
      })
      } 
    }, ignoreInit = FALSE)

    # Leaflet map
    output$map <- renderLeaflet({
      ctr <- c(lng = mean(seeds$lon, na.rm=TRUE), lat = mean(seeds$lat, na.rm=TRUE))
      leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = ctr["lng"], lat = ctr["lat"], zoom = 10) |>
        #setView(lng = mean(seeds$lon, na.rm=TRUE), lat = mean(seeds$lat, na.rm=TRUE), zoom = 10) |>
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
    observeEvent(list(input$domain, input$start, input$depth, input$sink), {
      leafletProxy(ns("map")) |> clearGroup("start")
      session$sendCustomMessage("clearDucks", list())
      session$sendCustomMessage("clearTrails", list())
    }, ignoreInit = TRUE)

    # Map click handler: restrict to seeds that exist for (domain, start, ptype)
    observeEvent(input$map_click, {
      req(input$domain, input$start)
      click <- input$map_click

      if(ptype == "duck"){
        seed_ids_here <- unique(traj_index$seed_id[
        traj_index$domain==input$domain &
          traj_index$start==input$start & 
          traj_index$ptype==ptype & 
          traj_index$release_depth==input$depth
      ])
      } else if(ptype == "poop"){
        seed_ids_here <- unique(traj_index$seed_id[
        traj_index$domain==input$domain &
          traj_index$start==input$start & 
          traj_index$ptype==ptype & 
          traj_index$sink_vel==input$sink
      ])
      }

      seeds_here <- seeds |>
        filter(domain == input$domain, ptype == ptype, seed_id %in% seed_ids_here) |>
        select(seed_id, lon, lat)

      snap <- nearest_seed(click$lng, click$lat, seeds_here)
      if (is.null(snap)) { showNotification("No seeds available for this domain/start and particle type.", type = "warning"); return() }

      res <- traj_from_points(domain = input$domain, start = input$start, depth = input$depth, ptype = ptype, seed_id = snap$seed_id, sinkvel = input$sink)
      if (is.null(res) || is.null(res$coords) || nrow(res$coords) < 2) { showNotification("No trajectory points (Parquet).", type = "warning"); return() }
      coords <- res$coords
      
      if (is.null(coords) || nrow(coords) < 2) { showNotification("No trajectory points (GeoParquet).", type = "warning"); return() }

      leafletProxy(ns("map")) |>
        addCircleMarkers(lng = snap$lon, lat = snap$lat, radius = 2, color = NA, fill = "#333", fillOpacity = 0.5, group = "start")

      age_min <- res$age_min
      
      thin_factor <- if (nrow(coords) > 5000L) 10L else 1L
      if (thin_factor > 1L) {
        idx <- unique(c(1L, seq(1L, nrow(coords), by = thin_factor), nrow(coords)))
        coords   <- coords[idx, , drop = FALSE]
        if (!is.null(age_min) && length(age_min) >= max(idx)) {
          age_min <- age_min[idx]
        }
      }

      # Δmodel minutes per segment
      if (!is.null(res$age_min) && length(res$age_min) == nrow(coords)) {
        delta_min <- diff(as.numeric(age_min))           # typically all 1s
      } else {
        delta_min <- rep(1, nrow(coords) - 1L)           # 1 min per step fallback
      }

      # real ms per segment: (Δmin / mps) * 1000
      times_v <- pmax(1, round((delta_min / as.numeric(input$mps)) * 1000))
      latlng <- lapply(seq_len(nrow(coords)), function(i) unname(c(coords[i,2], coords[i,1])))

      session$sendCustomMessage("addTrail", list(
        coords = latlng,
        keepLast = 6,
        trimOldest = TRUE
      ))

      if(ptype == "duck"){
        pngPath <- "img/duck.png"
      } else if (ptype == "poop"){
        pngPath <- "img/Pile of Poo.png"
      }
      

      session$sendCustomMessage("addDuck", list(
        coords = latlng,
        times   = as.numeric(times_v),
        age_min = as.numeric(age_min),
        iconPng = pngPath
      ))

      session$sendCustomMessage("setAgeBins", list(
        breaksMin = c(60, 300, 600),                  # 0–1h, 1–5h, 5–10h, >10h
        colors    = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c")  # green→red
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
  tags$style(HTML("
      .bounce-animation { animation: bounce 2s infinite; }
      @keyframes bounce { 0%,20%,60%,100% { transform: translateY(0);} 40% { transform: translateY(-20px);} 80% { transform: translateY(-10px);} }
      .duckEmoji{ font-size:26px; line-height:26px; filter:drop-shadow(0 0 2px #fff);} 
      .duckInactive{ filter:grayscale(1) drop-shadow(0 0 2px #fff); opacity:0.6; }

      /* Transparent age label (tooltip) */
      .leaflet-tooltip.duckAgeTT{
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        padding: 0 2px;
        font-weight: 700;
        color: #111;                          /* or #fff if you prefer */
        text-shadow: 0 0 3px rgba(255,255,255,.9), 0 0 8px rgba(255,255,255,.7);
        pointer-events: none;                  /* don't block map clicks */
      }
      .leaflet-tooltip.duckAgeTT:before{
        display: none !important;             /* hide the little arrow */
      }

    ")),
  tags$script(HTML("
  // ---- Helpers on window so they're always global -------------------------
  window._ensureGroups = function(map){
    if (!map._ducks)       { map._ducks = L.layerGroup().addTo(map); }
    if (!map._duckMarkers) { map._duckMarkers = []; }
    if (!map._trails)      { map._trails = L.layerGroup().addTo(map); }
    if (!map._trailLines)  { map._trailLines = []; }
  };

  window._registerMapInline = function(map){
    window.LEAFLET_MAP = map;
    window._ensureGroups(map);
    var hasMM = !!(L && ((L.Marker && L.Marker.movingMarker) || L.movingMarker));
    console.log('MovingMarker present:', hasMM);
  };

  // -------- Binned age color config (minutes) ------------------------------
  window._AGE_BIN_BREAKS_MIN = [60, 300, 600]; // 0–1h, 1–5h, 5–10h, >10h
  window._AGE_BIN_COLORS     = ['#2ecc71', '#f1c40f', '#e67e22', '#e74c3c'];

  function _ageBinColorFromMin(ageMin){
    var breaks = window._AGE_BIN_BREAKS_MIN || [60,300,600];
    var cols   = window._AGE_BIN_COLORS     || ['#2ecc71','#f1c40f','#e67e22','#e74c3c'];
    var idx = breaks.findIndex(function(b){ return +ageMin <= +b; });
    if (idx === -1) idx = breaks.length;
    return cols[Math.min(idx, cols.length - 1)];
  }

  function _fmtAge(min){
    min = +min;
    if (!isFinite(min)) return '';
    if (min >= 60){
      var h = min / 60;
      return (h >= 10 ? h.toFixed(0) : h.toFixed(1)) + ' h';
    }
    return Math.round(min) + ' min';
  }

  // --- Fallback: tint by fraction if no ages supplied ----------------------
  function _attachAgeTintFraction(mm, totalMs){
    var running=false, t0=0, acc=0, raf=0;
    function apply(frac){
      var el = mm && mm._icon; if (!el) return;
      var pseudoMin = frac * 600; // map 0..1 -> 0..600 min
      var col = _ageBinColorFromMin(pseudoMin);
      el.style.filter = 'drop-shadow(0 0 6px ' + col + ')';
    }
    function loop(){
      if (!running) return;
      var now = performance.now();
      var elapsed = acc + (now - t0);
      var frac = totalMs > 0 ? Math.max(0, Math.min(1, elapsed / totalMs)) : 0;
      apply(frac);
      raf = requestAnimationFrame(loop);
    }
    mm.on('start',  function(){ t0 = performance.now(); running = true;  loop(); });
    mm.on('resume', function(){ t0 = performance.now(); running = true;  loop(); });
    mm.on('pause',  function(){ if (!running) return; acc += performance.now() - t0; running = false; if (raf) cancelAnimationFrame(raf); });
    mm.on('end',    function(){ running = false; if (raf) cancelAnimationFrame(raf); if (mm._icon) mm._icon.style.filter = ''; });
  }

  // --- Main: binned color using SUPPLIED per-point ages (minutes) ----------
  function _attachAgeTintFromAges(mm, times, ages, opts){
    opts = opts || {};
    var showLabel = (opts.showLabel !== false);

    var cum = [0];
    for (var i=0; i<times.length; i++){ cum.push(cum[i] + Math.max(1, +times[i])); }
    var T = cum[cum.length - 1];

    var running=false, t0=0, acc=0, raf=0, idx=0;

    if (showLabel && typeof mm.bindTooltip === 'function'){
      mm.bindTooltip(_fmtAge(ages[0]), { permanent:true, direction:'top', className:'duckAgeTT', offset:[0,-18] });
    }

    function infoAt(elapsed){
      if (elapsed <= 0) return { ageMin:+ages[0] };
      if (elapsed >= T) return { ageMin:+ages[ages.length-1] };
      while (idx < times.length && elapsed > cum[idx+1]) idx++;
      var segMs   = cum[idx+1] - cum[idx];
      var segFrac = segMs > 0 ? (elapsed - cum[idx]) / segMs : 0;
      var a0 = +ages[idx], a1 = +ages[idx+1];
      return { ageMin: a0 + segFrac * (a1 - a0) };
    }

    function apply(ageMin){
      var el = mm && mm._icon; if (!el) return;
      var col = _ageBinColorFromMin(ageMin);
      el.style.filter = 'drop-shadow(0 0 6px ' + col + ')';
      if (mm._tooltip && showLabel){ mm._tooltip.setContent(_fmtAge(ageMin)); }
    }

    function loop(){
      if (!running) return;
      var now = performance.now();
      var elapsed = acc + (now - t0);
      apply(infoAt(elapsed).ageMin);
      raf = requestAnimationFrame(loop);
    }

    mm.on('start',  function(){ t0 = performance.now(); running = true;  loop(); });
    mm.on('resume', function(){ t0 = performance.now(); running = true;  loop(); });
    mm.on('pause',  function(){ if (!running) return; acc += performance.now() - t0; running = false; if (raf) cancelAnimationFrame(raf); });
    mm.on('end',    function(){ running = false; if (raf) cancelAnimationFrame(raf); if (mm._icon) mm._icon.style.filter = ''; });
  }

  // ---- Register Shiny handlers after Shiny is ready -----------------------
  function _registerHandlers(){
    Shiny.addCustomMessageHandler('setAgeBins', function(msg){
      try{
        if (msg && Array.isArray(msg.breaksMin)){
          var br = msg.breaksMin.map(function(x){ return +x; })
                                .filter(function(x){ return isFinite(x) && x >= 0; })
                                .sort(function(a,b){ return a-b; });
          if (br.length) window._AGE_BIN_BREAKS_MIN = br;
        }
        if (msg && Array.isArray(msg.colors) && msg.colors.length){
          window._AGE_BIN_COLORS = msg.colors.slice();
        }
        console.log('[ducks] age bins set:', window._AGE_BIN_BREAKS_MIN, window._AGE_BIN_COLORS);
      } catch(e){ console.warn('setAgeBins error', e); }
    });

    Shiny.addCustomMessageHandler('ducksCtrl', function(msg){
      var map = window.LEAFLET_MAP; if (!map || !map._duckMarkers) return;
      map._duckMarkers.forEach(function(l){
        try{
          if (msg.cmd === 'pause'  && l.pause)  l.pause();
          if (msg.cmd === 'resume' && l.resume) l.resume(); // resume is the proper API
        } catch(e){}
      });
    });

    Shiny.addCustomMessageHandler('addTrail', function(msg){
      var map = window.LEAFLET_MAP; if (!map) return;
      window._ensureGroups(map);
      var keepLast   = (msg && typeof msg.keepLast   !== 'undefined') ? msg.keepLast   : 6;
      var trimOldest = (msg && typeof msg.trimOldest !== 'undefined') ? msg.trimOldest : true;

      var pts = (msg.coords||[]).map(function(p){ return L.latLng(+p[0], +p[1]); });
      if (pts.length < 2) return;

      var poly = L.polyline(pts, {color:'#38ebbb', weight:4, opacity:0.95}).addTo(map._trails);
      map._trailLines.push(poly);

      var n = map._trailLines.length;
      for (var i=0; i<n; i++){
        var line = map._trailLines[i];
        var age  = n - 1 - i;
        var t    = Math.min(age / Math.max(keepLast-1,1), 1);
        var col  = (age === 0) ? '#38ebbb' : '#8d9e99';
        var op   = 0.95 * (1 - 0.8*t);
        var wt   = 4 - 2*t;
        try { line.setStyle({color: col, opacity: op, weight: wt}); } catch(e){}
      }

      if (trimOldest && n > keepLast){
        var excess = n - keepLast;
        for (var k=0; k<excess; k++){
          try { map._trails.removeLayer(map._trailLines[0]); } catch(e){}
          map._trailLines.shift();
        }
      }
    });

    Shiny.addCustomMessageHandler('addDuck', function(msg){
      try{
        var map = window.LEAFLET_MAP; if (!map) return;
        window._ensureGroups(map);

        var pts = (msg.coords||[]).map(function(p){ return L.latLng(+p[0], +p[1]); });
        if (pts.length < 2) return;

        var times = (msg.times||[]).map(function(x){ return Math.max(1, +x); });
        if (times.length !== pts.length - 1) times = Array(pts.length-1).fill(200);

        var MM = (L.Marker && L.Marker.movingMarker) ? L.Marker.movingMarker
                 : (L.movingMarker ? L.movingMarker : null);

        var opts = msg.iconPng
          ? { icon: L.icon({iconUrl: msg.iconPng, iconSize:[28,28], iconAnchor:[14,14]}) }
          : { icon: L.divIcon({html:'<div class=\"duckEmoji\">🦆</div>', className:'', iconSize:[26,26]}) };

        if (!MM){
          var m = L.marker(pts[0], opts).addTo(map._ducks);
          map._duckMarkers.push(m);
          return;
        }

        var totalMs = times.reduce(function(a,b){ return a + Math.max(1,+b); }, 0);
        var mm = MM(pts, times, opts).addTo(map._ducks);

        if (Array.isArray(msg.age_min) && msg.age_min.length === pts.length){
          _attachAgeTintFromAges(mm, times, msg.age_min, { showLabel:true });
        } else {
          _attachAgeTintFraction(mm, totalMs);
        }

        if (typeof mm.start === 'function') mm.start();
        map._duckMarkers.push(mm);

        mm.on('end', function(){
          var el = mm._icon;
          if (el) { el.classList.add('duckInactive'); }
        });
      } catch(e){ console.error('addDuck error:', e); }
    });

    Shiny.addCustomMessageHandler('clearDucks', function(msg){
      var map = window.LEAFLET_MAP; if (!map) return;
      if (map._duckMarkers){
        map._duckMarkers.forEach(function(l){
          try { if (l.stop) l.stop(); if (l.pause) l.pause(); if (l._raf) cancelAnimationFrame(l._raf); } catch(e){}
          try { map.removeLayer(l); } catch(e){}
        });
        map._duckMarkers = [];
      }
      try { if (map._ducks) map.removeLayer(map._ducks); } catch(e){}
      map._ducks = null;
      window._ensureGroups(map);
    });

    Shiny.addCustomMessageHandler('clearTrails', function(msg){
      var map = window.LEAFLET_MAP; if (!map) return;
      if (map._trails) { map._trails.clearLayers(); }
      map._trailLines = [];
    });
  }

  if (window.Shiny && Shiny.addCustomMessageHandler){
    _registerHandlers();
  } else {
    document.addEventListener('shiny:connected', _registerHandlers, { once:true });
  }
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
