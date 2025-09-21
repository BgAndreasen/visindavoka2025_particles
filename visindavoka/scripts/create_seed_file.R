library(tidyverse)
library(sf)
library(geojsonsf)


GEOJSON_ROOT <- "visindavoka/geojson"          # folder with domain/start/seed_ptype.geojson.gz
SEEDS_CSV    <- "visindavoka/data/seeds.csv"

read_geojson_gz <- function(fpath) {
  con <- gzfile(fpath, open = "rt"); on.exit(close(con), add = TRUE)
  txt <- paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "")
  geojsonsf::geojson_sf(txt)  # sf object
}

get_first_lonlat_from_gz <- function(fpath) {
  g <- read_geojson_gz(fpath)
  if (nrow(g) == 0) return(c(lon = NA_real_, lat = NA_real_))
  geom1 <- sf::st_geometry(g)[[1]]
  cc <- tryCatch(sf::st_coordinates(geom1), error = function(e) NULL)
  if (!is.null(cc) && nrow(cc) >= 1) return(c(lon = as.numeric(cc[1,"X"]), lat = as.numeric(cc[1,"Y"])))
  # fallback
  xy <- sf::st_coordinates(sf::st_point_on_surface(geom1))[1, ]
  c(lon = as.numeric(xy[1]), lat = as.numeric(xy[2]))
}

# sugegested faster, but not implemented
get_first_lonlat_sniff <- function(fpath, max_bytes = 200000L) {
  # read only the first N bytes from the gz stream
  con <- gzfile(fpath, open = "rb"); on.exit(close(con), add = TRUE)
  raw <- readBin(con, what = "raw", n = max_bytes)
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"

  # find `"coordinates": [` then grab the first number pair in the following array
  # robust-ish: allow spaces, negatives, decimals, exponents
  m <- regexpr('"coordinates"\\s*:\\s*\\[', txt, perl = TRUE)
  if (m == -1L) return(c(lon = NA_real_, lat = NA_real_))
  start <- m + attr(m, "match.length")

  # look ahead a window and extract first [lon, lat]
  window <- substr(txt, start, min(nchar(txt), start + 5000L))
  nums <- gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", window, perl = TRUE)[[1]]
  if (all(nums == -1L) || length(nums) < 2) return(c(lon = NA_real_, lat = NA_real_))

  # take the first two numbers
  vals <- regmatches(window, list(nums))[[1]]
  lon <- suppressWarnings(as.numeric(vals[1])); lat <- suppressWarnings(as.numeric(vals[2]))
  c(lon = lon, lat = lat)
}


# --- Build seeds.csv from existing GeoJSON files ---------------------------
build_seeds_csv <- function(root = GEOJSON_ROOT, out_csv = SEEDS_CSV) {
  list_domain_files <- function(domain) {
    droot  <- file.path(root, domain)
    starts <- list.dirs(droot, full.names = TRUE, recursive = FALSE)
    files  <- unlist(lapply(starts, function(sdir) {
      list.files(sdir, pattern = "\\.geojson(\\.gz)?$", full.names = TRUE)
    }), use.names = FALSE)
    if (!length(files)) return(NULL)
    data.frame(domain = domain, f = files, stringsAsFactors = FALSE)
  }

  domains <- sort(list.dirs(root, full.names = FALSE, recursive = FALSE))
  dfs <- lapply(domains, list_domain_files)
  df  <- do.call(rbind, dfs[!vapply(dfs, is.null, logical(1))])
  if (is.null(df) || !nrow(df)) {
    write.csv(data.frame(seed_id=character(), lon=numeric(), lat=numeric(),
                         domain=character(), ptype=character()),
              out_csv, row.names = FALSE)
    return(invisible(data.frame()))
  }

  # parse: seed_id_ptype.geojson(.gz)  — use the LAST underscore
  parse_fn <- function(path) {
    bn <- basename(path)
    bn2 <- sub("\\.geojson(\\.gz)?$", "", bn, perl = TRUE)
    us  <- gregexpr("_", bn2, fixed = TRUE)[[1]]
    if (all(us == -1L)) return(list(seed_id = bn2, ptype = NA_character_))
    cut <- tail(us, 1)
    list(seed_id = substr(bn2, 1, cut - 1),
         ptype   = substr(bn2, cut + 1, nchar(bn2)))
  }
  parsed <- lapply(df$f, parse_fn)
  df$seed_id <- vapply(parsed, `[[`, "", "seed_id")
  df$ptype   <- vapply(parsed, `[[`, "", "ptype")

  # choose ONE file per (domain, seed_id, ptype) to sample lon/lat
  df <- df[order(df$domain, df$seed_id, df$ptype, df$f), ]
  picks <- df[!duplicated(df[c("domain","seed_id","ptype")]), c("domain","seed_id","ptype","f")]

  # read first coordinate lon/lat from each pick
  lonlat <- t(vapply(picks$f, get_first_lonlat_from_gz, c(NA_real_, NA_real_)))
  out <- data.frame(
    seed_id = as.character(picks$seed_id),
    lon     = as.numeric(lonlat[, 1]),
    lat     = as.numeric(lonlat[, 2]),
    domain  = as.character(picks$domain),
    ptype   = as.character(picks$ptype),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$lon) & is.finite(out$lat), ]

  if (!dir.exists(dirname(out_csv))) dir.create(dirname(out_csv), TRUE, FALSE)
  write.csv(out, out_csv, row.names = FALSE)
  invisible(out)
}

build_seeds_csv(GEOJSON_ROOT, SEEDS_CSV)
