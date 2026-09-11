# DATASET 1

# THIS DATASET REMOVES MISSING VALUES FOR KEY VARIABLES AND DOES NOT TRY TO IMPUTE THEM

# GRAPHS ARE PRIMARILY MADE USING THIS DATASET SINCE IT HAS LESS ASSUMPTIONS

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(osmextract)
})

options(scipen = 999)
Sys.setenv("TZ" = "Europe/London")

in_path    <- "kaggle_london_house_price_data.csv"
crime_path <- "MPS Borough Level Crime (most recent 24 months) (1).csv"
cpih_path  <- "CPIH_Index.csv"
out_final  <- "Imputed.csv"

stopifnot(file.exists(in_path))
stopifnot(file.exists(crime_path))
stopifnot(file.exists(cpih_path))

# These are the exact column names in the Kaggle dataset

col_address   <- "fullAddress"
col_postcode  <- "postcode"
col_lat       <- "latitude"
col_lon       <- "longitude"

col_living    <- "livingRooms"
col_bath      <- "bathrooms"
col_bed       <- "bedrooms"
col_area      <- "floorAreaSqM"
col_tenure    <- "tenure"
col_sale      <- "saleEstimate_currentPrice"

col_hist_date <- "history_date"
col_hist_px   <- "history_price"

required_cols <- c(
  col_address, col_postcode, col_lat, col_lon,
  col_living, col_bath, col_bed, col_area, col_tenure, col_sale,
  col_hist_date, col_hist_px
)

# Helper Functions

clean_postcode <- function(x) toupper(gsub("\\s+", "", as.character(x)))

is_missing_num <- function(x) {
  x <- as.character(x)
  is.na(x) | trimws(x) == "" | toupper(trimws(x)) == "NA"
}

is_missing_text <- function(x) {
  x <- as.character(x)
  is.na(x) | trimws(x) == "" | toupper(trimws(x)) == "NA"
}

normalise_tenure <- function(x) {
  t <- as.character(x)
  t <- trimws(t)
  t[toupper(t) == "NA"] <- NA_character_
  
  t <- gsub("(?i)shared\\s*", "", t, perl = TRUE)
  t <- gsub("(?i)\\s+", " ", t, perl = TRUE)
  t <- trimws(t)
  
  t[tolower(t) == "feudal"] <- "leasehold"
  
  tl <- tolower(t)
  out <- rep(NA_character_, length(tl))
  out[tl %in% "freehold"] <- "freehold"
  out[tl %in% "leasehold"] <- "leasehold"
  out
}

nearest_dist_m <- function(from_sf, to_sf) { # to help with OSM features
  if (is.null(to_sf) || nrow(to_sf) == 0) return(rep(NA_real_, nrow(from_sf)))
  idx <- st_nearest_feature(from_sf, to_sf)
  as.numeric(st_distance(from_sf, to_sf[idx, ], by_element = TRUE))
}

count_within <- function(from_sf, to_sf, radius_m) {
  if (is.null(to_sf) || nrow(to_sf) == 0) return(rep(0L, nrow(from_sf)))
  lengths(st_is_within_distance(from_sf, to_sf, dist = radius_m, sparse = TRUE))
}

keep_points_only <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(st_sf(geometry = st_sfc(crs = 27700)))
  x <- st_as_sf(x)
  x <- st_zm(x, drop = TRUE, what = "ZM")
  x <- x[st_geometry_type(x) %in% c("POINT", "MULTIPOINT"), , drop = FALSE]
  if (nrow(x) == 0) return(st_sf(geometry = st_sfc(crs = 27700)))
  st_transform(x, 27700)
}

clean_borough_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("&", "AND", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

to_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(x))))

write_csv_no_sci <- function(dt_in, path, fallback_suffix = "_NEW") {
  dt_out <- copy(dt_in)
  
  money_like <- names(dt_out)[grepl(
    "(price|estimate|valuechange|numericchange|rent|sale|cpih|index|infl|factor|lp|premium)",
    tolower(names(dt_out))
  )]
  money_like <- intersect(money_like, names(dt_out))
  
  for (col in money_like) {
    if (is.numeric(dt_out[[col]])) {
      dt_out[[col]] <- ifelse(
        is.na(dt_out[[col]]),
        NA_character_,
        format(dt_out[[col]], scientific = FALSE, trim = TRUE, digits = 15)
      )
    }
  }
  
  ok <- TRUE
  tryCatch(
    fwrite(dt_out, path),
    error = function(e) {
      ok <<- FALSE
      msg <- conditionMessage(e)
      cat("\nWrite failed: ", msg, "\n", sep = "")
      
      base <- sub("\\.csv$", "", path, ignore.case = TRUE)
      fallback_path <- paste0(base, fallback_suffix, ".csv")
      cat("Writing fallback file: ", fallback_path, "\n", sep = "")
      fwrite(dt_out, fallback_path)
    }
  )
  
  invisible(ok)
}

# Inflation (CPIH) parsing helper 

parse_cpih_layout <- function(cpih_path) {
  cpih_raw <- fread(cpih_path, header = FALSE, fill = TRUE)
  if (nrow(cpih_raw) < 4) stop("Error")
  
  date_row <- as.character(unlist(cpih_raw[3, ], use.names = FALSE))
  val_row  <- as.character(unlist(cpih_raw[4, ], use.names = FALSE))
  
  is_uk_date <- function(x) grepl("^\\d{2}/\\d{2}/\\d{4}$", x)
  keep <- is_uk_date(date_row)
  if (!any(keep)) stop("No dd/mm/yyyy dates found")
  
  cpih <- data.table(
    month_date = as.Date(date_row[keep], format = "%d/%m/%Y"),
    cpih_index = suppressWarnings(as.numeric(gsub("[^0-9.]", "", val_row[keep])))
  )
  cpih <- cpih[!is.na(month_date) & !is.na(cpih_index) & cpih_index > 0]
  if (nrow(cpih) == 0) stop("CPIH parsed to zero usable rows")
  
  setorder(cpih, month_date)
  cpih
}

# OSM Cache

dir.create("osm_cache", showWarnings = FALSE)
options(osmextract.data_dir = normalizePath("osm_cache"))

cached_london <- list.files(
  getOption("osmextract.data_dir"),
  pattern = "greater-london.*\\.gpkg$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(cached_london) > 0) file.remove(cached_london)

tmp_london <- list.files(
  tempdir(),
  pattern = "greater-london.*\\.gpkg$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(tmp_london) > 0) file.remove(tmp_london)

# Load raw and standardise geo columns

cat("Loading raw Kaggle\n")
dt0 <- fread(in_path, showProgress = TRUE)

missing_required <- setdiff(required_cols, names(dt0))
if (length(missing_required) > 0) {
  stop(paste("Missing required columns:", paste(missing_required, collapse = ", ")))
}

dt0[, (col_lat) := suppressWarnings(as.numeric(get(col_lat)))]
dt0[, (col_lon) := suppressWarnings(as.numeric(get(col_lon)))]
dt0[, (col_postcode) := clean_postcode(get(col_postcode))]

dt <- dt0[
  !is.na(get(col_lat)) &
    !is.na(get(col_lon)) &
    !is.na(get(col_postcode)) &
    get(col_postcode) != ""
]

cat("Rows after latitude longitude postcode filter: ", nrow(dt), "\n", sep = "")

# Here we are removing dupes and missing data

dt[, (col_tenure) := normalise_tenure(get(col_tenure))]

dt[, living_missing := is_missing_num(get(col_living))]
dt[, bath_missing   := is_missing_num(get(col_bath))]
dt[, bed_missing    := is_missing_num(get(col_bed))]
dt[, area_missing   := is_missing_num(get(col_area))]
dt[, tenure_missing := is_missing_text(get(col_tenure))]
dt[, sale_missing   := is_missing_num(get(col_sale))]

dt[, any_missing := living_missing | bath_missing | bed_missing | area_missing | tenure_missing | sale_missing]

dt[, house_id := paste0(
  toupper(trimws(get(col_address))),
  "|",
  toupper(trimws(get(col_postcode)))
)]

dt[, history_date_parsed := as.IDate(get(col_hist_date))]
dt[, history_price_num := to_num(get(col_hist_px))]

## ============================================================
## Added: imputation logic (allow up to 1 missing structural var)
## ============================================================

dt[, struct_missing_count := living_missing + bath_missing + bed_missing + area_missing]

dt_imp_base <- dt[
  tenure_missing == FALSE &
    sale_missing == FALSE &
    struct_missing_count <= 1
]

dt_imp_base <- dt_imp_base[!is.na(history_date_parsed) & !is.na(history_price_num) & history_price_num > 0]
dt_imp_base[, row_id := .I]

cat("Rows in imputation base: ", nrow(dt_imp_base), "\n", sep = "")

if (nrow(dt_imp_base) == 0) stop("No rows available for imputation base")

df_imp <- as.data.frame(dt_imp_base)

num_vars <- c(col_living, col_bath, col_bed, col_area, col_lat, col_lon)
df_imp[num_vars] <- lapply(df_imp[num_vars], function(x) suppressWarnings(as.numeric(as.character(x))))

df_imp$living_miss <- is.na(df_imp[[col_living]])
df_imp$bath_miss   <- is.na(df_imp[[col_bath]])
df_imp$bed_miss    <- is.na(df_imp[[col_bed]])
df_imp$area_miss   <- is.na(df_imp[[col_area]])

# Impute livingRooms
if (any(df_imp$living_miss)) {
  fit_liv <- lm(
    as.formula(paste0(col_living, " ~ ", paste(c(col_bath, col_bed, col_area, col_lat, col_lon), collapse = " + "))),
    data = df_imp,
    subset = !is.na(df_imp[[col_living]])
  )
  df_imp[[col_living]][df_imp$living_miss] <- predict(fit_liv, newdata = df_imp[df_imp$living_miss, ])
}

# Impute bathrooms
if (any(df_imp$bath_miss)) {
  fit_bath <- lm(
    as.formula(paste0(col_bath, " ~ ", paste(c(col_living, col_bed, col_area, col_lat, col_lon), collapse = " + "))),
    data = df_imp,
    subset = !is.na(df_imp[[col_bath]])
  )
  df_imp[[col_bath]][df_imp$bath_miss] <- predict(fit_bath, newdata = df_imp[df_imp$bath_miss, ])
}

# Impute bedrooms
if (any(df_imp$bed_miss)) {
  fit_bed <- lm(
    as.formula(paste0(col_bed, " ~ ", paste(c(col_living, col_bath, col_area, col_lat, col_lon), collapse = " + "))),
    data = df_imp,
    subset = !is.na(df_imp[[col_bed]])
  )
  df_imp[[col_bed]][df_imp$bed_miss] <- predict(fit_bed, newdata = df_imp[df_imp$bed_miss, ])
}

# Impute floorAreaSqM
if (any(df_imp$area_miss)) {
  fit_area <- lm(
    as.formula(paste0(col_area, " ~ ", paste(c(col_living, col_bath, col_bed, col_lat, col_lon), collapse = " + "))),
    data = df_imp,
    subset = !is.na(df_imp[[col_area]])
  )
  df_imp[[col_area]][df_imp$area_miss] <- predict(fit_area, newdata = df_imp[df_imp$area_miss, ])
}

df_imp[[col_living]] <- pmax(df_imp[[col_living]], 0)
df_imp[[col_bath]]   <- pmax(df_imp[[col_bath]], 0)
df_imp[[col_bed]]    <- pmax(df_imp[[col_bed]], 0)
df_imp[[col_area]]   <- pmax(df_imp[[col_area]], 1)

df_imp[[col_living]] <- round(df_imp[[col_living]])
df_imp[[col_bath]]   <- round(df_imp[[col_bath]])
df_imp[[col_bed]]    <- round(df_imp[[col_bed]])

dt_ok <- as.data.table(df_imp)

dt_ok[, c("living_missing","bath_missing","bed_missing","area_missing","tenure_missing","sale_missing","any_missing","struct_missing_count") := NULL]

## ============================================================
## Continue original logic from here
## ============================================================

setorder(dt_ok, house_id, -history_date_parsed, -history_price_num, -row_id)
dt_latest <- dt_ok[, .SD[1], by = house_id]

cat("Rows after required fields filter and latest per house de dupe: ", nrow(dt_latest), "\n", sep = "")
cat("Unique houses: ", uniqueN(dt_latest$house_id), "\n", sep = "")

cat("Tenure distribution after cleaning:\n")
print(dt_latest[, .N, by = tenure][order(-N)])

dt_latest[, c(
  "row_id"
) := NULL]

# We are bringing everything to a baseline level of December 2025 pounds 

# This is so that we can compare properties that sold in the past to those today

# We do not focus on measuring dynamics of how house prices have evolved
# But bringing history_price to this common level lets us use it as an input to our postcode premium variable


cat("Loading CPIH\n")
cpih <- parse_cpih_layout(cpih_path)

base_month <- as.Date("01/12/2025", format = "%d/%m/%Y")
if (!(base_month %in% cpih$month_date)) base_month <- max(cpih$month_date, na.rm = TRUE)
cpih_base <- cpih[month_date == base_month, cpih_index][1]
if (is.na(cpih_base) || cpih_base <= 0) stop("Could not get a valid CPIH base value.")
cat("CPIH base month used: ", format(base_month), " base index: ", cpih_base, "\n", sep = "")

dt_latest[, history_month := as.IDate(format(history_date_parsed, "%Y-%m-01"))]

setkey(cpih, month_date)
dt_latest <- cpih[dt_latest, on = .(month_date = history_month)]

dt_latest <- dt_latest[!is.na(cpih_index) & cpih_index > 0]

dt_latest[, infl_factor := cpih_base / cpih_index]
dt_latest[, history_price_2025 := history_price_num * infl_factor]
dt_latest[, log_history_price_2025 := log(pmax(history_price_2025, 1))]

m <- 20
dt_latest[, global_mean_hist := mean(log_history_price_2025, na.rm = TRUE)]
enc <- dt_latest[, .(mu = mean(log_history_price_2025, na.rm = TRUE), n = .N), by = postcode]
enc[, postcode_lp := (n * mu + m * dt_latest$global_mean_hist[1]) / (n + m)]

setkey(dt_latest, postcode)
setkey(enc, postcode)
dt_latest[enc, postcode_lp := i.postcode_lp]

dt_latest[is.na(postcode_lp), postcode_lp := dt_latest$global_mean_hist[1]]

dt_latest[, global_mean_hist := NULL]

cat("CPIH and postcode_lp complete\n")
cat("Rows after CPIH match: ", nrow(dt_latest), "\n", sep = "")

# Winsorization Drop bottom 1% and top 1% of saleEstimate_currentPrice

dt_latest[, sale_y := to_num(get(col_sale))]
dt_latest <- dt_latest[!is.na(sale_y) & sale_y > 0]

q_lo <- quantile(dt_latest$sale_y, 0.01, na.rm = TRUE)
q_hi <- quantile(dt_latest$sale_y, 0.99, na.rm = TRUE)

cat("Sale estimate trim thresholds:\n")
cat("1% quantile :", format(q_lo, scientific = FALSE, trim = TRUE), "\n")
cat("99% quantile:", format(q_hi, scientific = FALSE, trim = TRUE), "\n")

before_trim <- nrow(dt_latest)
dt_latest <- dt_latest[sale_y >= q_lo & sale_y <= q_hi]
cat("Rows dropped by trim:", before_trim - nrow(dt_latest), "\n", sep = "")
cat("Rows after trim:", nrow(dt_latest), "\n", sep = "")

dt_latest[, sale_y := NULL]


# We construct the postcode premium feature

postcodes <- dt_latest[, .(
  latitude  = mean(get(col_lat), na.rm = TRUE),
  longitude = mean(get(col_lon), na.rm = TRUE),
  n_rows    = .N
), by = postcode]

pc_sf <- st_as_sf(
  postcodes,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)
pc_sf <- st_transform(pc_sf, 27700)

cat("Unique postcodes in cleaned de duped data: ", nrow(postcodes), "\n", sep = "")

# Using an API for the postcode OSM features

cat("Loading Greater London OSM points layers (cached after first run)\n")

osm_pts_core <- oe_get(
  place = "Greater London",
  layer = "points",
  extra_tags = c("amenity","leisure","landuse","railway","public_transport","station","subway")
)
osm_pts_core <- keep_points_only(osm_pts_core)

osm_pts_shop <- oe_get(
  place = "Greater London",
  layer = "points",
  extra_tags = c("shop")
)
osm_pts_shop <- keep_points_only(osm_pts_shop)

for (col in c("amenity","leisure","landuse","railway","public_transport","station","subway")) {
  if (col %in% names(osm_pts_core)) osm_pts_core[[col]] <- as.character(osm_pts_core[[col]])
}
if ("shop" %in% names(osm_pts_shop)) osm_pts_shop$shop <- as.character(osm_pts_shop$shop)

shops_sf <- osm_pts_shop[osm_pts_shop$shop %in% c("supermarket","convenience"), "geometry", drop = FALSE]
police_sf <- osm_pts_core[osm_pts_core$amenity %in% c("police"), "geometry", drop = FALSE]
hospitals_sf <- osm_pts_core[osm_pts_core$amenity %in% c("hospital"), "geometry", drop = FALSE]
parks_sf <- osm_pts_core[
  (osm_pts_core$leisure %in% c("park")) | (osm_pts_core$landuse %in% c("recreation_ground")),
  "geometry",
  drop = FALSE
]
schools_sf <- osm_pts_core[
  osm_pts_core$amenity %in% c("school","college","kindergarten","university","language_school","music_school"),
  "geometry",
  drop = FALSE
]

tube_sf_1 <- osm_pts_core[
  (osm_pts_core$railway %in% c("station")) & (osm_pts_core$station %in% c("subway")),
  "geometry",
  drop = FALSE
]
tube_sf_2 <- osm_pts_core[
  (osm_pts_core$public_transport %in% c("station")) & (osm_pts_core$subway %in% c("yes")),
  "geometry",
  drop = FALSE
]
tube_sf <- rbind(tube_sf_1, tube_sf_2)

cat("Computing postcode OSM features\n")

pc_features <- data.table(
  postcode  = postcodes$postcode,
  latitude  = postcodes$latitude,
  longitude = postcodes$longitude,
  n_rows    = postcodes$n_rows
)

pc_features[, osm_nearest_shop_m := nearest_dist_m(pc_sf, shops_sf)]
pc_features[, osm_shop_count_300m := count_within(pc_sf, shops_sf, 300)]
pc_features[, osm_shop_count_500m := count_within(pc_sf, shops_sf, 500)]
pc_features[, osm_shop_count_1000m := count_within(pc_sf, shops_sf, 1000)]

pc_features[, osm_nearest_police_station_m := nearest_dist_m(pc_sf, police_sf)]
pc_features[, osm_police_station_count_1000m := count_within(pc_sf, police_sf, 1000)]

pc_features[, osm_nearest_school_m := nearest_dist_m(pc_sf, schools_sf)]
pc_features[, osm_school_count_500m := count_within(pc_sf, schools_sf, 500)]
pc_features[, osm_school_count_1000m := count_within(pc_sf, schools_sf, 1000)]
pc_features[, osm_school_count_1500m := count_within(pc_sf, schools_sf, 1500)]

pc_features[, osm_nearest_tube_station_m := nearest_dist_m(pc_sf, tube_sf)]
pc_features[, osm_tube_station_count_1000m := count_within(pc_sf, tube_sf, 1000)]

pc_features[, osm_nearest_hospital_m := nearest_dist_m(pc_sf, hospitals_sf)]
pc_features[, osm_hospital_count_3000m := count_within(pc_sf, hospitals_sf, 3000)]

pc_features[, osm_nearest_park_m := nearest_dist_m(pc_sf, parks_sf)]
pc_features[, osm_park_count_1000m := count_within(pc_sf, parks_sf, 1000)]
pc_features[, osm_park_count_2000m := count_within(pc_sf, parks_sf, 2000)]

setkey(dt_latest, postcode)
setkey(pc_features, postcode)

new_cols <- setdiff(names(pc_features), "postcode")
dt_latest[pc_features, (new_cols) := mget(paste0("i.", new_cols))]

cat("OSM join complete\n")

# Here we assign the corresponding borough to each house

cat("Assigning borough to each postcode\n")

pc_unique <- unique(dt_latest[, .(
  postcode,
  latitude = get(col_lat),
  longitude = get(col_lon)
)])

pc_pts <- st_as_sf(pc_unique, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

borough_url <- "https://github.com/LingruFeng/GIS_assessment/raw/main/London_Boroughs.gpkg"
borough_gpkg <- file.path(tempdir(), "London_Boroughs.gpkg") # this geopackage file is used for our spacial data

if (!file.exists(borough_gpkg)) {
  download.file(borough_url, borough_gpkg, mode = "wb", quiet = TRUE)
}
stopifnot(file.exists(borough_gpkg))

boroughs <- st_read(borough_gpkg, quiet = TRUE)
bn <- names(boroughs)
bn_low <- tolower(bn)
name_col <- bn[match(TRUE, bn_low %in% c("name","borough","lad_name","lad23nm","lad24nm","lb_name","lbn","borough_name"))]
if (is.na(name_col)) {
  print(names(boroughs))
  stop("Could not detect the borough name column.")
}

boroughs <- st_transform(boroughs, st_crs(pc_pts))
pc_join <- st_join(pc_pts, boroughs[, name_col, drop = FALSE], left = TRUE, join = st_within)

pc_map <- as.data.table(pc_join)
setnames(pc_map, name_col, "borough")
pc_map <- pc_map[, .(postcode, borough)]

setkey(pc_map, postcode)
dt_latest[pc_map, borough := i.borough, on = "postcode"]

cat("Share missing borough before drop: ", mean(is.na(dt_latest$borough)), "\n", sep = "")

before_drop <- nrow(dt_latest)
dt_latest <- dt_latest[!(is.na(borough) | trimws(borough) == "" | toupper(trimws(borough)) == "NA")]
cat("Dropped rows due to missing borough: ", before_drop - nrow(dt_latest), "\n", sep = "")

## ============================================================
## 7) Crime aggregation and join by borough
## City of London fallback to Westminster
## ============================================================
cat("Loading and aggregating crime data\n")

raw <- fread(crime_path, header = FALSE, fill = TRUE, showProgress = TRUE)

hdr <- trimws(as.character(unlist(raw[1, ], use.names = FALSE)))
hdr[is.na(hdr) | hdr == ""] <- paste0("X", which(is.na(hdr) | hdr == ""))

setnames(raw, hdr)
crime <- raw[-1]
setnames(crime, trimws(names(crime)))

stopifnot(ncol(crime) >= 4)
setnames(crime, old = names(crime)[1:3], new = c("MajorText","MinorText","BoroughName"))

month_cols <- names(crime)[grepl("^\\d{6}$", names(crime))]
stopifnot(length(month_cols) > 0)

crime[, BoroughName := trimws(as.character(BoroughName))]

for (mc in month_cols) {
  set(crime, j = mc, value = suppressWarnings(as.numeric(crime[[mc]])))
  crime[is.na(get(mc)), (mc) := 0]
}

crime_tot <- crime[, lapply(.SD, sum, na.rm = TRUE),
                   by = BoroughName,
                   .SDcols = month_cols]

month_cols_sorted <- sort(month_cols)
last_month <- month_cols_sorted[length(month_cols_sorted)]
last3  <- tail(month_cols_sorted, 3)
last12 <- tail(month_cols_sorted, 12)

crime_tot[, crime_last_month := get(last_month)]
crime_tot[, crime_last_3m := rowSums(.SD), .SDcols = last3]
crime_tot[, crime_last_12m := rowSums(.SD), .SDcols = last12]
crime_tot[, crime_mean_last_12m := crime_last_12m / length(last12)]

crime_feat <- crime_tot[, .(
  BoroughName,
  crime_last_month,
  crime_last_3m,
  crime_last_12m,
  crime_mean_last_12m
)]

dt_latest[, borough_key := clean_borough_key(borough)]
crime_feat[, borough_key := clean_borough_key(BoroughName)]

setkey(dt_latest, borough_key)
setkey(crime_feat, borough_key)

dt_latest[crime_feat, `:=`(
  crime_last_month = i.crime_last_month,
  crime_last_3m = i.crime_last_3m,
  crime_last_12m = i.crime_last_12m,
  crime_mean_last_12m = i.crime_mean_last_12m
)]

city_key <- clean_borough_key("City of London")
west_key <- clean_borough_key("Westminster")

west_vals <- crime_feat[borough_key == west_key, .(
  crime_last_month,
  crime_last_3m,
  crime_last_12m,
  crime_mean_last_12m
)]

if (nrow(west_vals) != 1) {
  stop("Could not find a unique Westminster row in crime_feat after key cleaning.")
}

if (dt_latest[borough_key == city_key, .N] > 0) {
  dt_latest[
    borough_key == city_key & (is.na(crime_last_month) | is.na(crime_mean_last_12m)),
    `:=`(
      crime_last_month = west_vals$crime_last_month,
      crime_last_3m = west_vals$crime_last_3m,
      crime_last_12m = west_vals$crime_last_12m,
      crime_mean_last_12m = west_vals$crime_mean_last_12m
    )
  ]
}

dt_latest[, borough_key := NULL]

cat("Share missing crime_last_month: ", mean(is.na(dt_latest$crime_last_month)), "\n", sep = "")

## ============================================================
## Added: Cook's distance trimming (top 0.1%)
## ============================================================

dt_latest[, .orig_id_for_influence := .I]

safe_log <- function(x) log(pmax(x, 1))

dt_latest[, sale_num_for_cook := to_num(get(col_sale))]
dt_latest[, y := safe_log(sale_num_for_cook)]
dt_latest[, log_floorArea := safe_log(to_num(get(col_area)))]
dt_latest[, log_crime := safe_log(crime_last_12m + 1)]

dt_latest[, log_nearest_shop := safe_log(osm_nearest_shop_m)]
dt_latest[, log_shop_count := safe_log(osm_shop_count_1000m + 1)]
dt_latest[, log_nearest_school := safe_log(osm_nearest_school_m)]
dt_latest[, log_school_count := safe_log(osm_school_count_1000m + 1)]
dt_latest[, log_nearest_tube := safe_log(osm_nearest_tube_station_m)]
dt_latest[, log_tube_count := safe_log(osm_tube_station_count_1000m + 1)]
dt_latest[, log_nearest_hospital := safe_log(osm_nearest_hospital_m)]
dt_latest[, log_hospital_count := safe_log(osm_hospital_count_3000m + 1)]
dt_latest[, log_nearest_park := safe_log(osm_nearest_park_m)]
dt_latest[, log_park_count := safe_log(osm_park_count_2000m + 1)]
dt_latest[, log_nearest_police := safe_log(osm_nearest_police_station_m)]
dt_latest[, log_police_count := safe_log(osm_police_station_count_1000m + 1)]

keep <- c(
  "y","postcode_lp","log_floorArea",
  "bedrooms","bathrooms","livingRooms","tenure",
  "log_crime",
  "log_nearest_shop","log_shop_count",
  "log_nearest_school","log_school_count",
  "log_nearest_tube","log_tube_count",
  "log_nearest_hospital","log_hospital_count",
  "log_nearest_park","log_park_count",
  "log_nearest_police","log_police_count",
  ".orig_id_for_influence"
)

d_model <- dt_latest[complete.cases(dt_latest[, ..keep]), ..keep]

if (nrow(d_model) == 0) {
  cat("No complete case rows for influence analysis, skipping Cook trimming\n")
} else {
  fml <- as.formula(paste("y ~", paste(setdiff(keep, c("y", ".orig_id_for_influence")), collapse = " + ")))
  lm_tmp <- lm(fml, data = d_model)
  
  cook <- cooks.distance(lm_tmp)
  cutoff <- quantile(cook, 0.999, na.rm = TRUE)
  
  kept <- cook <= cutoff
  kept_ids <- d_model$.orig_id_for_influence[kept]
  n_removed <- sum(!kept)
  
  cat("Cook trimming removed: ", n_removed, "\n", sep = "")
  
  dt_latest <- dt_latest[.orig_id_for_influence %in% kept_ids]
  cat("Rows after Cook trimming: ", nrow(dt_latest), "\n", sep = "")
}

cols_to_drop <- c(
  "sale_num_for_cook",
  "y", "log_floorArea", "log_crime",
  "log_nearest_shop","log_shop_count",
  "log_nearest_school","log_school_count",
  "log_nearest_tube","log_tube_count",
  "log_nearest_hospital","log_hospital_count",
  "log_nearest_park","log_park_count",
  "log_nearest_police","log_police_count",
  ".orig_id_for_influence"
)
existing_drop <- intersect(cols_to_drop, names(dt_latest))
if (length(existing_drop) > 0) dt_latest[, (existing_drop) := NULL]

# Writing the final CSV which is used for much of our analysis

cat("Writing final CSV: ", out_final, "\n", sep = "")
write_csv_no_sci(dt_latest, out_final)

cat("Wrote: ", out_final, "\n", sep = "")
cat("Rows written: ", nrow(dt_latest), "\n", sep = "")
cat("Cols written: ", ncol(dt_latest), "\n", sep = "")
cat("Done\n")
