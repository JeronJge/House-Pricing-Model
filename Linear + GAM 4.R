rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(lmtest)
  library(sandwich)
  library(car)
})

options(scipen = 999)
Sys.setenv("TZ" = "Europe/London")


dt <- fread("Imputed.csv", showProgress = TRUE)

# These are ouyr required columns for the regression

need_cols <- c(
  "saleEstimate_currentPrice",
  "floorAreaSqM",
  "bedrooms",
  "bathrooms",
  "livingRooms",
  "tenure",
  "postcode_lp",
  "crime_last_12m",
  "osm_nearest_shop_m","osm_shop_count_1000m",
  "osm_nearest_school_m","osm_school_count_1000m",
  "osm_nearest_tube_station_m","osm_tube_station_count_1000m",
  "osm_nearest_hospital_m","osm_hospital_count_3000m",
  "osm_nearest_park_m","osm_park_count_2000m",
  "osm_nearest_police_station_m","osm_police_station_count_1000m"
)

missing <- setdiff(need_cols, names(dt))
if (length(missing) > 0) stop(paste("Missing columns:", paste(missing, collapse = ", ")))

dt <- dt[, ..need_cols]

# Transformations are done here

to_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(x))))
safe_log <- function(x) log(pmax(x, 1))

num_vars <- setdiff(need_cols, c("saleEstimate_currentPrice","tenure"))
for (v in num_vars) dt[, (v) := to_num(get(v))]

dt[, tenure := factor(
  fifelse(grepl("free", tolower(tenure)), "freehold",
          fifelse(grepl("lease", tolower(tenure)), "leasehold", NA_character_))
)]

dt[, y := safe_log(saleEstimate_currentPrice)]
dt[, log_floorArea := safe_log(floorAreaSqM)]
dt[, log_crime := safe_log(crime_last_12m + 1)]
dt[, log_nearest_shop := safe_log(osm_nearest_shop_m)]
dt[, log_shop_count := safe_log(osm_shop_count_1000m + 1)]
dt[, log_nearest_school := safe_log(osm_nearest_school_m)]
dt[, log_school_count := safe_log(osm_school_count_1000m + 1)]
dt[, log_nearest_tube := safe_log(osm_nearest_tube_station_m)]
dt[, log_tube_count := safe_log(osm_tube_station_count_1000m + 1)]
dt[, log_nearest_hospital := safe_log(osm_nearest_hospital_m)]
dt[, log_hospital_count := safe_log(osm_hospital_count_3000m + 1)]
dt[, log_nearest_park := safe_log(osm_nearest_park_m)]
dt[, log_park_count := safe_log(osm_park_count_2000m + 1)]
dt[, log_nearest_police := safe_log(osm_nearest_police_station_m)]
dt[, log_police_count := safe_log(osm_police_station_count_1000m + 1)]

# This is for our Modelling sample

keep <- c(
  "y","postcode_lp","log_floorArea",
  "bedrooms","bathrooms","livingRooms","tenure",
  "log_crime",
  "log_nearest_shop","log_shop_count",
  "log_nearest_school","log_school_count",
  "log_nearest_tube","log_tube_count",
  "log_nearest_hospital","log_hospital_count",
  "log_nearest_park","log_park_count",
  "log_nearest_police","log_police_count"
)

d <- dt[complete.cases(dt[, ..keep])]

# Here we perform 5-Fold Cross-Validation

set.seed(37)

K <- 5
n <- nrow(d)

# create fold IDs
d[, fold := sample(rep(1:K, length.out = n))]

fml <- as.formula(paste("y ~", paste(setdiff(keep, "y"), collapse = " + ")))

rmse_vec <- numeric(K)
mae_vec  <- numeric(K)
r2_vec   <- numeric(K)

for (k in 1:K) {
  
  train <- d[fold != k]
  test  <- d[fold == k]
  
  model <- lm(fml, data = train)
  
  preds <- predict(model, newdata = test)
  
  actual <- test$y
  
  rmse_vec[k] <- sqrt(mean((actual - preds)^2))
  mae_vec[k]  <- mean(abs(actual - preds))
  r2_vec[k]   <- cor(actual, preds)^2
}

cat("\nThese are our 5-FOLD CV RESULTS \n")
cat("Average RMSE:", mean(rmse_vec), "\n")
cat("Average MAE :", mean(mae_vec), "\n")
cat("Average R2  :", mean(r2_vec), "\n")

# The below is for our coefficient table

ols_full <- lm(fml, data = d)

cat("\n OLS")
print(summary(ols_full))

rob_se <- sqrt(diag(vcovHC(ols_full, type = "HC1")))

cat("\nRobust SE (Full Sample):\n")
print(cbind(Estimate = coef(ols_full), Robust_SE = rob_se))

# This is the non-linear GAM model

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(mgcv)
})

options(scipen = 999)
Sys.setenv("TZ" = "Europe/London")


dt <- fread("Imputed.csv", showProgress = TRUE)

# These are our only required columns 

need_cols <- c(
  "saleEstimate_currentPrice",
  "floorAreaSqM",
  "bedrooms",
  "bathrooms",
  "livingRooms",
  "tenure",
  "postcode_lp",
  "crime_last_12m",
  "osm_nearest_shop_m","osm_shop_count_1000m",
  "osm_nearest_school_m","osm_school_count_1000m",
  "osm_nearest_tube_station_m","osm_tube_station_count_1000m",
  "osm_nearest_hospital_m","osm_hospital_count_3000m",
  "osm_nearest_park_m","osm_park_count_2000m",
  "osm_nearest_police_station_m","osm_police_station_count_1000m"
)

missing <- setdiff(need_cols, names(dt))
if (length(missing) > 0) stop(paste("Missing columns:", paste(missing, collapse = ", ")))

dt <- dt[, ..need_cols]

# Cleaning 

to_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(x))))
safe_log <- function(x) log(pmax(x, 1))

num_vars <- setdiff(need_cols, c("saleEstimate_currentPrice", "tenure"))
for (v in num_vars) dt[, (v) := to_num(get(v))]

dt[, tenure := factor(
  fifelse(grepl("free", tolower(tenure)), "freehold",
          fifelse(grepl("lease", tolower(tenure)), "leasehold", NA_character_))
)]

# Outcome and key transforms
dt[, y := safe_log(saleEstimate_currentPrice)]
dt[, log_floorArea := safe_log(floorAreaSqM)]

# Log transforms for OSM and crime 
dt[, log_nearest_shop := safe_log(osm_nearest_shop_m)]
dt[, log_shop_count := safe_log(osm_shop_count_1000m + 1)]
dt[, log_nearest_school := safe_log(osm_nearest_school_m)]
dt[, log_school_count := safe_log(osm_school_count_1000m + 1)]
dt[, log_nearest_tube := safe_log(osm_nearest_tube_station_m)]
dt[, log_tube_count := safe_log(osm_tube_station_count_1000m + 1)]
dt[, log_nearest_hospital := safe_log(osm_nearest_hospital_m)]
dt[, log_hospital_count := safe_log(osm_hospital_count_3000m + 1)]
dt[, log_nearest_park := safe_log(osm_nearest_park_m)]
dt[, log_park_count := safe_log(osm_park_count_2000m + 1)]
dt[, log_nearest_police := safe_log(osm_nearest_police_station_m)]
dt[, log_police_count := safe_log(osm_police_station_count_1000m + 1)]
dt[, log_crime_last_12m := safe_log(crime_last_12m + 1)]

# Modelling sample 

keep <- c(
  "y",
  "postcode_lp",
  "log_floorArea",
  "bedrooms",
  "bathrooms",
  "livingRooms",
  "tenure",
  "log_crime_last_12m",
  "log_nearest_shop","log_shop_count",
  "log_nearest_school","log_school_count",
  "log_nearest_tube","log_tube_count",
  "log_nearest_hospital","log_hospital_count",
  "log_nearest_park","log_park_count",
  "log_nearest_police","log_police_count"
)

d <- dt[complete.cases(dt[, ..keep])]

cat("Rows in modelling sample:", nrow(d), "\n")
cat("Tenure split:\n")
print(d[, .N, by = tenure][order(-N)])

# Here we set up the train test split

set.seed(37)
d[, u := runif(.N)]
train <- d[u <= 0.8]
test  <- d[u > 0.8]

# This is our first non linear model

set.seed(37)
max_train <- 120000
if (nrow(train) > max_train) {
  train_fit <- train[sample.int(nrow(train), max_train)]
} else {
  train_fit <- train
}
cat("Rows used to fit GAM:", nrow(train_fit), "\n")

n_unique_ok <- function(x) {
  x <- x[is.finite(x)]
  length(unique(x))
}

make_term <- function(var, k_wanted, bs = "cs") {
  nu <- n_unique_ok(train_fit[[var]])
  if (is.na(nu) || nu <= 1) return(NULL)
  if (nu <= 4) return(var)
  k_use <- min(k_wanted, nu - 1)
  sprintf("s(%s, k = %d, bs = '%s')", var, k_use, bs)
}

terms_core <- c(
  make_term("postcode_lp", 15, bs = "cs"),
  make_term("log_floorArea", 12, bs = "cs"),
  make_term("bathrooms", 6, bs = "cr"),
  make_term("livingRooms", 6, bs = "cr"),
  "bedrooms"
)
terms_core <- terms_core[!sapply(terms_core, is.null)]

# Smooth the same amenity and crime set (fixed list)
terms_opt_vars <- c(
  "log_crime_last_12m",
  "log_nearest_tube","log_tube_count",
  "log_nearest_park","log_park_count",
  "log_nearest_school","log_school_count",
  "log_nearest_shop","log_shop_count",
  "log_nearest_hospital","log_hospital_count",
  "log_nearest_police","log_police_count"
)

terms_opt <- lapply(terms_opt_vars, function(v) make_term(v, 8, bs = "cs"))
terms_opt <- terms_opt[!sapply(terms_opt, is.null)]

all_terms <- c(terms_core, unlist(terms_opt))

print(all_terms)

gam_formula <- as.formula(paste(
  "y ~ tenure +",
  paste(all_terms, collapse = " + ")
))

gam_fit <- bam(
  gam_formula,
  data = train_fit,
  method = "fREML",
  select = TRUE,
  discrete = TRUE
)

print(summary(gam_fit))

# Out of sample performance

pred_test <- as.numeric(predict(gam_fit, newdata = test))

rmse_log <- sqrt(mean((test$y - pred_test)^2))
cat("\nTest RMSE (log scale):", round(rmse_log, 4), "\n")

ratio <- exp(pred_test) / exp(test$y)
cat("Mean predicted/actual ratio:", round(mean(ratio), 4), "\n")
cat("Median predicted/actual ratio:", round(median(ratio), 4), "\n")
cat("Mean bias percent:", round((mean(ratio) - 1) * 100, 2), "\n")
cat("Share overestimates percent:", round(mean(ratio > 1) * 100, 2), "\n")

pred_price <- exp(pred_test)
actual_price <- exp(test$y)
r2_price <- 1 - sum((pred_price - actual_price)^2) / sum((actual_price - mean(actual_price))^2)
cat("Test R squared (price scale):", round(r2_price, 4), "\n")

# Calculating counterfactual % price impacts

stopifnot(exists("gam_fit"))

pct <- function(dlog) 100 * (exp(dlog) - 1)

set.seed(37)
n_eff <- min(2000, nrow(test))
base <- copy(test[sample.int(nrow(test), n_eff)])

p0 <- as.numeric(predict(gam_fit, newdata = base))

cf <- function(label, modify) {
  nd <- copy(base)
  modify(nd)
  p1 <- as.numeric(predict(gam_fit, newdata = nd))
  dlog <- mean(p1 - p0, na.rm = TRUE)
  data.table(effect = label, avg_pct_price = pct(dlog))
}

eff <- rbindlist(list(
  cf("postcode_lp +0.10", function(nd) nd[, postcode_lp := postcode_lp + 0.10]),
  cf("floor area +10%", function(nd) {
    nd[, floorAreaSqM := floorAreaSqM * 1.10]
    nd[, log_floorArea := log(pmax(floorAreaSqM, 1))]
  }),
  cf("bathrooms +1", function(nd) nd[, bathrooms := bathrooms + 1]),
  cf("livingRooms +1", function(nd) nd[, livingRooms := livingRooms + 1]),
  cf("bedrooms +1", function(nd) nd[, bedrooms := bedrooms + 1]),
  cf("leasehold vs freehold", function(nd) {
    nd[, tenure := factor("leasehold", levels = levels(test$tenure))]
  }),
  cf("halve dist to tube", function(nd) {
    nd[, osm_nearest_tube_station_m := pmax(osm_nearest_tube_station_m / 2, 1)]
    nd[, log_nearest_tube := log(pmax(osm_nearest_tube_station_m, 1))]
  })
), fill = TRUE)

eff[, abs_pct := abs(avg_pct_price)]
setorder(eff, -abs_pct)
eff[, abs_pct := NULL]

cat("\nAvg % price impacts (", n_eff, " test homes):\n", sep = "")
print(eff)

# We create a 3x3 matrix of the Most Important Smooth Plots


pdf("GAM_top9_smooths.pdf", width = 12, height = 12)

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# 1
plot(gam_fit, select = 1, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Postcode LP)",
     xlab = "Postcode Log Price",
     ylab = "Partial Effect")

# 2
plot(gam_fit, select = 2, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Log Floor Area)",
     xlab = "Log Floor Area",
     ylab = "Partial Effect")

# 3
plot(gam_fit, select = 6, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Log Crime 12m)",
     xlab = "Log Crime (12m)",
     ylab = "Partial Effect")

# 4
plot(gam_fit, select = 3, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Bathrooms)",
     xlab = "Bathrooms",
     ylab = "Partial Effect")

# 5
plot(gam_fit, select = 4, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Living Rooms)",
     xlab = "Living Rooms",
     ylab = "Partial Effect")

# 6
plot(gam_fit, select = 7, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Log Nearest Tube)",
     xlab = "Log Distance to Tube",
     ylab = "Partial Effect")

# 7
plot(gam_fit, select = 9, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Log Nearest Park)",
     xlab = "Log Distance to Park",
     ylab = "Partial Effect")

# 8
plot(gam_fit, select = 14, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Log Hospital Count)",
     xlab = "Log Hospital Count",
     ylab = "Partial Effect")

# 9  (NEW — replacing rooms_density)
plot(gam_fit, select = 13, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE,
     main = "s(Log Nearest Hospital)",
     xlab = "Log Distance to Hospital",
     ylab = "Partial Effect")

dev.off()

cat("3x3 smooth plots saved to GAM_top9_smooths.pdf\n")


