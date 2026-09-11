rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(ranger)
  library(caret)
})

options(scipen = 999)
set.seed(37)
Sys.setenv("TZ" = "Europe/London")



dt <- fread("Imputed.csv", showProgress = TRUE)

# Keeping only these regressors + target

target <- "saleEstimate_currentPrice"

feature_vars <- c(
  "floorAreaSqM",
  "bedrooms",
  "bathrooms",
  "livingRooms",
  "postcode_lp",
  "osm_nearest_shop_m",
  "osm_shop_count_1000m",
  "osm_nearest_police_station_m",
  "osm_police_station_count_1000m",
  "osm_nearest_school_m",
  "osm_school_count_1000m",
  "osm_nearest_tube_station_m",
  "osm_tube_station_count_1000m",
  "osm_nearest_hospital_m",
  "osm_hospital_count_3000m",
  "osm_nearest_park_m",
  "osm_park_count_2000m",
  "crime_last_12m",
  "tenure"
)

need_cols <- c(target, feature_vars)
missing <- setdiff(need_cols, names(dt))
stopifnot(length(missing) == 0)

dt_model <- dt[, ..need_cols]

# Cleaning tenure
dt_model[, tenure := factor(tolower(str_trim(tenure)))]
dt_model <- dt_model[tenure %in% c("freehold", "leasehold")]
dt_model <- dt_model[complete.cases(dt_model)]

# Setting up the train test split 
set.seed(37)
dt_model[, u := runif(.N)]
train <- dt_model[u <= 0.8]
test  <- dt_model[u > 0.8]
dt_model[, u := NULL]


# Cap for speed
train_cap <- 120000
if (nrow(train) > train_cap) {
  train <- train[sample.int(nrow(train), train_cap)]
}

train[, y := log(get(target))]
test[,  y := log(get(target))]

# Ranger model

rf_formula <- as.formula(paste("y ~", paste(feature_vars, collapse = " + ")))

rf_model <- ranger(
  formula = rf_formula,
  data = train,
  num.trees = 400,
  mtry = max(2, floor(sqrt(length(feature_vars)))),
  min.node.size = 20,
  importance = "impurity",
  num.threads = parallel::detectCores(),
  seed = 37
)

pred_log <- predict(rf_model, data = test)$predictions
pred <- exp(pred_log)

rmse <- sqrt(mean((pred - test[[target]])^2))
r2 <- cor(pred, test[[target]])^2

rmse_log <- sqrt(mean((pred_log - test$y)^2))
r2_log <- cor(pred_log, test$y)^2

cat("Ranger RF (selected regressors, log target)\n")
cat("Features used: ", length(feature_vars), "\n", sep = "")
cat("Train rows used: ", nrow(train), "\n", sep = "")
cat("RMSE (£): ", round(rmse, 0), "\n", sep = "")
cat("R-squared: ", round(r2, 4), "\n", sep = "")
cat("RMSE (log): ", round(rmse_log, 4), "\n", sep = "")
cat("R-squared (log): ", round(r2_log, 4), "\n", sep = "")

# CHecking the variable importance

imp <- sort(rf_model$variable.importance, decreasing = TRUE)
print(head(imp, 30))
