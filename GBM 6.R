rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(caret)
  library(xgboost)
})

options(scipen = 999)
set.seed(37)
Sys.setenv("TZ" = "Europe/London")

# loading our data

dt <- fread("Imputed.csv", showProgress = TRUE)

#  Keep only these regressors and target

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

# Cleaning tenure and drop missing

dt_model[, tenure := factor(tolower(str_trim(tenure)))]
dt_model <- dt_model[tenure %in% c("freehold", "leasehold")]
dt_model <- dt_model[complete.cases(dt_model)]

# Setting up the Train test split

set.seed(37)
dt_model[, u := runif(.N)]
train <- dt_model[u <= 0.8]
test  <- dt_model[u > 0.8]
dt_model[, u := NULL]


## Optional cap for speed
train_cap <- 120000
if (nrow(train) > train_cap) {
  train <- train[sample.int(nrow(train), train_cap)]
}

# Our outcome variable is in logs, as in all models

train[, y := log(get(target))]
test[,  y := log(get(target))]


x_train <- model.matrix(
  ~ floorAreaSqM + bedrooms + bathrooms + livingRooms +
    postcode_lp +
    osm_nearest_shop_m + osm_shop_count_1000m +
    osm_nearest_police_station_m + osm_police_station_count_1000m +
    osm_nearest_school_m + osm_school_count_1000m +
    osm_nearest_tube_station_m + osm_tube_station_count_1000m +
    osm_nearest_hospital_m + osm_hospital_count_3000m +
    osm_nearest_park_m + osm_park_count_2000m +
    crime_last_12m + tenure - 1,
  data = train
)

x_test <- model.matrix(
  ~ floorAreaSqM + bedrooms + bathrooms + livingRooms +
    postcode_lp +
    osm_nearest_shop_m + osm_shop_count_1000m +
    osm_nearest_police_station_m + osm_police_station_count_1000m +
    osm_nearest_school_m + osm_school_count_1000m +
    osm_nearest_tube_station_m + osm_tube_station_count_1000m +
    osm_nearest_hospital_m + osm_hospital_count_3000m +
    osm_nearest_park_m + osm_park_count_2000m +
    crime_last_12m + tenure - 1,
  data = test
)

dtrain <- xgb.DMatrix(data = x_train, label = train$y)
dtest  <- xgb.DMatrix(data = x_test,  label = test$y)

# Gradient boosting

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 6,
  min_child_weight = 5,
  subsample = 0.8,
  colsample_bytree = 0.8,
  lambda = 1,
  alpha = 0
)

watchlist <- list(train = dtrain, test = dtest)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 5000,
  watchlist = watchlist,
  early_stopping_rounds = 50,
  verbose = 1
)

# Here we predict and evaluate, getting output in both logs and £

pred_log <- predict(xgb_model, dtest)
pred <- exp(pred_log)

rmse <- sqrt(mean((pred - test[[target]])^2))
r2 <- cor(pred, test[[target]])^2

rmse_log <- sqrt(mean((pred_log - test$y)^2))
r2_log <- cor(pred_log, test$y)^2

cat("XGBoost (selected regressors, log target)\n")
cat("Train rows used: ", nrow(train), "\n", sep = "")
cat("Best iteration: ", xgb_model$best_iteration, "\n", sep = "")
cat("RMSE (£): ", round(rmse, 0), "\n", sep = "")
cat("R-squared: ", round(r2, 4), "\n", sep = "")
cat("RMSE (log): ", round(rmse_log, 4), "\n", sep = "")
cat("R-squared (log): ", round(r2_log, 4), "\n", sep = "")

# Generating our Feature importance

imp <- xgb.importance(model = xgb_model)
print(head(imp, 30))

# Converting this into a plot to be used in our report
library(ggplot2)

# Take top 15 most important features
imp_plot <- imp[order(-Gain)][1:15]

ggplot(imp_plot, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "XGBoost Variable Importance",
    subtitle = "Top 15 Features by Gain",
    x = "",
    y = "Relative Importance (Gain)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

