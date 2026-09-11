# This generates our table comparing the 4 models
# It also has the ROC Graph and performs 5 fold cross validation



# Setup
rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(lmtest)
  library(sandwich)
  library(mgcv)
  library(ranger)
  library(caret)
  library(xgboost)
  library(pROC)
  library(ggplot2)
})

options(scipen = 999)
Sys.setenv("TZ" = "Europe/London")

# Helpers
to_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(x))))
safe_log <- function(x) log(pmax(x, 1))
pct_from_dlog <- function(dlog) 100 * (exp(dlog) - 1)

fmt_pct <- function(x) {
  ifelse(
    is.na(x),
    "",
    paste0(ifelse(x > 0, "+", ""), format(round(x, 2), nsmall = 2), "%")
  )
}

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE)

rmse_vec <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))

# Load data
dt <- fread("Imputed.csv", showProgress = TRUE)

# Feature list (raw)
raw_features <- c(
  "floorAreaSqM",
  "bedrooms",
  "bathrooms",
  "livingRooms",
  "postcode_lp",
  "osm_nearest_shop_m", "osm_shop_count_1000m",
  "osm_nearest_school_m", "osm_school_count_1000m",
  "osm_nearest_tube_station_m", "osm_tube_station_count_1000m",
  "osm_nearest_hospital_m", "osm_hospital_count_3000m",
  "osm_nearest_park_m", "osm_park_count_2000m",
  "osm_nearest_police_station_m", "osm_police_station_count_1000m",
  "crime_last_12m",
  "tenure"
)

target <- "saleEstimate_currentPrice"

# Build cleaned dataset
d <- dt[, c(target, raw_features), with = FALSE]

# Clean tenure
d[, tenure := factor(
  fifelse(grepl("free", tolower(tenure)), "freehold",
          fifelse(grepl("lease", tolower(tenure)), "leasehold", NA_character_))
)]

# Clean numerics
num_vars <- setdiff(c(target, raw_features), "tenure")
for (v in num_vars) d[, (v) := to_num(get(v))]

# Filter usable rows
d <- d[complete.cases(d)]
d <- d[tenure %in% c("freehold", "leasehold")]

# Log outcome
d[, y := safe_log(get(target))]

# Log transforms for OLS/GAM (raw kept for ML)
d[, log_floorArea := safe_log(floorAreaSqM)]
d[, log_crime := safe_log(crime_last_12m + 1)]
d[, log_nearest_shop := safe_log(osm_nearest_shop_m)]
d[, log_shop_count := safe_log(osm_shop_count_1000m + 1)]
d[, log_nearest_school := safe_log(osm_nearest_school_m)]
d[, log_school_count := safe_log(osm_school_count_1000m + 1)]
d[, log_nearest_tube := safe_log(osm_nearest_tube_station_m)]
d[, log_tube_count := safe_log(osm_tube_station_count_1000m + 1)]
d[, log_nearest_hospital := safe_log(osm_nearest_hospital_m)]
d[, log_hospital_count := safe_log(osm_hospital_count_3000m + 1)]
d[, log_nearest_park := safe_log(osm_nearest_park_m)]
d[, log_park_count := safe_log(osm_park_count_2000m + 1)]
d[, log_nearest_police := safe_log(osm_nearest_police_station_m)]
d[, log_police_count := safe_log(osm_police_station_count_1000m + 1)]

cat("Final sample size:", nrow(d), "\n")

# Train / test split
set.seed(37)
d[, u := runif(.N)]
train <- d[u <= 0.8]
test  <- d[u > 0.8]

cat("Train:", nrow(train), "| Test:", nrow(test), "\n")

# 5-fold CV on train
set.seed(37)
folds <- caret::createFolds(train$y, k = 5, returnTrain = FALSE)

cv_results <- data.table(
  fold = integer(),
  model = character(),
  rmse = numeric()
)

# OLS features and formula
ols_features <- c(
  "postcode_lp", "log_floorArea",
  "bedrooms", "bathrooms", "livingRooms", "tenure",
  "log_crime",
  "log_nearest_shop", "log_shop_count",
  "log_nearest_school", "log_school_count",
  "log_nearest_tube", "log_tube_count",
  "log_nearest_hospital", "log_hospital_count",
  "log_nearest_park", "log_park_count",
  "log_nearest_police", "log_police_count"
)

fml_ols <- as.formula(paste("y ~", paste(ols_features, collapse = " + ")))

# GAM helpers
n_unique_ok <- function(x) {
  x <- x[is.finite(x)]
  length(unique(x))
}

make_term <- function(df, var, k_wanted, bs = "cs") {
  nu <- n_unique_ok(df[[var]])
  if (is.na(nu) || nu <= 1) return(NULL)
  if (nu <= 4) return(var)
  k_use <- min(k_wanted, nu - 1)
  sprintf("s(%s, k = %d, bs = '%s')", var, k_use, bs)
}

gam_terms <- c(
  make_term(train, "postcode_lp", 15, "cs"),
  make_term(train, "log_floorArea", 12, "cs"),
  make_term(train, "bathrooms", 6, "cr"),
  make_term(train, "livingRooms", 6, "cr"),
  "bedrooms",
  make_term(train, "log_crime", 8, "cs"),
  make_term(train, "log_nearest_tube", 8, "cs"),
  make_term(train, "log_tube_count", 8, "cs"),
  make_term(train, "log_nearest_park", 8, "cs"),
  make_term(train, "log_park_count", 8, "cs"),
  make_term(train, "log_nearest_school", 8, "cs"),
  make_term(train, "log_school_count", 8, "cs"),
  make_term(train, "log_nearest_shop", 8, "cs"),
  make_term(train, "log_shop_count", 8, "cs"),
  make_term(train, "log_nearest_hospital", 8, "cs"),
  make_term(train, "log_hospital_count", 8, "cs"),
  make_term(train, "log_nearest_police", 8, "cs"),
  make_term(train, "log_police_count", 8, "cs")
)
gam_terms <- gam_terms[!sapply(gam_terms, is.null)]

fml_gam <- as.formula(paste("y ~ tenure +", paste(gam_terms, collapse = " + ")))

# RF features and formula (raw)
rf_features <- c(
  "floorAreaSqM", "bedrooms", "bathrooms", "livingRooms",
  "postcode_lp",
  "osm_nearest_shop_m", "osm_shop_count_1000m",
  "osm_nearest_school_m", "osm_school_count_1000m",
  "osm_nearest_tube_station_m", "osm_tube_station_count_1000m",
  "osm_nearest_hospital_m", "osm_hospital_count_3000m",
  "osm_nearest_park_m", "osm_park_count_2000m",
  "osm_nearest_police_station_m", "osm_police_station_count_1000m",
  "crime_last_12m",
  "tenure"
)

fml_rf <- as.formula(paste("y ~", paste(rf_features, collapse = " + ")))

# XGB design matrix on full train
xgb_formula <- as.formula(paste("~ ", paste(rf_features, collapse = " + "), " - 1"))
x_train_full <- model.matrix(xgb_formula, data = train)
dtrain_full  <- xgb.DMatrix(data = x_train_full, label = train$y)

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

# CV loops for OLS / GAM / RF
for (k in seq_along(folds)) {
  idx_val <- folds[[k]]
  tr_k <- train[-idx_val]
  va_k <- train[idx_val]
  
  # OLS CV
  ols_k <- lm(fml_ols, data = tr_k)
  pred_ols_k <- as.numeric(predict(ols_k, newdata = va_k))
  cv_results <- rbind(cv_results, data.table(
    fold = k, model = "OLS", rmse = rmse_vec(va_k$y, pred_ols_k)
  ))
  
  # GAM CV
  set.seed(37)
  gam_k <- bam(fml_gam, data = tr_k, method = "fREML", select = TRUE, discrete = TRUE)
  pred_gam_k <- as.numeric(predict(gam_k, newdata = va_k))
  cv_results <- rbind(cv_results, data.table(
    fold = k, model = "GAM", rmse = rmse_vec(va_k$y, pred_gam_k)
  ))
  
  # RF CV
  rf_k <- ranger(
    formula = fml_rf,
    data = tr_k,
    num.trees = 400,
    mtry = max(2, floor(sqrt(length(rf_features)))),
    min.node.size = 20,
    importance = "impurity",
    num.threads = parallel::detectCores(),
    seed = 37
  )
  pred_rf_k <- as.numeric(predict(rf_k, data = va_k)$predictions)
  cv_results <- rbind(cv_results, data.table(
    fold = k, model = "Random forest", rmse = rmse_vec(va_k$y, pred_rf_k)
  ))
}

# XGBoost 5-fold CV
set.seed(37)

xgb_cv <- xgb.cv(
  params = params,
  data = dtrain_full,
  nrounds = 5000,
  nfold = 5,
  early_stopping_rounds = 50,
  showsd = TRUE,
  verbose = 0
)

best_nrounds_cv <- xgb_cv$best_iteration
if (is.null(best_nrounds_cv) || length(best_nrounds_cv) == 0 || is.na(best_nrounds_cv)) {
  best_nrounds_cv <- which.min(xgb_cv$evaluation_log$test_rmse_mean)
}

best_rmse_cv <- as.numeric(
  xgb_cv$evaluation_log[best_nrounds_cv, test_rmse_mean]
)

cv_summary <- cv_results[, .(
  rmse_mean = mean(rmse, na.rm = TRUE),
  rmse_sd = sd(rmse, na.rm = TRUE)
), by = model][order(rmse_mean)]

cv_summary <- rbind(
  cv_summary,
  data.table(
    model = "Gradient boosting",
    rmse_mean = best_rmse_cv,
    rmse_sd = NA_real_
  ),
  fill = TRUE
)

cat("\CV RESULTS (5 fold)\n")
print(cv_summary)

cat("\nXGBoost CV best nrounds:", best_nrounds_cv, "\n")

# Fit final models (log price)
# OLS
ols <- lm(fml_ols, data = train)
ols_rob_se <- sqrt(diag(vcovHC(ols, type = "HC1")))

# GAM
set.seed(37)
gam_fit <- bam(fml_gam, data = train, method = "fREML", select = TRUE, discrete = TRUE)

# RF (raw)
rf_model <- ranger(
  formula = fml_rf,
  data = train,
  num.trees = 400,
  mtry = max(2, floor(sqrt(length(rf_features)))),
  min.node.size = 20,
  importance = "impurity",
  num.threads = parallel::detectCores(),
  seed = 37
)

# XGB (raw)
x_train <- model.matrix(xgb_formula, data = train)
x_test  <- model.matrix(xgb_formula, data = test)

dtrain <- xgb.DMatrix(data = x_train, label = train$y)
dtest  <- xgb.DMatrix(data = x_test,  label = test$y)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 5000,
  evals = list(train = dtrain, test = dtest),
  early_stopping_rounds = 50,
  verbose = 0
)

# Counterfactuals (log predictions)
pred_ols_log <- function(newdata) as.numeric(predict(ols, newdata = newdata))
pred_gam_log <- function(newdata) as.numeric(predict(gam_fit, newdata = newdata))
pred_rf_log  <- function(newdata) as.numeric(predict(rf_model, data = newdata)$predictions)
pred_xgb_log <- function(newdata) {
  mm <- model.matrix(xgb_formula, data = newdata)
  as.numeric(predict(xgb_model, xgb.DMatrix(mm)))
}

# Update log features
update_logs <- function(nd) {
  nd[, log_floorArea := safe_log(floorAreaSqM)]
  nd[, log_crime := safe_log(crime_last_12m + 1)]
  nd[, log_nearest_tube := safe_log(osm_nearest_tube_station_m)]
  nd[, log_tube_count := safe_log(osm_tube_station_count_1000m + 1)]
  nd[, log_nearest_park := safe_log(osm_nearest_park_m)]
  nd[, log_park_count := safe_log(osm_park_count_2000m + 1)]
  nd[, log_nearest_school := safe_log(osm_nearest_school_m)]
  nd[, log_school_count := safe_log(osm_school_count_1000m + 1)]
  nd[, log_nearest_shop := safe_log(osm_nearest_shop_m)]
  nd[, log_shop_count := safe_log(osm_shop_count_1000m + 1)]
  nd[, log_nearest_hospital := safe_log(osm_nearest_hospital_m)]
  nd[, log_hospital_count := safe_log(osm_hospital_count_3000m + 1)]
  nd[, log_nearest_police := safe_log(osm_nearest_police_station_m)]
  nd[, log_police_count := safe_log(osm_police_station_count_1000m + 1)]
  invisible(nd)
}

set.seed(37)
n_eff <- min(5000, nrow(test))
base <- copy(test[sample.int(nrow(test), n_eff)])

p0_ols <- pred_ols_log(base)
p0_gam <- pred_gam_log(base)
p0_rf  <- pred_rf_log(base)
p0_xgb <- pred_xgb_log(base)

cf_row <- function(key, desc, apply_fn, baseline_fn = NULL) {
  
  # Baseline
  base_mod <- copy(base)
  if (!is.null(baseline_fn)) {
    baseline_fn(base_mod)
    update_logs(base_mod)
    p0_ols_local <- pred_ols_log(base_mod)
    p0_gam_local <- pred_gam_log(base_mod)
    p0_rf_local  <- pred_rf_log(base_mod)
    p0_xgb_local <- pred_xgb_log(base_mod)
  } else {
    p0_ols_local <- p0_ols
    p0_gam_local <- p0_gam
    p0_rf_local  <- p0_rf
    p0_xgb_local <- p0_xgb
  }
  
  # Counterfactual
  nd <- copy(base_mod)
  apply_fn(nd)
  update_logs(nd)
  
  data.table(
    scenario = key,
    interpretation = desc,
    ols = pct_from_dlog(mean(pred_ols_log(nd) - p0_ols_local, na.rm = TRUE)),
    gam = pct_from_dlog(mean(pred_gam_log(nd) - p0_gam_local, na.rm = TRUE)),
    rf  = pct_from_dlog(mean(pred_rf_log(nd)  - p0_rf_local,  na.rm = TRUE)),
    xgb = pct_from_dlog(mean(pred_xgb_log(nd) - p0_xgb_local, na.rm = TRUE))
  )
}

effects <- rbindlist(list(
  cf_row("postcode_lp +0.10", "Increase postcode_lp by 0.10",
         function(nd) nd[, postcode_lp := postcode_lp + 0.10]),
  cf_row("floorAreaSqM +10%", "Increase floorAreaSqM by 10%",
         function(nd) nd[, floorAreaSqM := floorAreaSqM * 1.10]),
  cf_row("bedrooms +1", "Increase bedrooms by 1",
         function(nd) nd[, bedrooms := bedrooms + 1]),
  cf_row("bathrooms +1", "Increase bathrooms by 1",
         function(nd) nd[, bathrooms := bathrooms + 1]),
  cf_row("livingRooms +1", "Increase livingRooms by 1",
         function(nd) nd[, livingRooms := livingRooms + 1]),
  cf_row("crime_last_12m +10%", "Increase crime by 10%",
         function(nd) nd[, crime_last_12m := crime_last_12m * 1.10]),
  cf_row("nearest tube /2", "Halve distance to nearest tube",
         function(nd) nd[, osm_nearest_tube_station_m := pmax(osm_nearest_tube_station_m / 2, 1)]),
  cf_row("leasehold vs freehold", "Leasehold compared to freehold",
         function(nd) nd[, tenure := factor("leasehold", levels = c("freehold", "leasehold"))],
         baseline_fn = function(nd) nd[, tenure := factor("freehold", levels = c("freehold", "leasehold"))])
))

effects_fmt <- copy(effects)
effects_fmt[, `:=`(
  ols = fmt_pct(ols),
  gam = fmt_pct(gam),
  rf  = fmt_pct(rf),
  xgb = fmt_pct(xgb)
)]
setnames(effects_fmt, c("ols", "gam", "rf", "xgb"),
         c("Linear OLS", "GAM", "Random Forest", "Gradient Boosting"))

# Test-set performance (log scale)
pred_test_ols <- pred_ols_log(test)
pred_test_gam <- pred_gam_log(test)
pred_test_rf  <- pred_rf_log(test)
pred_test_xgb <- pred_xgb_log(test)

actual_log <- test$y

perf <- data.table(
  scenario = c("Test RMSE (log scale)", "Test R²"),
  interpretation = c("Root mean squared error on held-out test set", "R-squared on held-out test set"),
  ols = c(
    rmse_vec(actual_log, pred_test_ols),
    1 - sum((actual_log - pred_test_ols)^2) / sum((actual_log - mean(actual_log))^2)
  ),
  gam = c(
    rmse_vec(actual_log, pred_test_gam),
    1 - sum((actual_log - pred_test_gam)^2) / sum((actual_log - mean(actual_log))^2)
  ),
  rf = c(
    rmse_vec(actual_log, pred_test_rf),
    1 - sum((actual_log - pred_test_rf)^2) / sum((actual_log - mean(actual_log))^2)
  ),
  xgb = c(
    rmse_vec(actual_log, pred_test_xgb),
    1 - sum((actual_log - pred_test_xgb)^2) / sum((actual_log - mean(actual_log))^2)
  )
)

# Format performance
perf_fmt <- copy(perf)
perf_fmt[scenario == "Test RMSE (log scale)", `:=`(
  ols = sprintf("%.4f", ols),
  gam = sprintf("%.4f", gam),
  rf  = sprintf("%.4f", rf),
  xgb = sprintf("%.4f", xgb)
)]
perf_fmt[scenario == "Test R²", `:=`(
  ols = sprintf("%.4f", ols),
  gam = sprintf("%.4f", gam),
  rf  = sprintf("%.4f", rf),
  xgb = sprintf("%.4f", xgb)
)]

setnames(perf_fmt, c("ols", "gam", "rf", "xgb"),
         c("Linear OLS", "GAM", "Random Forest", "Gradient Boosting"))

# Combine effects + performance
effects_fmt <- rbind(effects_fmt, perf_fmt, fill = TRUE)

cat("\nCOUNTERFACTUAL EFFECTS + PERFORMANCE\n")
print(effects_fmt)

fwrite(effects_fmt, "myimpute.csv")

# ROC comparison (classification)
top_share <- 0.05
cutoff <- as.numeric(quantile(train$y, probs = 1 - top_share, na.rm = TRUE))

train[, y_class := factor(as.integer(y >= cutoff), levels = c(0, 1))]
test[,  y_class := factor(as.integer(y >= cutoff), levels = c(0, 1))]

# Features for classification
x_vars_cls <- c(
  "postcode_lp", "log_floorArea",
  "bedrooms", "bathrooms", "livingRooms", "tenure",
  "log_crime",
  "log_nearest_shop", "log_shop_count",
  "log_nearest_school", "log_school_count",
  "log_nearest_tube", "log_tube_count",
  "log_nearest_hospital", "log_hospital_count",
  "log_nearest_park", "log_park_count",
  "log_nearest_police", "log_police_count"
)

fml_cls <- as.formula(paste("y_class ~", paste(x_vars_cls, collapse = " + ")))

# Logistic regression
glm_cls <- glm(fml_cls, data = train, family = binomial(link = "logit"))
p_glm <- as.numeric(predict(glm_cls, newdata = test, type = "response"))

# GAM classifier
terms_gam_cls <- c(
  make_term(train, "postcode_lp", 15, "cs"),
  make_term(train, "log_floorArea", 12, "cs"),
  make_term(train, "log_crime", 10, "cs"),
  make_term(train, "log_nearest_tube", 10, "cs"),
  make_term(train, "log_tube_count", 10, "cs"),
  make_term(train, "log_nearest_school", 10, "cs"),
  make_term(train, "log_school_count", 10, "cs"),
  make_term(train, "log_nearest_shop", 10, "cs"),
  make_term(train, "log_shop_count", 10, "cs"),
  make_term(train, "log_nearest_hospital", 10, "cs"),
  make_term(train, "log_hospital_count", 10, "cs"),
  make_term(train, "log_nearest_park", 10, "cs"),
  make_term(train, "log_park_count", 10, "cs"),
  make_term(train, "log_nearest_police", 10, "cs"),
  make_term(train, "log_police_count", 10, "cs"),
  "bedrooms", "bathrooms", "livingRooms", "tenure"
)
terms_gam_cls <- terms_gam_cls[!sapply(terms_gam_cls, is.null)]
fml_gam_cls <- as.formula(paste("y_class ~", paste(terms_gam_cls, collapse = " + ")))

set.seed(37)
gam_cls <- bam(
  fml_gam_cls,
  data = train,
  family = binomial(link = "logit"),
  method = "fREML",
  select = TRUE,
  discrete = TRUE
)
p_gam_cls <- as.numeric(predict(gam_cls, newdata = test, type = "response"))

# Random forest classifier
set.seed(37)
rf_cls <- ranger(
  formula = fml_cls,
  data = train[, c("y_class", x_vars_cls), with = FALSE],
  probability = TRUE,
  num.trees = 800,
  mtry = max(2, floor(sqrt(length(x_vars_cls)))),
  min.node.size = 10,
  seed = 37
)
pr_rf <- predict(rf_cls, data = test[, x_vars_cls, with = FALSE])$predictions
if (is.null(colnames(pr_rf))) colnames(pr_rf) <- c("0", "1")
p_rf_cls <- as.numeric(pr_rf[, "1"])

# XGBoost classifier
x_train_cls <- model.matrix(fml_cls, data = train)[, -1, drop = FALSE]
x_test_cls  <- model.matrix(fml_cls, data = test)[, -1, drop = FALSE]

y_train_cls <- as.integer(as.character(train$y_class))
y_test_cls  <- as.integer(as.character(test$y_class))

dtrain_cls <- xgb.DMatrix(data = x_train_cls, label = y_train_cls)
dtest_cls  <- xgb.DMatrix(data = x_test_cls,  label = y_test_cls)

set.seed(37)
xgb_cls <- xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.06,
    max_depth = 6,
    min_child_weight = 5,
    subsample = 0.85,
    colsample_bytree = 0.85
  ),
  data = dtrain_cls,
  nrounds = 2000,
  evals = list(train = dtrain_cls, test = dtest_cls),
  early_stopping_rounds = 50,
  verbose = 0
)
p_xgb_cls <- as.numeric(predict(xgb_cls, dtest_cls))

roc_dt <- function(p, name) {
  r <- pROC::roc(response = y_test_cls, predictor = p, quiet = TRUE)
  data.table(
    fpr = 1 - as.numeric(r$specificities),
    tpr = as.numeric(r$sensitivities),
    model = sprintf("%s (AUC %.3f)", name, as.numeric(pROC::auc(r)))
  )
}

roc_all <- rbindlist(list(
  roc_dt(p_glm,     "Linear (Logit)"),
  roc_dt(p_gam_cls, "GAM"),
  roc_dt(p_rf_cls,  "Random forest"),
  roc_dt(p_xgb_cls, "Gradient boosting")
))

p_roc <- ggplot(roc_all, aes(x = fpr, y = tpr, colour = model)) +
  geom_line(linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = sprintf("ROC curves comparing models, top %.0f%% price class", top_share * 100),
    subtitle = "All models trained and evaluated on the same cleaned data and same split",
    x = "False positive rate",
    y = "True positive rate",
    colour = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("ROC.png", plot = p_roc, width = 10, height = 7, dpi = 220)

cat("Positive class is top ", top_share * 100, "% by log price threshold from training set\n", sep = "")
