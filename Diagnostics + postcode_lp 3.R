
# This one makes the sales estimate vs postcode_lp graph

# it also quantifies the premium associated with premium areas, that is included in Appendix 2

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(mgcv)
  library(lmtest)
  library(sandwich)
  library(car)
})

options(scipen = 999)
Sys.setenv("TZ" = "Europe/London")

# Loading dataset (initial data exploration one)
dt <- fread("kaggle_cpih_winsorized.csv", showProgress = TRUE)

to_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(x))))

dt <- dt[, .(
  saleEstimate_currentPrice,
  postcode_lp,
  tenure
)]

dt[, saleEstimate_currentPrice := to_num(saleEstimate_currentPrice)]
dt[, postcode_lp := to_num(postcode_lp)]

dt[, tenure := tolower(str_trim(as.character(tenure)))]
dt[, tenure := fifelse(grepl("free", tenure), "freehold",
                       fifelse(grepl("lease", tenure), "leasehold", NA_character_))]
dt[, tenure := factor(tenure, levels = c("freehold", "leasehold"))]

dt <- dt[
  is.finite(saleEstimate_currentPrice) & saleEstimate_currentPrice > 0 &
    is.finite(postcode_lp) &
    !is.na(tenure)
]

dt[, log_price := log(saleEstimate_currentPrice)]

# Fit smooth
fit <- gam(
  log_price ~ tenure + s(postcode_lp, bs = "cs", k = 20),
  data = dt,
  method = "REML"
)

## -----------------------------
## Prediction grid
## -----------------------------
rng <- range(dt$postcode_lp, na.rm = TRUE)

grid <- data.table(
  postcode_lp = seq(rng[1], rng[2], length.out = 400),
  tenure = factor("freehold", levels = levels(dt$tenure))
)

pred <- predict(fit, newdata = grid, se.fit = TRUE)

grid[, `:=`(
  log_hat   = as.numeric(pred$fit),
  log_se    = as.numeric(pred$se.fit),
  price_hat = exp(as.numeric(pred$fit)),
  price_lo  = exp(as.numeric(pred$fit) - 1.96 * as.numeric(pred$se.fit)),
  price_hi  = exp(as.numeric(pred$fit) + 1.96 * as.numeric(pred$se.fit))
)]

## -----------------------------
## Light raw-data sample
## -----------------------------
set.seed(1)
raw_sample <- dt[sample.int(.N, min(.N, 15000))]

money_gbp <- label_number(prefix = "£", big.mark = ",", accuracy = 1)

## -----------------------------
## Plot
## -----------------------------
p <- ggplot() +
  geom_point(
    data = raw_sample,
    aes(x = postcode_lp, y = saleEstimate_currentPrice),
    alpha = 0.05,
    size = 0.6
  ) +
  geom_ribbon(
    data = grid,
    aes(x = postcode_lp, ymin = price_lo, ymax = price_hi),
    alpha = 0.20
  ) +
  geom_line(
    data = grid,
    aes(x = postcode_lp, y = price_hat),
    linewidth = 1.3
  ) +
  scale_y_continuous(labels = money_gbp) +
  labs(
    title = "Location premium and property prices\nSmooth relationship between postcode-level premium and sale estimates",
    x = "postcode_lp (location premium score)",
    y = "Sale estimate current price (£)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(p)

## -----------------------------
## Export PNG
## -----------------------------
png(
  filename = "postcode_lp_vs_price.png",
  width = 1600,
  height = 1000,
  res = 150
)

print(p)
dev.off()




library(data.table)
library(stringr)

stopifnot(file.exists("kaggle_cpih_winsorized.csv"))
dt <- fread("kaggle_cpih_winsorized.csv", showProgress = TRUE)

postcode_candidates <- c("postcode","Postcode","postalCode","postal_code","zipCode","ZipCode")
col_postcode <- intersect(names(dt), postcode_candidates)
stopifnot(length(col_postcode) >= 1L)
col_postcode <- col_postcode[1]

price_candidates <- c(
  "price","Price","sale_price","SalePrice","soldPrice",
  "saleEstimate_currentPrice","saleEstimateCurrentPrice"
)
col_price <- intersect(names(dt), price_candidates)
stopifnot(length(col_price) >= 1L)
col_price <- col_price[1]

controls <- c("bedrooms","bathrooms","floorAreaSqM")
controls <- controls[controls %in% names(dt)]
stopifnot(length(controls) >= 1L)

dt[, postcode_clean := toupper(trimws(as.character(get(col_postcode))))]

dt[, postcode_district := str_extract(
  postcode_clean,
  "^[A-Z]{1,2}[0-9][0-9A-Z]?"
)]

dt <- dt[!is.na(postcode_district) & postcode_district != ""]

min_n <- 40L
keep <- dt[, .N, by = postcode_district][N >= min_n, postcode_district]
dt <- dt[postcode_district %in% keep]

dt <- dt[!is.na(get(col_price)) & get(col_price) > 0]

rhs <- paste(c(controls, "postcode_district"), collapse = " + ")
fml <- as.formula(paste0("log(", col_price, ") ~ ", rhs))

model <- lm(fml, data = dt)

ref_district <- levels(factor(dt$postcode_district))[1]
print(paste("Reference postcode district:", ref_district))

targets <- c("SW1X","W1K","SW3","W8","E14")

cf <- coef(model)
vc <- vcov(model)

get_effect <- function(d) {
  term <- paste0("postcode_district", d)
  
  if (d == ref_district) {
    b <- 0
    se <- 0
  } else if (term %in% names(cf)) {
    b <- unname(cf[term])
    se <- sqrt(unname(vc[term, term]))
  } else {
    return(NULL)
  }
  
  ci_lo <- b - 1.96 * se
  ci_hi <- b + 1.96 * se
  
  data.table(
    postcode_district = d,
    log_premium = b,
    pct_premium = (exp(b) - 1) * 100,
    pct_ci_low = (exp(ci_lo) - 1) * 100,
    pct_ci_high = (exp(ci_hi) - 1) * 100,
    n_obs = dt[postcode_district == d, .N]
  )
}

res <- rbindlist(lapply(targets, get_effect), fill = TRUE)

if (ref_district %in% targets && !any(res$postcode_district == ref_district)) {
  res <- rbind(
    res,
    data.table(
      postcode_district = ref_district,
      log_premium = 0,
      pct_premium = 0,
      pct_ci_low = 0,
      pct_ci_high = 0,
      n_obs = dt[postcode_district == ref_district, .N]
    ),
    fill = TRUE
  )
}

setorder(res, -pct_premium)
print(res)

top_premiums <- data.table(term = names(cf), beta = as.numeric(cf))
top_premiums <- top_premiums[grepl("^postcode_district", term)]
top_premiums[, postcode_district := sub("^postcode_district", "", term)]
top_premiums[, pct_premium := (exp(beta) - 1) * 100]
setorder(top_premiums, -pct_premium)
print(top_premiums[1:20, .(postcode_district, pct_premium)])



#### COMBINED THE R SCRIPT SO IT HAS DIAGNOSTICS AS WELL


# In this file we are testing our dataset and seeing if our models are suited to the data we have
# We are checking for heteroskedasticity as well as outliers

suppressPackageStartupMessages({
  library(data.table)
  library(lmtest)
  library(sandwich)
  library(car)
})


data_path <- "Imputed.csv"

dt <- fread(data_path)

cat("Our data has", nrow(dt), "rows and", ncol(dt), "columns\n")

# Creating our main response variable which is log price

dt[, log_price := log(saleEstimate_currentPrice)]

stopifnot(all(is.finite(dt$log_price)))

# Here we fit our baseline hedonic model which is linear

lm_base <- lm(
  log_price ~ bedrooms + bathrooms + floorAreaSqM + livingRooms +
    tenure + factor(borough),
  data = dt
)


# We start the Residual diagnostics (linearity, variance, leverage)


par(mfrow = c(2,2))
plot(lm_base)
par(mfrow = c(1,1))

# Here we do the Heteroskedasticity test

cat("\nBreusch–Pagan test for heteroskedasticity:\n")
print(bptest(lm_base))

# Robust standard errors (HC1)

cat("\nRobust standard errors (HC1):\n")
print(coeftest(lm_base, vcov = vcovHC(lm_base, type = "HC1")))

# We check for clustered standard errors (borough level)

cat("\nClustered standard errors (by borough):\n")
print(coeftest(lm_base, vcov = vcovCL(lm_base, cluster = ~ borough)))

# Here we perform a Multicollinearity check

cat("\nVariance Inflation Factors (VIF):\n")
print(vif(lm_base))


# Functional form check 

cat("\nFunctional form check (fitted vs residuals)...\n")

plot(
  fitted(lm_base),
  resid(lm_base),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs fitted"
)
abline(h = 0, col = "red")

cat("\nDiagnostics complete.\n")


pdf_path <- "diagnostic_plots_imputed_dataset.pdf"

pdf(pdf_path, width = 10, height = 8)

## Bed and Bath Graphs for the appendix

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(scales)
})

options(scipen = 999)
Sys.setenv("TZ" = "Europe/London")


out_dir <- "plots_winsorised_123"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

dt <- fread("kaggle_cpih_winsorized.csv", showProgress = TRUE)

need_cols <- c("saleEstimate_currentPrice", "bedrooms", "bathrooms")
missing <- setdiff(need_cols, names(dt))
stopifnot(length(missing) == 0)

dt <- dt[
  !is.na(saleEstimate_currentPrice) &
    !is.na(bedrooms) &
    !is.na(bathrooms)
]

dt[, bedrooms := as.integer(bedrooms)]
dt[, bathrooms := as.integer(bathrooms)]
dt[, saleEstimate_currentPrice := as.numeric(saleEstimate_currentPrice)]

dt <- dt[saleEstimate_currentPrice > 0]

money_gbp <- label_number(prefix = "£", big.mark = ",", accuracy = 1)

# Helper Function

mean_by_discrete <- function(dt_in, var_name, label_name) {
  tmp <- dt_in[
    !is.na(get(var_name)),
    .(
      mean_price = mean(saleEstimate_currentPrice, na.rm = TRUE),
      n = .N
    ),
    by = .(level = get(var_name))
  ][order(level)]
  
  ggplot(tmp, aes(x = factor(level), y = mean_price, fill = factor(level))) +
    geom_col(alpha = 0.92) +
    geom_text(
      aes(label = paste0("n = ", format(n, big.mark = ","))),
      vjust = -0.35, size = 3.6, colour = "grey15"
    ) +
    scale_y_continuous(labels = money_gbp, expand = expansion(mult = c(0, 0.12))) +
    scale_fill_brewer(palette = "Spectral") +
    labs(
      title = paste0("Mean price by ", label_name),
      subtitle = "The n labels show sample size",
      x = label_name,
      y = "Mean sale estimate current price",
      fill = label_name
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
}

# Bedrooms plot 
p_bed <- mean_by_discrete(dt, "bedrooms", "Bedrooms")

ggsave(
  filename = file.path(out_dir, "bed.png"),
  plot = p_bed,
  width = 8,
  height = 5,
  dpi = 200
)

# Bathroom plot 
p_bath <- mean_by_discrete(dt, "bathrooms", "Bathrooms")

ggsave(
  filename = file.path(out_dir, "bath.png"),
  plot = p_bath,
  width = 8,
  height = 5,
  dpi = 200
)


