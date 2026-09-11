# London House Price Prediction

Quantifying the factors that drive London residential property prices. Starting from ~418,000 raw transaction records, I built a clean panel of ~108,000 unique properties enriched with inflation, crime, and geospatial amenity data, then compared four modelling approaches — OLS, GAM, Random Forest, and Gradient Boosting (XGBoost) — for predicting log(sale price).

Full write-up: [`ST309_Project.pdf`](./ST309_Project.pdf)

## Motivation

London is one of the world's most expensive housing markets, and understanding what drives price variation matters for both economic research and policy (e.g. debates over building on the green belt). This project uses a hedonic pricing framework — decomposing price into implicit prices for individual attributes — combined with modern statistical learning to compare linear and non-linear representations of price formation.

## Data

- **Source:** [Kaggle London house price dataset](https://www.kaggle.com/datasets/jakewright/house-price-data), UK CPIH index (ONS), Metropolitan Police borough-level crime data, [London Boroughs boundary file](https://github.com/LingruFeng/GIS_assessment/raw/main/London_Boroughs.gpkg), OpenStreetMap (via `osmextract`)
- **Deduplication:** ~418,000 raw transaction records reduced to ~137,000 unique properties by keeping only the latest sale per property (no renovation history was available, so tracking price appreciation without controlling for changes to the property wasn't meaningful)
- **Filtering:** dropped records missing a current price estimate or tenure, ~80 properties outside London boroughs, and under-1,000 "shared ownership" properties with atypical sale dynamics
- **Two parallel cleaned datasets**, tested against each other with near-identical results:
  - `kaggle_cpih_winsorized.csv` — drops any property missing bedrooms, bathrooms, living rooms, or floor area (~95,000 rows). Used for initial exploration and diagnostic plots, since it carries fewer assumptions.
  - `Imputed.csv` — additionally imputes any one of those four fields when the other three are present, using similar nearby properties (~108,632 rows, the modelling sample). This added ~13,000 extra usable observations.
- Winsorised (top/bottom 1%) and screened for high-influence observations (Cook's distance, 99.9% threshold) before modelling.
- Rent estimate was deliberately excluded as a regressor — it's mechanically derived from the sale price and would introduce endogeneity.

## Key feature engineering

- **Postcode location premium (`postcode_lp`):** for each postcode, the average log CPIH-adjusted historical price, regularised toward the London-wide mean via **empirical Bayes shrinkage** — postcodes with few transactions are pulled toward the global mean to avoid unstable estimates from sparse data. This captures persistent spatial desirability (reputation, connectivity, long-run demand) without leaking current-period information into the model.
- **Amenity features:** distance to and density of shops, schools, tube stations, hospitals, parks, and police stations (via OpenStreetMap), log-transformed to reflect diminishing marginal effects.
- **Crime:** borough-level crime intensity (24-month window), log-transformed.
- All monetary variables and the target (`saleEstimate_currentPrice`) are modelled in logs to stabilise variance and allow approximately percentage-based interpretation of coefficients.

## Models & validation

All four models are trained on the same cleaned sample with an identical 80/20 train/test split and evaluated with 5-fold cross-validation on the training set plus held-out test RMSE/R².

| Model | Test R² | Test RMSE (log) | Classification AUC (top 5% price tier) |
|---|---|---|---|
| OLS (linear) | 0.824 | 0.248 | 0.979 |
| GAM | 0.881 | 0.204 | 0.989 |
| Random Forest | 0.935 | 0.150 | 0.994 |
| Gradient Boosting (XGBoost) | 0.951 | 0.131 | 0.995 |

**OLS** — baseline linear specification; robust (HC1) standard errors used throughout to address heteroskedasticity confirmed in residual diagnostics (funnel-shaped residuals, heavy-tailed Q-Q plot). VIF confirmed multicollinearity was not a concern (all values comfortably below 5, most below 2).

**GAM** — relaxes linearity via penalised smooth splines per predictor, revealing genuine curvature (e.g. diminishing returns to floor area, threshold effects for bathrooms) without manually specifying interactions.

**Random Forest** (`ranger`, 400 trees) — captures interactions the parametric models can't (e.g. bedroom value depending on floor area), at the cost of interpretability.

**Gradient Boosting** (`xgboost`) — best overall predictive performance; feature importance (gain) is heavily concentrated in `postcode_lp` (~54%) and floor area (~25%), with crime, transport accessibility, and bathrooms contributing the remainder.

**Classification framing:** prices were also binarised (top 5% by log price in the training set) to compare models on identifying the high-end market via ROC/AUC — all four models discriminate well (AUC 0.98–0.995), with tree-based methods slightly ahead.

## Headline findings

- **Location dominates.** The postcode premium is the strongest predictor across every model (OLS coefficient of 1.79 on log postcode premium; ~54% of XGBoost's predictive gain).
- **Bathrooms matter more than bedrooms** once floor area is held constant — likely because floor area already absorbs much of what an extra bedroom would explain, and bathroom counts more reliably signal quality (plumbing requirements) than bedroom counts (a looser, more subjective classification).
- **Leasehold properties sell at a discount** (~9–18% depending on model), consistent with finite ownership horizons and ground rent.
- **Crime has a counterintuitive positive coefficient**, likely reflecting that borough-level crime is dominated by petty crime concentrated in wealthier, denser areas (pickpocketing) rather than a genuine price-increasing effect of crime itself.
- **Non-linear models add predictive power but reinforce the same economic story** as OLS — functional form matters more for interpretation (capturing diminishing returns, thresholds, interactions) than for the headline conclusion that location and scale dominate.

## Repository contents

| File | Description |
|---|---|
| `Cleaned_Winsorized_1.R` | Builds `kaggle_cpih_winsorized.csv` (complete-case dataset). |
| `Imputed_2.R` | Builds `Imputed.csv` (imputed dataset used for modelling). |
| `Diagnostics___postcode_lp_3.R` | Fits a GAM smooth of price against the postcode premium; produces the location-premium plot. |
| `Linear___GAM_4.R` | Fits OLS and GAM models; robust SEs, VIF, 5-fold CV. |
| `Random_Forest__Ranger__5.R` | Fits the random forest model. |
| `GBM_6.R` | Fits the XGBoost model; feature importance plot. |
| `Table_ROC_Graph_CV_8.R` | Final model comparison — CV table, counterfactual effects, ROC/AUC plot. |
| `ST309_Project.pdf` | Full written report with methodology, results, and references. |

## Limitations & possible future work

- No property-level quality data (condition, refurbishment, energy rating, floor level) — likely the largest source of remaining within-postcode price variation
- No formal variable selection (e.g. LASSO) for the linear model, and no systematic hyperparameter tuning or sensitivity analysis on regularisation strength for GAM/XGBoost
- Crime data aggregated at borough level rather than a more localised measure
- Likely underrepresents council housing, which is largely unavailable outside the Right to Buy scheme
- Finer spatial methods (e.g. explicit spatial autocorrelation modelling) could capture street-level variation not picked up by the postcode premium

## Tech stack

R, `data.table`, `sf`, `osmextract`, `mgcv`, `ranger`, `xgboost`, `caret`, `lmtest`, `sandwich`, `car`, `pROC`, `ggplot2`

## Requirements

Raw input files (not included in this repo) expected in the working directory:
- `kaggle_london_house_price_data.csv`
- `MPS Borough Level Crime (most recent 24 months) (1).csv`
- `CPIH_Index.csv`

Run `Cleaned_Winsorized_1.R` and `Imputed_2.R` first to generate the processed datasets, then run the modelling scripts.
