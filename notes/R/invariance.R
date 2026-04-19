library(lavaan)
library(lavaangui)
library(semTools)
library(modelsummary)
library(tinytable)
library(dplyr)

# Data from Williams & Edwards (2021) — OSF project: https://osf.io/m5dy6/
# Data download: https://osf.io/download/6mdb4/
# Exclusion criteria follow the preregistered analysis script.
# Data loading and exploration chunks are in data-exploration.R.

## ---- setup-invariance ----
sop_data_mg <- cbind(
    sop_data,
    gender = factor(we_raw$Gender, levels = 1:3,
                    labels = c("male", "female", "nonbinary"))
)
# Only use males and females as sample size is small for nonbinary group
sop_data_mg <- sop_data_mg[sop_data_mg$gender %in% c("male", "female"), ]

## ---- configural ----
model_1f <- "
  SOP =~ SOP_1 + SOP_2 + SOP_3 + SOP_4 + SOP_5
  SOP_1 ~~ SOP_2
"
fit_config <- cfa(
    model_1f,
    data = sop_data_mg,
    std.lv = TRUE,
    estimator = "MLR",
    missing = "FIML",
    group = "gender"
)

## ---- metric ----
fit_metric <- cfa(
    model_1f,
    data = sop_data_mg,
    std.lv = TRUE,
    estimator = "MLR",
    missing = "FIML",
    group = "gender",
    group.equal = "loadings"
)

## ---- scalar ----
fit_scalar <- cfa(
    model_1f,
    data = sop_data_mg,
    std.lv = TRUE,
    estimator = "MLR",
    missing = "FIML",
    group = "gender",
    group.equal = c("loadings", "intercepts")
)

## ---- strict ----
fit_strict <- cfa(
    model_1f,
    data = sop_data_mg,
    std.lv = TRUE,
    estimator = "MLR",
    missing = "FIML",
    group = "gender",
    group.equal = c("loadings", "intercepts", "residuals",
                    "residual.covariances")
)

## ---- compare-invariance ----
lavaan::lavTestLRT(fit_config, fit_metric, fit_scalar, fit_strict)

