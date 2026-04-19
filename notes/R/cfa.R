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

## ---- cfa-1factor ----
model_1f <- "
  SOP =~ SOP_1 + SOP_2 + SOP_3 + SOP_4 + SOP_5
"
fit_1f <- cfa(model_1f, data = sop_data,
              std.lv    = TRUE,
              estimator = "MLR")
summary(fit_1f, fit.measures = TRUE, standardized = TRUE)

## ---- cfa-1factor-plot ----
lavaangui::plot_lavaan(fit_1f)

## ---- mod-indices ----
modindices(fit_1f, sort. = TRUE, minimum.value = 5)

## ---- cor-residuals ----
res_cor <- lavResiduals(fit_1f, type = "cor")$cov
nv_res  <- nrow(res_cor)
bg_res  <- rgb(as.integer(res_cor > 0), 0, as.integer(res_cor < 0),
               alpha = abs(res_cor))
bg_res[upper.tri(res_cor, diag = TRUE) | abs(res_cor) <= 0.1] <- NA
res_df <- as.data.frame(round(res_cor, 3))
tinytable::tt(cbind(" " = rownames(res_df), res_df)) |>
  tinytable::style_tt(i = 1:nv_res, j = 1:nv_res + 1, background = bg_res)

## ---- fit-indices ----
lavaan::fitmeasures(fit_1f,
  c("cfi.robust", "tli.robust", "rmsea.robust",
    "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
    "srmr"))

## ---- fit-summary ----
glance_custom.lavaan <- function(x, ...) {
  m <- lavaan::fitmeasures(x)
  tibble::tibble(
    cfi.robust   = m[["cfi.robust"]],
    tli.robust   = m[["tli.robust"]],
    rmsea.robust = m[["rmsea.robust"]]
  )
}

gm <- data.frame(
  raw   = c("nobs", "statistic", "chi2.df", "p.chi2",
            "cfi.robust", "tli.robust", "rmsea.robust", "srmr"),
  clean = c("N", "Chisq", "df", "p(Chisq)",
            "CFI*", "TLI*", "RMSEA*", "SRMR"),
  fmt   = c(0, 2, 0, 3, 3, 3, 3, 3)
)
modelsummary::msummary(
    list("1-Factor" = fit_1f),
    fmt = fmt_decimal(digits = 2),
    estimate = "{estimate} [{conf.low}, {conf.high}]",
    statistic = NULL,
    metrics = "all",
    gof_map = gm
)

## ---- composite-reliability ----
semTools::compRelSEM(fit_1f)
