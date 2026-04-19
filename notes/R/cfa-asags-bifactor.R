library(lavaan)
library(lavaangui)
library(semTools)
library(modelsummary)
library(tinytable)

asags <- read.csv("https://osf.io/download/sy2d4/")

# fit_cfa is needed for model comparison and composite reliability
fit_cfa <- cfa(
  "MGA  =~ ga1 + ga4 + ga7 + ga10 + ga13
   SRGA =~ ga2 + ga5 + ga8 + ga11
   NGA  =~ ga3 + ga6 + ga9 + ga12",
  data = asags, std.lv = TRUE, estimator = "MLR"
)

## ---- cfa-bifactor ----
cfa_bi <- "
  GA   =~ ga1 + ga2 + ga3 + ga4 + ga5 + ga6 +
           ga7 + ga8 + ga9 + ga10 + ga11 + ga12 + ga13
  MGA  =~ ga1 + ga4 + ga7 + ga10 + ga13
  SRGA =~ ga2 + ga5 + ga8 + ga11
  NGA  =~ ga3 + ga6 + ga9 + ga12
"

fit_bi <- cfa(cfa_bi, data = asags,
              std.lv     = TRUE,
              estimator  = "MLR",
              orthogonal = TRUE)

summary(fit_bi, fit.measures = TRUE, standardized = TRUE)

## ---- cfa-bifactor-plot ----
lavaangui::plot_lavaan(fit_bi)
## ---- cor-residuals ----
res_cor <- lavResiduals(fit_bi, type = "cor")$cov
bg_res <- rgb(as.integer(res_cor > 0), 0, as.integer(res_cor < 0),
              alpha = abs(res_cor))
bg_res[upper.tri(res_cor, diag = TRUE) | abs(res_cor) <= 0.1] <- NA

nv_res <- ncol(res_cor)
res_df <- as.data.frame(round(res_cor, 3))
tt(cbind(" " = rownames(res_df), res_df)) |>
  style_tt(i = 1:nv_res, j = 1:nv_res + 1, background = bg_res)

## ---- mod-indices ----
modindices(fit_bi, sort. = TRUE, minimum.value = 10)

## ---- lrt-compare ----
lavTestLRT(fit_cfa, fit_bi)  # approximate; use AIC/BIC for non-nested comparison

## ---- fit-indices ----
fitMeasures(fit_bi, c("chisq", "df", "pvalue",
                      "cfi.robust", "tli.robust",
                      "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                      "srmr", "aic", "bic"))

## ---- fit-summary-table ----
# performance::model_performance() only extracts naive (non-robust) indices.
# glance_custom.lavaan appends the robust variants so gof_map can reference them.
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
            "cfi.robust", "tli.robust", "rmsea.robust", "srmr", "aic", "bic"),
  clean = c("N", "Chisq", "df", "p(Chisq)",
            "CFI*", "TLI*", "RMSEA*", "SRMR", "AIC", "BIC"),
  fmt   = c(0, 2, 0, 3, 3, 3, 3, 3, 1, 1)
)
modelsummary::msummary(
  list("First-order" = fit_cfa, "Bi-factor" = fit_bi),
  fmt      = fmt_decimal(digits = 2),
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL,
  metrics  = "all",
  gof_map  = gm
)

## ---- factor-scores ----
fs <- lavPredict(fit_bi)
head(fs)

## ---- fig-bi-residuals ----
resid_bi <- lavInspect(fit_bi, "data") - lavPredict(fit_bi, type = "ov")
psych::pairs.panels(resid_bi)

## ---- composite-reliability ----
semTools::compRelSEM(fit_cfa)  # first-order model (subscale reliability)
