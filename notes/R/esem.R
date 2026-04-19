library(readxl)
library(lavaan)
library(semTools)
library(modelsummary)
library(tinytable)
library(dplyr)

# Data from Alamer & Marsh (2022) — available from the IRIS Database:
# https://www.iris-database.org/details/WYMmh-Qz9eK
# Excel file saved as: data/Passion dataset S1- Alamer & Marsh 2022.xlsx

## ---- load-data ----
study1 <- readxl::read_xlsx(
  here::here("data", "Passion dataset S1- Alamer & Marsh 2022.xlsx"),
  na = "-99"
)

## ---- descriptives ----
modelsummary::datasummary_skim(dplyr::select(study1, -Gender))

## ---- cor-heatmap ----
items <- c("HP1", "HP2", "HP3", "HP4", "HP5", "HP6",
           "OP1", "OP2", "OP3", "OP4", "OP5", "OP6")
cor_s1 <- cor(study1[items], use = "pairwise.complete.obs")
bg <- rgb(as.integer(cor_s1 > 0), 0, as.integer(cor_s1 < 0),
          alpha = abs(cor_s1))
bg[upper.tri(cor_s1, diag = TRUE)] <- NA
nv <- length(items)
datasummary_correlation(study1[items]) |>
  style_tt(i = 1:nv, j = 1:nv + 1, background = bg)

## ---- cfa-passion ----
cfa_model <- "
  HP =~ HP1 + HP2 + HP3 + HP4 + HP5 + HP6
  OP =~ OP1 + OP2 + OP3 + OP4 + OP5 + OP6
  OP1 ~~ OP2   # correlated uniqueness (OP1 and OP2 share wording)
"
fit_cfa <- cfa(cfa_model, data = study1,
               std.lv    = TRUE,
               estimator = "MLR")
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

## ---- esem-passion ----
# ESEM: both factors estimated in one efa() block with geomin rotation.
# Cross-loadings are freely estimated (targeted toward zero by rotation).
esem_model <- "
  efa('esem')*HP + efa('esem')*OP =~
    HP1 + HP2 + HP3 + HP4 + HP5 + HP6 +
    OP1 + OP2 + OP3 + OP4 + OP5 + OP6
  OP1 ~~ OP2   # correlated uniqueness (OP1 and OP2)
"
target_mat <- matrix(NA, nrow = 12, ncol = 2)
target_mat[1:6, 2] <- 0  # HP items should not load on OP factor
target_mat[7:12, 1] <- 0 # OP items should not load on HP factor
fit_esem <- cfa(
    esem_model,
    data = study1,
    estimator = "MLR",
    rotation = "target",
    rotation.args = list(target = target_mat)
)
summary(fit_esem, fit.measures = TRUE, standardized = TRUE)

## ---- fit-cfa-esem ----
# Compare fit indices for CFA and ESEM
lavaan::lavTestLRT(fit_cfa, fit_esem)

## ---- fit-comparison ----
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
    list("CFA" = fit_cfa, "ESEM" = fit_esem),
    fmt = fmt_decimal(digits = 2),
    estimate = "{estimate} [{conf.low}, {conf.high}]",
    statistic = NULL,
    metrics = "all",
    gof_map = gm
)

## ---- factor-cor ----
# Factor correlations from CFA and ESEM
cfa_cor  <- lavInspect(fit_cfa,  "cor.lv")
esem_cor <- lavInspect(fit_esem, "cor.lv")
data.frame(
    Model = c("CFA", "ESEM"),
    r_HP_OP = c(round(cfa_cor["HP", "OP"], 3),
                round(esem_cor["HP", "OP"], 3))
)
