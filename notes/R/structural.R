library(ggdag)
library(ggplot2)
library(lavaan)
library(semPlot)
library(semptools)

source(here::here("notes", "R", "sem_table.R"))

# Data from Williams & Edwards (2021) — OSF project: https://osf.io/m5dy6/
# Data download: https://osf.io/download/6mdb4/
# Exclusion criteria follow the preregistered analysis script.
# Data loading and exploration chunks are in data-exploration.R.

source(here::here("notes", "R", "data-exploration.R"))

## ---- fig-dag ----
dag <- dagify(
    NGSE ~ SOP,
    PASS ~ NGSE + SOP,
    coords = list(
        x = c(SOP = 0, NGSE = 1, PASS = 2),
        y = c(SOP = 0, NGSE = 1, PASS = 0)
    )
)
ggdag(dag) +
    annotate("text", x = 0.38, y = 0.60,  label = "a",  fontface = "italic", size = 5) +
    annotate("text", x = 1.62, y = 0.60,  label = "b",  fontface = "italic", size = 5) +
    annotate("text", x = 1.00, y = -0.12, label = "c'", fontface = "italic", size = 5) +
    theme_dag()

## ---- we-cfa ----
model_cfa_we <- "
  SOP  =~ SOP_1 + SOP_2 + SOP_3 + SOP_4 + SOP_5
  NGSE =~ NGSE_1 + NGSE_2 + NGSE_3 + NGSE_4 +
          NGSE_5 + NGSE_6 + NGSE_7 + NGSE_8
  PASS =~ Pasgn + Pexam + Pread + Pgen
"
fit_cfa_we <- cfa(
    model_cfa_we,
    data = we_data,
    std.lv = TRUE,
    estimator = "MLR",
    missing = "FIML"
)
summary(fit_cfa_we, fit.measures = TRUE, standardized = TRUE)

## ---- we-sr ----
model_sr_we <- "
  # Measurement model
  SOP  =~ SOP_1 + SOP_2 + SOP_3 + SOP_4 + SOP_5
  NGSE =~ NGSE_1 + NGSE_2 + NGSE_3 + NGSE_4 +
          NGSE_5 + NGSE_6 + NGSE_7 + NGSE_8
  PASS =~ Pasgn + Pexam + Pread + Pgen

  # Structural model (mediation)
  NGSE ~ a * SOP
  PASS ~ b * NGSE + c * SOP

  # Defined parameters
  indirect := a * b
  total    := c + a * b
"
fit_sr_we <- sem(
    model_sr_we,
    data = we_data,
    std.lv = TRUE,
    estimator = "MLR",
    missing = "FIML"
)
summary(fit_sr_we, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

## ---- we-indirect ----
semTools::monteCarloCI(fit_sr_we, nRep = 20000)

## ---- fig-mediation-assumptions-dag ----
dag_u <- dagify(
    NGSE ~ SOP + U,
    PASS ~ NGSE + SOP + U,
    latent = "U",
    coords = list(
        x = c(SOP = 0, NGSE = 1, PASS = 2, U = 1.5),
        y = c(SOP = 0, NGSE = 1, PASS = 0, U = 1.8)
    )
)
ggdag(dag_u) + theme_dag()

## ---- tbl-sr-estimates ----
sem_table(fit_sr_we, caption = "Parameter Estimates for the Structural Regression Model")

## ---- fig-sr-plot ----
lavaangui::plot_lavaan(fit_sr_we)

## ---- fig-sr-sempaths ----
p <- semPaths(
    fit_sr_we,
    whatLabels = "std",
    style = "ram",
    layout = "tree2",
    rotation = 2,
    nCharNodes = 0,
    sizeMan = 5,
    sizeLat = 9,
    edge.label.cex = 0.7,
    curvePivot = TRUE,
    fade = FALSE,
    DoNotPlot = TRUE
)

factor_layout <- matrix(
    c(NA, "NGSE", NA,
      "SOP", NA, "PASS"),
    nrow = 2, byrow = TRUE
)

indicator_push <- c(
    SOP = 1.5,
    NGSE = 1.5,
    PASS = 1.5
)
loading_position <- c(NGSE = 0.7)

p2 <- set_sem_layout(
    p,
    indicator_push  = indicator_push,
    loading_position = loading_position,
    factor_layout    = factor_layout,
    factor_point_to  = c(SOP = "left", NGSE = "up", PASS = "right"),
    indicator_spread = c(SOP = 1.4, NGSE = 2.5, PASS = 1.2)
)

p2 <- mark_sig(p2, fit_sr_we)
plot(p2)
