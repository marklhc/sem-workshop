library(lavaan)
library(semPlot)
library(semptools)
asags <- read.csv("https://osf.io/download/sy2d4/")

## ---- cfa-3factor ----
cfa_model <- "
  MGA  =~ ga1 + ga4 + ga7 + ga10 + ga13
  SRGA =~ ga2 + ga5 + ga8 + ga11
  NGA  =~ ga3 + ga6 + ga9 + ga12
"

fit_cfa <- cfa(cfa_model, data = asags,
               std.lv    = TRUE,
               estimator = "MLR")

summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

## ---- cfa-3factor-plot ----
p <- semPaths(fit_cfa, whatLabels = "std", edge.label.cex = 0.8,
              style = "ram", nCharNodes = 0, mar = c(5, 1, 12, 1))

## ---- cfa-3factor-plot-styled ----
std_fit <- standardizedSolution(fit_cfa)
names(std_fit)[names(std_fit) == "est.std"] <- "est"
p |>
  mark_se(ests = std_fit, sep = "\n") |>
  set_cfa_layout(fcov_curve = 2.75, loading_position = .7) |>
  plot()
