library(modelsummary)
library(tinytable)

## ---- load-data ----
asags <- read.csv("https://osf.io/download/sy2d4/")

## ---- descriptives ----
modelsummary::datasummary_skim(asags)

## ---- cor-heatmap ----
cor_asags <- cor(asags, use = "pairwise.complete.obs")
bg <- rgb(as.integer(cor_asags > 0), 0, as.integer(cor_asags < 0),
          alpha = abs(cor_asags))
bg[upper.tri(cor_asags, diag = TRUE)] <- NA

nv <- ncol(cor_asags)
asags |>
  modelsummary::datasummary_correlation() |>
  style_tt(i = 1:nv, j = 1:nv + 1, background = bg)

## ---- fig-scree ----
fa_parallel <- psych::fa.parallel(
  asags,
  n.iter = 100, show.legend = FALSE, main = ""
)

## ---- eigenvalues ----
ev <- eigen(cor_asags, only.values = TRUE)$values
data.frame(
  Factor        = seq_along(ev),
  Eigenvalue    = round(ev, 3),
  Cumulative_var = round(cumsum(ev) / sum(ev) * 100, 1)
)
