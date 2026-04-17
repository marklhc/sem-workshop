library(modelsummary)
library(tinytable)
library(psych)
library(dplyr)

# Data from Williams & Edwards (2021) — OSF project: https://osf.io/m5dy6/
# Data download: https://osf.io/download/6mdb4/
# Exclusion criteria follow the preregistered analysis script.

## ---- load-data ----
we_path <- here::here("data", "we_data.csv")
if (!file.exists(we_path)) {
    download.file("https://osf.io/download/6mdb4/", we_path)
}
we_raw <- read.csv(we_path)
we_raw <- dplyr::rename(we_raw, Att_check = SOP_6)
# Exclude: under-18, non-students, >6 missing study items, failed attention check, duplicates
we_vars <- c(
    paste0("PASS_", c(1:9, 16:18)),
    paste0("NGSE_", 1:8),
    paste0("SOP_",  1:5)
)
we_raw <- we_raw[is.na(we_raw$Age) | we_raw$Age != 1, ]
we_raw <- we_raw[is.na(we_raw$Student) | we_raw$Student == 1, ]
we_raw$nmiss <- rowSums(is.na(we_raw[, we_vars]))
we_raw <- we_raw[we_raw$nmiss <= 6, ]
we_raw <- we_raw[!is.na(we_raw$Att_check) & we_raw$Att_check == 5, ]
we_raw <- we_raw[!duplicated(we_raw$PID_deID), ]

## ---- load-data-sop ----
sop_data <- we_raw[, paste0("SOP_", 1:5)]
nrow(sop_data)

## ---- descriptives ----
modelsummary::datasummary_skim(sop_data)

## ---- cor-heatmap ----
nv <- ncol(sop_data)
cor_sop <- cor(sop_data, use = "pairwise.complete.obs")
bg <- rgb(as.integer(cor_sop > 0), 0, as.integer(cor_sop < 0),
          alpha = abs(cor_sop))
bg[upper.tri(cor_sop, diag = TRUE)] <- NA
modelsummary::datasummary_correlation(sop_data) |>
  tinytable::style_tt(i = 1:nv, j = 1:nv + 1, background = bg)

## ---- fig-scree ----
psych::fa.parallel(sop_data, n.iter = 100,
                   main = "Parallel Analysis — SOP Items")

## ---- eigenvalues ----
ev <- eigen(cor_sop, only.values = TRUE)$values
data.frame(
  Factor        = seq_along(ev),
  Eigenvalue    = round(ev, 3),
  Cumulative_var = round(cumsum(ev) / sum(ev) * 100, 1)
)

## ---- we-parcels ----
we_raw$Pasgn <- rowSums(we_raw[, paste0("PASS_", 1:3)])
we_raw$Pexam <- rowSums(we_raw[, paste0("PASS_", 4:6)])
we_raw$Pread <- rowSums(we_raw[, paste0("PASS_", 7:9)])
we_raw$Pgen  <- rowSums(we_raw[, paste0("PASS_", 16:18)])

we_data <- we_raw[, c(
    paste0("SOP_",  1:5),
    paste0("NGSE_", 1:8),
    "Pasgn", "Pexam", "Pread", "Pgen"
)]

## ---- we-descriptives ----
modelsummary::datasummary_skim(we_data)

## ---- we-cor-heatmap ----
nv_we  <- ncol(we_data)
cor_we <- cor(we_data, use = "pairwise.complete.obs")
bg_we  <- rgb(as.integer(cor_we > 0), 0, as.integer(cor_we < 0),
              alpha = abs(cor_we))
bg_we[upper.tri(cor_we, diag = TRUE)] <- NA
modelsummary::datasummary_correlation(we_data) |>
  tinytable::style_tt(i = 1:nv_we, j = 1:nv_we + 1, background = bg_we)
