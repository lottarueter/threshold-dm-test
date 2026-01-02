# ------------
# STEP 0: Load packages
# ------------
library(lubridate) # for time-date formats
library(ggplot2)
library(tidyr)
library(dplyr) # for data management
library(patchwork) # for combined plots

rm(list = ls())

# set working directory
setwd("/Users/lotta/Library/Mobile Documents/com~apple~CloudDocs/Documents/Promotion/# 3 Clustering Inference/Code")

# load hac_hard_threshold_sigma
source("hac_hard_threshold_sigma.R")

# ------------
# STEP 1: Load target data and model fits
# ------------
CDS_diff <- data.frame(read.csv("CDS_data/CDS_spreads_naive_diff.csv")) %>%
  filter(Date >= "2011-01-01") %>%
  filter(Time >= "10:00:00" & Time <= "17:00:00")

# Country codes
countries <- names(CDS_diff)[3:13]

# Load all model fits
# Get all CSV files starting with "pred_" in the "predictions" folder
files <- list.files(path = "predictions", pattern = "^pred_.*\\.csv$", full.names = TRUE)

# Read all into a list of data frames
forecasts <- lapply(files, read.csv)

# Name each list element by its file name (without path or extension)
names(forecasts) <- gsub("\\.csv$", "", basename(files))

# ------------
# STEP 2: Perform initial checks
# ------------
# Check if Dates and Times are aligned for model predictions and true data
check_aligned <- function(forecast, truth) {
  sum(forecast[,c("Date", "Time")] != truth[,c("Date", "Time")]) == 0
}

for (model_name in names(forecasts)) {
  print(paste0("Dates and times are aligned with true data for ", model_name, ": ", check_aligned(forecasts[[model_name]], CDS_diff)))
}

# Compute forecast errors
forecast_errors <- lapply(forecasts, function(pred) { as.matrix(pred[,countries] - CDS_diff[,countries]) })

# Compare MSEs (all forecasts are mean forecasts)
MSEs <- unlist(lapply(forecast_errors, function(f_err) { mean(f_err^2) })); round(MSEs,3)

# ------------
# STEP 4: Compute d_its for all model combinations
# ------------
nms <- c("pred_lvcf", "pred_garch_CDS", "pred_linear_CDS", "pred_linear_intraday", "pred_linear_full", "pred_pvar_CDS", "pred_pvar_intraday", "pred_pvar_full",
         "pred_rf_CDS", "pred_rf_intraday", "pred_rf_full", "pred_rf_full_countries", "pred_xgb_CDS", "pred_xgb_intraday", "pred_xgb_full", "pred_xgb_full_countries")
n_nms <- length(nms)

# Initialize an empty list for results
D <- list()

# Loop over all pairs i < j
for (i in 1:(n_nms-1)) {
  for (j in (i+1):n_nms) {
    name_i <- nms[i]
    name_j <- nms[j]
    diff_name <- paste0("d_", name_i, "_", name_j)
    D[[diff_name]] <- forecast_errors[[name_i]]^2 - forecast_errors[[name_j]]^2
  }
}

# D_demeaned <- lapply(D, function(d) d-mean(d))

# ------------
# STEP 5: Compute variance estimators
# ------------
# hac_object_D <- lapply(D, hac_hard_threshold_sigma)
# save(hac_object_D, file="results/hac_object_D.RData")
load("results/hac_object_D.RData")

# hac_object_D_demeaned <- lapply(D_demeaned, hac_hard_threshold_sigma) # c = 0, C = 1, step_size = 0.05
# save(hac_object_D_demeaned, file="results/hac_object_D_demeaned.RData")
# load("results/hac_object_D_demeaned.RData")

# Compute test statistic
N <- ncol(D[[1]]); N
Tt <- nrow(D[[1]]); Tt

mean_D <- unlist(lapply(D, mean))
est_sigma_D <- unlist(lapply(hac_object_D, function(ls) sqrt(as.numeric(ls$sigma2_hat)))) * 1 / sqrt(N*Tt)

J_D <- mean_D / est_sigma_D # * sqrt(N*Tt)
p_val <- 2 * (1 - pnorm(abs(J_D)))
BF_p_val <- pmin(120 * p_val, 1)

M <- unlist(lapply(hac_object_D, function(ls) ls$M))
for(i in 1:length(hac_object_D)) {
  print(paste0("# of ignored cross-sect. dep. for models ", names(hac_object_D)[i], ": ", sum(!hac_object_D[[i]]$mask)))
}

## no time-lags, no cross-sectional dependencies
no_lags_sigma_D <- lapply(D, function(Di) hac_hard_threshold_sigma(D = Di, L=0, M = Inf))
est_no_lags_sigma_D <- unlist(lapply(no_lags_sigma_D, function(ls) sqrt(as.numeric(ls$sigma2_hat)))) * 1 / sqrt(N*Tt)

J_D_no_lags  <- mean_D / est_no_lags_sigma_D # * sqrt(N*Tt)
p_val_no_lags  <- 2 * (1 - pnorm(abs(J_D_no_lags)))
BF_p_val_no_lags <- pmin(120 * p_val_no_lags, 1)


## Newey-West (no cross-sectional dependencies)
NW_sigma_D <- lapply(D, function(Di) hac_hard_threshold_sigma(D = Di, M = Inf))
est_NW_sigma_D <- unlist(lapply(NW_sigma_D, function(ls) sqrt(as.numeric(ls$sigma2_hat)))) * 1 / sqrt(N*Tt)

J_D_NW <- mean_D / est_NW_sigma_D # * sqrt(N*Tt)
p_val_NW <- 2 * (1 - pnorm(abs(J_D_NW)))
BF_p_val_NW <- pmin(120 * p_val_NW, 1)


## Driscoll Kraay (include all cross-sectional dependencies)
DK_sigma_D <- lapply(D, function(Di) hac_hard_threshold_sigma(D = Di, M = 0))
est_DK_sigma_D <- unlist(lapply(DK_sigma_D, function(ls) sqrt(as.numeric(ls$sigma2_hat)))) * 1 / sqrt(N*Tt)

J_D_DK <- mean_D / est_DK_sigma_D # * sqrt(N*Tt)
p_val_DK <- 2 * (1 - pnorm(abs(J_D_DK)))
BF_p_val_DK <- pmin(120 * p_val_DK, 1)

test_results <- data.frame(mean_D,
                           M, est_sigma_D, J_D, p_val, BF_p_val,
                           # est_no_lags_sigma_D, J_D_no_lags, p_val_no_lags, BF_p_val_no_lags,
                           est_NW_sigma_D, J_D_NW, p_val_NW, BF_p_val_NW,
                           est_DK_sigma_D, J_D_DK, p_val_DK, BF_p_val_DK); round(test_results, 3)


print(round(test_results, 3), max=120*18)

# write.csv(round(test_results, 3), "results/test_results_all.csv")


# Consider specifications with smallest MSE per model for main body of paper
nms_selected <- c("pred_lvcf", "pred_garch_CDS", "pred_linear_intraday", "pred_pvar_intraday", "pred_rf_full_countries", "pred_xgb_full_countries")

model_combinations <- c()  # initialize empty vector

for (i in 1:(length(nms_selected)-1)) {
  for (j in (i+1):length(nms_selected)) {
    name_i <- nms_selected[i]
    name_j <- nms_selected[j]
    model_combinations <- c(model_combinations, paste0("d_", name_i, "_", name_j))
  }
}

test_results_selected <- round(test_results[model_combinations, ], 3); test_results_selected


# ------------
# STEP 6: Compute threshold-covariance estimates for y, y_hats, errors, errors^2 for comparison in visualisations
# ------------
# Compute threshold-covariance estimate for true values
hac_object_y <- hac_hard_threshold_sigma(as.matrix(CDS_diff[,3:13]))

# Compute threshold-covariance estimates for forecasts, forecast errors and squared errors
hac_object_y_hats <- lapply(nms, function(nm)
  hac_hard_threshold_sigma(as.matrix(forecasts[[nm]][, 3:13])))
hac_object_error <- lapply(nms, function(nm) 
  hac_hard_threshold_sigma(as.matrix(forecast_errors[[nm]])))
hac_object_error_squared <- lapply(nms, function(nm) 
  hac_hard_threshold_sigma((as.matrix(forecast_errors[[nm]]))^2))

hac_objects <- do.call(c, list(list(hac_object_y), hac_object_y_hats, hac_object_error)) #, hac_object_error_squared))
names(hac_objects) <- c("y", paste0("y_hat_", nms),  paste0("error_", nms)) #, paste0("error_sq_", models_M_pos))

save(hac_objects, file="results/hac_object_y_y_hat_errors_errors_squared.RData")
# load("results/hac_object_y_y_hat_errors_errors_squared.RData")
