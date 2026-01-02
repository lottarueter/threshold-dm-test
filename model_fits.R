# ------------
# STEP 0: Load packages
# ------------
library(lubridate)
library(systemfit)
library(dplyr)
library(rugarch)
library(ranger)
library(MultivariateRandomForest)
library(xgboost)

rm(list = ls())

# set working directory
setwd("/Users/lotta/Library/Mobile Documents/com~apple~CloudDocs/Documents/Promotion/# 3 Clustering Inference/Code")


# ------------
# STEP 1: Load target data and explanatory variables (daily and intra-day data)
# ------------
CDS_diff <- data.frame(read.csv("CDS_data/CDS_spreads_naive_diff.csv")) 

# Country codes
countries <- names(CDS_diff)[3:13]

# Load daily variables
daily_expl <- read.csv("daily_expl/daily_expl.csv")

# Load intra-day variables
eustoxx_30m <- read.csv("intra-day_expl/eustoxx_30m.csv")


# ------------
# STEP 2: Compute model predictions for 6 different approaches
# ------------
# a) Last-value carried forward
# b) Linear model, univariate
# c) GARCH-X, univariate
# d) PVAR-X, multivariate, linear
# e) XGBoost, multivariate, nonlinear

# Construct full data set with respective intra-day lags & daily regressors
# We use lag-one intra-day predictors & daily regressors of the most recent previously observed day
CDS_regressors <- CDS_diff %>% filter(Time >= "09:30:00" & Time <= "16:30:00")
names(CDS_regressors)[3:13] <- paste0(names(CDS_regressors)[3:13], "_lag")
intraday_predictors <- merge(CDS_regressors, eustoxx_30m, all = TRUE)

daily_rep <- daily_expl %>%
  dplyr::slice(rep(1:n(), each = 15))

CDS_all <- cbind(CDS_diff %>% filter(Time >= "10:00:00" & Time <= "17:00:00"), intraday_predictors[,3:15], daily_rep[,2:9]) %>%
  mutate(DateTime = ymd_hms(paste(Date, Time), tz = "UTC")) %>%
  arrange(DateTime)

# Define variable sets
Y_vars    <- countries
Y_lag_vars <- paste0(countries, "_lag")

# Common exogenous controls
W_intraday <- c("ret_30m","realized_vola")
W_daily <- c("funding_spread","curve_slope",
             "fx_gbp_ret","fx_chf_ret","fx_nok_ret","vix_diff","evz_diff","itrx_nonfin_diff")

rm(CDS_diff, CDS_regressors, daily_expl, daily_rep, eustoxx_30m, intraday_predictors)


# ------------
# a) Compute naive last-value carried forward forecast
# ------------
pred_lvcf <- CDS_all %>% dplyr::select(all_of(c("Date", "Time", paste0(countries, "_lag")))) %>%
  filter(Date >= "2011-01-01")
names(pred_lvcf)[3:13] <- countries

write.csv(pred_lvcf, "predictions/pred_lvcf.csv", row.names = FALSE)


# ------------
# b) Compute linear model (univariate, for each country separately)
# Similar to baseline in Blommestein et al. (JFS, 2016)
# ------------

# Initialize final storage
all_preds_linear_CDS <- all_preds_linear_intraday <- all_preds_linear_full <- list()
# all_preds_linear_full_countries <- list()
all_models_linear_CDS <- all_models_linear_intraday <- all_models_linear_full <- list()
# all_models_linear_full_countries <- list()

# Loop over time windows
for (train_start in seq(as.Date("2009-01-01"), as.Date("2012-12-01"), by = "month")) {
  
  train_start <- as.Date(train_start)
  train_end <- as.Date(train_start) %m+% months(24) - 1
  pred_start <- as.Date(train_end) + 1
  pred_end <- as.Date(pred_start) %m+% months(1) - 1
  
  data_train <- CDS_all %>% filter(Date >= train_start & Date <= train_end)
  data_pred  <- CDS_all %>% filter(Date >= pred_start & Date <= pred_end)
  
  # Initialize lists for this window
  CDS_linear_CDS <- CDS_linear_intraday <- CDS_linear_full <- list()
  # CDS_linear_full_countries <- list()
  pred_linear_CDS <- pred_linear_intraday <- pred_linear_full <- list() 
  # pred_linear_full_countries <- list()
  
  # Loop over countries
  for (i in seq_along(countries)) {
    dep_var <- countries[i]
    lag_var <- paste0(dep_var, "_lag")
    
    # Build model formulas
    formula_CDS <- as.formula(paste(dep_var, "~", lag_var))
    formula_intraday <- as.formula(paste(dep_var, "~", paste(c(lag_var, W_intraday), collapse = " + ")))
    formula_full <- as.formula(paste(dep_var, "~", paste(c(lag_var, W_intraday, W_daily), collapse = " + ")))
    # formula_full_countries <- as.formula(paste(dep_var, "~", paste(c(Y_lag_vars, W_intraday, W_daily), collapse = " + ")))
    
    # Train models
    CDS_linear_CDS[[dep_var]] <- lm(formula_CDS, data = data_train)
    CDS_linear_intraday[[dep_var]] <- lm(formula_intraday, data = data_train)
    CDS_linear_full[[dep_var]] <- lm(formula_full, data = data_train)
    # CDS_linear_full_countries[[dep_var]] <- lm(formula_full_countries, data = data_train)
    
    # Predict
    pred_linear_CDS[[dep_var]] <- predict(CDS_linear_CDS[[dep_var]], data_pred)
    pred_linear_intraday[[dep_var]] <- predict(CDS_linear_intraday[[dep_var]], data_pred)
    pred_linear_full[[dep_var]] <- predict(CDS_linear_full[[dep_var]], data_pred)
    # pred_linear_full_countries[[dep_var]] <- predict(CDS_linear_full_countries[[dep_var]], data_pred)
    
  }
  
  # Combine predictions into data frames
  pred_linear_CDS_df <- data.frame("Date" = data_pred$Date, "Time" = data_pred$Time,
                                   do.call(cbind, pred_linear_CDS))
  pred_linear_intraday_df <- data.frame("Date" = data_pred$Date, "Time" = data_pred$Time,
                                        do.call(cbind, pred_linear_intraday))
  pred_linear_full_df <- data.frame("Date" = data_pred$Date, "Time" = data_pred$Time,
                                    do.call(cbind, pred_linear_full))
  # pred_linear_full_countries_df <- data.frame("Date" = data_pred$Date, "Time" = data_pred$Time,
  #                                   do.call(cbind, pred_linear_full_countries))
  
  # Store results by window (using window start as name)
  window_name <- format(train_start, "%Y-%m")
  
  all_models_linear_CDS[[window_name]] <- CDS_linear_CDS
  all_models_linear_intraday[[window_name]] <- CDS_linear_intraday
  all_models_linear_full[[window_name]] <- CDS_linear_full
  # all_models_linear_full_countries[[window_name]] <- CDS_linear_full_countries
  
  all_preds_linear_CDS[[window_name]] <- pred_linear_CDS_df
  all_preds_linear_intraday[[window_name]] <- pred_linear_intraday_df
  all_preds_linear_full[[window_name]] <- pred_linear_full_df
  # all_preds_linear_full_countries[[window_name]] <- pred_linear_full_countries_df
  
  cat("Finished window:", window_name, "\n")
}

# Combine all predictions into one data frame per model type
pred_linear_CDS_df <- bind_rows(all_preds_linear_CDS)
pred_linear_intraday_df <- bind_rows(all_preds_linear_intraday)
pred_linear_full_df <- bind_rows(all_preds_linear_full)
# pred_linear_full_countries_df <- bind_rows(all_preds_linear_full_countries)

# Save to CSV
write.csv(pred_linear_CDS_df, "predictions/pred_linear_CDS.csv", row.names = FALSE)
write.csv(pred_linear_intraday_df, "predictions/pred_linear_intraday.csv", row.names = FALSE)
write.csv(pred_linear_full_df, "predictions/pred_linear_full.csv", row.names = FALSE)
# write.csv(pred_linear_full_countries_df, "predictions/pred_linear_full_countries.csv", row.names = FALSE)


# ------------
# c) Compute GARCH (univariate, for each country separately)
# ------------
# --- Initialize storage ---
all_models_garch_CDS <- list()
all_preds_garch_CDS <- list()


# --- Rolling window estimation ---
for (train_start in seq(as.Date("2009-01-01"), as.Date("2012-12-01"), by = "month")) {
  
  train_start <- as.Date(train_start)
  train_end <- train_start %m+% months(24) - days(1)
  pred_start <- train_end + days(1)
  pred_end <- pred_start %m+% months(1) - days(1)
  
  data_train <- CDS_all %>% filter(Date >= train_start & Date <= train_end)
  data_pred <- CDS_all %>% filter(Date >= pred_start & Date <= pred_end)
  
  preds_window <- data.frame(Date = data_pred$Date, Time = data_pred$Time)
  models_window <- list()
  
  # ----------------------------------------------------
  # Fit GARCH(1,1) using provided lagged variables
  # ----------------------------------------------------
  for (dep_var in countries) {
    
    # Dependent variable
    y_train <- data_train[[dep_var]]
    
    # Predictor (the lag already exists in CDS_all)
    lag_var <- paste0(dep_var, "_lag")
    x_train <- data_train[[lag_var]]
    
    # Fit GARCH(1,1) model to y_train using provided lagged regressor
    spec_lagged <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, external.regressors = matrix(x_train, ncol = 1)),
      distribution.model = "norm"
    )
    
    fit <- tryCatch({
      ugarchfit(spec = spec_lagged, data = y_train, solver = "hybrid")
    }, error = function(e) NULL)
    
    if (is.null(fit)) {
      preds_window[[dep_var]] <- NA
      next
    }
    
    # Forecast using external regressor for prediction sample
    x_pred <- matrix(data_pred[[lag_var]], ncol = 1)
    
    fc <- tryCatch({
      ugarchforecast(fit, n.ahead = 1, n.roll = 0, external.forecasts = list(mregfor = x_pred))
    }, error = function(e) NULL)
    
    if (is.null(fc)) {
      preds_window[[dep_var]] <- NA
      next
    }
    
    preds_window[[dep_var]] <- as.numeric(fitted(fc))  # mean forecast
    models_window[[dep_var]] <- fit
  }
  
  # ----------------------------------------------------
  # Store results
  # ----------------------------------------------------
  window_name <- format(train_start, "%Y-%m")
  all_models_garch_CDS[[window_name]] <- models_window
  all_preds_garch_CDS[[window_name]] <- preds_window
  
  cat("Finished GARCH(1,1) window:", window_name, "\n")
}

# --- Combine all predictions ---
preds_garch_CDS_df <- bind_rows(all_preds_garch_CDS)

# --- Save ---
write.csv(preds_garch_CDS_df, "predictions/pred_garch_CDS.csv", row.names = FALSE)


# ------------
# d) Compute PVAR-X (multivariate)
# Similar to Galariotis et al. (JFS, 2016); also use first-order model
# We use Ters, Urban (2018) model who employ PVAR from Canova, Ciccarelli (2013)
# ------------

# --- Initialize storage ---
all_preds_pvar_CDS <- all_preds_pvar_intraday <- all_preds_pvar_full <- list()
all_models_pvar_CDS <- all_models_pvar_intraday <- all_models_pvar_full <- list()

# --- Rolling-window estimation ---
for (train_start in seq(as.Date("2009-01-01"), as.Date("2012-12-01"), by = "month")) {
  
  train_start <- as.Date(train_start)
  train_end <- as.Date(train_start) %m+% months(24) - days(1)
  pred_start <- as.Date(train_end) + days(1)
  pred_end <- as.Date(pred_start) %m+% months(1) - days(1)
  
  data_train <- CDS_all %>%
    filter(Date >= train_start & Date <= train_end)
  data_pred <- CDS_all %>%
    filter(Date >= pred_start & Date <= pred_end)
  
  # --- 1. Build system of equations (one per country) ---
  rhs_terms_CDS <- c(Y_lag_vars)
  rhs_terms_intraday <- c(Y_lag_vars, W_intraday)
  rhs_terms_full <- c(Y_lag_vars, W_intraday, W_daily)
  
  eq_list_CDS <- lapply(countries, function(cty) {
    as.formula(paste(cty, "~", paste(rhs_terms_CDS, collapse = " + ")))
  })
  eq_list_intraday <- lapply(countries, function(cty) {
    as.formula(paste(cty, "~", paste(rhs_terms_intraday, collapse = " + ")))
  })
  eq_list_full <- lapply(countries, function(cty) {
    as.formula(paste(cty, "~", paste(rhs_terms_full, collapse = " + ")))
  })
  
  names(eq_list_CDS) <- names(eq_list_intraday) <- names(eq_list_full) <- countries
  
  # --- 2. Fit PVAR (SUR) for each specification ---
  fit_pvar_CDS <- systemfit(eq_list_CDS, data = data_train, method = "SUR")
  fit_pvar_intraday <- systemfit(eq_list_intraday, data = data_train, method = "SUR")
  fit_pvar_full <- systemfit(eq_list_full, data = data_train, method = "SUR")
  
  # --- 3. Predict manually for each equation and spec ---
  predict_system <- function(fit_obj, data_pred) {
    sapply(seq_along(fit_obj$eq), function(j) {
      eq_formula <- formula(fit_obj$eq[[j]])
      rhs_form <- as.formula(paste("~", as.character(eq_formula)[3]))
      M <- model.matrix(rhs_form, data = data_pred)
      beta <- coef(fit_obj$eq[[j]])
      drop(M %*% beta)
    })
  }
  
  pred_pvar_CDS <- predict_system(fit_pvar_CDS, data_pred)
  pred_pvar_intraday <- predict_system(fit_pvar_intraday, data_pred)
  pred_pvar_full <- predict_system(fit_pvar_full, data_pred)
  
  # --- 4. Ensure predictions are numeric matrices ---
  pred_pvar_CDS <- as.data.frame(pred_pvar_CDS)
  pred_pvar_intraday <- as.data.frame(pred_pvar_intraday)
  pred_pvar_full <- as.data.frame(pred_pvar_full)
  
  colnames(pred_pvar_CDS) <- colnames(pred_pvar_intraday) <- colnames(pred_pvar_full) <- countries
  
  # --- 5. Combine with Date/Time ---
  pred_pvar_CDS_df <- cbind(data_pred[c("Date", "Time")], pred_pvar_CDS)
  pred_pvar_intraday_df <- cbind(data_pred[c("Date", "Time")], pred_pvar_intraday)
  pred_pvar_full_df <- cbind(data_pred[c("Date", "Time")], pred_pvar_full)
  
  # --- 6. Store results ---
  window_name <- format(train_start, "%Y-%m")
  
  all_models_pvar_CDS[[window_name]] <- fit_pvar_CDS
  all_models_pvar_intraday[[window_name]] <- fit_pvar_intraday
  all_models_pvar_full[[window_name]] <- fit_pvar_full
  
  all_preds_pvar_CDS[[window_name]] <- pred_pvar_CDS_df
  all_preds_pvar_intraday[[window_name]] <- pred_pvar_intraday_df
  all_preds_pvar_full[[window_name]] <- pred_pvar_full_df
  
  cat("Finished PVAR window:", window_name, "\n")
}

# --- 7. Combine all predictions into single data frames ---
pred_pvar_CDS_df <- bind_rows(all_preds_pvar_CDS)
pred_pvar_intraday_df <- bind_rows(all_preds_pvar_intraday)
pred_pvar_full_df <- bind_rows(all_preds_pvar_full)

# --- 8. Save to CSV ---
write.csv(pred_pvar_CDS_df, "predictions/pred_pvar_CDS.csv", row.names = FALSE)
write.csv(pred_pvar_intraday_df, "predictions/pred_pvar_intraday.csv", row.names = FALSE)
write.csv(pred_pvar_full_df, "predictions/pred_pvar_full.csv", row.names = FALSE)



# ------------
# e) Compute RFs
# ------------

# --- Initialize storage ---
all_preds_rf_CDS <- all_preds_rf_intraday <- all_preds_rf_full <- all_preds_rf_full_countries <- list()
all_models_rf_CDS <- all_models_rf_intraday <- all_models_rf_full <- all_models_rf_full_countries <- list()

# --- Rolling window estimation ---
for (train_start in seq(as.Date("2009-01-01"), as.Date("2011-04-01"), by = "month")) {
  
  train_start <- as.Date(train_start)
  train_end <- as.Date(train_start) %m+% months(24) - days(1)
  pred_start <- as.Date(train_end) + days(1)
  pred_end <- as.Date(pred_start) %m+% months(1) - days(1)
  
  data_train <- CDS_all %>%
    filter(Date >= train_start & Date <= train_end)
  data_pred <- CDS_all %>%
    filter(Date >= pred_start & Date <= pred_end)
  
  # Initialize lists for models and predictions for this window
  rf_model_CDS <- rf_model_intraday <- rf_model_full <- crf_model_full <- list()
  pred_rf_CDS <- pred_rf_intraday <- pred_rf_full <- pred_rf_full_countries <- list()
  
  
  for (dep_var in countries) {
    lag_var <- paste0(dep_var, "_lag")
    predictors_full <- c(Y_lag_vars, W_intraday, W_daily)
    
    # --- Formulas ---
    formula_CDS <- as.formula(paste(dep_var, "~", lag_var))
    formula_intraday <- as.formula(paste(dep_var, "~", paste(c(lag_var, W_intraday), collapse = " + ")))
    formula_full <- as.formula(paste(dep_var, "~", paste(c(lag_var, W_intraday, W_daily), collapse = " + ")))
    formula_full_countries <- as.formula(paste(dep_var, "~", paste(predictors_full, collapse = " + ")))
    
    # --- Fit Random Forests (using ranger) ---
    rf_model_CDS[[dep_var]] <- tryCatch({
      ranger(formula_CDS, data = data_train, num.trees = 500, mtry = NULL, importance = "impurity")
    }, error = function(e) NULL)
    
    rf_model_intraday[[dep_var]] <- tryCatch({
      ranger(formula_intraday, data = data_train, num.trees = 500, mtry = NULL, importance = "impurity")
    }, error = function(e) NULL)
    
    rf_model_full[[dep_var]] <- tryCatch({
      ranger(formula_full, data = data_train, num.trees = 500, mtry = NULL, importance = "impurity")
    }, error = function(e) NULL)
    
    rf_model_full_countries[[dep_var]] <- tryCatch({
      ranger(formula_full_countries, data = data_train, num.trees = 500, mtry = floor(sqrt(length(predictors_full))), min.node.size = 50, importance = "impurity")
    }, error = function(e) NULL)
    
  
    # --- Predict (if model fitted successfully) ---
    pred_rf_CDS[[dep_var]] <- if (!is.null(rf_model_CDS[[dep_var]])) {
      predict(rf_model_CDS[[dep_var]], data = data_pred)$predictions
    } else NA
    
    pred_rf_intraday[[dep_var]] <- if (!is.null(rf_model_intraday[[dep_var]])) {
      predict(rf_model_intraday[[dep_var]], data = data_pred)$predictions
    } else NA
    
    pred_rf_full[[dep_var]] <- if (!is.null(rf_model_full[[dep_var]])) {
      predict(rf_model_full[[dep_var]], data = data_pred)$predictions
    } else NA
    
    pred_rf_full_countries[[dep_var]] <- if (!is.null(rf_model_full_countries[[dep_var]])) {
      predict(rf_model_full_countries[[dep_var]], data = data_pred)$predictions
    } else NA
  }
  
  # --- Combine predictions into data frames ---
  pred_rf_CDS_df <- cbind(
    data_pred[c("Date", "Time")],
    as.data.frame(pred_rf_CDS)
  )
  pred_rf_intraday_df <- cbind(
    data_pred[c("Date", "Time")],
    as.data.frame(pred_rf_intraday)
  )
  pred_rf_full_df <- cbind(
    data_pred[c("Date", "Time")],
    as.data.frame(pred_rf_full)
  )
  
  pred_rf_full_countries_df <- cbind(
    data_pred[c("Date", "Time")],
    as.data.frame(pred_rf_full_countries)
  )
  
  # --- Store results by window ---
  window_name <- format(train_start, "%Y-%m")
  
  all_models_rf_CDS[[window_name]] <- rf_model_CDS
  all_models_rf_intraday[[window_name]] <- rf_model_intraday
  all_models_rf_full[[window_name]] <- rf_model_full
  all_models_rf_full_countries[[window_name]]   <- rf_model_full_countries

  all_preds_rf_CDS[[window_name]] <- pred_rf_CDS_df
  all_preds_rf_intraday[[window_name]] <- pred_rf_intraday_df
  all_preds_rf_full[[window_name]] <- pred_rf_full_df
  all_preds_rf_full_countries[[window_name]]    <- pred_rf_full_countries_df
  
  cat("Finished RF window:", window_name, "\n")
}

# --- Combine all predictions into single data frames ---
pred_rf_CDS_df <- bind_rows(all_preds_rf_CDS)
pred_rf_intraday_df <- bind_rows(all_preds_rf_intraday)
pred_rf_full_df <- bind_rows(all_preds_rf_full)
pred_mvrf_full_df <- bind_rows(all_preds_mvrf_full)

# --- Save to CSV ---
write.csv(pred_rf_CDS_df, "predictions/pred_rf_CDS.csv", row.names = FALSE)
write.csv(pred_rf_intraday_df, "predictions/pred_rf_intraday.csv", row.names = FALSE)
write.csv(pred_rf_full_df, "predictions/pred_rf_full.csv", row.names = FALSE)
write.csv(pred_rf_full_countries_df,   "predictions/pred_rf_full_countries.csv",   row.names = FALSE)



# ------------
# f) Compute XGBoost
# ------------

# --- Initialize storage ---
all_models_xgb_CDS <- all_models_xgb_intraday <- all_models_xgb_full <- all_models_xgb_full_countries <- list()
all_preds_xgb_CDS <- all_preds_xgb_intraday <- all_preds_xgb_full <- all_preds_xgb_full_countries <- list()

# --- Rolling window estimation ---
for (train_start in seq(as.Date("2009-01-01"), as.Date("2012-12-01"), by = "month")) {
  
  train_start <- as.Date(train_start)
  train_end   <- train_start %m+% months(24) - days(1)
  pred_start  <- train_end + days(1)
  pred_end    <- pred_start %m+% months(1) - days(1)
  
  data_train <- CDS_all %>% filter(Date >= train_start & Date <= train_end)
  data_pred  <- CDS_all %>% filter(Date >= pred_start & Date <= pred_end)
  
  preds_CDS <- data.frame(Date = data_pred$Date, Time = data_pred$Time)
  preds_intraday <- preds_full <- preds_full_countries <- preds_CDS
  
  models_CDS <- models_intraday <- models_full <- models_full_countries <- list()
  
  for (dep_var in countries) {
    lag_var <- paste0(dep_var, "_lag")
    predictors_CDS <- lag_var
    predictors_intraday <- c(lag_var, W_intraday)
    predictors_full <- c(lag_var, W_intraday, W_daily)
    predictors_full_countries <- c(Y_lag_vars, W_intraday, W_daily)
    
    model_types <- list(
      CDS = predictors_CDS,
      intraday = predictors_intraday,
      full = predictors_full,
      full_countries = predictors_full_countries
    )
    
    for (model_type in names(model_types)) {
      
      predictors <- model_types[[model_type]]
      y_train <- data_train[[dep_var]]
      X_train <- as.matrix(data_train[, predictors])
      X_pred  <- as.matrix(data_pred[, predictors])
      
      # Drop rows with NA
      valid_idx <- complete.cases(y_train, X_train)
      y_train <- y_train[valid_idx]
      X_train <- X_train[valid_idx, , drop = FALSE]
      
      # Convert to DMatrix
      dtrain <- xgb.DMatrix(data = X_train, label = y_train)
      dtest  <- xgb.DMatrix(data = X_pred)
      
      # Fit XGBoost model
      params <- list(
        objective = "reg:squarederror",
        eta = 0.05,
        max_depth = 6,
        subsample = 0.8,
        colsample_bytree = 0.8,
        min_child_weight = 5
      )
      
      model <- tryCatch({
        xgb.train(
          params = params,
          data = dtrain,
          nrounds = 300,
          verbose = 0
        )
      }, error = function(e) NULL)
      
      if (is.null(model)) {
        assign(paste0("preds_", model_type))[[dep_var]] <- NA
        next
      }
      
      # Store forecasting results
      pred_list <- get(paste0("preds_", model_type))
      pred_list[[dep_var]] <- predict(model, dtest)
      assign(paste0("preds_", model_type), pred_list)
      
      # Store model
      model_list <- get(paste0("models_", model_type))
      model_list[[dep_var]] <- model
      assign(paste0("models_", model_type), model_list)
    }
  }
  
  window_name <- format(train_start, "%Y-%m")
  
  # Store results by window
  all_models_xgb_CDS[[window_name]] <- models_CDS
  all_models_xgb_intraday[[window_name]] <- models_intraday
  all_models_xgb_full[[window_name]] <- models_full
  all_models_xgb_full_countries[[window_name]] <- models_full_countries
  
  all_preds_xgb_CDS[[window_name]] <- preds_CDS
  all_preds_xgb_intraday[[window_name]] <- preds_intraday
  all_preds_xgb_full[[window_name]] <- preds_full
  all_preds_xgb_full_countries[[window_name]] <- preds_full_countries
  
  cat("Finished XGBoost window:", window_name, "\n")
}

# --- Combine all predictions ---
preds_xgb_CDS_df <- bind_rows(all_preds_xgb_CDS)
preds_xgb_intraday_df <- bind_rows(all_preds_xgb_intraday)
preds_xgb_full_df <- bind_rows(all_preds_xgb_full)
preds_xgb_full_countries_df <- bind_rows(all_preds_xgb_full_countries)

# --- Save ---
write.csv(preds_xgb_CDS_df, "predictions/pred_xgb_CDS.csv", row.names = FALSE)
write.csv(preds_xgb_intraday_df, "predictions/pred_xgb_intraday.csv", row.names = FALSE)
write.csv(preds_xgb_full_df, "predictions/pred_xgb_full.csv", row.names = FALSE)
write.csv(preds_xgb_full_countries_df, "predictions/pred_xgb_full_countries.csv", row.names = FALSE)
