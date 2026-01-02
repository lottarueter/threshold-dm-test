# ------------
# STEP 0: Load packages
# ------------
library(dplyr)
library(purrr)

rm(list = ls())

# set working directory
setwd("/Users/lotta/Library/Mobile Documents/com~apple~CloudDocs/Documents/Promotion/# 3 Clustering Inference/Code")


# ------------
# STEP 1: Load target data and explanatory variables (daily and intra-day data)
# ------------
CDS_diff <- data.frame(read.csv("CDS_data/CDS_spreads_naive_diff.csv")) %>%
  filter(Time >= "09:30:00" & Time <= "17:00:00") %>%
  select(-c(Date, Time))

# Load daily variables
daily_expl <- read.csv("daily_expl/daily_expl.csv") %>%
  select(-Date)

# Load intra-day variables
eustoxx_30m <- read.csv("intra-day_expl/eustoxx_30m.csv") %>%
  select(-c(Date, Time))


# Function to compute summary statistics for one vector
summ_fun <- function(x) {
  out <- tibble(
    Mean   = mean(x, na.rm = TRUE),
    SD     = sd(x, na.rm = TRUE),
    Min    = min(x, na.rm = TRUE),
    Q1     = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Q3     = quantile(x, 0.75, na.rm = TRUE),
    Max    = max(x, na.rm = TRUE),
    N      = length(x),
  )
  
  out %>%
    mutate(across(
      where(is.numeric),
      ~ sprintf("%.3f", .x)
    ))
}

# Function for a full data frame
summ_df <- function(df) {
  map_dfr(df, summ_fun, .id = "Variable")
}

# Apply to each of your datasets
summary_CDS_diff   <- summ_df(CDS_diff)
summary_eustoxx_30m <- summ_df(eustoxx_30m)
summary_daily_expl <- summ_df(daily_expl)

# View
summary_table <- rbind(summary_CDS_diff, summary_eustoxx_30m, summary_daily_expl)
write.csv(summary_table, "summary_table.csv")
