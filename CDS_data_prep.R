# ------------
# STEP 0: load packages and set working directory
# ------------
library(dplyr)       # data manipulation
library(tidyr)       # pivot_longer, pivot_wider
library(lubridate)   # date-time parsing
library(zoo)         # na.locf for forward/backward fill
library(imputeTS)    # na_kalman for Kalman filter
library(ggplot2)     # plotting
library(patchwork)   # combining plots
library(rlist)

rm(list = ls())

# set working directory
setwd("/Users/lotta/Library/Mobile Documents/com~apple~CloudDocs/Documents/Promotion/# 3 Clustering Inference/Code")

# ------------
# STEP 1: load data and perform basic (sanity) checks
# ------------
filenames <- list.files("CDS_data", pattern="*.txt", full.names=TRUE)
country_ids <- sub(".*CDS_5_([A-Z]{2})_.*", "\\1", filenames)

CDS_data <- lapply(filenames, read.delim)
names(CDS_data) <- country_ids

CDS_data <- lapply(CDS_data, function(ls)
  ls %>%
    rename(Bid_new = Time, Offer_new = Bid) %>% # correct column names
    mutate(Spread = (Bid_new + Offer_new) / 2,
           Datetime = as.POSIXct(Date, format = "%d.%m.%Y %H:%M")) %>% 
    select(!c("Offer", "Date", "Bid_new", "Offer_new")))

# Rename Spread to country name for merging of data frames
for (i in seq_along(CDS_data)) {
  names(CDS_data[[i]])[names(CDS_data[[i]]) == "Spread"] <- names(CDS_data)[i]
}

# Merge all by Date
CDS_spreads <- Reduce(function(x, y) merge(x, y, by = "Datetime", all = TRUE), CDS_data) %>%
  mutate(Date = as.Date(Datetime),
         Time = format(Datetime, "%H:%M:%S")) %>%
  arrange(Datetime)

colMeans(is.na(CDS_spreads))

# ------------
# STEP 2: Investigate data, in particular missingness
# ------------
# check correlation matrix
round(cor(CDS_spreads[,2:15], use="pairwise.complete.obs"),2)

## 2.1 Investigate overall degree of missingness
colMeans(is.na(CDS_spreads)[,2:15])

# dates_JIE <- as.POSIXct(rownames(mid.cds), "%Y-%m-%d %H:%M:%S", tz = "CET")
# CDS_spreads_JIE <- CDS_spreads %>%
#   filter(Datetime %in% dates_JIE)
# colMeans(is.na(CDS_spreads_JIE))
# data.frame(CDS_spreads_JIE$Datetime, CDS_spreads_JIE$PT, as.vector(mid.cds$PT)) # CDS spread computation is correct

# 2.1.1 ACTION: Remove data from Greece since 58.8% of missingness.
# Reason: "Reliable sovereign CDS data for Greece is only available until June 2011 due to liquidity issues;
# CDS trading for Greece ceased entirely with the restructuring in early 2012", see Buse, Schienle, Urban (JIE; 2022)
# mid.cds <- mid.cds[,-which(colnames(mid.cds)=="GR")] # former 'cds14'
# country_ids <- colnames(mid.cds)[1:6]

# ------------
## Step 2.2: Plot missingness
# ------------
# ------------
## PLOT 1: Number of missing time stamps per day for different countries
# ------------
# Convert to long format
df_long <- CDS_spreads %>%
  pivot_longer(cols = all_of(country_ids), names_to = "variable", values_to = "value") %>%
  group_by(Date, variable) %>%
  summarize(missing_count = sum(is.na(value)), .groups = "drop")  # number of missing per variable per day

# Plot heatmap
ggplot(df_long, aes(x = variable, y = Date, fill = missing_count)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("white", "lightyellow", "yellow", "orange", "red", "darkred", "black"),
    limits = c(0, max(df_long$missing_count, na.rm = TRUE)),
    name = "Missing values"
  ) +
  scale_y_date(
    date_breaks = "1 month",
    date_labels = "%Y-%m",
    expand = c(0, 0)
  ) +
  labs(x = "Variable", y = "Date") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Remove Greece, Japan and US, values before 2009 and after 2014
CDS_spreads <- CDS_spreads %>%
  select(!c("GR", "JP", "US")) %>%
  filter(Date < "2015-01-01") %>% 
  filter(Date >= "2009-01-01") 

country_ids <- country_ids[!country_ids %in% c("GR", "JP", "US")]


# ------------
## PLOT 2: Missingness vs. daily average CDS value per country over time
# ------------
# Convert to long format
daily_summary <- CDS_spreads %>%
  pivot_longer(cols = all_of(country_ids), names_to = "variable", values_to = "value") %>%
  group_by(Date, variable) %>%
  summarize(
    missing_frac = mean(is.na(value)),   # proportion missing
    avg_value    = mean(value, na.rm = TRUE),  # daily average
    .groups = "drop"
  )

# Determine scale factor per variable for dual axis
scale_factors <- daily_summary %>%
  group_by(variable) %>%
  summarize(max_avg = max(avg_value, na.rm = TRUE), .groups = "drop")

# Initialize empty list for plots
plot_list <- vector("list", length = length(unique(daily_summary$variable)))

# Loop through variables and create individual plots
for (i in seq_along(unique(daily_summary$variable))) {
  var <- unique(daily_summary$variable)[i]
  df_var <- daily_summary %>% filter(variable == var)
  scale_factor <- 1560 # scale_factors$max_avg[scale_factors$variable == var]
  
  plot_list[[i]] <- ggplot(df_var, aes(x = Date)) +
    # Missingness as points
    geom_point(aes(y = missing_frac), color = "red", size = 1.5) +
    # Average value as solid line
    geom_line(aes(y = avg_value / scale_factor), color = "blue", size = 0.8) +
    scale_y_continuous(
      name = "Proportion missing",
      limits = c(0,1),
      sec.axis = sec_axis(~ . * scale_factor, name = "Average value")
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    labs(title = var, x = "Date") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Combine 6 plots in 2x3 grid
combined_plot <- (plot_list[[1]] | plot_list[[2]] | plot_list[[3]] | plot_list[[4]]) /
  (plot_list[[5]] | plot_list[[6]] | plot_list[[7]] | plot_list[[8]]) /
  (plot_list[[9]] | plot_list[[10]] | plot_list[[11]])

# Display
combined_plot

# ------------
# CHECK correlation matrix
# ------------
round(cor(CDS_spreads[,2:12], use="pairwise.complete.obs"),2)

# ------------
## STEP 3: Remove days with  >9 missing values in at least one country
# ------------
is.NA_CDS_spreads <- data.frame(Date = CDS_spreads$Date, is.na(CDS_spreads)[,2:12])

number_NAs_per_day <- is.NA_CDS_spreads %>%
  group_by(Date) %>%
  summarise_all(sum)

## 3.1: How often do multiple consecutive missing values appear?
# 3.1.1 CHECK: Create potential patterns (note that very beginning of data set which starts with NAs is not included)
count_consecutive_missing <- list()

for(n_miss in 1:18) count_consecutive_missing[[n_miss]] <- rep(TRUE, times = n_miss)

missing_pattern <- lapply(count_consecutive_missing, function(miss_vec) c(FALSE, miss_vec, FALSE))
rm(count_consecutive_missing)

# 3.1.2 CHECK: Count how often these patterns appear in the data
count_pattern <- function(v, pattern) {
  n_v <- length(v)
  n_pattern <- length(pattern)
  
  # Slide a window across v
  sum(sapply(1:(n_v - n_pattern + 1), function(i) all(v[i:(i+n_pattern-1)] == pattern)))
}

number_pattern <- list.cbind(lapply(country_ids, function(id)
  unlist(
    lapply(missing_pattern, function(pattern)
      count_pattern(is.NA_CDS_spreads[, id], pattern))
  )))

colnames(number_pattern) <- country_ids

total_number_NAs_per_pattern <-  number_pattern * 1:nrow(number_pattern)
total_number_cons_NAs_le9_greater9 <- rbind(colSums(total_number_NAs_per_pattern[1:9,]),
                                          colSums(total_number_NAs_per_pattern[9:nrow(total_number_NAs_per_pattern),]))
rownames(total_number_cons_NAs_le9_greater9) <- c("<=9 consecutive NAs", ">9 consecutive NAs")

days_missing_greater9 <- number_NAs_per_day$Date[rowSums(number_NAs_per_day[,2:12]>9)>0]

CDS_spreads <- CDS_spreads[!CDS_spreads$Date %in% days_missing_greater9,]


# ------------
# STEP 4: Impute missing values
# ------------
# 4.1 ACTION: Variant 1: use naive imputation method
# if day starts with NA, use first observed value that day
# if day ends with NA, use last observed value
# in between: use mean ob previous and following observation

# Assuming df has a datetime column and numeric columns
fill_na_naive <- function(x) {
  # Forward fill (fills leading NAs)
  forward <- na_locf(x)
  # Backward fill (fills trailing NAs)
  backward <- rev(na_locf(rev(x)))
  
  # Middle NAs: average of forward/backward values
  na_idx <- which(is.na(x))
  if (length(na_idx) > 0) {
    x[na_idx] <- (forward[na_idx] + backward[na_idx]) / 2
  }
  
  x
}

# Apply to all numeric columns grouped by date
CDS_spreads_naive <- CDS_spreads %>%
  group_by(Date) %>%
  mutate(across(country_ids, fill_na_naive)) %>%
  ungroup()

CDS_spreads_naive <- data.frame(CDS_spreads_naive)
# rownames(CDS_spreads_naive) <- rownames(CDS_spreads)

# 4.2 ACTION: Variant 2: use Kalman filter to impute remaining observations
CDS_spreads_kalman <- CDS_spreads %>%
  mutate(across(country_ids, na_kalman))

# Compare imputed results
data.frame(CDS_spreads$Date, CDS_spreads$PT, CDS_spreads_naive$PT, CDS_spreads_kalman$PT)
plot(CDS_spreads_naive$PT, type="l")
lines(CDS_spreads_kalman$PT, type="l", col="BLUE")

plot(density(CDS_spreads_naive$PT - CDS_spreads_kalman$PT))

# ------------
# STEP 5: Compute first differences of imputed values to obtain CDS spread 'returns'
# ------------
CDS_spreads_NAs <- data.frame(Date = CDS_spreads$Date[-1], Time = CDS_spreads$Time[-1], diff(as.matrix(CDS_spreads[,2:12]))) %>%
  filter(Time >= "09:30:00" & Time <= "17:00:00") %>%
  select(-c(Date, Time))
round(colMeans(is.na(CDS_spreads_NAs)),3) # NAs from descriptive statistics table

CDS_spreads_naive_diff <- data.frame(Date = CDS_spreads_naive$Date[-1], Time = CDS_spreads_naive$Time[-1], diff(as.matrix(CDS_spreads_naive[,2:12])))
CDS_spreads_kalman_diff <- data.frame(Date = CDS_spreads_kalman$Date[-1], Time = CDS_spreads_kalman$Time[-1], diff(as.matrix(CDS_spreads_kalman[,2:12])))

# Remove overnight returns
CDS_spreads_naive_diff <- CDS_spreads_naive_diff %>%
  filter(!Time == "08:30:00")
CDS_spreads_kalman_diff <- CDS_spreads_kalman_diff %>%
  filter(!Time == "08:30:00")

plot(CDS_spreads_naive_diff$AT, type = "l")

head(CDS_spreads_naive_diff)

# We remove overnight returns as in Andersen, Bollerslev
write.csv(CDS_spreads_naive_diff, "CDS_data/CDS_spreads_naive_diff.csv", row.names = FALSE)
write.csv(CDS_spreads_kalman_diff, "CDS_data/CDS_spreads_kalman_diff.csv", row.names = FALSE)

# Plot the returns 
# Set up 3x4 plotting layout
par(mfrow=c(3,4), mar=c(4,4,2,1))  # 3 rows, 4 columns, adjust margins
ylim_range <- range(CDS_spreads_naive_diff[, country_ids], na.rm=TRUE)

# Loop over your country_ids to plot each series
for (country in country_ids) {
  plot(
    CDS_spreads_naive_diff$Date, 
    CDS_spreads_naive_diff[[country]], 
    type="l", 
    xlab="", ylab="CDS Spread", 
    ylim=ylim_range,
    main=country
  )
}
