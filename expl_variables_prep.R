# ------------
# STEP 0: Load packages
# ------------
library(lubridate)
library(zoo)
library(dplyr) # load this last to avoid conflicts with masked dplyr functions in the other packages

rm(list = ls())

# set working directory
setwd("/Users/lotta/Library/Mobile Documents/com~apple~CloudDocs/Documents/Promotion/# 3 Clustering Inference/Code")


# ------------
# STEP 1: Load intraday returns dataset, to match days
# ------------
CDS_diff <- data.frame(read.csv("CDS_data/CDS_spreads_naive_diff.csv"))


# ------------
# STEP 2: Prepare daily explanatory variables
# ------------
# Create common "Date" variable in same format & rename column of explanatory variable where necessary
Eonia3m <- read.table("daily_expl/Eonia3m.txt", header = TRUE) %>%
  mutate(Date = as.Date(Time, format = "%d.%m.%Y"), Eonia3m = Eonia) %>% 
  select(!"Time") %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01") %>%
  select(!"Eonia")

Eurepo3m <- read.table("daily_expl/Eurepo3m.txt", header = TRUE) %>% 
  mutate(Date = as.Date(Time, format = "%d.%m.%Y"), Eurepo3m = Eurepo) %>% 
  select(!"Time") %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01") %>%
  select(!"Eurepo")

Euribor3m <- read.table("daily_expl/Euribor3m.txt", header = TRUE) %>% 
  mutate(Date = as.Date(Time, format = "%d.%m.%Y"), Euribor3m = Euribor) %>% 
  select(!"Time") %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01") %>%
  select(!"Euribor")

EuroSwap15 <- read.table("daily_expl/Euroswap_15.txt", header = TRUE) %>% 
  mutate(Date = as.Date(Date, format = "%d.%m.%Y"), EuroSwap15 = SwapRate) %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01") %>%
  select(!"SwapRate")

EVZ <- read.table("daily_expl/EVZ.txt", header = TRUE) %>% 
  mutate(Date = as.Date(Time, format = "%d.%m.%Y")) %>% 
  select(!"Time") %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01")

ITRXTSF5 <- read.table("daily_expl/ITRXTSF5.txt", header = TRUE) %>% 
  mutate(Date = as.Date(Date, format = "%d.%m.%Y"), ITRXTSF5 = PX_LAST) %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01") %>%
  select(!"PX_LAST")

ITRXTUF5 <- read.table("daily_expl/ITRXTUF5.txt", header = TRUE) %>% 
  mutate(Date = as.Date(Date, format = "%d.%m.%Y"), ITRXTUF5 = PX_LAST) %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01") %>%
  select(!"PX_LAST")

VIX <- read.table("daily_expl/VIX.txt", header = TRUE) %>% 
  mutate(Date = as.Date(Time, format = "%d.%m.%Y")) %>% 
  select(!"Time") %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01")

Exchange_rates <- read.csv2("daily_expl/Exchange_rates.csv", header = TRUE) %>%
  mutate(Date = as.Date(Timestamp, origin = "1899-12-30")) %>% 
  select(!"Timestamp") %>% 
  filter(Date >= "2009-01-01" & Date < "2015-01-01") %>%
  mutate(GBPEUR_Bid_Close = as.numeric(GBPEUR_Bid_Close),
         CHFEUR_Bid_Close = as.numeric(CHFEUR_Bid_Close),
         NOKEUR_Bid_Close = as.numeric(NOKEUR_Bid_Close))

daily_expl <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE),
                     list(Eonia3m, Eurepo3m, Euribor3m, EuroSwap15, EVZ, ITRXTSF5, ITRXTUF5, VIX, Exchange_rates))

rm(Eonia3m, Eurepo3m, Euribor3m, EuroSwap15, EVZ, ITRXTSF5, ITRXTUF5, VIX, Exchange_rates)

# Replace tiny amount of NA values via linear interpolation of previous and consecutive observation
daily_expl[] <- lapply(daily_expl, function(x) 
  if(is.numeric(x)) na.approx(x, rule = 2) else x
)


# modify variables to include in the models
daily_expl <- daily_expl %>%
  mutate(
    funding_spread = Euribor3m - Eonia3m, # Warum ist das so anders als EBFREU3M = Eurepo3m!
    curve_slope = EuroSwap15 - Euribor3m,
    fx_gbp_ret = c(NA, diff(log(GBPEUR_Bid_Close))),
    fx_chf_ret = c(NA, diff(log(CHFEUR_Bid_Close))),
    fx_nok_ret = c(NA, diff(log(NOKEUR_Bid_Close))),
    vix_diff = c(NA, diff(VIX)),
    evz_diff = c(NA, diff(EVZ)),
    itrx_fin_diff = c(NA, diff(ITRXTSF5)),
    itrx_nonfin_diff = c(NA, diff(ITRXTUF5))
  ) %>%
  filter(Date %in% c("2009-01-05", unique(CDS_diff$Date)[1:(length(unique(CDS_diff$Date))-1)])) %>% 
  arrange(Date)

colMeans(is.na(daily_expl))

cor(daily_expl[,13:21], use = "pairwise.complete.obs")

daily_expl_all <- daily_expl

# remove original variables and itrx_fin_diff b/c highly correlated w/ itrx_nonfin_diff
daily_expl <- daily_expl[,c(1,13:19,21)]

# save data set
write.csv(daily_expl, "daily_expl/daily_expl.csv", row.names = FALSE)


# ------------
# STEP 3: Prepare intra-day explanatory variables
# ------------
date_df <- unique(CDS_diff %>% select(Date))

eustoxx_hf <- read.csv("intra-day_expl/eustoxx_hf.csv") %>%
  select(-c("X", "Ticker")) %>%
  mutate(Date = as.Date(Date))

eustoxx_hf <- merge(date_df, eustoxx_hf, all.x = TRUE)

# 2012-05-01, 2012-05-01, 2012-05-01 are missing in the eustoxx 50 data, but available in the CDS data
# replace missing values with values from the previous day
eustoxx_hf <- rbind(eustoxx_hf, cbind("Date" = "2012-05-01", "Time" = eustoxx_hf$Time, "PX" = eustoxx_hf$PX)[which(eustoxx_hf$Date == "2012-04-30"),])
eustoxx_hf <- rbind(eustoxx_hf, cbind("Date" = "2013-05-01", "Time" = eustoxx_hf$Time, "PX" = eustoxx_hf$PX)[which(eustoxx_hf$Date == "2013-04-30"),])
eustoxx_hf <- rbind(eustoxx_hf, cbind("Date" = "2014-05-01", "Time" = eustoxx_hf$Time, "PX" = eustoxx_hf$PX)[which(eustoxx_hf$Date == "2014-04-30"),])

eustoxx_hf <- data.frame(eustoxx_hf) %>% mutate(Datetime = as.POSIXct(paste(Date, Time),
                                                          format = "%Y-%m-%d %H:%M:%OS")) %>%
  filter(
    format(Datetime, "%H:%M:%S") >= "09:15:00", # remove 09:00-09:15 data to avoid overnight effects
    format(Datetime, "%H:%M:%S") <= "16:30:00"
  ) %>%
  mutate(End_ts = ceiling_date(Datetime, unit = "30 minutes")) %>%
  arrange(Datetime)

eustoxx_hf$PX <- as.numeric(eustoxx_hf$PX)

# Aggregate to 30-min return and realized volatility
eustoxx_30m <- eustoxx_hf %>% group_by(Date, End_ts) %>%
  arrange(Datetime) %>% 
  summarise(
    ret_30m = last(log(PX)) - first(log(PX)), # log return over (a,b]
    realized_vola = sqrt(sum(diff(log(PX))^2, na.rm = TRUE)), # sum of squared 15s returns within (a,b]
    .groups = "drop"
  ) %>%
  arrange(Date, End_ts) %>%
  mutate(Time = format(End_ts, "%H:%M:%S")) %>%  # extract only time
  select(Date, Time, ret_30m, realized_vola) # reorder / drop End_ts

write.csv(eustoxx_30m, "intra-day_expl/eustoxx_30m.csv", row.names = FALSE)
