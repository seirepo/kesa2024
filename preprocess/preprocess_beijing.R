setwd("/scratch/dongelr1/susannar/kesa2024")

library(dplyr)
library(stringr)
# library(purrr)
# library('stringr')
library(lubridate)
# library(zoo)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(data.table)
library(tidyr)

rename_columns <- function(df) {
  names(df)[names(df) == "SA"] <- "SA_cm3"
  names(df)[names(df) == "WD"] <- "wind_direction"
  names(df)[names(df) == "TEMP"] <- "temperature"
  names(df)[names(df) == "RH"] <- "relative_humidity"
  names(df)[names(df) == "CS"] <- "CS_rate"
  names(df)[names(df) == "WS"] <- "wind_speed"
  names(df)[names(df) == "date"] <- "Time"
  names(df)[names(df) == "Sector"] <- "sector"
  return(df)
}

load_and_preprocess_data <- function(filter_outliers = TRUE) {
  dat_all <- read.csv("data/beijing/raw/data_summary_BUCT_all_gases.csv")
  dat_subset <- read.csv("data/beijing/raw/data_summary_BUCT_Sheet2.csv")
  
  dat_all <- dat_all %>% 
    mutate_all(~ifelse(is.nan(.), NA, .)) %>% 
    dplyr::select(date, Sector, ToL, CS) %>%
    mutate(date = as.POSIXct(date, format="%Y/%m/%d %H:%M:%S", tz="UTC"))
  
  dat_subset <- dat_subset %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    dplyr::select(-NO, -CO, -HOM.all, -PM25, -BLH) %>%
    mutate(date = as.POSIXct(date, format="%Y/%m/%d %H:%M:%S", tz="UTC"))
  
  setDT(dat_all)
  setDT(dat_subset)
  
  dat <- dat_all[dat_subset, on = c("date")] %>%
    rename_columns %>%
    mutate(temp_K = temperature + 272.15) %>%
    select(-temperature) %>%
    mutate(wdir_sin = sin(pi * wind_direction/180)) %>%
    mutate(wdir_cos = cos(pi * wind_direction/180)) %>%
    select(-wind_direction)

  dat$Time <- lubridate::round_date(dat$Time, unit = "1 hour")
  
  if (filter_outliers) {
    # Filter CS_rate, SO2 and NOx to remove large values
    dat <- dat %>% filter(CS_rate <= quantile(CS_rate, 0.95, na.rm = TRUE)) %>% filter(NOx <= quantile(NOx, 0.95, na.rm = TRUE)) %>% filter(SO2 <= quantile(SO2, 0.95, na.rm = TRUE))
  }
  
  # Include hour
  dat$Hour <- hour(dat$Time)
  dat$hour_sin <- sin(2 * pi * dat$Hour/24.0)
  dat$hour_cos <- cos(2 * pi * dat$Hour/24.0)
  dat$Hour <- NULL
  
  return(dat)
}


log_transform_data <- function(dat) {
  dat <- dat %>% mutate_at(vars(-SA_cm3, -Time, -wdir_sin, -wdir_cos, -sector), ~ dplyr::if_else(. < 0, NA, log(. + 0.01)) )
  # dat <- dat %>% mutate_at(vars(-Time, -wdir_sin, -wdir_cos, -sector), ~ dplyr::if_else(. < 0, NA, log(.)) )
  return(dat)
}

normalize_data_min_max <- function(dat) {
  dat <- dat %>% mutate_at(vars(-SA_cm3, -Time, -sector), function(x) { return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) })
  return(dat)
}

normalize_data_std <- function(dat) {
  dat <- dat %>% mutate_at(vars(-SA_cm3, -Time, -sector), function(x) (scale(x) %>% as.vector))
  return(dat)
}

# To build the proxies, calculate the reaction rate k as in paper A statistical proxy for sulphuric acid concentration by Mikkonen et al.
calc_reaction_constant <- function(dat) {
  temp_K <- dat$temp_K
  M <- 0.101 * (1.381 * 1e-23 * temp_K)^-1
  k1 <- 4e-31
  k2 <- 3.3
  k3 <- 2e-12
  k5 <- -0.8
  A <- k1 * M * (300 / temp_K)^k2
  k <- A * k3 / (A + k3) * exp(k5 * (1 + log10(A / k3)^2)^-1)
  return(k)
}

dat_with_proxies <- function(dat) {
  # If CS_rate or relative_humidity is 0, drop it before proceeding
  dat <- dat %>% filter(CS_rate != 0) %>% filter(relative_humidity != 0)
  k <- calc_reaction_constant(dat)
  dat <- dat %>% mutate(k = k) %>%
    mutate(x1 = k * UVB * SO2 / CS_rate) %>%
    mutate(x2 = k * UVB * SO2) %>%
    mutate(x3 = k * UVB * SO2^0.5) %>%
    mutate(x4 = k * UVB * SO2 / relative_humidity) %>%
    mutate(x5 = k * UVB * SO2 / (CS_rate * relative_humidity)) %>%
    dplyr::select(-k)
  return(dat)
}

load_dataset <- function(path) {
  dat <- read.csv(path)
  dat$Time <- lubridate::ymd_hms(dat$Time, tz = "UTC", truncated = 3)
  return(dat)
}

dat <- load_and_preprocess_data()

# Save data with outlier filtering of CS, NOx and SO2 (default)
write.csv(dat, "data/beijing/preprocessed/unfiltered.csv", row.names = FALSE)
# see uvb_threshold_filtering.R
filtered_uvb <- dat %>% filter(UVB > 0.0045)
filtered_uvb_so2 <- dat %>% filter(UVB > 0.0045 & SO2 > 0.1)

write.csv(filtered_uvb, "data/beijing/preprocessed/uvb_filtered.csv", row.names = FALSE)
write.csv(filtered_uvb_so2, "data/beijing/preprocessed/uvb_so2_filtered.csv", row.names = FALSE)


# Save data without outlier filtering of CS, NOx and SO2
dat_unfiltered <- load_and_preprocess_data(filter_outliers = FALSE)
write.csv(dat_unfiltered, "data/beijing/preprocessed_no_outlier_filtering/unfiltered.csv", row.names = FALSE)

filtered2_uvb <- dat_unfiltered %>% filter(UVB > 0.0045)
filtered2_uvb_so2 <- dat_unfiltered %>% filter(UVB > 0.0045 & SO2 > 0.1)

write.csv(filtered2_uvb, "data/beijing/preprocessed_no_outlier_filtering/uvb_filtered.csv", row.names = FALSE)
write.csv(filtered2_uvb_so2, "data/beijing/preprocessed_no_outlier_filtering/uvb_so2_filtered.csv", row.names = FALSE)

