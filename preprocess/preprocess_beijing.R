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

