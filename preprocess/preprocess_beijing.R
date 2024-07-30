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


# ggplot(data = dat, aes(x = CS_rate, y = after_stat(density))) + geom_histogram(bins = 100, na.rm = TRUE)
# ggplot(data = dat) + geom_point(aes(x = Time, y = CS_rate), na.rm = TRUE)
# # ggplot(data = dat) + 
  # geom_histogram(aes(x = CS_rate, y = after_stat(density)), position = "identity") + 
  # geom_density(aes(x = CS_rate, y = after_stat(density)))
# 
# dat1 <- dat %>% filter(CS_rate <= quantile(CS_rate, 0.95, na.rm = TRUE))
# ggplot(data = dat1) + geom_point(aes(x = Time, y = CS_rate), na.rm = TRUE)
# 
# dat2 <- dat %>% filter(NOx <= quantile(NOx, 0.95, na.rm = TRUE))
# ggplot(data = dat2) + geom_point(aes(x = Time, y = NOx), na.rm = TRUE)
# 
# dat3 <- dat %>% filter(SO2 <= quantile(SO2, 0.95, na.rm = TRUE))
# ggplot(data = dat3) + geom_point(aes(x = Time, y = SO2), na.rm = TRUE)

# d_cs <- dat %>% filter(CS_rate > quantile(CS_rate, 0.975, na.rm = TRUE))
# d_nox <- dat %>% filter(NOx > quantile(NOx, 0.975, na.rm = TRUE))
# d_so2 <- dat %>% filter(SO2 > quantile(SO2, 0.975, na.rm = TRUE))
# 
# ggplot(d_cs, aes(x = CS_rate)) + geom_histogram(na.rm = TRUE)
# ggplot(d_nox, aes(x = NOx)) + geom_histogram(na.rm = TRUE)
# ggplot(d_so2, aes(x = SO2)) + geom_histogram(na.rm = TRUE)

# Save data with outlier filtering of CS, NOx and SO2 (default)
write.csv(dat, "data/beijing/preprocessed/dataset.csv", row.names = FALSE)
# see uvb_threshold_filtering.R
filtered_uvb <- dat %>% filter(UVB > 0.0045)
filtered_uvb_so2 <- dat %>% filter(UVB > 0.0045 & SO2 > 0.1)

write.csv(filtered_uvb, "data/beijing/preprocessed/dataset_uvb_filtered.csv", row.names = FALSE)
write.csv(filtered_uvb_so2, "data/beijing/preprocessed/dataset_uvb_so2_filtered.csv", row.names = FALSE)


# Save data without outlier filtering of CS, NOx and SO2
dat_unfiltered <- load_and_preprocess_data(filter_outliers = FALSE)
write.csv(dat_unfiltered, "data/beijing/preprocessed_no_outlier_filtering/dataset.csv", row.names = FALSE)

filtered2_uvb <- dat_unfiltered %>% filter(UVB > 0.0045)
filtered2_uvb_so2 <- dat_unfiltered %>% filter(UVB > 0.0045 & SO2 > 0.1)

write.csv(filtered2_uvb, "data/beijing/preprocessed_no_outlier_filtering/dataset_uvb_filtered.csv", row.names = FALSE)
write.csv(filtered2_uvb_so2, "data/beijing/preprocessed_no_outlier_filtering/dataset_uvb_so2_filtered.csv", row.names = FALSE)


# test <- load_dataset("data/beijing/preprocessed/dataset.csv") # uvb, so2, nox, o3
# p1 <- ggplot(test, aes(x = O3)) + geom_histogram(na.rm = TRUE)
# p2 <- ggplot(test, aes(x = log(O3))) + geom_histogram(na.rm = TRUE)
# ggarrange(plotlist = list(p1, p2))

# dat_norm_min_max <- normalize_data_min_max(dat)
# dat_norm_std <- normalize_data_std(dat)
# dat_log <- log_transform_data(dat)
# dat_log_norm_min_max <- normalize_data_min_max(dat_log)
# dat_log_norm_std <- normalize_data_std(dat_log)
# dat_proxies <- dat_with_proxies(dat)
# write.csv(dat_norm_min_max, "data/beijing/preprocessed_transformed/norm_min_max.csv", row.names = FALSE)
# write.csv(dat_norm_std, "data/beijing/preprocessed_transformed/norm_std.csv", row.names = FALSE)
# write.csv(dat_log, "data/beijing/preprocessed_transformed/log.csv", row.names = FALSE)
# write.csv(dat_log_norm_min_max, "data/beijing/preprocessed_transformed/log_norm_min_max.csv", row.names = FALSE)
# write.csv(dat_log_norm_std, "data/beijing/preprocessed_transformed/log_norm_std.csv", row.names = FALSE)
# write.csv(dat_proxies, "data/beijing/preprocessed_transformed/dataset_with_proxies.csv", row.names = FALSE)

# test <- load_dataset("data/beijing/preprocessed_transformed/log.csv")
# test2 <- load_dataset("data/beijing/preprocessed_transformed/dataset.csv")

## Test the transforming functions
# test_dat <- data.frame(Time = dat$Time[1:5], x1 = c(exp(1) - 1, exp(2) - 1, exp(3) - 1, exp(4) - 1, exp(5) - 1), x2 = c(0, 0, -2, 0, 0), wdir_sin = sample(dat$wdir_sin, 5), wdir_cos = sample(dat$wdir_cos, 5), sector = c(1, 1, 1, 2, 3))
# test_dat
# t <- log_transform_data(test_dat)
# print(t)

# test_dat <- data.frame(Time = dat$Time[1:5], x1 = c(0, 1, 2, 3, 4), x2 = c(0, 1, -2, NA, 0), wdir_sin = sample(dat$wdir_sin, 5), wdir_cos = sample(dat$wdir_cos, 5), sector = c(1, 1, 1, 2, 3))
# print(test_dat)
# # x1 = c(0, 0.25, 0.5, 0.75, 1)
# # x2 = c(0.667, 1, 0, NA, 0.667)
# t <- normalize_data_min_max(test_dat)
# print(t)

# test_dat <- data.frame(Time = dat$Time[1:5], x1 = c(0, 1, 2, 3, 4), x2 = c(0, 1, -2, NA, 0), wdir_sin = sample(dat$wdir_sin, 5), wdir_cos = sample(dat$wdir_cos, 5), sector = c(1, 1, 1, 2, 3))
# print(test_dat)
# t <- normalize_data_std(test_dat)
# print(t)
# lapply(t, mean, na.rm = TRUE)
# lapply(t, sd, na.rm = TRUE)


t2 <- load_dataset("data/beijing/preprocessed/dataset.csv") %>% drop_na


################################################################

get_deg <- function(s, c) {
   d <- atan2(s, c) * 180 / pi
   if (d > 0) {
       return(d + 270)
   } else {
       # return(d + 270)
     d <- d + 270
     return
   }
}

get_deg(0, -1)



compare_to_hyytiala <- function(dat) {
  # Compare variables with Hyytiälä dataset
  # hyy <- read.csv("data/all_data_merged_f30.csv")
  hyy <- read.csv("data/hyytiala/preprocessed/f30.csv")
  hyy <- hyy %>%
    mutate(wind_direction = atan2(wdir_sin, wdir_cos) * 180 / pi,
           wind_direction = ifelse(wind_direction < 0, wind_direction + 360, wind_direction)) %>%
    # mutate(temperature = temp_K - 272.15) %>%
    mutate(site = "hyytiala") %>%
    drop_na
  
  dat <- rename_columns(dat) %>%
    mutate(site = "beijing") %>%
    drop_na
  
  features <- intersect(colnames(dat), colnames(hyy))
  log_features <- c("CS_rate", "NOx", "O3", "SO2", "SA_cm3", "site")
  o_features <- union(setdiff(features, log_features), c("site"))
  df_log <- rbind(hyy[,log_features], dat[,..log_features])
  df_o <- rbind(hyy[,o_features], dat[,..o_features])
  
  melt_log <- reshape2::melt(df_log, id.vars = "site")
  
  obs_b <- df_log %>% filter(site == "beijing") %>% nrow
  obs_h <- df_log %>% filter(site == "hyytiala") %>% nrow
  
  p_log <- ggplot(melt_log, aes(x = variable, y = value, fill = site)) +
    geom_boxplot(na.rm = TRUE) +
    facet_wrap(~variable, scales = "free") +
    scale_fill_discrete(labels = c(paste("beijing,", obs_b, "obs"), paste("hyytiala,", obs_h, "obs"))) +
    scale_y_continuous(trans = "log10")
  
  o_features <- setdiff(o_features, c("site"))
  
  plots <- list(
    ggplot(data = df_o, aes(x = relative_humidity, fill = site)) + geom_histogram(alpha = 0.2, position = "identity"),
    ggplot(data = df_o, aes(x = ToL, fill = site)) + geom_histogram(alpha = 0.2, position = "identity"),
    ggplot(data = df_o, aes(x = wind_speed, fill = site)) + geom_histogram(alpha = 0.2, position = "identity"),
    ggplot(data = df_o, aes(x = wind_direction, fill = site)) + geom_histogram(alpha = 0.2, position = "identity") + scale_x_continuous(breaks = seq(-360, 360, 90)),
    ggplot(data = df_o, aes(x = temp_K, fill = site)) + geom_histogram(alpha = 0.2, position = "identity")
  )
  
  p_o <- ggarrange(plotlist = plots, common.legend = TRUE)
  
  # ggarrange(plotlist = list(p_log, p_o))
  plot(p_log)
  plot(p_o)
}

# compare_to_hyytiala(dat %>% mutate(wind_direction = atan2(wdir_sin, wdir_cos) * 180 / pi, wind_direction = ifelse(wind_direction < 0, wind_direction + 360, wind_direction)))

################################################################

# Compare the global_radiation and UVB
dat <- read.csv("data/beijing/preprocessed/dataset.csv")
dat$Time <- lubridate::ymd_hms(dat$Time, tz = "UTC", truncated = 3)

hyy <- read.csv("data/hyytiala/preprocessed/f30.csv")
hyy$Time <- lubridate::ymd_hms(hyy$Time, tz = "UTC", truncated = 3)

dat <- dat %>%
  mutate(hour = hour(Time)) %>% #, period = ifelse(hour >= 7 & hour <= 17, "day", "night"))
  drop_na


# Daylight time in Beijing seems to be from around 5-7 to 17-20
dat_filtered <- dat %>% filter(hour > 6 & hour <= 18)

ggplot(dat_filtered, aes(x = factor(hour), y = UVB)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("beijing, daylight time")

print(dim(dat_filtered))

dat_filtered <- dat_filtered %>% filter(SO2 > 0.1)
ggplot(dat_filtered, aes(x = factor(hour), y = UVB)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("beijing, daylight time and SO2 > 0.1")

print(dim(dat_filtered))

hyy_mut <- hyy %>%
  mutate(hour = hour(Time)) %>%
  filter(global_radiation > 10)

p1 <- ggplot(hyy_mut, aes(factor(hour(Time)), y = global_radiation)) +
  geom_boxplot() +
  theme_minimal() + 
  ggtitle("hyytiala")
p1

dat_uvb <- dat %>% select(Time, UVB) %>% drop_na
hyy_gr <- hyy %>% select(Time, global_radiation) %>% drop_na

joined <- dplyr::full_join(dat_uvb, hyy_gr, by = join_by(Time == Time))

ggplot(dat = joined, aes(x = global_radiation, y = UVB)) + geom_point()

# TODO: Figure out how to filter the data properly. The paper A proxy for atmospheric daytime gaseous sulfuric acid concentration in urban Beijing states by Lu
# says "Only data between local sunrise and sunset were used in the subsequent analysis.". Now the data is filtered based on some search engine results on
# sunrise and sunset times in Beijing. Daylight time in Beijing seems to be from around 5-7 to 17-20, depending on the time of the year
dat <- dat %>% filter(hour(Time) > 6 & hour(Time) < 19)


dat <- load_dataset("data/beijing/preprocessed/dataset.csv")


