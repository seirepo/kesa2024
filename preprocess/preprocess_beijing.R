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

load_and_preprocess_data <- function() {
  dat_all <- read.csv("data/beijing/data_summary_BUCT_all_gases.csv")
  dat_subset <- read.csv("data/beijing/data_summary_BUCT_Sheet2.csv")
  
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
    mutate(wind_direction = ifelse(wind_direction < 0, wind_direction + 180, wind_direction)) %>%
    mutate(temp_K = temperature + 272.15) %>%
    select(-temperature) %>%
    mutate(wdir_sin = sin(pi * wind_direction/180)) %>%
    mutate(wdir_cos = cos(pi * wind_direction/180)) %>%
    select(-wind_direction)

  dat$Time <- lubridate::round_date(dat$Time, unit = "1 hour")
  
  return(dat)
}

compare_to_hyytiala <- function(dat) {
  # Compare variables with Hyytiälä dataset
  hyy <- read.csv("data/all_data_merged_f30.csv")
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

dat <- load_and_preprocess_data() #%>% drop_na %>% mutate(wind_direction1 = atan2(wdir_sin, wdir_cos) * 180 / pi, wind_direction2 = ifelse(wind_direction1 < 0, wind_direction1 + 360, wind_direction1))

# compare_to_hyytiala(dat %>% mutate(wind_direction = atan2(wdir_sin, wdir_cos) * 180 / pi, wind_direction = ifelse(wind_direction < 0, wind_direction + 360, wind_direction)))

write.csv(dat, "data/beijing_dataset.csv", row.names = FALSE)

################################################################
################################################################

dat <- read.csv("data/beijing_dataset.csv")
dat$Time <- lubridate::ymd_hms(dat$Time, tz = "UTC", truncated = 3)

hyy <- read.csv("data/all_data_merged_f30.csv")
hyy$Time <- lubridate::ymd_hms(hyy$Time, tz = "UTC", truncated = 3)

dat <- dat %>%
  mutate(hour = hour(Time))#, period = ifelse(hour >= 7 & hour <= 17, "day", "night"))

# Daylight time in Beijing seems to be from around 5-7 to 17-20
dat_filtered <- dat %>% filter(hour > 6 & hour <= 18)
ggplot(dat_filtered, aes(x = factor(hour), y = UVB)) +
  geom_boxplot() +
  theme_minimal()

hyy_mut <- hyy %>%
  mutate(hour = hour(Time)) %>%
  filter(global_radiation > 10)

ggplot(hyy_mut, aes(factor(hour(Time)), y = global_radiation)) +
  geom_boxplot() +
  theme_minimal()

