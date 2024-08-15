setwd("/scratch/dongelr1/susannar/kesa2024")

library(dplyr)
library(purrr)
library('stringr')
library(lubridate)
library(zoo)
library(ggplot2)
library(ggpubr)
library(tidyr)
library("scales")
library(hdf5r)
library(timetk)
library(caret)
library(pracma)

preprocess_smear_data <- function() {
  rename_smear_columns <- function(df) {
    names(df)[names(df) == "HYY_META.Pamb0"] <- "air_pressure"
    names(df)[names(df) == "HYY_META.T336"] <- "temperature"
    names(df)[names(df) == "HYY_META.Glob"] <- "global_radiation"
    names(df)[names(df) == "HYY_META.NOx336"] <- "NOx"
    names(df)[names(df) == "HYY_META.O3336"] <- "O3"
    names(df)[names(df) == "HYY_META.RHTd"] <- "relative_humidity"
    names(df)[names(df) == "HYY_META.SO2168"] <- "SO2"
    names(df)[names(df) == "HYY_META.WDU336"] <- "wind_direction"
    names(df)[names(df) == "HYY_META.WSU336"] <- "wind_speed"
    names(df)[names(df) == "HYY_META.UV_B"] <- "UVB"
    return(df)
  }
  
  merge_smear_data <- function(folder_path) {
    x <- list.files(path = folder_path, full.names = TRUE) %>%
      lapply(read.csv) %>%
      lapply(function(x) {x$Time <- as.POSIXct(str_c(x$Year, "-", x$Month, "-", x$Day, " ", x$Hour, ":", x$Minute, ":00"), format="%Y-%m-%d %H:%M:%S", tz="UTC");x}) %>%
      lapply(function(x) arrange(x, Time)) %>%
      lapply(function(x) x[!names(x) %in% c("Minute","Second", "Hour", "Day", "Year", "Month")]) %>%
      lapply(function(x) rename_smear_columns(x)) %>%
      lapply(function(x) arrange(x, Time)) %>%
      reduce(cbind) %>%
      .[unique(colnames(.))] %>%
      mutate_at(vars(-Time), function(x) ifelse(is.nan(x), NA, x))
    return(x)
  }
  
  path1113 <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/smear_hyytiala_2011_2013/"
  path1819 <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/smear_hyytiala_2018_2019/"
  path22 <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/smear_hyytiala_2022/"
  hyytiala1113 <- merge_smear_data(path1113)
  hyytiala1819 <- merge_smear_data(path1819)
  hyytiala22 <- merge_smear_data(path22)
  
  smear_data <- reduce(list(hyytiala1113, hyytiala1819, hyytiala22), rbind)
  
  write.csv(smear_data, "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/smear_merged.csv", row.names = FALSE)
  
  # Split wind direction into two columns using sine/cosine transformation
  
  smear_data$wdir_sin <- sin(pi * smear_data$wind_direction/180)
  smear_data$wdir_cos <- cos(pi * smear_data$wind_direction/180)
  
  # Replace temperature with kelvins, drop the wind_direction column
  
  smear_data <- smear_data %>% mutate(temp_K = temperature + 273.15) %>% dplyr::select(-wind_direction, -temperature)
  
  # Set negative values in global_radiation, UVB, NOx and SO2 to 0
  smear_data$global_radiation[smear_data$global_radiation < 0] <- 0
  smear_data$UVB[smear_data$UVB < 0] <- 0
  smear_data$NOx[smear_data$NOx < 0] <- 0
  smear_data$SO2[smear_data$SO2 < 0] <- 0
  
  return(smear_data)
}

preprocess_tol_data <- function(path) {
  load_tol_data <- function(folder_path) {
    x <- list.files(path = folder_path, full.names = TRUE) %>%
      lapply(function(x) read.csv(x, header = TRUE, col.names = c("Time", "ToL", "sector"))) %>%
      lapply(function(x) {y <- mutate(x, Time = as.POSIXct(Time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")); y}) %>%
      lapply(function(x) arrange(x, Time)) %>%
      reduce(rbind)
    return(x)
  }
  
  # ToL is measured once every hour. Impute the missing 30 min values linearly if the sector or theconsecutive
  # measurements remains the same, otherwise impute NA
  impute_missing_tol_data <- function(df) {
    tol_na_rem <- df %>% drop_na()
    
    # Split the data by sector and gather the row indices of each sector into a list.
    # Split the list into vector containing only consecutive row indices
    # If rows corresponding to sector 1 were 4, 5, 6, 8, 10, 11, this would result in a list
    # containing vectors c(4, 5, 6), c(8) and c(10, 11)
    sectors <- tol_na_rem %>% group_by(sector) %>% group_data() %>% split(.$sector)
    s1 <- sectors[[1]]$.rows %>% unlist %>% sort %>% split(cumsum(c(1, diff(.) != 1)))
    s2 <- sectors[[2]]$.rows %>% unlist %>% sort %>% split(cumsum(c(1, diff(.) != 1)))
    s3 <- sectors[[3]]$.rows %>% unlist %>% sort %>% split(cumsum(c(1, diff(.) != 1)))
    s4 <- sectors[[4]]$.rows %>% unlist %>% sort %>% split(cumsum(c(1, diff(.) != 1)))
    
    pad_30min <- function(x, df) {
      df <- pad_by_time(df[x,], Time, .by = "30 mins", .pad_value = NA)
      return(df)
    }
    
    # Pad 30 min observations to the original dataframe with NAs removed,
    # sector by sector given the corresponding indices in lists s1 to s4
    # Will cause warnings "datetime variable does not vary for 1 of the groups, no padding applied on this / these group(s)"
    # if there's only one consecutive observation with the same sector
    padded_s1 <- lapply(s1, FUN = pad_30min, df = tol_na_rem) %>% reduce(rbind) 
    padded_s2 <- lapply(s2, FUN = pad_30min, df = tol_na_rem) %>% reduce(rbind)
    padded_s3 <- lapply(s3, FUN = pad_30min, df = tol_na_rem) %>% reduce(rbind)
    padded_s4 <- lapply(s4, FUN = pad_30min, df = tol_na_rem) %>% reduce(rbind)
    
    # Linearly impute the ToL values on padded rows, now the sector shouldn't change around them
    imputed_s1 <- lapply(padded_s1[2:3], function(X) approxfun(seq_along(X), X)(seq_along(X)))
    imputed_s2 <- lapply(padded_s2[2:3], function(X) approxfun(seq_along(X), X)(seq_along(X)))
    imputed_s3 <- lapply(padded_s3[2:3], function(X) approxfun(seq_along(X), X)(seq_along(X)))
    imputed_s4 <- lapply(padded_s4[2:3], function(X) approxfun(seq_along(X), X)(seq_along(X)))
    
    padded_s1[2:3] = imputed_s1
    padded_s2[2:3] = imputed_s2
    padded_s3[2:3] = imputed_s3
    padded_s4[2:3] = imputed_s4
    
    tol_imputed <- rbind(padded_s1, padded_s2, padded_s3, padded_s4)
    tol_imputed <- tol_imputed %>% arrange(Time) %>% pad_by_time(Time, .by = "30 mins", .pad_value = NA)
    
    # pad_30_min ends up padding every date between 2011-2022, select only the actual years the data is from
    tol_imputed <- filter(tol_imputed, year(Time) %in% c(2011, 2012, 2013, 2018, 2019, 2022))
    
    return(tol_imputed)
  }
  
  tol <- load_tol_data("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/time_over_land/")
  tol_imputed <- impute_missing_tol_data(tol)
  
  # Add column for the name of the sector
  sector_names <- c("clean", "europe", "east", "mixed")
  names(tol_imputed)[names(tol_imputed) == "sector"] <- "sector_num"
  tol_imputed <- tol_imputed %>%
    mutate(sector = sector_names[sector_num])
  
  # Categorize the sector names
  dmy <- dummyVars(" ~ sector", data = tol_imputed)
  cat_df <- data.frame(predict(dmy, newdata = tol_imputed))
  names(cat_df)[names(cat_df) == "sectorclean"] <- "sector.clean"
  names(cat_df)[names(cat_df) == "sectoreurope"] <- "sector.europe"
  names(cat_df)[names(cat_df) == "sectoreast"] <- "sector.east"
  names(cat_df)[names(cat_df) == "sectormixed"] <- "sector.mixed"
  
  tol_imputed <- cbind(tol_imputed, cat_df) %>% select(-sector_num, -sector)
  
  return(tol_imputed)
}

preprocess_cs_data <- function(path) {
  cs_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/condensation_sink/CS_2004_2023_10min.txt"
  cs_all <- read.table(cs_path, col.names = c("Year", "Month", "Day", "Hour", "Minute", "Second", "CS_rate"))
  cs <- filter(cs_all, (Year >= 2011 & Year <= 2013) | (Year >= 2018 & Year <= 2019) | (Year == 2022))
  cs$Time <- as.POSIXct(str_c(cs$Year, "-", cs$Month, "-", cs$Day, " ", cs$Hour, ":", cs$Minute, ":00"), format="%Y-%m-%d %H:%M:%S", tz="UTC")
  cs <- cs[!names(cs) %in% c("Minute","Second", "Hour", "Day", "Year", "Month")]

  cs$Time <- floor_date(cs$Time, "30 mins")
  cs <- group_by(cs, Time)
  cs <- summarise(cs, CS_rate = mean(CS_rate, na.rm = FALSE))

  return(cs)
}

preprocess_sa_data <- function(path, filter_outliers = TRUE) {
  load_sa_data <- function(folder_path) {
    x <- list.files(path = folder_path, full.names = TRUE) %>%
      lapply(read.table) %>%
      lapply(function(x) {names(x) <- c("Time", "SA_cm3", "3", "4"); x}) %>%
      lapply(function(x) {x[3:4] <- list(NULL); x}) %>% # Drop columns containing HOM/ELVOCs
      lapply(function(x) {y <- mutate(x, Time=as.POSIXct((Time - 719529)*86400, origin = "1970-01-01", tz="UTC")); y}) %>%
      lapply(function(x) arrange(x, Time)) %>%
      reduce(rbind) %>%
      arrange(Time)
    return(x)
  }
  
  sa_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/sulphuric_acid/"
  sa <- load_sa_data(sa_path) %>%
    filter((SA_cm3 > -1e5) & (SA_cm3 < 1e9)) # Remove very unusual observations
  
  sa_years <- split(sa, year(sa$Time))
  sa11 <- sa_years[["2011"]]
  sa12 <- sa_years[["2012"]]
  sa13 <- sa_years[["2013"]]
  sa18 <- sa_years[["2018"]]
  sa19 <- sa_years[["2019"]]
  sa22 <- sa_years[["2022"]]
  
  # SA data in 2011 and 2012 is measured every 2 minutes (other data is every 30 min). Before calculating the 30 min average, filter out possible outliers
  # and save the outliers removed
  
  # Interquartile range (IQR): filter out data points that are further than 1.5 * IQR away from Q1 or Q3 of the data in the current window. If the window doesn't fit (in the beginning/end), just keep the data as is.
  iqr_filter <- function(df, k, q1 = 0.25, q2 = 0.75) {
    width = 2 * k + 1
    filtered <- df %>%
      mutate(lower = rollapply(
        df$SA_cm3, width, align = "center",  fill = NA,
        FUN = function(x) {
          r <- quantile(x, probs = c(0.25, 0.75));
          iqr <- r[2] - r[1];
          lower_lim <- r[1] - 1.5*iqr;
          upper_lim <- r[2] + 1.5*iqr;
          lower_lim
        })) %>%
      mutate(upper = rollapply(
        df$SA_cm3, width, align = "center",  fill = NA,
        FUN = function(x) { 
          r <- quantile(x, probs = c(0.25, 0.75));
          iqr <- r[2] - r[1];
          lower_lim <- r[1] - 1.5*iqr;
          upper_lim <- r[2] + 1.5*iqr;
          upper_lim})) %>%
      filter(is.na(lower) | between(SA_cm3, lower, upper)) %>%
      mutate(lower = NULL, upper = NULL)
    return(filtered)
  }
  
  # Filter by median absolute deviation, an option to the IQR filtering.
  # res$ind contain the indices of outliers, res$y the corrected time series
  hampel_filter <- function(df, k, t0 = 3) {
    res <- hampel(x = df$SA_cm3, k = k, t0 = t0)
    return(res)
  }
  
  # Round the SA values to 30 min
  compute_SA_30min_average <- function(df) {
    df$Time <- lubridate::floor_date(df$Time, "30 minutes")
    df <- group_by(df, Time) %>% summarise(SA_cm3 = mean(SA_cm3, na.rm = FALSE))
    return(df)
  }
  
  # Filter outliers using IQR filter. Hampel filter could also be used for this
  if (filter_outliers) {
    sa11_filtered <- iqr_filter(sa11, k = 7)
    sa12_filtered <- iqr_filter(sa12, k = 7)
    
    removed11 <- setdiff(sa11, sa11_filtered)
    removed12 <- setdiff(sa12, sa12_filtered)
    removed_sa_data <- rbind(removed11, removed12)
    saveRDS(removed_sa_data, "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/left_out_sa_data_iqr_k_7.rds")
    
    sa11 <- sa11_filtered
    sa12 <- sa12_filtered
    print("SA outlier filtering done")
  }
  
  sa11_rounded <- compute_SA_30min_average(sa11)
  sa12_rounded <- compute_SA_30min_average(sa12)
  
  # 2013 data contains duplicate rows. Keep only unique rows and round the dates to nearest 30 min to round times such as 11:29:59
  sa13 <- sa13 %>% distinct(.keep_all = TRUE)
  sa13$Time <- round_date(sa13$Time, unit = "30 mins")
  
  # Round dates in 2018, 2019 and 2022 to the nearest 30 mins to round times such as 11:29:59
  sa18$Time <- round_date(sa18$Time, unit = "30 mins")
  sa19$Time <- round_date(sa19$Time, unit = "30 mins")
  sa22$Time <- round_date(sa22$Time, unit = "30 mins")
  
  sa_data <- reduce(list(sa11_rounded, sa12_rounded, sa13, sa18, sa19, sa22), rbind)
  # Drop the 20 negative values that are still left in the data
  sa_data <- sa_data %>% filter(SA_cm3 >= 0)
  return(sa_data)
}

load_dataset <- function(path) {
  dat <- read.csv(path)
  dat$Time <- lubridate::ymd_hms(dat$Time, tz = "UTC", truncated = 3)
  return(dat)
}

filter_outliers <- function(dat) {
  # Filter CS_rate, SO2 and NOx to remove the extremely large observations
  dat <- dat %>% filter(CS_rate <= quantile(CS_rate, 0.95, na.rm = TRUE)) %>% filter(NOx <= quantile(NOx, 0.95, na.rm = TRUE)) %>% filter(SO2 <= quantile(SO2, 0.95, na.rm = TRUE))
  return(dat)
}

save_data <- function(data, target_path) {
  write.csv(data, target_path, row.names = FALSE)
  print(paste("Data saved to", target_path))
}

add_hour_cols <- function(dat) {
  dat$Hour <- hour(dat$Time)
  dat$hour_sin <- sin(2 * pi * dat$Hour/24.0)
  dat$hour_cos <- cos(2 * pi * dat$Hour/24.0)
  dat$Hour <- NULL
  return(dat)
}

merge_filter_save <- function(data_list, target_path, filter_outliers = TRUE) {
  merged_data <- Reduce(function(x, y) merge(x, y, all = TRUE), data_list)
  
  if (filter_outliers) {
    merged_data <- filter_outliers(merged_data)
  }
  
  merged_data <- add_hour_cols(merged_data)
  save_data(merged_data, target_path)
}


########################################
# Preprocess data and save the results #
########################################

smear_dat_with_uvb <- preprocess_smear_data()
smear_dat <- smear_dat_with_uvb %>% dplyr::select(-UVB)

tol_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/time_over_land/"
tol_dat <- preprocess_tol_data(tol_path)

cs_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/condensation_sink/CS_2004_2023_10min.txt"
cs_dat <- preprocess_cs_data(cs_path)

sa_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/sulphuric_acid/"
sa_dat <- preprocess_sa_data(sa_path, filter_outliers = TRUE)



# Save data with outlier filtering
save_filtered_data <- function() {
  data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/unfiltered.csv"
  merge_filter_save(data_list = list(sa_dat, smear_dat_with_uvb, cs_dat, tol_dat), target_path = data_path)
  
  merged <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/unfiltered.csv")
  data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/gr_so2_filtered.csv"
  filtered <- merged %>% filter(global_radiation > 10 & SO2 > 0.1)
  save_data(filtered, data_path)
  
  # Save a dataset ready to feed to a model by filtering out rows with UVB > 0.0045 (before: global_radiation > 10) and SO2 > 0.1
  merged <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/unfiltered.csv")
  data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/uvb_so2_filtered.csv"
  filtered <- merged %>% filter(UVB > 0.0045 & SO2 > 0.1)
  save_data(filtered, data_path)
  
  # Filter only by UVB
  merged <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/unfiltered.csv")
  data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/uvb_filtered.csv"
  filtered <- merged %>% filter(UVB > 0.0045)
  save_data(filtered, data_path)
}

# Save data similarly but without outlier filtering
save_unfiltered_data <- function() {
  data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed_no_outlier_filtering/unfiltered.csv"
  merge_filter_save(list(sa_dat, smear_dat_with_uvb, cs_dat, tol_dat), data_path, filter_outliers = FALSE)
  
  merged <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed_no_outlier_filtering/unfiltered.csv")
  data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed_no_outlier_filtering/gr_so2_filtered.csv"
  filtered <- merged %>% filter(global_radiation > 10 & SO2 > 0.1)
  save_data(filtered, data_path)
  
  merged <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed_no_outlier_filtering/unfiltered.csv")
  data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed_no_outlier_filtering/uvb_so2_filtered.csv"
  filtered <- merged %>% filter(UVB > 0.0045 & SO2 > 0.1)
  save_data(filtered, data_path)
  
  merged <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed_no_outlier_filtering/unfiltered.csv")
  data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed_no_outlier_filtering/uvb_filtered.csv"
  filtered <- merged %>% filter(UVB > 0.0045)
  save_data(filtered, data_path)
}

save_filtered_data()
save_unfiltered_data()
