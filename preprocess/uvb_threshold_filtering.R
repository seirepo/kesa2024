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


###########################################################################################
#### Find a rough UVB threshold for filtering out the nighttime/dark time observations ####
###########################################################################################

load_dataset <- function(path) {
  dat <- read.csv(path)
  dat$Time <- lubridate::ymd_hms(dat$Time, tz = "UTC", truncated = 3)
  return(dat)
}

# hyy_data <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/untransformed.csv") %>% drop_na
hyy_data <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/unfiltered.csv") %>% drop_na
bei_data <- load_dataset("/scratch/dongelr1/susannar/kesa2024/data/beijing/preprocessed/unfiltered.csv") %>% drop_na


font_size <- 15
basic_theme <- theme(legend.text=element_text(size = font_size),
                     axis.text.x = element_text(size = font_size),
                     axis.text.y = element_text(size = font_size),
                     axis.title=element_text(size = font_size))

# Plot UVB and global radiation data
ggplot(data = hyy_data, aes(x = global_radiation, y = UVB)) + geom_point() +
  # geom_vline(xintercept = 10, color = "red") +
  # scale_y_continuous(limits = c(0, 0.05)) +
  # scale_x_continuous(limits = c(0, 20))
  basic_theme

# dat <- hyy_data %>% filter()
ggplot(data = hyy_data, aes(x = global_radiation, y = UVB)) + geom_point() +
  geom_vline(xintercept = 10, color = "red") +
  scale_y_continuous(limits = c(0, 0.05)) +
  scale_x_continuous(limits = c(9.5, 10.5)) +
  geom_hline(yintercept = 0.0044, color = "red")


dat <- hyy_data %>% filter(global_radiation <= 25)# %>% filter(UVB < 0.03)
ggplot(data = dat, aes(x = UVB, y = global_radiation)) + geom_point() +
  geom_hline(yintercept = 10, color = "red") #+
  # scale_x_continuous(limits = c(0, 0.1)) +
  # scale_y_continuous(limits = c(0, 100))


plot_leftover_data <- function(m, x_feat = "UVB") {
  leftover <- hyy_data %>% filter(UVB <= m | SO2 <= 0.1)
  filtered <- hyy_data %>% filter(UVB > m & SO2 > 0.1)
  
  leftover_gr <- hyy_data %>% filter(global_radiation <= 10 | SO2 <= 0.1)
  filtered_gr <- hyy_data %>% filter(global_radiation > 10 & SO2 > 0.1)
  
  print(paste(nrow(filtered), nrow(filtered_gr)))
  
  leftover$filtered_by <- "uvb"
  filtered$filtered_by <- "uvb"
  leftover_gr$filtered_by <- "gr"
  filtered_gr$filtered_by <- "gr"
  
  d1 <- rbind(leftover, leftover_gr)
  d2 <- rbind(filtered, filtered_gr)
  
  # ggplot(data = leftover, aes(x = UVB)) + geom_histogram()
  p1 <- ggplot(data = d1, aes(x = .data[[x_feat]], fill = filtered_by)) + 
    geom_histogram(alpha = 0.2, position = "identity") +
    basic_theme +
    ggtitle("leftover")
  
  p2 <- ggplot(data = d2, aes(x = .data[[x_feat]], fill = filtered_by)) +
    geom_histogram(alpha = 0.2, position = "identity") +
    basic_theme +
    ggtitle("filtered")
  
  g <- ggarrange(plotlist = list(p1, p2))
  plot(g)
}


# Find a threshold for UVB and plot the histogram of the filtered data and the data that was filtered out
t <- hyy_data %>% filter(global_radiation > 9.5 & global_radiation < 10.5) %>% dplyr::select(global_radiation, UVB)
m <- round(mean(t$UVB), 4)
print(m)
plot_leftover_data(m, "global_radiation")



hourly_sa_data <- function(df, feat) {
  sa <- df %>%
    select(Time, feat) %>%
    dplyr::group_by(hour(Time)) %>%
    dplyr::summarize(
      count = n()
    )
  
  names(sa)[names(sa) == "hour(Time)"] <- "hour"
  return(sa)
}

get_barplot <- function(df, title) {
  p <- ggplot(df, aes(x = hour, y = count)) +
    geom_bar(stat = "identity") +
    labs(title = title,
         x = "hour",
         y = "SA obs") +
    # scale_x_discrete(breaks = seq(0, 23, by = 1)) +
    scale_x_continuous(breaks = seq(0, 23, by = 1)) +
    # scale_x_discrete(breaks = as.character(0:23)) +
    scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 500))
  return(p)
}


leftover <- hyy_data %>% filter(UVB <= m | SO2 <= 0.1)
filtered <- hyy_data %>% filter(UVB > m & SO2 > 0.1)

hourly_f <- hourly_sa_data(filtered, feat = "global_radiation")
get_barplot(hourly_f, "test")

hourly_l <- hourly_sa_data(leftover, feat = "global_radiation")
get_barplot(hourly_l, "test")

bei_data %>% filter(UVB > m & SO2 > 0.1) %>% hourly_sa_data(feat = "UVB") %>% get_barplot(title = "beijing test")
