setwd("/scratch/dongelr1/susannar/kesa2024")

library(doParallel)
library(caret)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

set.seed(999)

source("/scratch/dongelr1/susannar/kesa2024/model_helpers.R")

start.time <- Sys.time()
print(paste("Starting at", start.time))

# cl <- makePSOCKcluster(parallelly::availableCores())
# registerDoParallel(cl)
# clusterEvalQ(cl, .libPaths("/scratch/dongelr1/laantito/"))

# Load and filter data according to the paper (A statistical proxy for sulphuric acid concentration)
dat <- read.csv("data/all_data_merged.csv", stringsAsFactors = FALSE)
dat$Time <- ymd_hms(dat$Time, tz = "UTC", truncated = 3)
dat <- dat %>% filter(global_radiation > 10 & SO2 > 0.1) %>% mutate(temp_K = temperature + 273.15)

# Calculate the reaction rate constant k (as in Eq. 3)
# M <- 0.101 * (1.381 * 1e-23 * dat$temp_K)^-1
# k1 <- 4e-31
# k2 <- 3.3
# k3 <- 2e-12
# k5 <- -0.8
# A <- k1 * M * (300 / dat$temp_K)^k2
# 
# k <- A * k3 / (A + k3) * exp(k5 * (1 + log10(A / k3)^2)^-1)
# 
# dat <- dat %>% mutate(k = k) %>%
#   mutate(x1 = k * global_radiation * SO2 / CS_rate) %>% # L1
#   mutate(x2 = k * global_radiation * SO2) %>% # L2
#   mutate(x3 = k * global_radiation * SO2^0.5) %>% # L3
#   mutate(x4 = k * global_radiation * SO2 / relative_humidity) %>% # L4
#   mutate(x5 = k * global_radiation * SO2 / (CS_rate * relative_humidity)) # L5

dsets <- create_datasets(dat)


# l1 <- dat %>% select(Time, SA_cm3, x1) %>% drop_na
# l2 <- dat %>% select(Time, SA_cm3, x2) %>% drop_na
# l3 <- dat %>% select(Time, SA_cm3, x3) %>% drop_na
# l4 <- dat %>% select(Time, SA_cm3, x4) %>% drop_na
# l5 <- dat %>% select(Time, SA_cm3, x5) %>% drop_na

#dat_split <- split(dat, year(dat$Time))
l <- list(l1, l2, l3, l4, l5)
l_years <- lapply(l, FUN = function(x) split(x, year(x$Time)))
# Filter out NA values of the following data lists
l1_data <- l_years[[1]]
l2_data <- l_years[[2]]
l3_data <- l_years[[3]]
l4_data <- l_years[[4]]
l5_data <- l_years[[5]]

train_lin <- function(df, model, formula) {
  y <- unique(year(df$Time))
  in_training <- createDataPartition(df$SA_cm3, p = .75, list = FALSE)
  training <- df[in_training,]
  testing <- df[-in_training,]
  fit <- train(formula, data = training, method = "lm")
  p <- predict(fit, testing)
  test_r2 <- R2(p, testing$SA_cm3)
  test_rmse <- RMSE(p, testing$SA_cm3)
  #lin_fit$test_r2 <- test_r2
  
  #return(lin_fit)
  
  # View(fit)
  d <- data.frame(year = rep(y, 4), proxy = rep(model, 4), score_type = rep(c("R2", "RMSE"), 2), split = c(rep("train", 2), rep("test", 2)), score = c(fit$results$Rsquared, fit$results$RMSE, test_r2, test_rmse))
  return(d)
}

r1 <- lapply(l1_data, function(x) { train_lin(x, "L1", SA_cm3 ~ x1) }) %>% reduce(rbind)
r2 <- lapply(l2_data, function(x) { train_lin(x, "L2", SA_cm3 ~ x2) }) %>% reduce(rbind)
r3 <- lapply(l3_data, function(x) { train_lin(x, "L3", SA_cm3 ~ x3) }) %>% reduce(rbind)
r4 <- lapply(l4_data, function(x) { train_lin(x, "L4", SA_cm3 ~ x4) }) %>% reduce(rbind)
r5 <- lapply(l5_data, function(x) { train_lin(x, "L5", SA_cm3 ~ x5) }) %>% reduce(rbind)

all <- dat %>% select(Time, SA_cm3, x1, x2, x3, x4, x5) %>% drop_na %>% split(., year(.$Time))
r_all <- lapply(all, function(x) { train_lin(x, "all", SA_cm3 ~ x1 + x2 + x3 + x4 + x5) }) %>% reduce(rbind)
# r3 <- lapply(dat_split, function(x) { train_lin(x[,c("SA_cm3", "x3")]) })
# r4 <- lapply(dat_split, function(x) { train_lin(x[,c("SA_cm3", "x4")]) })
# r5 <- lapply(dat_split, function(x) { train_lin(x[,c("SA_cm3", "x5")]) })
# r_all <- lapply(dat_split, function(x) { train_lin(x[,c("SA_cm3", "x1", "x2", "x3", "x4", "x5")]) })

# l1 <- lapply(r1, function(x) round(x$results, 4))
# l2 <- lapply(r2, function(x) round(x$results, 4))
# l3 <- lapply(r3, function(x) round(x$results, 4))
# l4 <- lapply(r4, function(x) round(x$results, 4))
# l5 <- lapply(r5, function(x) round(x$results, 4))
# l_all <- lapply(r_all, function(x) round(x$results, 4))
# 
# list_all <- list(l1, l2, l3, l4, l5, l_all)
# df_all <- lapply(list_all, function(x) { t <- reduce(x, rbind); row.names(t) <- names(x); t; })
# 
# lapply(df_all, View)

scores_all <- rbind(r1, r2, r3, r4, r5, r_all)

path = "/scratch/dongelr1/susannar/kesa2024/lm_score_df_yearly.RData"
save(scores_all, file = path)

print(round(Sys.time() - start.time, 2))
print("DONE")

# stopCluster(cl)
