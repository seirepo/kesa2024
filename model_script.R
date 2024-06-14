library(caret)
library(plyr)
library(Cubist, lib = "/scratch/dongelr1/laantito/")
library(Metrics, lib = "/scratch/dongelr1/laantito/")
library(xgboost)
library(tidyr)
library(lubridate)
library(dplyr)
library(rsample)
library(caretEnsemble, lib = "/scratch/dongelr1/laantito/")
library(prediction, lib = "/scratch/dongelr1/laantito/")
library(Metrics, lib = "/scratch/dongelr1/laantito/")
library(bst, lib = "/scratch/dongelr1/laantito/")
library(ggpattern, lib = "/scratch/dongelr1/laantito/")
library(doParallel)
library(RColorBrewer)

setwd("/scratch/dongelr1/susannar/kesa2024")

start.time <- Sys.time()
print(paste("Start at", start.time))

cl <- makePSOCKcluster(parallelly::availableCores())
registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("/scratch/dongelr1/laantito/"))

print("Cores:")
print(parallelly::availableCores())

calc_reaction_constant <- function(dat) {
  # Calculate the reaction rate constant k (as in Eq. 3)
  M <- 0.101 * (1.381 * 1e-23 * dat$temp_K)^-1
  k1 <- 4e-31
  k2 <- 3.3
  k3 <- 2e-12
  k5 <- -0.8
  A <- k1 * M * (300 / dat$temp_K)^k2
  k <- A * k3 / (A + k3) * exp(k5 * (1 + log10(A / k3)^2)^-1)
  return(k)
}

create_datasets <- function(dat) {
  # return list of different datasets in the same order as defined in names
  dat <- dat %>% mutate(temp_K = temperature + 273.15) %>% select(-wind_direction, -temperature) # Unfiltered data
  dat_filtered <- dat %>% filter(global_radiation > 10 & SO2 > 0.1) # Filtered data
  # l1 <- dat_filtered %>% mutate(x = k * global_radiation * SO2 / CS_rate) %>% select(SA_cm3, x)
  # l2 <- dat_filtered %>% mutate(x = k * global_radiation * SO2) %>% select(SA_cm3, x)
  # l3 <- dat_filtered %>% mutate(x = k * global_radiation * SO2^0.5) %>% select(SA_cm3, x)
  # l4 <- dat_filtered %>% mutate(x = k * global_radiation * SO2 / relative_humidity) %>% select(SA_cm3, x)
  # l5 <- dat_filtered %>% mutate(x = k * global_radiation * SO2 / (CS_rate * relative_humidity)) %>% select(SA_cm3, x)
  k <- calc_reaction_constant(dat_filtered)
  all_proxies <- dat_filtered %>% mutate(k = k) %>%
    mutate(x1 = k * global_radiation * SO2 / CS_rate) %>%
    mutate(x2 = k * global_radiation * SO2) %>%
    mutate(x3 = k * global_radiation * SO2^0.5) %>%
    mutate(x4 = k * global_radiation * SO2 / relative_humidity) %>%
    mutate(x5 = k * global_radiation * SO2 / (CS_rate * relative_humidity)) %>%
    select(SA_cm3, x1, x2, x3, x4, x5)
  
  l1 <- select(all_proxies, SA_cm3, x1)
  l2 <- select(all_proxies, SA_cm3, x2)
  l3 <- select(all_proxies, SA_cm3, x3)
  l4 <- select(all_proxies, SA_cm3, x4)
  l5 <- select(all_proxies, SA_cm3, x5)
  
  dat_with_proxies <- merge(all_proxies, dat_filtered, by = "SA_cm3")
  
  l <- list(l1, l2, l3, l4, l5, all_proxies, dat_filtered, dat_with_proxies, dat)
  return(l)
}

# Load data
dat <- read.csv("data/all_data_merged.csv", stringsAsFactors = FALSE) %>% drop_na() %>% select(-Time)
#l <- list(dat, dat, dat) # Different datasets
dset_list <- create_datasets(dat)

p_val = 0.75
set.seed(3214)

### Model training
df <- data.frame(
  model = character(),
  type = character(),
  scoreType = character(),
  split = character(),
  score = double(),
  stringsAsFactors = TRUE
)

names <- c("L1", "L2", "L3", "L4", "L5", "all_proxies", "all_filtered", "all_with_proxies", "all_unfiltered")
model_names <- c("rf", "lm")

train_models <- function() {
  
  model_list <- list()
  
  start.time <- Sys.time()
  
  for (i in 1:length(dset_list)) {
    
    # Splitting the data to train and test
    split <- initial_split(dset_list[[i]], prop = p_val, strata = SA_cm3)
    train <- training(split)
    test <- testing(split)
  
    folds <- createFolds(train$SA_cm3, k = 5)
    
    trainControl <- trainControl(method="repeatedcv", 
                                 number=5,
                                 repeats=5,
                                 index = folds,
                                 savePredictions="final",
                                 verbose = TRUE,
                                 returnData = FALSE,
                                 trim = TRUE
    )
    
    rangerGrid <- expand.grid(
      mtry = seq(1, ncol(train)-1, 1),
      splitrule = c("variance", "extratrees"),
      min.node.size = c(3, 5, 8, 12, 18)
    )
  
    
    modelTypes <- list(
      rf     = caretModelSpec(method="ranger", tuneGrid = rangerGrid),
      lm     = caretModelSpec(method="lm")
    )
    
    models <- caretList(
      SA_cm3~., data=train,
      trControl=trainControl,
      metric = "RMSE",
      tuneList = modelTypes
    )
    
    model_list[[i]] <- models
    
    for (j in model_names) {
      print("asd")
      scores <- calculate_scores(models[j], train, names[i], "Train", j)
      scores_test <- calculate_scores(models[j], test, names[i], "Test", j)
      df <- rbind(df, scores, scores_test)
    }
  
  }
  
  print(round(Sys.time() - start.time, 2))
  print("DONE")
  
  return(list(df, model_list))
}

calculate_scores <- function(model, data, type, split, model_name) {
  
  scores <- c(
    RMSE(data$SA_cm3, predict(model, data)),
    R2(data$SA_cm3, predict(model, data))
  )
  
  result <- data.frame(
    model = model_name,
    type = type,
    scoreType = c("RMSE", "R2"),
    split = split,
    score = scores,
    stringsAsFactors = TRUE
  )
  
  return(result)
}

f <- train_models()

path = "/scratch/dongelr1/susannar/kesa2024/model_script_models.RData"
fits <- f[[2]]
save(fits, file = path)

path = "/scratch/dongelr1/susannar/kesa2024/model_script_test_df.RData"
score_df <- f[[1]]
save(score_df, file = path)

# load(file = path)
# 
# plot_scores <- function(data, metric) {
#   
#   data <- subset(data, scoreType == metric)
#   breaks <- seq(0, max(data$score), length.out = 10)
#   limits <- c(0, max(data$score))
#   if (metric == "R2") {
#     breaks <- seq(0, 1, 0.25)
#     limits <- c(0, 1.05)
#   }
#   
#   
#   test_data <- subset(data, scoreType == metric & split == "Test")
#   train_data <- subset(data, scoreType == metric & split == "Train")
#   
#   p <- ggplot(data = test_data, aes(x = type, y = score, fill = model)) +
#     geom_bar(stat = "identity", position = "dodge", color = "black", show.legend = c(fill = TRUE)) +
#     geom_point(data = train_data, shape = 4, color = "black", aes(x = type, y = score, fill = model), position = position_dodge(0.9), size = 3) +
#     scale_y_continuous(breaks = breaks, limits = limits) +
#     ggtitle(metric) +
#     scale_fill_manual(values = setNames(c(color_palette[2], color_palette[4]), c("rf", "lm")),
#                       breaks = c("rf", "lm")) +
#     guides(fill = guide_legend(override.aes = list(pattern = c("none", "none")))) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     theme(panel.border=element_rect(linetype=1, fill=NA)) +
#     labs(x = NULL, y = NULL) +
#     theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + 
#     geom_text(aes(label = sprintf("%.2f", score)), position = position_dodge(width = 0.9), vjust = -0.3)  
#   
#   return(p + theme(legend.position = "bottom"))
# }
# 
# color_palette <- brewer.pal(n = 4, name = "Dark2")
# 
# 
# plot_scores(g, "R2")
# plot_scores(g, "RMSE")

print(round(Sys.time() - start.time, 2))
print("DONE")

#f[["scores"]] <- df

stopCluster(cl)