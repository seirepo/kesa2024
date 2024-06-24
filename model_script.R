library(caret)
# library(plyr)
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

# Calculate the reaction rate constant k (as in Eq. 3 in paper A statistical proxy for sulphuric acid concentration by Mikkonen et al.)
calc_reaction_constant <- function(dat) {
  M <- 0.101 * (1.381 * 1e-23 * dat$temp_K)^-1
  k1 <- 4e-31
  k2 <- 3.3
  k3 <- 2e-12
  k5 <- -0.8
  A <- k1 * M * (300 / dat$temp_K)^k2
  k <- A * k3 / (A + k3) * exp(k5 * (1 + log10(A / k3)^2)^-1)
  return(k)
}

# TODO: Figure out why the fits where the data contains all proxies is doubled: there are now two rf fits and two linear fits for some reason -> rerunning the code fixes the issue

# Create datasets for all of the proxies separately, all proxies combined, filtered data without and with proxies and the unfiltered data.
# The proxies are defined as in paper A statistical proxy for sulphuric acid concentration by Mikkonen et al.
create_datasets <- function(dat) {
  # dat remains as the unfiltered data set
  dat_filtered <- dat %>% filter(global_radiation > 10 & SO2 > 0.1) # filtered data
  k <- calc_reaction_constant(dat_filtered)
  all_features <- dat_filtered %>% mutate(k = k) %>%
    mutate(x1 = k * global_radiation * SO2 / CS_rate) %>%
    mutate(x2 = k * global_radiation * SO2) %>%
    mutate(x3 = k * global_radiation * SO2^0.5) %>%
    mutate(x4 = k * global_radiation * SO2 / relative_humidity) %>%
    mutate(x5 = k * global_radiation * SO2 / (CS_rate * relative_humidity)) %>%
    dplyr::select(-k)
  
  all_proxies <- all_features %>% dplyr::select(SA_cm3, x1, x2, x3, x4, x5)
  
  l1 <- dplyr::select(all_proxies, SA_cm3, x1) #%>% drop_na
  l2 <- dplyr::select(all_proxies, SA_cm3, x2) #%>% drop_na
  l3 <- dplyr::select(all_proxies, SA_cm3, x3) #%>% drop_na
  l4 <- dplyr::select(all_proxies, SA_cm3, x4) #%>% drop_na
  l5 <- dplyr::select(all_proxies, SA_cm3, x5) #%>% drop_na
  
  # dat <- dat %>% drop_na
  # dat_filtered <- dat_filtered %>% drop_na
  # all_proxies <- all_proxies %>% drop_na
  # all_features <- all_features %>% drop_na
  
  l <- list(l1, l2, l3, l4, l5, all_proxies, dat_filtered, all_features, dat)
  names(l) <- list("l1", "l2", "l3", "l4", "l5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
  return(l)
}

# Create datasets from the data that is removed for the actual model to see if the models are good for the data filtered out
# create_datasets_for_removed_data <- function(dat) {
#   dat_filtered <- dat %>% filter(global_radiation <= 10 | SO2 <= 0.1) 
#   k <- calc_reaction_constant(dat_filtered)
#   all_features <- dat_filtered %>% mutate(k = k) %>%
#     mutate(x1 = k * global_radiation * SO2 / CS_rate) %>%
#     mutate(x2 = k * global_radiation * SO2) %>%
#     mutate(x3 = k * global_radiation * SO2^0.5) %>%
#     mutate(x4 = k * global_radiation * SO2 / relative_humidity) %>%
#     mutate(x5 = k * global_radiation * SO2 / (CS_rate * relative_humidity)) %>%
#     dplyr::select(-k)
#   
#   all_proxies <- all_features %>% dplyr::select(SA_cm3, x1, x2, x3, x4, x5)
#   
#   l1 <- dplyr::select(all_proxies, SA_cm3, x1) #%>% drop_na
#   l2 <- dplyr::select(all_proxies, SA_cm3, x2) #%>% drop_na
#   l3 <- dplyr::select(all_proxies, SA_cm3, x3) #%>% drop_na
#   l4 <- dplyr::select(all_proxies, SA_cm3, x4) #%>% drop_na
#   l5 <- dplyr::select(all_proxies, SA_cm3, x5) #%>% drop_na
#   
#   l <- list(l1, l2, l3, l4, l5, all_proxies, dat_filtered, all_features, dat)
#   names(l) <- list("l1", "l2", "l3", "l4", "l5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
#   return(l)
# }

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

names <- c("L1", "L2", "L3", "L4", "L5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
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
                                 # verbose = TRUE,
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
      SA_cm3 ~ ., data=train,
      trControl=trainControl,
      metric = "RMSE",
      tuneList = modelTypes#,
      # preProcess =  c("center", "scale")
    )
    
    model_list[[i]] <- models
    model_list[[i]]$testData <- test
    model_list[[i]]$trainData <- train
    model_list[[i]]$datasetName <- names[[i]]
    
    for (j in model_names) {
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


# Load data, drop NA values
dat <- read.csv("data/all_data_merged.csv", stringsAsFactors = FALSE) %>% drop_na() %>% dplyr::select(-Time)

dset_list <- create_datasets(dat)
# dset_list <- create_datasets_for_removed_data(dat)

results <- train_models()

path = "/scratch/dongelr1/susannar/kesa2024/model_script_fitted_models.RData"
fits <- results[[2]]
save(fits, file = path)

path = "/scratch/dongelr1/susannar/kesa2024/model_script_score_df.RData"
score_df <- results[[1]]
save(score_df, file = path)


# path = "/scratch/dongelr1/susannar/kesa2024/model_script_fitted_models_preprocessed.RData"
# fits <- results[[2]]
# save(fits, file = path)
# 
# path = "/scratch/dongelr1/susannar/kesa2024/model_script_score_df_preprocessed.RData"
# score_df_preprocessed <- results[[1]]
# save(score_df_preprocessed, file = path)


# path = "/scratch/dongelr1/susannar/kesa2024/model_script_fitted_models_removed_data.RData"
# fits <- results[[2]]
# save(fits, file = path)
# 
# path = "/scratch/dongelr1/susannar/kesa2024/model_script_score_df_removed_data.RData"
# score_df <- results[[1]]
# save(score_df, file = path)

print(round(Sys.time() - start.time, 2))
print("DONE")

stopCluster(cl)