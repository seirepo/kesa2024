library(caret)
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

########################
##### SET TEST RUN #####
########################
test_run <- FALSE
########################

start.time <- Sys.time()
print(paste("Start at", start.time))

if (!test_run) {
  cl <- makePSOCKcluster(parallelly::availableCores())
  registerDoParallel(cl)
  clusterEvalQ(cl, .libPaths("/scratch/dongelr1/laantito/"))
  
  print("Cores:")
  print(parallelly::availableCores())
}


# Create datasets similarly as in Hyytiälä case.
# The proxies are such as in paper A statistical proxy for sulphuric acid concentration by Mikkonen et al.
create_datasets <- function(dat_filtered, dat) {
  dsets <- list(dat_filtered, dat)
  names(dsets) <- list("all_features_filtered", "all_features_unfiltered")
  return(dsets)
}

create_dataset_with_proxies <- function(dat) {
  all_proxies <- dat_filtered %>% dplyr::select(SA_cm3, x1, x2, x3, x4, x5)
  
  l1 <- dplyr::select(all_proxies, SA_cm3, x1)
  l2 <- dplyr::select(all_proxies, SA_cm3, x2)
  l3 <- dplyr::select(all_proxies, SA_cm3, x3)
  l4 <- dplyr::select(all_proxies, SA_cm3, x4)
  l5 <- dplyr::select(all_proxies, SA_cm3, x5)
  
  proxy_datasets <- list(l1, l2, l3, l4, l5, all_proxes)
  return(proxy_datasets)
}

p_val = 0.75
# p_vals = c(0.65, 0.75, 0.8)

if (test_run) {
  p_val <- 0.01
  # p_vals <- c(0.01, 0.02, 0.03)
}

print(paste("test-train split size", p_val))

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

# names <- c("L1", "L2", "L3", "L4", "L5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
# model_names <- c("rf", "lm")

train_models <- function(dset_list, p_val) {
  
  model_list <- list()
  mod_names <- names(dset_list)
  
  start.time <- Sys.time()
  
  for (i in 1:length(dset_list)) {
    print(mod_names[[i]])
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
      # splitrule = c("extratrees"),
      min.node.size = c(3, 5, 8, 12, 18)
    )
    
    modelTypes <- list(
      rf     = caretModelSpec(method="ranger", tuneGrid = rangerGrid),
      lm     = caretModelSpec(method="lm")
    )
    
    model_names <- names(modelTypes)
    
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
    model_list[[i]]$datasetName <- mod_names[[i]]
    
    for (j in model_names) {
      scores <- calculate_scores(models[j], train, mod_names[i], "Train", j)
      scores_test <- calculate_scores(models[j], test, mod_names[i], "Test", j)
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
    R2(data$SA_cm3, predict(model, data))#,
    # rmsle(data$SA_cm3, predict(model, data))
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

load_data <- function(path) {
  # Drop sector, there was only one of them
  dat <- read.csv(path, stringsAsFactors = FALSE) %>% drop_na
  dat$Time <- lubridate::ymd_hms(dat$Time, tz = "UTC", truncated = 3)
  print(paste("Data loaded from", path))
  
  return(dat)
}

save_results <- function(results, fit_path, score_df_path, dataset_name) {
  fits <- results[[2]]
  fits$dataset_name <- dataset_name
  saveRDS(fits, file = fit_path)
  print(paste("Model saved to", fit_path))
  
  score_df <- results[[1]]
  score_df$dataset_name <- c(dataset_name)
  saveRDS(score_df, file = score_df_path)
  print(paste("Score df saved to", score_df_path))
}


# target_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
score_target_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/same_features_as_beijing_no_outlier_filtering"
fit_target_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing_no_outlier_filtering"
# target_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing"
score_target_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/same_features_as_hyy_no_outlier_filtering"
fit_target_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy_no_outlier_filtering"

# data_dir_hyy <- "data/hyytiala/preprocessed"
# # Hyytiälä
# dat_hyy_og <- load_data(data_path) %>% dplyr::select(-Time, -sector.mixed, -UVB) # The original Hyytiälä features
# dat_hyy <- load_data(data_path) %>% dplyr::select(-Time, -sector.clean, -sector.east, -sector.europe, -sector.mixed, -air_pressure, -global_radiation) # Same features as in Beijing data
# # Beijing
# dat_bei <- load_data(data_path) %>% dplyr::select(-Time, -sector) # Same features asn in Hyytiälä data and also original Beijing features because there was only single sector

# dat_hyy_og <- lapply(files_hyy, function(file_path) load_data(file_path)) %>% lapply(function(df) dplyr::select(df, -Time, -sector.mixed, -UVB))
# dataset_names_hyy <- lapply(files_hyy, function(file_path) tools::file_path_sans_ext(basename(file_path)))
# names(dat_hyy_og) = dataset_names_hyy

# Train on Hyytiälä data, both datasets?


# fit_models <- function(data_path, p_val, excluded_features, target_dir, sub_dir) {
fit_models <- function(data_path, p_val, excluded_features, score_target, fit_target) {
  print(p_val)
  # target_fit <- file.path(target_dir, sub_dir, "fitted_models")
  # target_score <- file.path(target_dir, sub_dir, "score")
  # 
  # if (!test_run) {
  #   if (!dir.exists(target_fit)) {
  #     dir.create(target_fit, recursive = TRUE)
  #     print(paste("Created directory", target_fit))
  #   }
  # 
  #   if (!dir.exists(target_score)) {
  #     dir.create(target_score, recursive = TRUE)
  #     print(paste("Created directory", target_score))
  #   }
  # }

  dataset_name <- tools::file_path_sans_ext(basename(data_path))
  fit_path <- file.path(fit_target, paste0(dataset_name, ".rds"))
  score_path <- file.path(score_target, paste0(dataset_name, ".rds"))

  dat <- load_data(data_path) %>% dplyr::select(-all_of(excluded_features))

  # dset_list <- create_datasets(dat)
  dset_list <- list(dat)
  names(dset_list) <- list(dataset_name)

  results <- train_models(dset_list, p_val)

  if (!test_run) {
    # save_results(results, fit_path = fit_path, score_df_path = score_path, dataset_name)
    save_results(results, fit_path = fit_path, score_df_path = score_path, dataset_name)
  }

}

data_dir_hyy <- "data/hyytiala/preprocessed_no_outlier_filtering"
data_dir_bei <- "data/beijing/preprocessed_no_outlier_filtering"

files_hyy <- list.files(path = data_dir_hyy, full.names = TRUE)
files_bei <- list.files(path = data_dir_bei, full.names = TRUE)

purrr::map(files_hyy, fit_models,
           excluded_features = c("Time", "sector.clean", "sector.east", "sector.europe", "sector.mixed", "air_pressure", "global_radiation"),
           score_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/same_features_as_beijing_no_outlier_filtering",
           fit_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing_no_outlier_filtering",
           p_val = p_val)

purrr::map(files_bei, fit_models, excluded_features = c("Time", "sector"),
           score_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/same_features_as_hyy_no_outlier_filtering",
           fit_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy_no_outlier_filtering",
           p_val = p_val)

print("Models without outlier filtering done")


data_dir_hyy <- "data/hyytiala/preprocessed"
data_dir_bei <- "data/beijing/preprocessed"

files_hyy <- list.files(path = data_dir_hyy, full.names = TRUE)
files_bei <- list.files(path = data_dir_bei, full.names = TRUE)

purrr::map(files_hyy, fit_models,
           excluded_features = c("Time", "sector.clean", "sector.east", "sector.europe", "sector.mixed", "air_pressure", "global_radiation"),
           score_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/same_features_as_beijing",
           fit_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing",
           p_val = p_val)

purrr::map(files_bei, fit_models, excluded_features = c("Time", "sector"),
           score_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/same_features_as_hyy",
           fit_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy",
           p_val = p_val)

print("Models with outlier filtering done")



# purrr::map(files_hyy, fit_models,
#            excluded_features = c("Time", "sector.clean", "sector.east", "sector.europe", "sector.mixed", "air_pressure", "global_radiation"),
#            target_dir = target_dir_hyy,
#            sub_dir = "same_features_as_in_beijing_data",
#            p_val = p_val)
# 
# purrr::map(files_bei, fit_models, excluded_features = c("Time", "sector"),
#            target_dir = target_dir_bei,
#            sub_dir = "same_features_as_in_hyytiala_data",
#            p_val = p_val)


# Tests #

# fit_target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models"
# score_target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores"
# 
# fit_models <- function(data_path) {
#   
#   dataset_name <- tools::file_path_sans_ext(basename(data_path))
#   fit_path <- file.path(fit_target_dir, paste0(dataset_name, ".rds"))
#   score_path <- file.path(score_target_dir, paste0(dataset_name, ".rds"))
#   
#   # dat <- load_data(data_path) %>% dplyr::select(-Time, -sector.mixed, -UVB)
#   dat <- load_data(data_path) %>% dplyr::select(-Time, -sector.clean, -sector.east, -sector.europe, -sector.mixed, -air_pressure, -global_radiation) # Same features as in beijing data
#   
#   dataset_name <- tools::file_path_sans_ext(basename(data_path))
#   dset_list <- list(dat)
#   names(dset_list) <- list(dataset_name)
#   
#   results <- train_models(dset_list, p_val)
#   
#   if (!test_run) {
#     save_results(results, fit_path = fit_path, score_df_path = score_path, dataset_name = dataset_name)
#   }
# }
# 
# data_dir <- "data/hyytiala/preprocessed"
# files <- list.files(path = data_dir, full.names = TRUE)
# # print(files)
# 
# purrr::map(files, fit_models)

######

# Fit model with different train-test split sizes
# fit_models <- function(data_path, p_vals, excluded_features, target_dir, sub_dir) {
#   target_fit <- file.path(target_dir, sub_dir, paste("fitted_models", sep = "_"))
#   target_score <- file.path(target_dir, sub_dir, paste("scores", sep = "_"))
#   
#   if (!test_run) {
#     if (!dir.exists(target_fit)) {
#       dir.create(target_fit, recursive = TRUE)
#       print(paste("Created directory", target_fit))
#     }
#     
#     if (!dir.exists(target_score)) {
#       dir.create(target_score, recursive = TRUE)
#       print(paste("Created directory", target_score))
#     }
#   }
#   
#   dataset_name <- tools::file_path_sans_ext(basename(data_path))
#   
#   # Load data from path
#   dat <- load_data(data_path) %>% dplyr::select(-all_of(excluded_features))
#   
#   for (p_val in p_vals) {
#     print(paste("Fitting for split", p_val))
#     dset_list <- list(dat)
#     dset_name <- paste(dataset_name, p_val, sep = "_")
#     names(dset_list) <- list(dset_name)
#     
#     fit_path <- file.path(target_fit, paste0(dset_name, ".rds"))
#     score_path <- file.path(target_score, paste0(dset_name, ".rds"))
#     
#     results <- train_models(dset_list, p_val)
#     
#     if (!test_run) {
#       save_results(results, fit_path = fit_path, score_df_path = score_path, dataset_name = dataset_name)
#     }
#   }
# }
# 
# files_hyy <- list.files(path = data_dir_hyy, full.names = TRUE)
# files_bei <- list.files(path = data_dir_bei, full.names = TRUE)
# 
# purrr::map(files_hyy, fit_models,
#            excluded_features = c("Time", "sector.mixed", "UVB"),
#            target_dir = target_dir_hyy,
#            sub_dir = "original_features",
#            p_vals = p_vals)
# purrr::map(files_hyy, fit_models,
#            excluded_features = c("Time", "sector.clean", "sector.east", "sector.europe", "sector.mixed", "air_pressure", "global_radiation"),
#            target_dir = target_dir_hyy,
#            sub_dir = "same_features_as_in_beijing_data",
#            p_vals = p_vals)
# purrr::map(files_bei, fit_models, excluded_features = c("Time", "sector"),
#            target_dir = target_dir_bei,
#            sub_dir = "same_features_as_in_hyytiala_data",
#            p_vals = p_vals)



print(round(Sys.time() - start.time, 2))
print("DONE")

end.time <- Sys.time()
print(paste("Finish at", end.time))

if (!test_run) {
  stopCluster(cl)
}

