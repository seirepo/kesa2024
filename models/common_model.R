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

p_val = 0.75

if (test_run) {
  p_val <- 0.01
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

train_models <- function(dset_list, p_val) {
  
  model_list <- list()
  mod_names <- names(dset_list)
  learning_curve <- data.frame()
  
  start.time <- Sys.time()
  
  for (i in 1:length(dset_list)) {
    print(mod_names[[i]])
    # Splitting the data to train and test
    split <- initial_split(dset_list[[i]], prop = p_val, strata = SA_cm3)
    train <- training(split)
    test <- testing(split)
    
    folds <- createFolds(train$SA_cm3, k = 5) # define indices for the folds
    
    trainControl <- trainControl(method="repeatedcv", 
                                 number = 5, # number of folds in each round of cross-validation
                                 repeats = 5, # how many times the cross-validation is repeated
                                 index = folds,
                                 savePredictions="final",
                                 # verboseIter = TRUE,
                                 returnData = TRUE,
                                 trim = TRUE
    )
    
    rangerGrid <- expand.grid(
      mtry = seq(1, ncol(train)-1, 1),
      splitrule = c("variance", "extratrees"),
      # splitrule = c("extratrees"),
      min.node.size = c(3, 5, 8, 12, 18)
    )
    
    modelTypes <- list(
      rf     = caretModelSpec(method = "ranger", tuneGrid = rangerGrid),
      lm     = caretModelSpec(method = "lm")
    )
    
    model_names <- names(modelTypes)
    
    # Fit the models
    models <- caretList(
      SA_cm3 ~ ., data=train,
      trControl = trainControl,
      metric = "RMSE",
      tuneList = modelTypes#,
      # preProcess =  c("center", "scale")
    )
    
    model_list[[i]] <- models
    model_list[[i]]$testData <- test
    model_list[[i]]$trainData <- train
    model_list[[i]]$datasetName <- mod_names[[i]]
    
    # Calculate scores for test and train data and combine them
    for (j in model_names) {
      scores <- calculate_scores(models[j], train, mod_names[i], "Train", j)
      scores_test <- calculate_scores(models[j], test, mod_names[i], "Test", j)
      df <- rbind(df, scores, scores_test)
    }
    
    ## Calculate learning curves for both models and combine them ##
    # The custom fold indíces conflict with the learning curve calculation with some of the rangerGrid values.
    # if the fold indices are defined in trainControl, remove indices to calculate learning curves for the random forest model
    trainControl_no_index <- trainControl
    trainControl_no_index$index <- NULL
    
    lm_dat <- learning_curve_dat(
      dat = train,
      outcome = "SA_cm3",
      test_prop = 0.25,
      method = "lm",
      metric = "RMSE",
      trControl = trainControl
    )

    rf_dat <- learning_curve_dat(
      dat = train,
      outcome = "SA_cm3",
      test_prop = 0.25,
      method = "ranger",
      metric = "RMSE",
      trControl = trainControl_no_index
    )

    lm_dat$method = "lm"
    rf_dat$method = "rf"
    learning_curve <- rbind(lm_dat, rf_dat)
    # learning_curve <- data.frame(method = c("lm", "rf"))
    
  }
  
  print(round(Sys.time() - start.time, 2))
  print("DONE")
  
  return(list(scores = df, fits = model_list, learning_curves = learning_curve))
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
  dat <- read.csv(path, stringsAsFactors = FALSE) %>% drop_na
  dat$Time <- lubridate::ymd_hms(dat$Time, tz = "UTC", truncated = 3)
  print(paste("Data loaded from", path))
  
  return(dat)
}

save_results <- function(results, fit_path, score_df_path, lc_df_path, dataset_name) {
  fits <- results$fits
  fits$dataset_name <- dataset_name
  saveRDS(fits, file = fit_path)
  print(paste("Model saved to", fit_path))
  
  score_df <- results$scores
  score_df$dataset_name <- c(dataset_name)
  saveRDS(score_df, file = score_df_path)
  print(paste("Score df saved to", score_df_path))
  
  lc_df <- results$learning_curves
  lc_df$dataset_name <- c(dataset_name)
  saveRDS(lc_df, file = lc_df_path)
  print(paste("Learning curve df saved to", lc_df_path))
}


fit_models <- function(data_path, p_val, excluded_features, score_target, fit_target, lc_target) {
  
  if (!file.exists(fit_target)) {
    dir.create(fit_target)
    print(paste("Created dir", fit_target))
  }
  
  if (!file.exists(score_target)) {
    dir.create(score_target)
    print(paste("Created dir", score_target))
  }
  
  if (!file.exists(lc_target)) {
    dir.create(lc_target)
    print(paste("Created dir", lc_target))
  }

  dataset_name <- tools::file_path_sans_ext(basename(data_path))
  score_path <- file.path(score_target, paste0(dataset_name, ".rds"))
  fit_path <- file.path(fit_target, paste0(dataset_name, ".rds"))
  lc_path <- file.path(lc_target, paste0(dataset_name, ".rds"))

  print(paste("Fitting models on data", dataset_name))
  
  dat <- load_data(data_path) %>% dplyr::select(-all_of(excluded_features))

  dset_list <- list(dat)
  names(dset_list) <- list(dataset_name)

  results <- train_models(dset_list, p_val)
  
  if (!test_run) {
    save_results(results, fit_path = fit_path, score_df_path = score_path, lc_df_path = lc_path, dataset_name)
  }

}


train_models_with_outlier_filtering <- function() {
  data_dir_hyy <- "data/hyytiala/preprocessed"
  data_dir_bei <- "data/beijing/preprocessed"
  
  files_hyy <- list.files(path = data_dir_hyy, full.names = TRUE)
  files_bei <- list.files(path = data_dir_bei, full.names = TRUE)
  
  path_h <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
  subd_h <- "same_features_as_beijing_test"
  
  purrr::map(files_hyy, fit_models,
             excluded_features = c("Time", "sector.clean", "sector.east", "sector.europe", "sector.mixed", "air_pressure", "global_radiation", "hour_sin", "hour_cos"),
             # score_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/same_features_as_beijing",
             # fit_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing",
             # lc_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/learning_curves/same_features_as_beijing",
             score_target = file.path(path_h, "scores", subd_h),
             fit_target = file.path(path_h, "fitted_models", subd_h),
             lc_target = file.path(path_h, "learning_curves", subd_h),
             p_val = p_val)
  
  path_b <- "/scratch/dongelr1/susannar/kesa2024/results/beijing"
  subd_b <- "same_features_as_hyy_test"
  
  purrr::map(files_bei, fit_models, excluded_features = c("Time", "sector", "hour_sin", "hour_cos"),
             # score_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/same_features_as_hyy",
             # fit_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy",
             # lc_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/learning_curves/same_features_as_hyy",
             score_target = file.path(path_b, "scores", subd_b),
             fit_target = file.path(path_b, "fitted_models", subd_b),
             lc_target = file.path(path_b, "learning_curves", subd_b),
             p_val = p_val)
  
  print("Models with outlier filtering done")
}

train_models_without_outlier_filtering <- function() {
  data_dir_hyy <- "data/hyytiala/preprocessed_no_outlier_filtering"
  data_dir_bei <- "data/beijing/preprocessed_no_outlier_filtering"
  
  files_hyy <- list.files(path = data_dir_hyy, full.names = TRUE)
  files_bei <- list.files(path = data_dir_bei, full.names = TRUE)
  
  path_h <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
  subd_h <- "same_features_as_beijing_no_outlier_filtering"
  
  purrr::map(files_hyy, fit_models,
             excluded_features = c("Time", "sector.clean", "sector.east", "sector.europe", "sector.mixed", "air_pressure", "global_radiation", "hour_sin", "hour_cos"),
             # score_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/same_features_as_beijing_no_outlier_filtering",
             # fit_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing_no_outlier_filtering",
             # lc_target = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/learning_curves/same_features_as_beijing_no_outlier_filtering",
             score_target = file.path(path_h, "scores", subd_h),
             fit_target = file.path(path_h, "fitted_models", subd_h),
             lc_target = file.path(path_h, "learning_curves", subd_h),
             p_val = p_val)
  
  path_b <- "/scratch/dongelr1/susannar/kesa2024/results/beijing"
  subd_b <- "same_features_as_hyy_no_outlier_filtering"
  
  purrr::map(files_bei, fit_models, excluded_features = c("Time", "sector", "hour_sin", "hour_cos"),
             # score_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/same_features_as_hyy_no_outlier_filtering",
             # fit_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy_no_outlier_filtering",
             # lc_target = "/scratch/dongelr1/susannar/kesa2024/results/beijing/learning_curves/same_features_as_hyy_no_outlier_filtering",
             score_target = file.path(path_b, "scores", subd_b),
             fit_target = file.path(path_b, "fitted_models", subd_b),
             lc_target = file.path(path_b, "learning_curves", subd_b),
             p_val = p_val)
  
  print("Models without outlier filtering done")
}

# The default is to filter outliers
train_models_with_outlier_filtering()
# train_models_without_outlier_filtering()

print(round(Sys.time() - start.time, 2))
print("DONE")

end.time <- Sys.time()
print(paste("Finish at", end.time))

if (!test_run) {
  stopCluster(cl)
}

