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

# if (!test_run) {
#   cl <- makePSOCKcluster(parallelly::availableCores())
#   registerDoParallel(cl)
#   clusterEvalQ(cl, .libPaths("/scratch/dongelr1/laantito/"))
#   
#   print("Cores:")
#   print(parallelly::availableCores())
# }

########################################################
##################### Prepare data #####################
########################################################

# Calculate the reaction rate (as in Eq. 3 in paper A statistical proxy for sulphuric acid concentration by Mikkonen et al.)
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

# Create datasets to train the different models.
create_datasets <- function(dat) {
  dat_filtered <- dat %>% filter(global_radiation > 10 & SO2 > 0.1)
  k <- calc_reaction_constant(dat_filtered)
  all_features <- dat_filtered %>% mutate(k = k) %>%
    mutate(x1 = k * global_radiation * SO2 / CS_rate) %>%
    mutate(x2 = k * global_radiation * SO2) %>%
    mutate(x3 = k * global_radiation * SO2^0.5) %>%
    mutate(x4 = k * global_radiation * SO2 / relative_humidity) %>%
    mutate(x5 = k * global_radiation * SO2 / (CS_rate * relative_humidity)) %>%
    dplyr::select(-k)
  
  all_proxies <- all_features %>% dplyr::select(SA_cm3, x1, x2, x3, x4, x5)
  
  l1 <- dplyr::select(all_proxies, SA_cm3, x1)
  l2 <- dplyr::select(all_proxies, SA_cm3, x2)
  l3 <- dplyr::select(all_proxies, SA_cm3, x3)
  l4 <- dplyr::select(all_proxies, SA_cm3, x4)
  l5 <- dplyr::select(all_proxies, SA_cm3, x5)
  
  l <- list(l1, l2, l3, l4, l5, all_proxies, dat_filtered, all_features, dat)
  names(l) <- list("l1", "l2", "l3", "l4", "l5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
  return(l)
}

# Same but for log data, returning the transformed reaction rate
calc_reaction_constant_log_data <- function(dat) {
  temp_K_exp <- exp(dat$temp_K)
  M <- 0.101 * (1.381 * 1e-23 * temp_K_exp)^-1
  k1 <- 4e-31
  k2 <- 3.3
  k3 <- 2e-12
  k5 <- -0.8
  A <- k1 * M * (300 / temp_K_exp)^k2
  k <- A * k3 / (A + k3) * exp(k5 * (1 + log10(A / k3)^2)^-1)
  return(log(k))
}

create_datasets_log_data <- function(dat) {
  dat_filtered <- dat %>% filter(global_radiation > log(10) & SO2 > log(0.1)) # filtered data
  k <- calc_reaction_constant_log_data(dat_filtered)
  all_features <- dat_filtered %>% mutate(k = k) %>%
    mutate(x1 = k + global_radiation + SO2 - CS_rate) %>%
    mutate(x2 = k + global_radiation + SO2) %>%
    mutate(x3 = k + global_radiation + SO2 / 2) %>%
    mutate(x4 = k + global_radiation + SO2 - relative_humidity) %>%
    mutate(x5 = k + global_radiation + SO2 - (CS_rate + relative_humidity)) %>%
    dplyr::select(-k)
  
  all_proxies <- all_features %>% dplyr::select(SA_cm3, x1, x2, x3, x4, x5)
  
  l1 <- dplyr::select(all_proxies, SA_cm3, x1)
  l2 <- dplyr::select(all_proxies, SA_cm3, x2)
  l3 <- dplyr::select(all_proxies, SA_cm3, x3)
  l4 <- dplyr::select(all_proxies, SA_cm3, x4)
  l5 <- dplyr::select(all_proxies, SA_cm3, x5)
  
  l <- list(l1, l2, l3, l4, l5, all_proxies, dat_filtered, all_features, dat)
  names(l) <- list("l1", "l2", "l3", "l4", "l5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
  return(l)
}


#########################################################

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

names <- c("L1", "L2", "L3", "L4", "L5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
# model_names <- c("rf", "lm")

train_models <- function(dset_list) {

  model_list <- list()
  
  start.time <- Sys.time()
  
  for (i in 1:length(dset_list)) {
    print(names[[i]])
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

save_results <- function(results, fit_path, score_df_path) {
  print(paste("Try to save fit to", fit_path))
  fits <- results[[2]]
  saveRDS(fits, file = fit_path)
  print(paste("Fit saved to", fit_path))
  
  print(paste("Try to save scores to", score_df_path))
  score_df <- results[[1]]
  saveRDS(score_df, file = score_df_path)
  print(paste("Scores saved to", score_df_path))
}

# Paths for downloading the data
data_paths <- list(
  "data/hyytiala/preprocessed/hyy_data.csv",
  "data/hyytiala/preprocessed/hyy_data_f30.csv"
)

log_data_paths <- list(
  "data/hyytiala/preprocessed/hyy_data_logtransformed.csv",
  "data/hyytiala/preprocessed/hyy_data_logtransformed_f30.csv"
)

# Paths to save the results to
model_path <- list(
  "model_results/model_script_fitted_models_std.rds",
  "model_results/fitted_models_f30_std.rds"
)

score_path <- list(
  "model_results/model_script_score_df_std.rds",
  "model_results/score_df_f30_std.rds"
)

log_model_path <- list(
  "model_results/fitted_models_log_std.rds",
  "model_results/fitted_models_log_f30_std.rds"
)

log_score_path <- list(
  "model_results/score_df_log_std.rds",
  "model_results/score_df_log_f30_std.rds"
)

target_paths <- rlist::list.zip(model_path, score_path)
log_target_paths <- rlist::list.zip(log_model_path, log_score_path)

fit_model_on_std_data <- function(data_path, target_path, log_dat) {
  fit_path <- target_path[[1]]
  score_path <- target_path[[2]]
  
  print(fit_path)
  print(score_path)
  
  dat <- read.csv(data_path, stringsAsFactors = FALSE) %>% drop_na() %>% dplyr::select(-Time) %>% dplyr::select(-sector.mixed)
  print(paste("Data loaded from", data_path))
  
  if (log_dat) {
    dset_list <- create_datasets_log_data(dat)
  } else {
    dset_list <- create_datasets(dat)
  }

  # Standardize the data to have mean 0 and standard deviation 1
  standardized_dset_list <- lapply(dset_list, FUN = function(x) {
    cols <- x %>% select_if(!(colnames(.) %in% c("sector.clean", "sector.europe", "sector.east", "SA_cm3"))) %>% colnames;
    x[cols] <- scale(x[cols]);
    # print(paste("standardized columns:", paste(cols, collapse = ", ")));
    x
  })
  
  results <- train_models(standardized_dset_list)
  
  if (test_run) {
    save_results(results, fit_path = fit_path, score_df_path = score_path)
  }
  
}

purrr::map2(data_paths, target_paths, fit_model_on_std_data, log_dat = FALSE)
purrr::map2(log_data_paths, log_target_paths, fit_model_on_std_data, log_dat = TRUE)
print(round(Sys.time() - start.time, 2))
print("DONE")

# if (!test_run) {
#   stopCluster(cl)
# }