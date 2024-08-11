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

# Calculate the reaction rate k (as in Eq. 3 in paper A statistical proxy for sulphuric acid concentration by Mikkonen et al.)
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
create_datasets_for_removed_data <- function(dat) {
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

  l1 <- dplyr::select(all_proxies, SA_cm3, x1) #%>% drop_na
  l2 <- dplyr::select(all_proxies, SA_cm3, x2) #%>% drop_na
  l3 <- dplyr::select(all_proxies, SA_cm3, x3) #%>% drop_na
  l4 <- dplyr::select(all_proxies, SA_cm3, x4) #%>% drop_na
  l5 <- dplyr::select(all_proxies, SA_cm3, x5) #%>% drop_na

  l <- list(l1, l2, l3, l4, l5, all_proxies, dat_filtered, all_features, dat)
  names(l) <- list("l1", "l2", "l3", "l4", "l5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
  print("Dataset created for data removed in the filtering process")
  return(l)
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

names <- c("L1", "L2", "L3", "L4", "L5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
# model_names <- c("rf", "lm")

train_models <- function(dset_list) {
  
  model_list <- list()
  
  start.time <- Sys.time()
  
  for (i in 1:length(dset_list)) {
    print(names(dset_list)[[i]])
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
      # min.node.size = seq(10, 200, 10)
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
    R2(data$SA_cm3, predict(model, data)),
    rmsle(data$SA_cm3, predict(model, data))
  )
  
  result <- data.frame(
    model = model_name,
    type = type,
    scoreType = c("RMSE", "R2", "RMSLE"),
    split = split,
    score = scores,
    stringsAsFactors = TRUE
  )
  
  return(result)
}


save_results <- function(results, fit_path, score_df_path, dataset_name) {
  # path = "/scratch/dongelr1/susannar/kesa2024/model_script_fitted_models.RData"
  fits <- results[[2]]
  fits$dataset_name <- dataset_name
  saveRDS(fits, file = fit_path)
  print(paste("Fit saved to", fit_path))
  
  # path = "/scratch/dongelr1/susannar/kesa2024/model_script_score_df.RData"
  score_df <- results[[1]]
  score_df$dataset_name <- score_df$type
  saveRDS(score_df, file = score_df_path)
  print(paste("Scores saved to", score_df_path))
}

# Load data, drop NA values
# path <- "data/all_data_merged.csv"
data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed/unfiltered.csv"
fit_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/all_feature_subsets/model_basic_fitted_models.rds"
score_df_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/all_feature_subsets/model_basic_score_df.rds"
paths1 <- list(data_path = data_path, fit_path = fit_path, score_df_path = score_df_path)

data_path <- "/scratch/dongelr1/susannar/kesa2024/data/hyytiala/preprocessed_no_outlier_filtering/unfiltered.csv"
fit_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/all_feature_subsets_no_outlier_filtering/model_basic_fitted_models.rds"
score_df_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/all_feature_subsets_no_outlier_filtering/model_basic_score_df.rds"
paths2 <- list(data_path = data_path, fit_path = fit_path, score_df_path = score_df_path)

paths_list <- list(paths1, paths2)

# fit_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/model_basic_fitted_models.rds"
# score_df_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/model_basic_score_df.rds"

# fit_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/model_basic_fitted_lin_models.rds"
# score_df_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/model_basic_lin_score_df.rds"

# Same dataset as above but with preprocessed SA
# path <- "data/all_data_merged_f30.csv"
# fit_path <- "/scratch/dongelr1/susannar/kesa2024/model_results/fitted_models_f30.rds"
# score_df_path <- "/scratch/dongelr1/susannar/kesa2024/model_results/score_df_f30.rds"


train_and_save <- function(data_path, fit_path, score_df_path) {
  # Drop column for sector.mixed to avoid dummy variable trap
  dat <- read.csv(data_path, stringsAsFactors = FALSE) %>% drop_na() %>% dplyr::select(-Time) %>% dplyr::select(-sector.mixed, -hour_sin, -hour_cos)
  print(paste("Data loaded from", data_path))
  
  # dataset_name <- tools::file_path_sans_ext(basename(data_path))
  dataset_name <- "all_feature_subsets"
  
  dset_list <- create_datasets(dat)
  # dset_list <- dset_list[1:5]
  # print(names(dset_list))
  # dset_list <- create_datasets_for_removed_data(dat)
  
  results <- train_models(dset_list)
  
  if (!test_run) {
    print("Fitting done, saving the results")
    save_results(results, fit_path = fit_path, score_df_path = score_df_path, dataset_name = dataset_name)
  }
}

for (paths in paths_list) {
  # print(paths)
  data_path <- paths$data_path
  fit_path <- paths$fit_path
  score_df_path <- paths$score_df_path
  
  train_and_save(data_path, fit_path, score_df_path)
}

# path = "/scratch/dongelr1/susannar/kesa2024/model_script_fitted_models_preprocessed.RData"
# fits <- results[[2]]
# save(fits, file = path)
# 
# path = "/scratch/dongelr1/susannar/kesa2024/model_script_score_df_preprocessed.RData"
# score_df_preprocessed <- results[[1]]
# save(score_df_preprocessed, file = path)

# 
# load("/scratch/dongelr1/susannar/kesa2024/model_script_score_df.RData")
# 
# plot_scores <- function(data, metric) {
#   data$score[data$scoreType == "RMSE"] <- data$score[data$scoreType == "RMSE"] / 1e6
# 
#   data <- subset(data, scoreType == metric)
#   breaks <- seq(0, 3, 0.5)
#   limits <- c(0, max(data$score))
#   if (metric == "R2") {
#     breaks <- seq(0, 1, 0.25)
#     limits <- c(0, 1.05)
#   }
# 
#   test_data <- subset(data, scoreType == metric & split == "Test")
#   train_data <- subset(data, scoreType == metric & split == "Train")
# 
#   p <- ggplot(data = test_data, aes(x = type, y = score, fill = model)) +
#     geom_bar(stat = "identity", position = "dodge", color = "black", show.legend = c(fill = TRUE)) +
#     geom_point(data = train_data, shape = 4, color = "black", aes(x = type, y = score, fill = model), position = position_dodge(0.9), size = 3) +
#     scale_y_continuous(breaks = breaks, limits = limits) +
#     ggtitle(metric) +
#     scale_fill_manual(values = setNames(color_palette, c("rf", "lm", "nnls")),
#                       breaks = c("rf", "lm", "nnls")) +
#     guides(fill = guide_legend(override.aes = list(pattern = c("none", "none", "none")))) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     theme(panel.border=element_rect(linetype=1, fill=NA)) +
#     labs(x = NULL, y = NULL) +
#     theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
#     geom_text(aes(label = sprintf("%.2f", score)), position = position_dodge(width = 0.9), vjust = -0.3)
# 
#   return(p + theme(legend.position = "bottom"))
# }
# 
# color_palette <- brewer.pal(n = 6, name = "Dark2")
# 
# plot_scores(score_df, "R2")
# plot_scores(score_df, "RMSE")


print(round(Sys.time() - start.time, 2))
print("DONE")

if (!test_run) {
  stopCluster(cl)
}
