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
library(ggpubr)
library(tools)

setwd("/scratch/dongelr1/susannar/kesa2024")

########################
##### SET TEST RUN #####
########################
test_run <- TRUE
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
create_datasets <- function(dat) {
  dat_filtered <- dat %>% filter(hour(Time) > 6 & hour(Time) < 19)
  
  dsets <- list(dat_filtered, dat)
  dsets <- lapply(dsets, function(x) dplyr::select(x, -Time))
  names(dsets) <- list("all_features_filtered", "all_features_unfiltered")
  return(dsets)
}

create_dataset_with_proxies <- function(dat) {
  dat_filtered <- dat %>% filter(hour(Time) > 6 & hour(Time) < 19)
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

# names <- c("L1", "L2", "L3", "L4", "L5", "all_proxies", "all_features", "all_features_with_proxies", "all_features_unfiltered")
# model_names <- c("rf", "lm")

train_models <- function(dset_list) {
  
  model_list <- list()
  mod_names <- names(dset_list)
  
  start.time <- Sys.time()
  
  for (i in 1:length(dset_list)) {
    print(mod_names[[i]])
    # print(names(dset_list)[i])
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


# Beijing data
# path <- "data/beijing/preprocessed/untransformed_dataset.csv"
# fit_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models_dataset.rds"
# score_df_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/score_df_dataset.rds"
# 
# # Drop sector, there was only one of them
# dat <- load_data(path) %>% dplyr::select(-sector)
# dset_list <- create_datasets(dat)
# results <- train_models(dset_list)
# 
# if (!test_run) {
#   save_results(results, fit_path = fit_path, score_df_path = score_df_path)
#   print(paste("Results saved to", fit_path, score_df_path))
# }

# dataset_type <- "untransformed_dataset"
# fits <- results[[2]]
# fits$dataset_type <- dataset_type
# score_df <- results[[1]]
# score_df$dataset_type <- c(dataset_type)
# View(fits)
# View(score_df)

# tools::file_path_sans_ext(basename(path))

data_paths <- list(
  untransformed_dataset <- "data/beijing/preprocessed/untransformed_dataset.csv",
  norm_min_max <- "data/beijing/preprocessed/norm_min_max.csv",
  norm_std.rds <- "data/beijing/preprocessed/norm_std.csv",
  log <- "data/beijing/preprocessed/log.csv",
  log_norm_min_max <- "data/beijing/preprocessed/log_norm_min_max.csv",
  log_norm_std <- "data/beijing/preprocessed/log_norm_std.csv"
)

# model_path <- list(
#   untransformed_dataset <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/untransformed_dataset.csv",
#   norm_min_max <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/norm_min_max.rds",
#   norm_std.rds <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/norm_std.rds",
#   log <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/log.rds",
#   log_norm_min_max <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/log_norm_min_max.rds",
#   log_norm_std <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/log_norm_std.rds"
# )
# 
# score_path <- list(
#   untransformed_dataset = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/untransformed_dataset.csv",
#   norm_min_max = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/norm_min_max.rds",
#   norm_std = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/norm_std.rds",
#   log = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/log.rds",
#   log_norm_min_max = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/log_norm_min_max.rds",
#   log_norm_std = "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/log_norm_std.rds"
# )
# 
# target_paths <- rlist::list.zip(model_path, score_path)

fit_target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models"
score_target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores"

fit_models <- function(data_path, target_path, log_dat) {
  # fit_path <- target_path[[1]]
  # score_path <- target_path[[2]]
  
  dataset_name <- tools::file_path_sans_ext(basename(data_path))
  fit_path <- file.path(fit_target_dir, paste0(dataset_name, ".rds"))
  score_path <- file.path(score_target_dir, paste0(dataset_name, ".rds"))
  
  dat <- load_data(data_path) %>% dplyr::select(-sector)
  
  dset_list <- create_datasets(dat)
  
  results <- train_models(dset_list)
  
  if (!test_run) {
    save_results(results, fit_path = fit_path, score_df_path = score_path)
  }
}

purrr::map(data_paths, fit_models)
# purrr::map2(data_paths, target_paths, fit_models)

print(round(Sys.time() - start.time, 2))
print("DONE")

if (!test_run) {
  stopCluster(cl)
}


# plot_score <- function(data, metric, title = "") {
#   color_palette <- brewer.pal(n = 9, name = "Set3")
#   # data$score[data$scoreType == "RMSE"] <- data$score[data$scoreType == "RMSE"] / 1e6
# 
#   data <- subset(data, scoreType == metric)
#   breaks <- seq(0, max(data$score), 1)
#   limits <- c(0, max(data$score))
#   if (metric == "R2") {
#     breaks <- seq(0, 1, 0.25)
#     limits <- c(0, 1.05)
#   }
# 
#   if (metric == "RMSE") {
#     title <- paste(title, "1e6")
#   }
# 
#   test_data <- subset(data, scoreType == metric & split == "Test")
#   train_data <- subset(data, scoreType == metric & split == "Train")
# 
#   p <- ggplot(data = test_data, aes(x = type, y = score, fill = model)) +
#     geom_bar(stat = "identity", position = "dodge", color = "black", show.legend = c(fill = TRUE)) +
#     geom_point(data = train_data, shape = 4, color = "black", aes(x = type, y = score, fill = model), position = position_dodge(0.9), size = 3) +
#     scale_y_continuous(breaks = breaks, limits = limits) +
#     ggtitle(paste(metric, title)) +
#     scale_fill_manual(values = setNames(c(color_palette[5], color_palette[8]), c("rf", "lm")),
#                       breaks = c("rf", "lm")) +
#     guides(fill = guide_legend(override.aes = list(pattern = c("none", "none")))) +
#     # theme(plot.title = element_text(hjust = 0.5)) +
#     # theme(panel.border=element_rect(linetype=1, fill=NA)) +
#     labs(x = NULL, y = NULL) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) +
#     theme(axis.text.y = element_text(size = 13)) +
#     # theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
#     geom_text(aes(label = sprintf("%.2f", score)), position = position_dodge(width = 0.9), vjust = -0.3)
# 
#   return(p + theme(legend.position = "bottom"))
# }
# 
# # untransformed_model <- readRDS("/scratch/dongelr1/susannar/kesa2024/results/beijing/score_df_dataset.rds")
# score1 <- readRDS("/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/dataset.rds")
# plot_score(score, "RMSE")
# plot_score(score, "R2")
# 
# # plots <- list()
# rmses <- list()
# r2s <- list()
# 
# for (i in 1:length(score_path)) {
#   p <- score_path[[i]]
#   t <- names(score_path)[[i]]
#   s <- readRDS(p)
#   p1 <- plot_score(s, "RMSE", title = t)
#   p2 <- plot_score(s, "R2", title = t)
#   
#   rmses[[i]] <- p1
#   r2s[[i]] <- p2
#   # p <- ggarrange(plotlist = list(p1, p2), common.legend = TRUE, legend = "bottom")
#   # p <- annotate_figure(p, top = text_grob(t))
#   # plots[[i]] <- p
# }
# 
# # lapply(plots, plot)
# # ggarrange(plotlist = plots)
# 
# p1 <- ggarrange(plotlist = r2s)
# p2 <- ggarrange(plotlist = rmses)
# 
# ggarrange(plotlist = list(p1, p2))
