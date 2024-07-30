setwd("/scratch/dongelr1/susannar/kesa2024")

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(tidyr)
library("scales")
library(purrr)
library('stringr')
library(corrplot)
library(RColorBrewer)
library("iml", lib = "/scratch/dongelr1/laantito/")
library("prediction", lib = "/scratch/dongelr1/laantito/")
library("Metrics", lib = "/scratch/dongelr1/laantito/")


# font_size <- 15
# basic_theme <- theme(legend.text=element_text(size = font_size),
#                      axis.text.x = element_text(size = font_size),
#                      axis.text.y = element_text(size = font_size),
#                      axis.title=element_text(size = font_size))

################################################################################

model_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/model_basic_fitted_models.rds"
score_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/model_basic_score_df.rds"
target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/result_exploration"

################################################################################


# color_palette <- brewer.pal(n = 9, name = "Set3")
# color_palette <- brewer.pal(n = 8, name = "Dark2")

# color_palette <- brewer.pal(n = 4, name = "Set1")

color_palette <- brewer.pal(n = 8, name = "Set2")[2:3]

# plot_scores <- function(data, metric, title = "") {
plot_scores <- function(data, metric) {
  data$score[data$scoreType == "RMSE"] <- data$score[data$scoreType == "RMSE"] / 1e6
  
  # title <- paste(title, metric, sep = ", ")
  title <- metric
  
  data <- subset(data, scoreType == metric)
  breaks <- seq(0, max(data$score))
  limits <- c(0, max(data$score))
  
  if (metric == "R2") {
    breaks <- seq(0, 1, 0.25)
    limits <- c(0, 1.05)
  }
  
  if (metric == "RMSE") {
    title <- paste(title, "1e6")
  }
  
  test_data <- subset(data, scoreType == metric & split == "Test")
  train_data <- subset(data, scoreType == metric & split == "Train")
  
  p <- ggplot(data = test_data, aes(x = dataset_name, y = score, fill = model)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", show.legend = c(fill = TRUE)) +
    geom_point(data = train_data, shape = 4, color = "black", aes(x = dataset_name, y = score, fill = model), position = position_dodge(0.9), size = 3) +
    scale_y_continuous(breaks = breaks, limits = limits) +
    ggtitle(title) +
    # scale_fill_manual(values = setNames(c(color_palette[5], color_palette[8]), c("rf", "lm")),
    #                   breaks = c("rf", "lm")) +
    scale_fill_manual(values = setNames(color_palette, c("rf", "lm")),
                      breaks = c("rf", "lm"),
                      labels = c("rf" = "random forest", "lm" = "linear model")) +
    guides(fill = guide_legend(override.aes = list(pattern = c("none", "none")))) +
    # theme(plot.title = element_text(hjust = 0.5)) +
    # theme(panel.border=element_rect(linetype=1, fill=NA)) +
    theme(plot.title = element_text(size = 12)) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
    # theme(axis.text.y = element_text(size = 13)) +
    # theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
    geom_text(aes(label = sprintf("%.2f", score)), position = position_dodge(width = 0.9), vjust = -0.3)
  
  return(p + theme(legend.position = "bottom"))
}

create_score_plots <- function(score_dir, main_title = "") {
  score_dfs <- list.files(path = score_dir, full.names = TRUE) %>% lapply(readRDS)
  print(paste("Scores loaded from", score_dir))
  scores_combined <- reduce(score_dfs, rbind)
  p1 <- plot_scores(scores_combined, "R2")#, main_title)
  p2 <- plot_scores(scores_combined, "RMSE")#, main_title)

  p <- ggarrange(plotlist = list(p1, p2), common.legend = TRUE, legend = "bottom")
  p <- annotate_figure(p, ggpubr::text_grob(main_title))
  return(p)
}

save_score_plot <- function(p, target_dir) {
  ggsave(file.path(target_dir, "r2_rmse.png"), plot = p, width = 9, height = 6)
  print(paste("R2 and RMSE for datase saved to", target_dir))
}

create_and_save_score_plots <- function(score_dir, target_dir, main_title = "") {
  p <- create_score_plots(score_dir, main_title)
  save_score_plot(p, target_dir)
  return(p)
}

#######

# create_score_plots(score_dir_hyy, "Hyytiälä")
# create_score_plots(score_dir_bei, "Beijing")

# create_and_save_score_plots(score_dir_hyy, target_dir_hyy, "Hyytiälä")
# create_and_save_score_plots(score_dir_bei, target_dir_bei, "Beijing")


# score_df <- readRDS(score_path) %>% filter(type %in% c("all_features", "all_features_unfiltered"))
# plot_scores(score_df, "RMSE")
# plot_scores(score_df, "R2")

###############################################################
############### Create ALE and importance plots ###############
###############################################################
# Load the fitted models trained with all features
# Calculate feature importances and ALE for the model
# Create a plot of both and return nested list containing a pair of plots for each fit

get_predictor <- function(model, x, y) {
  predictor <- Predictor$new(model, data = x, y = y)
  return(predictor)
}

get_importances <- function(predictor) {
  imp <- FeatureImp$new(predictor, loss = "rmse")
  return(imp)
}

get_ale_results <- function(predictor) {
  eff <- FeatureEffects$new(predictor)
  return(eff$results)
}

combine_importance_results <- function(importance_results) {
  combined_df <- data.frame()
  names <- c("rf", "lm")
  for (m in 1:2) {
    model_df <- importance_results[[m]]$results
    model_df$model <- names[m]
    combined_df <- rbind(combined_df, model_df)
  }
  
  return(combined_df)
}

average_importances <- function(combined_importance) {
  mean_importance <- combined_importance %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(mean_importance = mean(importance, na.rm = TRUE)) %>%
    dplyr::arrange(mean_importance)
  return(mean_importance)
}

# Importance plot
create_importance_plot <- function(combined_importance, title = "") {
  mean_importance <- average_importances(combined_importance)
  # The resulting mean_importances are in ascending order, but when the combined_importance is plotted on a vertical
  # axis, the factor levels are placed on the axis starting from the bottom
  combined_importance$feature <- factor(combined_importance$feature, levels = mean_importance$feature)
  
  # color_palette <- brewer.pal(n = 4, name = "Dark2")
  
  custom_breaks <- function(x) {
    unique(c(1.0, pretty(x)))
  }
  
  p <- ggplot(combined_importance, aes(x = importance, y = feature, shape = model)) +
    geom_errorbar(aes(xmin = importance.05, xmax = importance.95), width = 0, linewidth = 1,
                  position = position_dodge(width = 0.5)) + 
    geom_point(data = combined_importance, aes(x = importance, y = feature, fill = model), size = 2.5,
               position = position_dodge(width = 0.5)) +
    # scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    scale_x_continuous(breaks = custom_breaks) +
    scale_fill_manual(values = setNames(color_palette, c("random forest", "linear model"))) +
    scale_shape_manual(values = c(21, 21)) +
    theme(panel.border = element_rect(linetype = 1, fill = NA)) +
    ylab("Feature") +
    xlab("Importance") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom") +
    # basic_theme +
    ggtitle(title)
  
  p <- p + geom_hline(yintercept = seq(1.5, length(unique(combined_importance$feature)) - 0.5, by = 1), 
                      linetype = "dashed", color = "grey")
}

plot_feature_effect_models <- function(feat, data, res, bgc) {
  models <- c("random forest", "linear model")
  dff <- data.frame()
  
  for(i in seq_along(models)) {
    res[[i]][[feat]][["Type"]] <- models[[i]]
    dff <- rbind(dff, res[[i]][[feat]])
  } 
  
  dff$Type <- factor(dff$Type, levels = models)
  
  # color_palette <- brewer.pal(n = 8, name = "Pastel2")
  
  p <- ggplot(dff, aes(x = .borders, y = .value, linetype = Type)) +
    geom_line(linewidth = 1.3, color = "black") +  # Black "outline"
    geom_line(aes(color = Type), linewidth = 1) + # Actual colored line
    labs(x = feat, y = "SA") +
    scale_color_manual(values = color_palette) +
    # theme(panel.background = element_rect(fill = bgc)) + 
    scale_linetype_manual(values = rep("solid", 4)) +
    theme(panel.border=element_rect(linetype = 1, fill=NA)) +
    # scale_fill_manual(values = setNames(c(color_palette[5], color_palette[8]), c("random forest", "linear model")),
    #                   breaks = c("random forest", "linear model")) +
    
    scale_fill_manual(values = setNames(color_palette, c("rf", "lm")),
                      c("random forest", "linear model"),
                      breaks = c("random forest", "linear model")) +
    geom_rug(data = data, aes(x = !!sym(feat), y = NULL), linetype = "solid", color = "black", alpha = 0.1) #+
  # basic_theme
  return(p)
}

save_data <- function(data, target) {
  saveRDS(data, file = target)
  print(paste("Data saved to", target))
}

save_fi_ale_results <- function(path, target_dir, title = "") {
  
  fit_obj <- readRDS(path)
  dataset_name <- fit_obj$dataset_name
  print(paste("save_fi_ale_results for the following data:", dataset_name))
  
  fit_obj <- fit_obj[[1]] # There should be only one fit, the second item is the dataset name
  
  x <- fit_obj$testData %>% dplyr::select(-SA_cm3)
  y <- fit_obj$testData$SA_cm3
  
  if (ncol(x) == 1) {
    print("Data contains only one feature, returning")
    return(NA)
  }
  
  print("Calculate predictors")
  
  pred_rf <- get_predictor(fit_obj$rf, x, y)
  pred_lm <- get_predictor(fit_obj$lm, x, y)
  
  imp_rf <- get_importances(pred_rf)
  imp_lm <- get_importances(pred_lm)
  
  print("Importances calculated")
  
  combined_importance <- combine_importance_results(list(imp_rf, imp_lm))
  target <- file.path(target_dir, paste0("importances_", dataset_name, ".rds"))
  save_data(combined_importance, target)
  
  # If there's a title, combine it with the dataset name
  if (title != "") {
    title <- paste(title, dataset_name, sep = ", ")
  } else {
    title <- dataset_name
  }
  
  # p_0_imp <- create_importance_plot(combined_importance, title = paste("Unfiltered SA data,", fit_obj$datasetName))
  fi_plot <- create_importance_plot(combined_importance, title = title)
  ggsave(file.path(target_dir, paste0("importances_", dataset_name, ".png")), plot = fi_plot, width = 9, height = 6)
  
  eff_rf <- get_ale_results(pred_rf)
  eff_lm <- get_ale_results(pred_lm)
  
  print("ALE results calculated")
  
  ale_results <- list(eff_rf, eff_lm)
  names(ale_results) <- list("rf", "lm")
  target <- file.path(target_dir, paste0("ale_results_", dataset_name, ".rds"))
  save_data(ale_results, target)
  
  mean_importances <- average_importances(combined_importance)
  mean_importances <- dplyr::arrange(mean_importances, desc(mean_importance))
  features <- setdiff(mean_importances$feature, c("SA_cm3"))
  
  l1 <- lapply(features, plot_feature_effect_models, data = fit_obj$testData, res = ale_results, bgc = "#F5F5DC")
  ale_plot <- ggpubr::ggarrange(plotlist = l1, common.legend = TRUE, legend = "bottom")
  ale_plot <- annotate_figure(ale_plot, top = ggpubr::text_grob(title))
  ggsave(file.path(target_dir, paste0("ale_", dataset_name, ".png")), plot = ale_plot, width = 9, height = 6)
  
  return(list(importance_plot = fi_plot, ale_plot = ale_plot))
}

# get_fi_ale_plots <- function(model_paths, target_dir, title = "") {
get_fi_ale_plots <- function(model_dir, target_dir, title = "") {
  # plots <- purrr::map2(model_paths, titles, save_fi_ale_results)
  model_paths <- list.files(path = model_dir, full.names = TRUE)
  plots <- purrr::map(model_paths, save_fi_ale_results, target_dir = target_dir, title)
  return(plots)
}

########

# model_paths_hyy <- list.files(path = model_dir_hyy, full.names = TRUE)
# plots_nested_hyy <- get_fi_ale_plots(model_paths_hyy, target_dir = target_dir_hyy, title = "Hyytiala, same features as in Beijing")
# 
# fi_plots_hyy <- purrr:::map(plots_nested_hyy, "importance_plot")
# ale_plots_hyy <- purrr::map(plots_nested_hyy, "ale_plot")
# 
# 
# model_paths_bei <- list.files(path = model_dir_bei, full.names = TRUE)
# plots_nested_bei <- get_fi_ale_plots(model_paths_bei, target_dir = target_dir_bei, title = "Beijing")
# 
# fi_plots_bei <- purrr:::map(plots_nested_bei, "importance_plot")
# ale_plots_bei <- purrr::map(plots_nested_bei, "ale_plot")
# 
# lapply(fi_plots_bei, plot)
# ggarrange(plotlist = fi_plots_bei)
