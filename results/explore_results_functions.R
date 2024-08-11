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

# color_palette <- brewer.pal(n = 8, name = "Set2")[2:3]
color_palette <- brewer.pal(n = 8, name = "Set2")[7:8]
# color_palette <- brewer.pal(n = 8, name = "Pastel1")[8:9]
# color_palette <- brewer.pal(n = 8, name = "Pastel2")[7:8]

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
    geom_text(aes(label = sprintf("%.2f", score)), position = position_dodge(width = 0.9), vjust = -0.3, size = 3)
  
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

# score_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/same_features_as_beijing"
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
  
  # Spell out the model names to match the color labels in scale_fill_manual below
  combined_importance <- combined_importance %>%
    mutate(model = dplyr::recode(model, rf = "random forest", lm = "linear model"))
  
  p <- ggplot(combined_importance, aes(x = importance, y = feature, shape = model)) +
    geom_errorbar(aes(xmin = importance.05, xmax = importance.95), width = 0, linewidth = 1,
                  position = position_dodge(width = 0.5)) + 
    geom_point(data = combined_importance, aes(x = importance, y = feature, fill = model), size = 2.5,
               position = position_dodge(width = 0.5)) +
    # scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    scale_x_continuous(breaks = custom_breaks) +
    # scale_fill_manual(values = setNames(color_palette, c("rf", "lm"))) +
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

# Create importance plot from list of feature importance dfs with shared x-axis
create_importance_plot_from_list <- function(importance_list, title = "") {
  
  x_max <- max(sapply(importance_list, function(x) max(x$importance.95)))
  x_min <- min(sapply(importance_list, function(x) min(x$importance.05)))
  x_max <- ceiling(x_max * 10) / 10
  x_min <- floor(x_min * 10) / 10
  xlimits <- c(x_min, x_max)
  
  plots <- lapply(importance_list, function(df) {
    create_importance_plot(df, title = df$dataset_name)
  })
  
  custom_breaks <- function(x) {
    unique(c(1.0, pretty(x)))
  }
  
  plots <- lapply(plots, function(p) {
    p + scale_x_continuous(breaks = custom_breaks, limits = xlimits)
  })
  
  g <- ggarrange(plotlist = plots, common.legend = TRUE, legend = "bottom", nrow = 1)
  g <- annotate_figure(g, ggpubr::text_grob(title))
  return(g)
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
  ggsave(file.path(target_dir, paste0("importances_", dataset_name, ".png")), plot = fi_plot, width = 5, height = 6)
  
  # Calculate ALE results for both of the models
  eff_rf <- get_ale_results(pred_rf)
  eff_lm <- get_ale_results(pred_lm)
  
  print("ALE results calculated")
  
  # Sort ALE results as the according to the importances of features
  feat_rf <- combined_importance %>% filter(model == "rf") %>% .$feature
  feat_lm <- combined_importance %>% filter(model == "lm") %>% .$feature
  eff_rf <- eff_rf[feat_rf]
  eff_lm <- eff_lm[feat_lm]
  
  ale_results <- list(eff_rf, eff_lm)
  names(ale_results) <- list("rf", "lm")
  ale_results$testData <- fit_obj$testData
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

# For testing/debugging
# save_fi_ale_results(path = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing/dataset_uvb_so2_filtered.rds",
#                     target_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results/same_features_as_beijing",
#                     title = "Hyytiälä, same features as Beijing")


### Functions to plot ALE comparison plots
plot_comparisons <- function(plotlist) {
  l <- lapply(plotlist, function(sublist) { g <- ggarrange(plotlist = sublist, common.legend = TRUE, legend = "bottom"); })
  splitted_l <- split(l, ceiling(seq_along(l) / 2))
  plots <- lapply(splitted_l, function(sublist) { g <- ggarrange(plotlist = sublist, common.legend = TRUE, legend = "bottom", ncol = 1, nrow = 2); })
  return(plots)
}

create_ale_comparison_plots <- function(dataset_name, hyy_fi, bei_fi, n) {
  h_path <- file.path("/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results/same_features_as_beijing")
  b_path <- file.path("/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results/same_features_as_hyy")
  
  ale_path_hyy <- file.path(h_path, paste0("ale_results_", dataset_name, ".rds"))
  ale_path_bei <- file.path(b_path, paste0("ale_results_", dataset_name, ".rds"))
  ale_hyy <- readRDS(ale_path_hyy)
  ale_bei <- readRDS(ale_path_bei)
  
  fit_hyy <- readRDS(file.path("/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing", paste0(dataset_name, ".rds")))[[1]]
  fit_bei <- readRDS(file.path("/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy", paste0(dataset_name, ".rds")))[[1]]

  h_fi <- hyy_fi[[dataset_name]]
  b_fi <- bei_fi[[dataset_name]]
  
  comb_fi <- rbind(h_fi, b_fi)
  av_fi <- average_importances(comb_fi)
  features <- arrange(av_fi, desc(mean_importance))$feature
  
  if (n <= length(features)) {
    features <- features[1:n]
  }
  
  fplots <- list()
  for (k in 1:length(features)) {
    ph <- plot_feature_effect_models(features[[k]], fit_hyy$testData, ale_hyy) + ggtitle("Hyytiälä")
    pb <- plot_feature_effect_models(features[[k]], fit_bei$testData, ale_bei) + ggtitle("Beijing")
    plots <- list(ph, pb)
    # names(plots) <- c(paste(m[[i]], "hyy"), paste(m[[i]], "bei"))
    names(plots) <- c("unfiltered", "unfiltered")
    fplots[[k]] <- plots
  }
  
  plot_test <- plot_comparisons(fplots)
  g <- ggarrange(plotlist = plot_test, common.legend = TRUE, legend = "bottom")
  g <- annotate_figure(g, ggpubr::text_grob(dataset_name))
  return(g)
}

########## Plot and save learning curves ##############

save_learning_curves_from_results <- function(sub_dir, title, base_dir) {
  lc_dir <- file.path(base_dir, "learning_curves", sub_dir)
  lc_dfs <- list.files(path = lc_dir, full.names = TRUE) %>% lapply(readRDS)
  print(paste("Learning curves loaded from", lc_dir))
  
  # Exclude the resampling data and write out the method names
  lc_dfs <- lc_dfs %>%
    lapply(function(x) filter(x, Data != "Resampling")) %>%
    lapply(function(x) mutate(x, method = dplyr::recode(method, rf = "random forest", lm = "linear model")))
  
  plots_all <- lc_dfs %>% lapply(function(lc) {
    ggplot(lc, aes(x = Training_Size, y = RMSE, color = Data)) +
      # geom_smooth(method = loess, span = .8, se = FALSE) +
      # geom_smooth(method = loess, span = .8, formula = y ~ x) +
      geom_line() +
      geom_point(shape = 1) +
      facet_wrap(~method) +
      scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
      theme_bw() +
      ggtitle(lc$dataset_name)
  })
  
  g <- ggarrange(plotlist = plots_all, common.legend = TRUE, legend = "bottom")
  g <- annotate_figure(g, ggpubr::text_grob(title))
  
  target_dir <- file.path(base_dir, "interpret_results", sub_dir)
  ggsave(file.path(target_dir, paste0("lc_all.png")), plot = g, width = 9, height = 6)
  print(paste("Learning curve saved to", target_dir))
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
