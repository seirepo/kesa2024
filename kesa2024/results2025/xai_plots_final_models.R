setwd("/scratch/dongelr1/susannar/kesa2024")

library(dplyr)
# library(lubridate)
library(ggplot2)
library(ggpubr)
# library(tidyr)
# library("scales")
library(purrr)
library('stringr')
# library(corrplot)
library(RColorBrewer)
# library("data.table")
library("iml", lib = "/scratch/dongelr1/laantito/")
# library("prediction", lib = "/scratch/dongelr1/laantito/")
#library("Metrics", lib = "/scratch/dongelr1/laantito/")


font_size <- 12
basic_theme <- theme(legend.text=element_text(size = font_size),
                     axis.text.x = element_text(size = font_size),
                     axis.text.y = element_text(size = font_size),
                     axis.title=element_text(size = font_size))
color_palette <- brewer.pal(n = 8, name = "Set2")[7:8]

##################################################################
# Plot the scores of all subsets into the same plot
##################################################################
plot_scores2 <- function(data, metric) {
  data$score[data$scoreType == "RMSE"] <- data$score[data$scoreType == "RMSE"] / 1e6
  
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
  
  p <- ggplot(data = test_data, aes(x = features, y = score, fill = model)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", show.legend = c(fill = TRUE)) +
    geom_point(data = train_data, shape = 4, color = "black", aes(x = features, y = score, fill = model), position = position_dodge(0.9), size = 3) +
    scale_y_continuous(breaks = breaks, limits = limits) +
    ggtitle(title) +
    scale_fill_manual(values = setNames(color_palette, c("rf", "lm")),
                      breaks = c("rf", "lm"),
                      labels = c("rf" = "random forest", "lm" = "linear model")) +
    guides(fill = guide_legend(override.aes = list(pattern = c("none", "none")))) +
    theme(plot.title = element_text(size = 12)) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
    geom_text(aes(label = sprintf("%.2f", score)), position = position_dodge(width = 0.9), vjust = -0.3, size = 3)
  
  return(p + theme(legend.position = "bottom"))
}

create_score_plots_for_subset_data <- function(base_dir, dataset, title) {
  df_list <- list()
  for (dir in dirs) {
    score_dir <- file.path(base_dir, "scores", dir)
    score_df <- readRDS(file.path(score_dir, paste0(dataset, ".rds")))
    score_df <- score_df %>% mutate(features = dir)
    df_list[[dir]] <- score_df
  }
  
  scores_combined <- reduce(df_list, rbind)
  r2 <- plot_scores2(scores_combined, "R2")
  rmse <- plot_scores2(scores_combined, "RMSE")
  
  p <- ggarrange(plotlist = list(r2, rmse), common.legend = TRUE, legend = "bottom")
  p <- annotate_figure(p, ggpubr::text_grob(title))
  return(p)
}

# plot_hyy <- create_score_plots_for_subset_data(
#   base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
#   dataset = "uvb_so2_filtered",
#   title = "Hyytiälä (uvb_so2_filtered dataset)"
# )
# 
# plot_bei1 <- create_score_plots_for_subset_data(
#   base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
#   dataset = "uvb_so2_filtered",
#   title = "Beijing (uvb_so2_filtered dataset)"
# )

plot_bei <- create_score_plots_for_subset_data(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results2025/beijing",
  dataset = "unfiltered",
  title = "Beijing"
)

plot_bei
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025/beijing/explain_results", "r2_rmse_all_subsets_unfiltered.png"), plot = plot_bei, width = 9, height = 6)

##################################################################
##################################################################

##################################################################
# Create an ALE plot for the given feature and both models. The
# data argument is either test or train dataset of the model
##################################################################
plot_feature_effect_models <- function(feat, data, res) {
  models <- c("random forest", "linear model")
  dff <- data.frame()
  
  for(i in seq_along(models)) {
    eff <- res[[i]][[feat]]
    
    if (is.null(eff)) {
      print("eff is null")
      next
    }
    
    if (!".borders" %in% colnames(eff)) {
      message(paste("Skipping", feat, "for model", models[[i]], ": .borders not found"))
      next
    }
    
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
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    scale_fill_manual(values = setNames(color_palette, c("rf", "lm")),
                      c("random forest", "linear model"),
                      breaks = c("random forest", "linear model")) +
    geom_rug(data = data, aes(x = !!sym(feat), y = NULL), linetype = "solid", color = "black", alpha = 0.1) +
    basic_theme +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}


#################################################################
############ Plot the ales of subset feature models #############
#################################################################
get_ales_of_subset_model <- function(base_path, dirs, features, dataset="", title="") {
  #dirs <- c("uvb_temp", "uvb_temp_so2", "uvb_temp_so2_o3", "uvb_temp_so2_o3_rh")
  #features <- c("UVB", "temp_K", "SO2", "O3", "relative_humidity")
  #features <- c("temp_K", "SO2", "O3", "relative_humidity")
  ales <- list()
  
  for (i in seq_along(dirs)) {
    dir <- dirs[[i]]
    ale <- readRDS(file.path(base_path, dir, paste0("ale_results_", dataset, ".rds")))
    feats <- features[1:(i+1)]
    
    l1 <- lapply(feats, plot_feature_effect_models, data = ale$testData, res = ale)
    
    ale_plot <- ggpubr::ggarrange(plotlist = l1, common.legend = TRUE, legend = "bottom", ncol = 1, nrow = 5)
    ale_plot <- annotate_figure(ale_plot, top = ggpubr::text_grob(dir))
    ales[[dir]] <- ale_plot
  }
  
  p <- ggarrange(plotlist = ales, nrow = 1, common.legend = TRUE, legend = "bottom")
  p <- annotate_figure(p, top = ggpubr::text_grob(paste(title, dataset, sep = ", ")))
  return(p)
}

dirs <- c("uvb_temp", "uvb_temp_so2", "uvb_temp_so2_o3", "uvb_temp_so2_o3_rh")
features <- c("UVB", "temp_K", "SO2", "O3", "relative_humidity")

base_path <- "/scratch/dongelr1/susannar/kesa2024/results2025/beijing/explain_results"
p <- get_ales_of_subset_model(base_path, dirs = dirs, features = features, dataset = "unfiltered", title = "Beijing")
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025/beijing/explain_results", "ale_all_subsets_unfiltered.png"), plot = p, width = 12, height = 12)


dirs <- c("uvb_so2", "uvb_so2_rh", "uvb_so2_rh_temp", "uvb_so2_rh_temp_cs")
features <- c("UVB", "SO2", "relative_humidity", "temp_K", "CS_rate")

base_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results"
p <- get_ales_of_subset_model(base_path, dirs = dirs, features = features, dataset = "unfiltered", title = "Beijing")
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025/beijing/explain_results", "ale_all_subsets_unfiltered_same_as_hyy.png"), plot = p, width = 12, height = 12)



dirs <- c("uvb_so2", "uvb_so2_rh", "uvb_so2_rh_temp", "uvb_so2_rh_temp_cs")
features <- c("UVB", "SO2", "relative_humidity", "temp_K", "CS_rate")

base_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results"
p <- get_ales_of_subset_model(base_path, dirs = dirs, features = features, dataset = "uvb_so2_filtered", title = "Hyytiälä")
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025/hyytiala/explain_results", "ale_all_subsets_uvb_so2_filtered.png"), plot = p, width = 12, height = 12)




###########################################
########### FEATURE IMPORTANCE ############
###########################################

average_importances <- function(combined_importance) {
  mean_importance <- combined_importance %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(mean_importance = mean(importance, na.rm = TRUE)) %>%
    dplyr::arrange(mean_importance)
  return(mean_importance)
}

##################################################################
# Create importance plot from the given importance results in
# combined_importance, containing importances for both models
# (random forest and linear model)
##################################################################
create_importance_plot <- function(combined_importance, title = "") {
  mean_importance <- average_importances(combined_importance)
  # The resulting mean_importances are in ascending order, but when the combined_importance is plotted on a vertical
  # axis, the factor levels are placed on the axis starting from the bottom
  combined_importance$feature <- factor(combined_importance$feature, levels = mean_importance$feature)
  
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
    basic_theme +
    ggtitle(title)
  
  p <- p + geom_hline(yintercept = seq(1.5, length(unique(combined_importance$feature)) - 0.5, by = 1), 
                      linetype = "dashed", color = "grey")
}

##################################################################
# Create importance plot from list of feature importance dfs with
# a shared x-axis, with a tick for x = 1.0 added separately
##################################################################
create_importance_plot_from_list <- function(importance_list, title = "") {
  
  x_max <- max(sapply(importance_list, function(x) max(x$importance.95)))
  x_min <- min(sapply(importance_list, function(x) min(x$importance.05)))
  x_max <- ceiling(x_max * 10) / 10
  x_min <- floor(x_min * 10) / 10
  xlimits <- c(x_min, x_max)
  
  plots <- lapply(importance_list, function(df) {
    create_importance_plot(df, title = df$dataset_name)
  })
  
  # Force the x-axis to contain 1.0
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

#########################################################################
# Create feature importance plots with the same x-axis and combine them #
#########################################################################

load_importances <- function(path, site, dataset) {
  imp <- list.files(path = path, pattern = "importances_.*.rds", full.names = TRUE) %>% lapply(function(p) {
    dataset_name <- tools::file_path_sans_ext(basename(p))
    dataset_name <- str_remove(dataset_name, "importances_")
    df <- readRDS(p)
    df <- df %>% mutate(dataset_name = dataset_name)
  }) %>%
    lapply(function(x) mutate(x, site = site))
  names(imp) = lapply(imp, function(df) unique(df$dataset_name))
  return(imp)
}

hyy_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results/same_features_as_beijing", site = "hyy")
#bei_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results/same_features_as_hyy", site = "bei")

# h <- hyy_imp[c("uvb_so2_filtered", "unfiltered")]
# b <- bei_imp[c("uvb_so2_filtered", "unfiltered")]
h <- hyy_imp
b <- bei_imp

p1 <- create_importance_plot(h, title = "Hyytiälä", dataset = "uvb_so2_filtered")

ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results/same_features_as_beijing", paste0("importances_all.png")), plot = p1, width = 15, height = 6)
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results/same_features_as_hyy", paste0("importances_all.png")), plot = p2, width = 15, height = 6)



##################################################################
# Plot and save learning curves of all learning curve results
# within the given sub_dir
##################################################################
save_learning_curves_from_results <- function(sub_dir, title, base_dir) {
  lc_dir <- file.path(base_dir, "learning_curves", sub_dir)
  print(lc_dir)
  lc_dfs <- list.files(path = lc_dir, full.names = TRUE) %>% lapply(readRDS)
  print(paste("Learning curves loaded from", lc_dir))
  
  # Exclude the resampling data and write out the method names
  lc_dfs <- lc_dfs %>%
    lapply(function(x) filter(x, Data != "Resampling")) %>%
    lapply(function(x) mutate(x, method = dplyr::recode(method, rf = "random forest", lm = "linear model")))
  
  print("2. processed df")
  print(lc_dfs)
  
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
  
  target_dir <- file.path(base_dir, "explain_results", sub_dir)
  ggsave(file.path(target_dir, paste0("lc_all.png")), plot = g, width = 9, height = 6)
  print(paste("Learning curve saved to", target_dir))
}

##################################################################
# Save the learning curve plots 
##################################################################

save_learning_curves_from_results(
  sub_dir = "same_features_as_beijing", 
  title = "Hyytiälä", 
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
)

save_learning_curves_from_results(
  sub_dir = "uvb_temp_so2_o3_rh", 
  title = "Beijing", 
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results2025/beijing"
)
