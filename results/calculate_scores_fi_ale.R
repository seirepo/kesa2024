source("/scratch/dongelr1/susannar/kesa2024/results/explore_results_functions.R")

model_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models"
model_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models"

calculate_results_from_fits <- function(base_dir, sub_dir, title) {
  score_dir <- file.path(base_dir, "scores", sub_dir)
  model_dir <- file.path(base_dir, "fitted_models", sub_dir)
  target_dir <- file.path(base_dir, "explain_results", sub_dir)
  
  create_and_save_score_plots(score_dir, target_dir, title)
  get_fi_ale_plots(model_dir, target_dir, title)
}

########################################################################################

# Save the scores, feature importances and ALEs
calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
  sub_dir = "same_features_as_beijing", 
  title = "Hyytiälä"
)

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
  sub_dir = "same_features_as_beijing_no_outlier_filtering", 
  title = "Hyytiälä (data containing outliers)"
)

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
  sub_dir = "same_features_as_hyy", 
  title = "Beijing"
)

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
  sub_dir = "same_features_as_hyy_no_outlier_filtering", 
  title = "Beijing (data containing outliers)"
)

########################################################################################

## Save the scores, feature importances and ALEs for subsets of data ##

dirs <- c("uvb", "uvb_so2", "uvb_so2_rh", "uvb_so2_rh_temp", "uvb_so2_rh_temp_cs")

for (dir in dirs) {
  print(paste("Calculating results for dir", dir))
  calculate_results_from_fits(
    base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
    sub_dir = dir, 
    title = paste("Hyytiälä", dir, sep = ", ")
  )
  
  calculate_results_from_fits(
    base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
    sub_dir = dir, 
    title = paste("Beijing", dir, sep = ", ")
  )
}

# Plot the scores of all subsets into the same plot

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

base_dir <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
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

plot_hyy <- create_score_plots_for_subset_data(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
  dataset = "uvb_so2_filtered",
  title = "Hyytiälä (uvb_so2_filtered dataset)"
)

plot_bei1 <- create_score_plots_for_subset_data(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
  dataset = "uvb_so2_filtered",
  title = "Beijing (uvb_so2_filtered dataset)"
)

plot_bei2 <- create_score_plots_for_subset_data(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
  dataset = "unfiltered",
  title = "Beijing (unfiltered dataset)"
)

plot_hyy
plot_bei1
plot_bei2

ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results", "r2_rmse_all_subsets_uvb_so2_filtered.png"), plot = plot_hyy, width = 9, height = 6)
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results", "r2_rmse_all_subsets_uvb_so2_filtered.png"), plot = plot_bei1, width = 9, height = 6)
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results", "r2_rmse_all_subsets_unfiltered.png"), plot = plot_bei2, width = 9, height = 6)


########################################################################################

## Save the scores, feature importances and ALEs for data containing hour of the day components ##

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
  sub_dir = "same_features_as_hyy_hour", 
  title = "Beijing, same features as Hyytiälä"
)

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
  sub_dir = "same_features_as_beijing_hour", 
  title = "Hyytiälä, same features as Beijing"
)

########################################################################################

## Save the learning curve plots ##

save_learning_curves_from_results(
  sub_dir = "same_features_as_beijing", 
  title = "Hyytiälä", 
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
)

save_learning_curves_from_results(
  sub_dir = "same_features_as_beijing_no_outlier_filtering", 
  title = "Hyytiälä (data containing outliers)", 
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
)

save_learning_curves_from_results(
  sub_dir = "same_features_as_hyy", 
  title = "Beijing", 
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing"
)

save_learning_curves_from_results(
  sub_dir = "same_features_as_hyy_no_outlier_filtering", 
  title = "Beijing (data containing outliers)",
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing"
)

# There are no learning curves for models with hour in the train data

########################################################################################

## Create score plots for the Hyytiälä models trained on all data subsets (e.g. proxies) ##
calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
  sub_dir = "all_feature_subsets", 
  title = "Hyytiälä, all feature subsets"
)

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
  sub_dir = "all_feature_subsets_no_outlier_filtering", 
  title = "Hyytiälä, all feature subsets (data containing outliers)"
)

########################################################################################
########################################################################################

# Plot fit results of some of the models
files_hyy <- list.files(path = file.path(model_dir_hyy, "same_features_as_beijing"), pattern = "unfiltered.rds|uvb_so2_filtered.rds", full.names = TRUE)
files_bei <- list.files(path = file.path(model_dir_bei, "same_features_as_hyy"), pattern = "unfiltered.rds|uvb_so2_filtered.rds", full.names = TRUE)

fits_hyy <- lapply(files_hyy, readRDS) %>% lapply(function(x) x[[1]])
fits_bei <- lapply(files_bei, readRDS) %>% lapply(function(x) x[[1]])

plots_hyy <- fits_hyy %>% lapply(function(x) ggplot(x$rf) + ggtitle(x$datasetName))
plots_bei <- fits_bei %>% lapply(function(x) ggplot(x$rf) + ggtitle(x$datasetName))

p_hyy <- ggarrange(plotlist = plots_hyy, common.legend = TRUE, legend = "bottom")
p_hyy <- annotate_figure(p_hyy, ggpubr::text_grob("Hyytiälä"))
p_bei <- ggarrange(plotlist = plots_bei, common.legend = TRUE, legend = "bottom")
p_bei <- annotate_figure(p_bei, ggpubr::text_grob("Beijing"))

p_hyy
p_bei

########################################################################################

### Create feature importance plots with the same x-axis and combine them ###

load_importances <- function(path, site) {
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
bei_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results/same_features_as_hyy", site = "bei")

# h <- hyy_imp[c("uvb_so2_filtered", "unfiltered")]
# b <- bei_imp[c("uvb_so2_filtered", "unfiltered")]
h <- hyy_imp
b <- bei_imp

p1 <- create_importance_plot_from_list(h, title = "Hyytiälä")
p2 <- create_importance_plot_from_list(b, title = "Beijing")

ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results/same_features_as_beijing", paste0("importances_all.png")), plot = p1, width = 15, height = 6)
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results/same_features_as_hyy", paste0("importances_all.png")), plot = p2, width = 15, height = 6)

########################################################################################

# Compare the importances of Hyytiälä and Beijing datasets
comb <- list(hyy_imp[["uvb_so2_filtered"]] %>% mutate(dataset_name = paste("Hyytiälä", dataset_name, sep = ", ")),
             bei_imp[["uvb_so2_filtered"]] %>% mutate(dataset_name = paste("Beijing", dataset_name, sep = ", ")))
create_importance_plot_from_list(comb)

comb <- list(hyy_imp[["unfiltered"]] %>% mutate(dataset_name = paste("Hyytiälä", dataset_name, sep = ", ")),
             bei_imp[["unfiltered"]] %>% mutate(dataset_name = paste("Beijing", dataset_name, sep = ", ")))
create_importance_plot_from_list(comb)

### Create ALE comparison plots ###

p1 <- create_ale_comparison_plots("unfiltered", hyy_fi = hyy_imp, bei_fi = bei_imp, n = 25)
p2 <- create_ale_comparison_plots("uvb_so2_filtered", hyy_fi = hyy_imp, bei_fi = bei_imp, n = 25)

plot(p1)
plot(p2)



#################################################################
############ Plot the ales of subset feature models #############
#################################################################

get_ales_of_subset_model <- function(base_path, dataset, title) {
  dirs <- c("uvb_so2", "uvb_so2_rh", "uvb_so2_rh_temp", "uvb_so2_rh_temp_cs")
  features <- c("UVB", "SO2", "relative_humidity", "temp_K", "CS_rate")
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

base_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results"
dataset <- "unfiltered"

get_ales_of_subset_model(base_path, dataset, "Beijing")

base_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results"
dataset <- "uvb_so2_filtered"

get_ales_of_subset_model(base_path, dataset, "Beijing")

base_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results"
dataset <- "uvb_so2_filtered"

get_ales_of_subset_model(base_path, dataset, "Hyytiälä")
