source("/scratch/dongelr1/susannar/kesa2024/results/explore_results_functions.R")

score_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores"
model_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models"
lc_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/learning_curves"
target_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results"

score_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores"
model_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models"
lc_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/learning_curves"
target_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results"


calculate_results_from_fits <- function(base_dir, sub_dir, title) {
  score_dir <- file.path(base_dir, "scores", sub_dir)
  model_dir <- file.path(base_dir, "fitted_models", sub_dir)
  target_dir <- file.path(base_dir, "interpret_results", sub_dir)
  
  create_and_save_score_plots(score_dir, target_dir, title)
  get_fi_ale_plots(model_dir, target_dir, title)
}

# Save the scores, feature importances and ALEs
calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
  sub_dir = "same_features_as_beijing", 
  title = "Hyytiälä, same features as Beijing"
)

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala",
  sub_dir = "same_features_as_beijing_no_outlier_filtering", 
  title = "Hyytiälä, same features as Beijing (data containing outliers)"
)

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
  sub_dir = "same_features_as_hyy", 
  title = "Beijing, same features as Hyytiälä"
)

calculate_results_from_fits(
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing",
  sub_dir = "same_features_as_hyy_no_outlier_filtering", 
  title = "Beijing, same features as Hyytiälä data (data containing outliers)"
)


## Scores etc. for data containing hour of the day components ##

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


## Save the learning curve plots ##

save_learning_curves_from_results(
  sub_dir = "same_features_as_beijing", 
  title = "Hyytiälä, same features as Beijing", 
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
)

save_learning_curves_from_results(
  sub_dir = "same_features_as_beijing_no_outlier_filtering", 
  title = "Hyytiälä, same features as Beijing (data containing outliers)", 
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala"
)

save_learning_curves_from_results(
  sub_dir = "same_features_as_hyy", 
  title = "Beijing, same features as Hyytiälä", 
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing"
)

save_learning_curves_from_results(
  sub_dir = "same_features_as_hyy_no_outlier_filtering", 
  title = "Beijing, same features as Hyytiälä data (data containing outliers)",
  base_dir = "/scratch/dongelr1/susannar/kesa2024/results/beijing"
)

# There are no learning curves for models with hour in the train data

# calculate_results_from_fits(site = "hyytiala", sub_dir = "same_features_as_beijing", title = "Hyytiälä, same features as Beijing")
# calculate_results_from_fits(site = "hyytiala", sub_dir = "same_features_as_beijing_no_outlier_filtering", title = "Hyytiälä, same features as Beijing (data containing outliers)")
# calculate_results_from_fits(site = "beijing", sub_dir = "same_features_as_hyy", title = "Beijing, same features as Hyytiälä")
# calculate_results_from_fits(site = "beijing", sub_dir = "same_features_as_hyy_no_outlier_filtering", title = "Beijing, same features as Hyytiälä data (data containing outliers)")

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

# calculate_results_from_fits(site = "hyytiala", sub_dir = "all_feature_subsets", title =  "Hyytiälä, all feature subsets")
# calculate_results_from_fits(site = "hyytiala", sub_dir = "all_feature_subsets_no_outlier_filtering", title = "Hyytiälä, all feature subsets (data containing outliers)")

###########################
###########################

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

###########################

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

hyy_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results/same_features_as_beijing", site = "hyy")
bei_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results/same_features_as_hyy", site = "bei")

# h <- hyy_imp[c("uvb_so2_filtered", "unfiltered")]
# b <- bei_imp[c("uvb_so2_filtered", "unfiltered")]
h <- hyy_imp
b <- bei_imp

p1 <- create_importance_plot_from_list(h, title = "Hyytiälä")
p2 <- create_importance_plot_from_list(b, title = "Beijing")

ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results/same_features_as_beijing", paste0("importances_all.png")), plot = p1, width = 15, height = 6)
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results/same_features_as_hyy", paste0("importances_all.png")), plot = p2, width = 15, height = 6)


#####################

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