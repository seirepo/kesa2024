source("/scratch/dongelr1/susannar/kesa2024/results/explore_results_functions.R")

score_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores"
model_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models"
target_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results"

score_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/scores"
model_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models"
target_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results"


calculate_results_from_fits <- function(site, sub_dir, title) {
  if (site == "hyytiala") {
    score_dir <- file.path(score_dir_hyy, sub_dir)
    model_dir <- file.path(model_dir_hyy, sub_dir)
    target_dir <- file.path(target_dir_hyy, sub_dir)
  } else if (site == "beijing") {
    score_dir <- file.path(score_dir_bei, sub_dir)
    model_dir <- file.path(model_dir_bei, sub_dir)
    target_dir <- file.path(target_dir_bei, sub_dir)
  } else {
    print(paste("Unidentified site", site))
  }
  
  create_and_save_score_plots(score_dir, target_dir, title)
  get_fi_ale_plots(model_dir, target_dir, title)
}


calculate_results_from_fits(site = "hyytiala", sub_dir = "same_features_as_beijing", title = "Hyytiälä, same features with Beijing")
calculate_results_from_fits(site = "hyytiala", sub_dir = "same_features_as_beijing_no_outlier_filtering", title = "Hyytiälä, same features with Beijing (data containing outliers)")
calculate_results_from_fits(site = "beijing", sub_dir = "same_features_as_hyy", title = "Beijing, same features with Hyytiälä")
calculate_results_from_fits(site = "beijing", sub_dir = "same_features_as_hyy_no_outlier_filtering", title = "Beijing, same features with Hyytiälä data (data containing outliers)")


calculate_results_from_fits(site = "hyytiala", sub_dir = "all_feature_subsets", title =  "Hyytiälä, all feature subsets")
calculate_results_from_fits(site = "hyytiala", sub_dir = "all_feature_subsets_no_outlier_filtering", title = "Hyytiälä, all feature subsets (data containing outliers)")


# score_dfs <- list.files(path = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/all_feature_subsets", full.names = TRUE) %>% lapply(readRDS)







