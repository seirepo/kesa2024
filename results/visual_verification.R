library(ggplot2)
library(ggpubr)
library(dplyr)

setwd("/scratch/dongelr1/susannar/kesa2024")

#####################################################################
#### Plot the predicted and actual SA values and their residuals ####
#####################################################################

font_size <- 12
basic_theme <- theme(legend.text=element_text(size = font_size),
                     axis.text.x = element_text(size = font_size),
                     axis.text.y = element_text(size = font_size),
                     axis.title=element_text(size = font_size))

color_palette <- brewer.pal(n = 8, name = "Set2")[7:8]
# color_palette <- brewer.pal(n = 8, name = "Pastel1")[8:9]

plot_results <- function(df, title) {
  p1 <- ggplot(data = df, aes(x = SA_actual, y = SA_predicted)) +
    geom_point(alpha = 0.3) + 
    geom_abline(slope = 1, intercept = 0, color = "red") +
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10") +
    # ggtitle("Actual vs. predicted")
    basic_theme
  
  p2 <- ggplot(data = df, aes(x = SA_actual, y = relative_residuals)) + 
    geom_point(alpha = 0.5) + 
    scale_x_continuous(trans = "log10") + 
    geom_hline(yintercept = 0, color = "red") + 
    basic_theme +
    ggtitle("Relative residuals ((actual - pred) / actual)")
  
  p3 <- ggplot(data = df) +
    geom_histogram(aes(x = SA_actual, fill = "SA_actual"), alpha = 0.5, position = "identity") +
    geom_histogram(aes(x = SA_predicted, fill = "SA_predicted"), alpha = 0.5, position = "identity") +
    scale_fill_manual(values = setNames(color_palette, c("SA_actual", "SA_predicted"))) +
    # labs(x = "Values", title = "Histogram, actual vs. predicted") +
    basic_theme +
    # scale_fill_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red"))
    scale_x_continuous(trans = "log10")
  
  p4 <- ggplot(data = df) +
    geom_boxplot(aes(y = SA_actual, x = "SA_actual", fill = "SA_actual")) +
    geom_boxplot(aes(y = SA_predicted, x = "SA_predicted", fill = "SA_predicted")) +
    scale_fill_manual(values = setNames(color_palette, c("SA_actual", "SA_predicted"))) +
    basic_theme +
    scale_y_continuous(trans = "log10")
    # labs(x = "Group", y = "Values", title = "Box Plot of Actual and Predicted Values") #+
    # scale_fill_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red"))
    # ggtitle("Boxplot, actual vs. predicted")

  p <- ggarrange(plotlist = list(p1, p2, p3, p4), common.legend = TRUE, legend = "bottom")
  p <- annotate_figure(p, ggpubr::text_grob(title))
  
  return(p)
}

get_plotted_results <- function(fit, title) {
  actual <- fit$testData$SA_cm3
  predicted <- predict(fit$rf, fit$testData)
  residuals <- actual - predicted
  relative_residuals <- (actual - predicted) / actual
  
  df <- data.frame(SA_actual = actual, SA_predicted = predicted, residuals = residuals, relative_residuals = relative_residuals)
  
  p <- plot_results(df, title)
  
  if (min(df$SA_predicted) < 0) {
    df_neg <- df %>% filter(SA_predicted <= 0)
    print(paste("Negative predictions", nrow(df_neg)))
    p_neg <- plot_results(df_neg, paste(title, "negative predictions", sep = ", "))
    return(list(p, p_neg))
  }
  
  return(p)
}



# Load fitted model
# predict the test data with the model
# plot the test data and predictions

# fit_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/all_feature_subsets/model_basic_fitted_models.rds"
# fit <- readRDS(fit_path)[[7]]

# fit_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing/dataset.rds"
# fit_unf <- readRDS(fit_path)[[1]]
# fit_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing/dataset_uvb_so2_filtered.rds"
# fit_uvb_so2 <- readRDS(fit_path)[[1]]
# res_unf <- get_plotted_results(fit_unf, title = "Hyytiälä, unfiltered dataset")
# res_uvb_so2 <- get_plotted_results(fit_uvb_so2, title = "Hyytiälä, data filtered with UVB and SO2")
# plot(res_unf)
# plot(res_uvb_so2)


save_plots <- function(model_path, target_dir, title) {
  fit <- readRDS(model_path)[[1]]
  dataset_name <- fit$datasetName
  title <- paste(title, dataset_name, sep = ", ")
  res_plot <- get_plotted_results(fit, title)
  
  target_path <- file.path(target_dir, paste0("prediction_plots_", dataset_name, ".png"))
  ggsave(target_path, plot = res_plot)#, width = 9, height = 6)
}


model_path_list <- list.files(path = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing", full.names = TRUE)
target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results/same_features_as_beijing"
purrr::map(model_path_list, target_dir = target_dir, title = "Hyytiälä", save_plots)

model_path_list <- list.files(path = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing_no_outlier_filtering", full.names = TRUE)
target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results/same_features_as_beijing_no_outlier_filtering"
purrr::map(model_path_list, target_dir = target_dir, title = "Hyytiälä", save_plots)

model_path_list <- list.files(path = "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy", full.names = TRUE)
target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results/same_features_as_hyy"
purrr::map(model_path_list, target_dir = target_dir, title = "Beijing", save_plots)

model_path_list <- list.files(path = "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy_no_outlier_filtering", full.names = TRUE)
target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results/same_features_as_hyy_no_outlier_filtering"
purrr::map(model_path_list, target_dir = target_dir, title = "Beijing", save_plots)




