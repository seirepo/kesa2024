setwd("/scratch/dongelr1/susannar/kesa2024")

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(purrr)
library(RColorBrewer)

font_size <- 12
basic_theme <- theme(legend.text=element_text(size = font_size),
                     axis.text.x = element_text(size = font_size),
                     axis.text.y = element_text(size = font_size),
                     axis.title=element_text(size = font_size))

color_palette <- brewer.pal(n = 4, name = "Set1")

average_importances <- function(combined_importance) {
  mean_importance <- combined_importance %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(mean_importance = mean(importance, na.rm = TRUE)) %>%
    dplyr::arrange(mean_importance)
  return(mean_importance)
}

# Importance plot
create_importance_plot <- function(combined_importance, title = "", xlimits, xbreaks) {
  mean_importance <- average_importances(combined_importance)
  
  # The resulting mean_importances are in ascending order, but when the combined_importance is plotted on a vertical
  # axis, the factor levels are placed on the axis starting from the bottom
  combined_importance$feature <- factor(combined_importance$feature, levels = mean_importance$feature)
  
  p <- ggplot(combined_importance, aes(x = importance, y = feature, shape = dataset)) +
    # geom_vline(xintercept = 1.0) +
    geom_errorbar(aes(xmin = importance.05, xmax = importance.95), width = 0, linewidth = 1,
                  position = position_dodge(width = 0.5)) + 
    geom_point(data = combined_importance, aes(x = importance, y = feature, fill = dataset), size = 2.5,
               position = position_dodge(width = 0.5)) +
    # scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    scale_fill_manual(values = setNames(color_palette, c("hyy", "bei"))) +
    # scale_shape_manual(values = c(21, 24)) +
    scale_shape_manual(values = c(21, 21)) +
    theme(panel.border = element_rect(linetype = 1, fill = NA)) +
    ylab("Feature") +
    xlab("Importance") +
    scale_x_continuous(limits = xlimits, breaks = xbreaks) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom") +
    basic_theme +
    ggtitle(title)
  
  p <- p + geom_hline(yintercept = seq(1.5, length(unique(combined_importance$feature)) - 0.5, by = 1), 
                      linetype = "dashed", color = "grey")
  return(p)
}

plot_feature_effect_models <- function(features, test_data, ale_res) {
  models <- c("rf", "lm")
  dff <- data.frame()
  
  for(i in seq_along(models)) {
    ale_res[[i]][[features]][["Type"]] <- models[[i]]
    dff <- rbind(dff, ale_res[[i]][[features]])
  } 
  
  dff$Type <- factor(dff$Type, levels = models)
  
  color_palette <- brewer.pal(n = 8, name = "Pastel2")
  
  p <- ggplot(dff, aes(x = .borders, y = .value, linetype = Type)) +
    geom_line(linewidth = 1.3, color = "black") +  # Black "outline"
    geom_line(aes(color = Type), linewidth = 1) + # Actual colored line
    labs(x = features, y = "SA") +
    scale_color_manual(values = color_palette) +
    # theme(panel.background = element_rect(fill = bgc)) + 
    scale_linetype_manual(values = rep("solid", 4)) +
    theme(panel.border=element_rect(linetype = 1, fill=NA)) +
    scale_fill_manual(values = setNames(c(color_palette[5], color_palette[8]), c("rf", "lm")),
                      breaks = c("rf", "lm")) +
    basic_theme +
    geom_rug(data = test_data, aes(x = !!sym(features), y = NULL), linetype = "solid", color = "black", alpha = 0.1)
  return(p)
}


########################################################
#### Save importances of the corresponding datasets ####
########################################################

target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/comparisons"

create_comparison_fi <- function(fi_path_hyy, fi_path_bei) {
  fi_hyy <- readRDS(fi_path_hyy)
  fi_bei <- readRDS(fi_path_bei)
  
  fi_hyy <- fi_hyy %>% filter(model == "rf") %>% mutate(dataset = "hyy")
  fi_bei <- fi_bei %>% filter(model == "rf") %>% mutate(dataset = "bei") 
  
  combined <- rbind(fi_hyy, fi_bei)
  return(combined)
}

hyy1 <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results/same_features_as_beijing/importances_dataset_uvb_so2_filtered.rds"
bei1 <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results/same_features_as_hyy/importances_dataset_uvb_so2_filtered.rds"
c1 <- create_comparison_fi(hyy1, bei1)

hyy2 <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results/same_features_as_beijing/importances_dataset_uvb_filtered.rds"
bei2 <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results/same_features_as_hyy/importances_dataset_uvb_filtered.rds"
c2 <- create_comparison_fi(hyy2, bei2)

hyy3 <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results/same_features_as_beijing/importances_dataset.rds"
bei3 <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results/same_features_as_hyy/importances_dataset.rds"
c3 <- create_comparison_fi(hyy3, bei3)

x_max <- max(sapply(list(c1, c2, c3), function(x) max(x$importance.95)))
x_min <- min(sapply(list(c1, c2, c3), function(x) min(x$importance.05)))
x_max <- ceiling(x_max * 10) / 10
x_min <- floor(x_min * 10) / 10
xlimits <- c(x_min, x_max)
xbreaks <- seq(x_min, x_max, 0.2)

p1 <- create_importance_plot(c1, "UVB and SO2 filtered", xlimits, xbreaks)#, Beijing and Hyytiälä")
# plot(p1)

p2 <- create_importance_plot(c2, "UVB filtered", xlimits, xbreaks)#, Beijing and Hyytiälä")
# plot(p2)

p3 <- create_importance_plot(c3, "Unfiltered", xlimits, xbreaks)#, Beijing and Hyytiälä")
# plot(p3)

plots_all <- ggarrange(plotlist = list(p1, p2, p3), common.legend = TRUE, legend = "bottom", nrow = 1)

color_palette <- brewer.pal(n = 4, name = "Set1")

ggsave(file.path(target_dir, "importances_dataset_uvb_so2_filtered.png"), plot = p1, width = 5)
ggsave(file.path(target_dir, "importances_dataset_uvb_filtered.png"), plot = p2, width = 5)
ggsave(file.path(target_dir, "importances_dataset.png"), plot = p3, width = 5)
ggsave(file.path(target_dir, "importances_all.png"), plot = plots_all, width = 13, height = 6)


########################################################
#### Save ale results of the corresponding datasets ####
########################################################

dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/interpret_results/same_features_as_beijing"
dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/interpret_results/same_features_as_hyy"

model_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models/same_features_as_beijing"
model_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/fitted_models/same_features_as_hyy"

target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/comparisons"

ale_paths_hyy <- list(
  file.path(dir_hyy, "ale_results_dataset_uvb_so2_filtered.rds"),
  file.path(dir_hyy, "ale_results_dataset_uvb_filtered.rds"),
  file.path(dir_hyy, "ale_results_dataset.rds")
)

ale_paths_bei <- list(
  file.path(dir_bei, "ale_results_dataset_uvb_so2_filtered.rds"),
  file.path(dir_bei, "ale_results_dataset_uvb_filtered.rds"),
  file.path(dir_bei, "ale_results_dataset.rds")
)

ales_hyy <- lapply(ale_paths_hyy, readRDS)
ales_bei <- lapply(ale_paths_bei, readRDS)

fit_paths_hyy <- list(
  file.path(model_dir_hyy, "dataset_uvb_filtered.rds"),
  file.path(model_dir_hyy, "dataset_uvb_so2_filtered.rds"),
  file.path(model_dir_hyy, "dataset.rds" )
)

fit_paths_bei <- list(
  file.path(model_dir_bei, "dataset_uvb_filtered.rds"),
  file.path(model_dir_bei, "dataset_uvb_so2_filtered.rds"),
  file.path(model_dir_bei, "dataset.rds" )
)

# Fits are needed only to get the test data..
fits_hyy <- lapply(fit_paths_hyy, readRDS) %>% lapply(function(x) x[[1]])
fits_bei <- lapply(fit_paths_bei, readRDS) %>% lapply(function(x) x[[1]])

# Calculate average importance over all importances to get the order of features for ale plots
c1 <- c1 %>% mutate(data = "UVB_SO2")
c2 <- c2 %>% mutate(data = "UVB")
c3 <- c3 %>% mutate(data = "unfiltered")
comb_all <- rbind(c1, c2, c3)
av_imp <- average_importances(comb_all)
features <- arrange(av_imp, desc(mean_importance))$feature


m <- list("UVB_SO2", "UVB", "Unfiltered")
# For each feature, produce an ale plot of each of the sites
all_plots <- list()
for (k in 1:length(features)) {
  fplots <- list()
  for (i in 1:3) {
    fit_h <- fits_hyy[[i]]#[[1]]
    fit_b <- fits_bei[[i]]#[[1]]
    ale_h <- ales_hyy[[i]]
    ale_b <- ales_bei[[i]]
    ph <- plot_feature_effect_models(features[[k]], fit_h$testData, ale_h) + ggtitle("Hyytiälä")
    pb <- plot_feature_effect_models(features[[k]], fit_b$testData, ale_b) + ggtitle("Beijing")
    plots <- list(ph, pb)
    names(plots) <- c(m[[i]], m[[i]])
    fplots[[i]] <- plots
  }
  all_plots[[k]] <- fplots
}

uvb_so2_plots <- lapply(all_plots, function(sublist) sublist[[1]])
uvb_plots <- lapply(all_plots, function(sublist) sublist[[2]])
unf_plots <- lapply(all_plots, function(sublist) sublist[[3]])

# Get paired plots, two at a time
plot_comparisons <- function(plotlist) {
  l <- lapply(plotlist, function(sublist) { g <- ggarrange(plotlist = sublist, common.legend = TRUE, legend = "bottom"); })
  splitted_l <- split(l, ceiling(seq_along(l) / 2))
  plots <- lapply(splitted_l, function(sublist) { g <- ggarrange(plotlist = sublist, common.legend = TRUE, legend = "bottom", ncol = 1, nrow = 2); })
  return(plots)
}

plots_uvb_so2 <- plot_comparisons(uvb_so2_plots)
plots_uvb <- plot_comparisons(uvb_plots)
plots_unfiltered <- plot_comparisons(unf_plots)

save_ale_plot <- function(plotlist, target_path, fname, title) {
  g <- ggarrange(plotlist = plotlist, common.legend = TRUE, legend = "bottom")
  g <- annotate_figure(g, ggpubr::text_grob(title))
  target_path <- file.path(target_dir, paste0(fname, ".png"))
  ggsave(target_path, plot = g, width = 16, height = 13)
  print(paste("Plot saved to", target_path))
}

g <- ggarrange(plotlist = plots_uvb_so2, common.legend = TRUE, legend = "bottom")
g <- annotate_figure(g, ggpubr::text_grob("Data filtered by UVB > 0.0045 and SO2 > 0.1"))
g

ggsave(file.path(target_dir, "ales_uvb_so2.png"), plot = g, width = 16, height = 13)

save_ale_plot(plots_uvb_so2, target_dir, "ales_uvb_so2", "Data filtered by UVB > 0.0045 and SO2 > 0.1")
save_ale_plot(plots_uvb, target_dir, "ales_uvb", "Data filtered by UVB > 0.0045")
save_ale_plot(plots_unfiltered, target_dir, "ales_unfiltered", "Unfiltered data")


