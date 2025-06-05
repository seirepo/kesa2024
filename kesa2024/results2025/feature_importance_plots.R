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
#library("iml", lib = "/scratch/dongelr1/laantito/")
#library("prediction", lib = "/scratch/dongelr1/laantito/")
#library("Metrics", lib = "/scratch/dongelr1/laantito/")

font_size <- 12
basic_theme <- theme(legend.text=element_text(size = font_size),
                     axis.text.x = element_text(size = font_size),
                     axis.text.y = element_text(size = font_size),
                     axis.title=element_text(size = font_size))

# model_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/model_basic_fitted_models.rds"
# score_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/model_basic_score_df.rds"
# target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/original_models/result_exploration"


color_palette <- brewer.pal(n = 8, name = "Set2")[7:8]


##################################################################
# Create feature importance plots with the same x-axis and combine them
##################################################################

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
  #mean_importance <- combined_importance %>%
  #  dplyr::group_by(feature) %>%
  #  dplyr::summarise(mean_importance = mean(importance, na.rm = TRUE)) %>%
  #  dplyr::arrange(mean_importance)
  #return(mean_importance)
  rf_only <- combined_importance %>% filter(model == "rf")
  rf_sorted <- rf_only %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(mean_importance = mean(importance, na.rm = TRUE)) %>%
    #dplyr::arrange(desc(mean_importance))
    dplyr::arrange(mean_importance)
  return(rf_sorted)
}

create_importance_plot_from_list <- function(importance_list, title = "") {
  print("1")
  x_max <- max(sapply(importance_list, function(x) max(x$importance.95)))
  x_min <- min(sapply(importance_list, function(x) min(x$importance.05)))
  x_max <- ceiling(x_max * 10) / 10
  x_min <- floor(x_min * 10) / 10
  xlimits <- c(x_min, x_max)
  
  print("2")
  plots <- lapply(importance_list, function(df) {
    create_importance_plot(df, title = df$dataset_name)
  })
  
  print("3")
  # Force the x-axis to contain 1.0
  custom_breaks <- function(x) {
    unique(c(1.0, pretty(x)))
  }
  
  print("4")
  plots <- lapply(plots, function(p) {
    p + scale_x_continuous(breaks = custom_breaks, limits = xlimits)
  })
  
  print("5")
  g <- ggarrange(plotlist = plots, common.legend = TRUE, legend = "bottom", nrow = 1)
  g <- annotate_figure(g, ggpubr::text_grob(title))
  return(g)
}


create_importance_plot <- function(combined_importance, title = "") {
  print("2.1")
  mean_importance <- average_importances(combined_importance)
  # The resulting mean_importances are in ascending order, but when the combined_importance is plotted on a vertical
  # axis, the factor levels are placed on the axis starting from the bottom
  combined_importance$feature <- factor(combined_importance$feature, levels = mean_importance$feature)
  
  print("2.2")
  custom_breaks <- function(x) {
    unique(c(1.0, pretty(x)))
  }
  
  print("2.3")
  # Spell out the model names to match the color labels in scale_fill_manual below
  combined_importance <- combined_importance %>%
    mutate(model = dplyr::recode(model, rf = "random forest", lm = "linear model"))
  
  #print(combined_importance)
  
  print("2.4")
  p <- ggplot(combined_importance, aes(x = importance, y = feature, shape = model)) +
    geom_errorbar(aes(xmin = importance.05, xmax = importance.95), width = 0, linewidth = 1,
                  position = position_dodge(width = 0.5)) + 
    geom_point(data = combined_importance, aes(x = importance, y = feature, fill = model), size = 2.5,
               position = position_dodge(width = 0.5)) +
    # scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    scale_x_continuous(breaks = custom_breaks) +
    # scale_fill_manual(values = setNames(color_palette, c("rf", "lm"))) +
    #scale_fill_manual(values = setNames(color_palette, c("random forest", "linear model"))) +
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


hyy_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results/uvb_so2_rh_temp_cs", site = "hyy")
bei_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results/uvb_so2_rh_temp_cs", site = "bei")
bei_imp["unfiltered"]$unfiltered$dataset_name = "Beijing"#, unfiltered"
hyy_imp["uvb_so2_filtered"]$uvb_so2_filtered$dataset_name = "Hyytiälä"#, uvb_so2_filtered"
l <- c(hyy_imp["uvb_so2_filtered"], bei_imp["unfiltered"])
p <- create_importance_plot_from_list(l, title = "")
p

ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025", paste0("importances_comparison_final_models.png")), plot = p, width = 8, height = 6)

#p <- create_importance_plot_from_list(hyy_imp["uvb_so2_filtered"], title = "Hyytiälä")
#ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025/hyytiala", paste0("importances_final_model.png")), plot = p, width = 5, height = 6)


################################################################################

bei_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/beijing/explain_results/same_features_as_hyy", site = "bei")

bei_imp["uvb_so2_filtered"]$uvb_so2_filtered$dataset_name = "UVB-SO2 filtered"

h <- hyy_imp[c("uvb_so2_filtered", "unfiltered")]
b <- bei_imp[c("uvb_so2_filtered", "unfiltered")]
# h <- hyy_imp
# b <- bei_imp

p2 <- create_importance_plot_from_list(b, title = "Beijing")

bei_imp["unfiltered"]$unfiltered$dataset_name = "Beijing, unfiltered"
hyy_imp["uvb_so2_filtered"]$uvb_so2_filtered$dataset_name = "Hyytiälä, UVB-SO2 filtered"
h <- c(hyy_imp["uvb_so2_filtered"], bei_imp["unfiltered"])

p1 <- create_importance_plot_from_list(h, title = "")

ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025", paste0("importances_comparison.png")), plot = p1, width = 10, height = 6)
ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025/beijing/", paste0("beijing_importances_comparison.png")), plot = p2, width = 10, height = 6)


################################################################################

hyy_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/explain_results/uvb_so2_rh_temp_cs", site = "hyy")
bei_imp <- load_importances(path = "/scratch/dongelr1/susannar/kesa2024/results2025/beijing/explain_results/uvb_temp_so2_o3_rh", site = "bei")
bei_imp["unfiltered"]$unfiltered$dataset_name = "Beijing"#, unfiltered"
hyy_imp["uvb_so2_filtered"]$uvb_so2_filtered$dataset_name = "Hyytiälä"#, uvb_so2_filtered"
l <- c(hyy_imp["uvb_so2_filtered"], bei_imp["unfiltered"])
p <- create_importance_plot_from_list(l, title = "")
p

ggsave(file.path("/scratch/dongelr1/susannar/kesa2024/results2025", paste0("importances_comparison_final_models_own_top5.png")), plot = p, width = 8, height = 6)
