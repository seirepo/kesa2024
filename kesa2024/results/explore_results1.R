setwd("/scratch/dongelr1/susannar/kesa2024")

library(dplyr)
# library(lubridate)
library(ggplot2)
library(ggpubr)
library(tidyr)
# library("scales")
library(purrr)
# library('stringr')
library(RColorBrewer)

# fi_uvb_hyy_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/result_exploration/importances_dataset_uvb_filtered.rds"
# ale_uvb_hyy_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/result_exploration/ale_results_dataset_uvb_filtered.rds"
# fi_uvb_so2_hyy_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/result_exploration/importances_dataset_uvb_so2_filtered.rds"
# ale_uvb_so2_hyy_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/result_exploration/ale_results_dataset_uvb_so2_filtered.rds"
# fi_hyy_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/result_exploration/importances_dataset.rds"
# ale_hyy_path <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/result_exploration/ale_results_dataset.rds"
# 
# fi_uvb_bei_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/untransformed/result_exploration/importances_dataset_uvb_filtered.rds"
# ale_uvb_bei_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/untransformed/result_exploration/ale_results_dataset_uvb_filtered.rds"
# fi_uvb_so2_bei_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/untransformed/result_exploration/importances_dataset_uvb_so2_filtered.rds"
# ale_uvb_so2_bei_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/untransformed/result_exploration/ale_results_dataset_uvb_so2_filtered.rds"
# fi_bei_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/untransformed/result_exploration/importances_dataset.rds"
# ale_bei_path <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/untransformed/result_exploration/ale_results_dataset.rds"
# 
# model_dir_hyy <- "/scratch/dongelr1/susannar/kesa2024/results/hyytiala/fitted_models"
# model_dir_bei <- "/scratch/dongelr1/susannar/kesa2024/results/beijing/untransformed/fitted_models"


font_size <- 12
basic_theme <- theme(legend.text=element_text(size = font_size),
                     axis.text.x = element_text(size = font_size),
                     axis.text.y = element_text(size = font_size),
                     axis.title=element_text(size = font_size))

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
    scale_fill_manual(values = setNames(color_palette, c("bei", "hyy"))) +
    scale_shape_manual(values = c(21, 24)) +
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
#### Plot importances of the corresponding datasets ####
########################################################

fi1_hyy <- readRDS(fi_uvb_hyy_path)
fi2_hyy <- readRDS(fi_uvb_so2_hyy_path)
fi3_hyy <- readRDS(fi_hyy_path)

fi1_bei <- readRDS(fi_uvb_bei_path)
fi2_bei <- readRDS(fi_uvb_so2_bei_path)
fi3_bei <- readRDS(fi_bei_path)

fi1_hyy <- fi1_hyy %>% filter(model == "rf") %>% mutate(dataset = "hyy") # UVB filtered
fi2_hyy <- fi2_hyy %>% filter(model == "rf") %>% mutate(dataset = "hyy") # UVB and SO2 filtered
fi3_hyy <- fi3_hyy %>% filter(model == "rf") %>% mutate(dataset = "hyy") # Unfiltered

fi1_bei <- fi1_bei %>% filter(model == "rf") %>% mutate(dataset = "bei") # UVB filtered
fi2_bei <- fi2_bei %>% filter(model == "rf") %>% mutate(dataset = "bei") # UVB and SO2 filtered
fi3_bei <- fi3_bei %>% filter(model == "rf") %>% mutate(dataset = "bei") # Unfiltered

combined1 <- rbind(fi1_hyy, fi1_bei)
combined2 <- rbind(fi2_hyy, fi2_bei)
combined3 <- rbind(fi3_hyy, fi3_bei)

x_max <- max(sapply(list(combined1, combined2, combined3), function(x) max(x$importance.95)))
x_min <- min(sapply(list(combined1, combined2, combined3), function(x) min(x$importance.05)))
x_max <- ceiling(x_max * 10) / 10
x_min <- floor(x_min * 10) / 10
xlimits <- c(x_min, x_max)
xbreaks <- seq(x_min, x_max, 0.2)
# xbreaks <- sort(c(1.0, seq(x_min, x_max, 0.2)))

color_palette <- brewer.pal(n = 4, name = "Set1")

p1 <- create_importance_plot(combined1, "UVB filtered", xlimits, xbreaks)#, Beijing and Hyytiälä")
# plot(p1)

p2 <- create_importance_plot(combined2, "UVB and SO2 filtered", xlimits, xbreaks)#, Beijing and Hyytiälä")
# plot(p2)

p3 <- create_importance_plot(combined3, "Unfiltered", xlimits, xbreaks)#, Beijing and Hyytiälä")
# plot(p3)

ggarrange(plotlist = list(p1, p2, p3), common.legend = TRUE, legend = "bottom", nrow = 1)


########################################################
#### Plot ale results of the corresponding datasets ####
########################################################

# ale1_hyy <- readRDS(ale_uvb_hyy_path)
# ale2_hyy <- readRDS(ale_uvb_so2_hyy_path)
# ale3_hyy <- readRDS(ale_hyy_path)
# 
# ale1_bei <- readRDS(ale_uvb_bei_path)
# ale2_bei <- readRDS(ale_uvb_so2_bei_path)
# ale3_bei <- readRDS(ale_bei_path)

# Read ale results of both sites
ale_paths_hyy <- list(ale_uvb_hyy_path, ale_uvb_so2_hyy_path, ale_hyy_path)
ale_paths_bei <- list(ale_uvb_bei_path, ale_uvb_so2_bei_path, ale_bei_path)

ales_hyy <- lapply(ale_paths_hyy, readRDS)
ales_bei <- lapply(ale_paths_bei, readRDS)

# Read fit results of both sites
fits_hyy <- list.files(path = model_dir_hyy, full.names = TRUE)
fits_bei <- list.files(path = model_dir_bei, full.names = TRUE)

fits_hyy <- lapply(fits_hyy, readRDS) %>% lapply(function(x) x[[1]])
fits_bei <- lapply(fits_bei, readRDS) %>% lapply(function(x) x[[1]])

# fit1_hyy <- fits_hyy[[1]][[1]]
# fit1_bei <- fits_bei[[1]][[1]]
# fit2_hyy <- fits_hyy[[2]][[1]]
# fit2_bei <- fits_bei[[2]][[1]]
# fit3_hyy <- fits_hyy[[3]][[1]]
# fit3_bei <- fits_bei[[3]][[1]]

# Calculate average importance over all importances to get the order of ale plots
c1 <- combined1 %>% mutate(data = "UVB")
c2 <- combined2 %>% mutate(data = "UVB_SO2")
c3 <- combined3 %>% mutate(data = "unfiltered")
comb_all <- rbind(c1, c2, c3)
av_imp <- average_importances(comb_all)
features <- arrange(av_imp, desc(mean_importance))$feature

# Get the order of importance according to each filtering type to use to order ALE plots,
# but maybe it's easier if they are all in the same order so skip this
# features <- lapply(list(c1, c2, c3), function(c) {
#   av_fi <- average_importances(c)
#   f <- arrange(av_fi, desc(mean_importance))$feature
#   return(f)
# })

m <- list("UVB", "UVB, SO2", "Unfiltered")
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
    # names(plots) <- c(paste(m[[i]], "hyy"), paste(m[[i]], "bei"))
    names(plots) <- c(m[[i]], m[[i]])
    fplots[[i]] <- plots
  }
  all_plots[[k]] <- fplots
}

uvb_plots <- lapply(all_plots, function(sublist) sublist[[1]])
uvb_so2_plots <- lapply(all_plots, function(sublist) sublist[[2]])
unf_plots <- lapply(all_plots, function(sublist) sublist[[3]])

# l <- lapply(uvb_plots, function(sublist) sublist[[1]])

# This produces the comparison plots for both of the sites when the data is filtered only by UVB.
# l <- lapply(uvb_plots, function(sublist) { g <- ggarrange(plotlist = sublist, common.legend = TRUE, legend = "bottom"); })
# splitted_l <- split(l, ceiling(seq_along(l) / 2))
# plots <- lapply(splitted_l, function(sublist) { g <- ggarrange(plotlist = sublist, common.legend = TRUE, legend = "bottom", ncol = 1); })

# Same as above: get paired plots, two at a time
plot_comparisons <- function(plotlist) {
  l <- lapply(plotlist, function(sublist) { g <- ggarrange(plotlist = sublist, common.legend = TRUE, legend = "bottom"); })
  splitted_l <- split(l, ceiling(seq_along(l) / 2))
  plots <- lapply(splitted_l, function(sublist) { g <- ggarrange(plotlist = sublist, common.legend = TRUE, legend = "bottom", ncol = 1, nrow = 2); })
  return(plots)
}

plots_uvb <- plot_comparisons(uvb_plots)
plots_uvb_so2 <- plot_comparisons(uvb_so2_plots)
plots_unfiltered <- plot_comparisons(unf_plots)

g <- ggarrange(plotlist = plots_uvb_so2, common.legend = TRUE, legend = "bottom")
g <- annotate_figure(g, ggpubr::text_grob("Data filtered by UVB > 0.0045 and SO2 > 0.1"))
g

g1 <- ggarrange(plotlist = plots_unfiltered, common.legend = TRUE, legend = "bottom")
g1 <- annotate_figure(g1, ggpubr::text_grob("Unfiltered data"))
g1

g2 <- ggarrange(plotlist = plots_uvb, common.legend = TRUE, legend = "bottom")
g2 <- annotate_figure(g2, ggpubr::text_grob("Data filtered with UVB > 0.0045"))
g2

# target_dir <- "/scratch/dongelr1/susannar/kesa2024/results/"
# ggsave(file.path(target_dir, "ale_comparison_hyy_bei.png"), plot = g, width = 9, height = 6)



# p1 <- plot_feature_effect_models("UVB", fits_hyy[[2]][[1]]$testData, ales_hyy[[2]]) + ggtitle("Hyytiälä")
# p2 <- plot_feature_effect_models("UVB", fits_bei[[2]][[1]]$testData, ales_bei[[2]]) + ggtitle("Beijing")
# ggarrange(plotlist = list(p1, p2))


########################################################
############ Explore the fitted models #################
########################################################


titles <- list("UVB filtered", "UVB and SO2 filtered","Unfiltered")

hyy_plots <- purrr::map2(fits_hyy, titles, function(fit, title) ggplot(fit$rf, highlight = TRUE) + ggtitle(title))
hyy_res <- ggarrange(plotlist = hyy_plots, common.legend = TRUE, legend = "bottom", nrow = 1)
hyy_res <- annotate_figure(hyy_res, ggpubr::text_grob("Hyytiälä"))

bei_plots <- purrr::map2(fits_bei, titles, function(fit, title) ggplot(fit$rf, highlight = TRUE) + ggtitle(title))
bei_res <- ggarrange(plotlist = bei_plots, common.legend = TRUE, legend = "bottom", nrow = 1)
bei_res <- annotate_figure(bei_res, ggpubr::text_grob("Beijing"))

hyy_res
bei_res


test <- read.table("/scratch/dongelr1/susannar/kesa2024/data/hyytiala/raw/condensation_sink/CS_2004_2023_10min.txt")
