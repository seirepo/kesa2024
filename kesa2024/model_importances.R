library("iml", lib = "/scratch/dongelr1/laantito/")
library("prediction", lib = "/scratch/dongelr1/laantito/")
library("Metrics", lib = "/scratch/dongelr1/laantito/")
library(tidyr)
library(dplyr)
library(future.apply)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(corrplot)

setwd("/scratch/dongelr1/susannar/kesa2024")

load("/scratch/dongelr1/susannar/kesa2024/model_script_fitted_models.RData")
# load("/scratch/dongelr1/susannar/kesa2024/model_script_score_df.RData")

# load("/scratch/dongelr1/susannar/kesa2024/fitted_models_f30.RData")


font_size = 15
basic_theme <- theme(legend.text=element_text(size = font_size),
                     axis.text.x = element_text(size = font_size),
                     axis.text.y = element_text(size = font_size),
                     axis.title=element_text(size = font_size))

#############################
#### Feature importances ####
#############################


get_importances <- function(model, data) {
  predictor <- Predictor$new(model, data = data %>% dplyr::select(-SA_cm3), y = data$SA_cm3)
  imp <- FeatureImp$new(predictor, loss = "rmse")
  return(imp)
}

# Compare train and test importances as an additional check for overfitting
combine_test_train_results <- function(train_test_importances) {
  combined <- data.frame()
  names <- c("train", "test")
  for (m in 1:length(names)) {
    df <- train_test_importances[[m]]$results
    df$dataset <- names[m]
    combined <- rbind(combined, df)
  }
  return(combined)
}

train_test_comparison_plot <- function(imp_train, imp_test, title) {
  combined <- combine_test_train_results(list(imp_train, imp_test))
  p <- ggplot(combined, aes(x = importance, y = feature, shape = dataset)) +
    geom_point(aes(fill = dataset), size = 2.5) +
    scale_shape_manual(values = c(train = 4, test = 24)) +
    theme(legend.position = "bottom",
          # legend.text=element_text(size = font_size),
          # axis.text.x = element_text(size = font_size),
          # axis.text.y = element_text(size = font_size),
          # axis.title=element_text(size = font_size)
          ) +
    basic_theme + 
    ggtitle(title)
  return(p)
}

fit_obj <- fits[[7]]

# train data importances: "which features are important for the model in the sense that it depends on them for making predictions"
# test data importances: how well the model generalizes
imp_rf_train <- get_importances(fit_obj$rf, fit_obj$trainData)
imp_rf_test <- get_importances(fit_obj$rf, fit_obj$testData)
imp_lm_train <- get_importances(fit_obj$lm, fit_obj$trainData)
imp_lm_test <- get_importances(fit_obj$lm, fit_obj$testData)


# Plot feature importances for each of the models separately
max_imp <- max(imp_rf_test$results$importance, imp_lm_test$results$importance)
min_imp <- min(imp_rf_test$results$importance, imp_lm_test$results$importance)
p1 <- plot(imp_rf_test) + ggtitle("Random forest") + basic_theme + scale_x_continuous(limits = c(min_imp, max_imp))
p2 <- plot(imp_lm_test) + ggtitle("Linear model") + basic_theme + scale_x_continuous(limits = c(min_imp, max_imp))
p <- ggarrange(plotlist = list(p1, p2), common.legend = TRUE, nrow = 1, ncol = 2, legend = "bottom")
p <- annotate_figure(p, top = text_grob(paste("Feature importances,", fit_obj$datasetName), size = 15))
p

# Create comparison plot for test and train data for each of the models
# x-axis interpretation: increase of model error if the feature is permuted if calculated with e_perm - e_orig, ratio if calculated with e_perm/e_orig (increase by a factor of the importance)
p1 <- train_test_comparison_plot(imp_rf_train, imp_rf_test, paste("Random forest,", fit_obj$datasetName))
p2 <- train_test_comparison_plot(imp_lm_train, imp_lm_test, paste("Linear model,", fit_obj$datasetName))
p <- ggarrange(plotlist = list(p1, p2), common.legend = TRUE, nrow = 1, ncol = 2, legend = "bottom")
p

# Check the correlation of data used to see how the features are correlated and if it should affect the interpretation of the importances
corr <- cor(x = fit_obj$testData, use = "pairwise.complete.obs")
corrplot(corr, method = "number")#, type = "upper")

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

average_importances <- function(combined_df) {
  mean_importance <- combined_df %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(mean_importance = mean(importance, na.rm = TRUE)) %>%
    dplyr::arrange(mean_importance)
  return(mean_importance)
}

create_importance_plot <- function(combined_df, title = "") {
  # mean_importance <- combined_df %>%
  #   dplyr::group_by(feature) %>%
  #   dplyr::summarise(mean_importance = mean(importance, na.rm = TRUE)) %>%
  #   dplyr::arrange(mean_importance)
  
  mean_importance <- average_importances(combined_df)
  combined_df$feature <- factor(combined_df$feature, levels = mean_importance$feature)

  color_palette <- brewer.pal(n = 4, name = "Dark2")
  
  p <- ggplot(combined_df, aes(x = importance, y = feature, shape = model)) +
    geom_errorbar(aes(xmin = importance.05, xmax = importance.95), width = 0, linewidth = 1,
                  position = position_dodge(width = 0.5)) + 
    geom_point(data = combined_df, aes(x = importance, y = feature, fill = model), size = 2.5,
               position = position_dodge(width = 0.5)) +
    # scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    scale_fill_manual(values = setNames(color_palette, c("rf", "lm"))) +
    scale_shape_manual(values = c(21, 21)) +
    # coord_cartesian(xlim = c(1, 2)) +
    theme(panel.border = element_rect(linetype = 1, fill = NA)) +
    ylab("Feature") +
    xlab("Importance") +
    theme(plot.title = element_text(hjust = 0.5, size = font_size),
          legend.title = element_blank(),
          legend.position = "bottom") +
    basic_theme +
    ggtitle(title)
    # theme(axis.text.y = element_text(size = 15)) +
    # theme(axis.title=element_text(size=15))
  
  p <- p + geom_hline(yintercept = seq(1.5, length(unique(combined_df$feature)) - 0.5, by = 1), 
                      linetype = "dashed", color = "grey")

  return(p)
}

combined_importance <- combine_importance_results(list(imp_rf_test, imp_lm_test))
create_importance_plot(combined_importance, title = fit_obj$datasetName)


######################################
# model specific feature importances #
######################################
# for comparison

v <- varImp(fit_obj$lm, scale = TRUE)[["importance"]]
v$Overall <- v$Overall / sum(v$Overall)
v

# df <- tibble::rownames_to_column(df, "VALUE")
# v["feature"] <- rownames(v)
# v

v$Overall <- v[order(v$Overall),]
v

plot(v)



###################
#### ALE plots ####
###################

fit_obj <- fits[[7]]

data <- fit_obj$testData

mod_rf <- Predictor$new(fit_obj$rf, data = data %>% dplyr::select(-SA_cm3), y = data$SA_cm3)
mod_lm <- Predictor$new(fit_obj$lm, data = data %>% dplyr::select(-SA_cm3), y = data$SA_cm3)

importances <- list(imp_rf_test, imp_lm_test)

eff_rf <- FeatureEffects$new(mod_rf)
eff_lm <- FeatureEffects$new(mod_lm)

# eff_rf$plot() + basic_theme
# eff_lm$plot()

plot_ale_with_title <- function(eff, title, vjust = 135, hjust = -15) {
  # eff <- eff$plot() + basic_theme # theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15))
  p <- plot(eff) +
    ggtitle(title) +
    theme(plot.title = element_text(vjust = vjust, hjust = hjust)) #+
    # theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) +
    # basic_theme
  return(p)
}

plot_ale_with_title_locscale <- function(eff, title) {
  plot_list <- lapply(eff$effects, function(effect) {
    p <- effect$plot() + 
      basic_theme
    return(p)
  })
  combined_plot <- ggarrange(plotlist = plot_list, nrow = 4, ncol = ceiling(length(plot_list) / 4))
  
  final_plot <- annotate_figure(combined_plot, top = ggpubr::text_grob(title, size = 15))

  return(final_plot)
}

# plot_ale_with_title_locscale(eff_rf, title = paste("Random forest,", fit_obj$datasetName))
# plot_ale_with_title_locscale(eff_lm, title = paste("Linear model,", fit_obj$datasetName))

# Fit 7
plot_ale_with_title(eff_rf, title = paste("Random forest,", fit_obj$datasetName), vjust = -8)
plot_ale_with_title(eff_lm, title = paste("Linear model,", fit_obj$datasetName))
# Fit 8
plot_ale_with_title(eff_rf, title = paste("Random forest,", fit_obj$datasetName), vjust = 177, hjust = 0.5)
plot_ale_with_title(eff_lm, title = paste("Linear model,", fit_obj$datasetName), vjust = 177, hjust = 0.5)

# Compute the accumulated local effects for all features
eff <- FeatureEffects$new(mod)
eff$plot()
eff$plot(features = c("air_pressure"))

####################

fit_obj <- fits[[7]]

ALE <- function(model, y, X) {
  mod <- Predictor$new(model, data = X, y = y)
  eff <- FeatureEffects$new(mod)
  return(eff$results)
}

X <- fit_obj$testData %>% dplyr::select(-SA_cm3)
y <- fit_obj$testData$SA_cm3


mods <- list(fit_obj$rf, fit_obj$lm)
ale_results <- lapply(mods, function(model) {ALE(model, y, X)})

# ale_results[[1]] <- ale_results[[1]][imp_rf_test$results$feature]

# save(ale_results, file="/scratch/dongelr1/laantito/ALE_all_30.5.RData")
#load(file="/scratch/dongelr1/laantito/ALE_est_30.5.RData")

models <- c("rf", "lm")

plot_feature_effect_models <- function(feat, data, res, bgc) {
  dff <- data.frame()
  
  for(i in seq_along(models)) {
    res[[i]][[feat]][["Type"]] <- models[[i]]
    dff <- rbind(dff, res[[i]][[feat]])
  } 
  
  dff$Type <- factor(dff$Type, levels = models)
  
  # View(dff)
  
  color_palette <- brewer.pal(n = 8, name = "Pastel2")
  
  p <- ggplot(dff, aes(x = .borders, y = .value, linetype = Type)) +
    geom_line(linewidth = 1.3, color = "black") +  # Black "outline"
    geom_line(aes(color = Type), linewidth = 1) + # Actual colored line
    labs(x = feat, y = "SA") +
    scale_color_manual(values = color_palette) +
    # theme(panel.background = element_rect(fill = bgc)) + 
    scale_linetype_manual(values = rep("solid", 4)) +
    theme(panel.border=element_rect(linetype = 1, fill=NA)) +
    scale_fill_manual(values = setNames(c(color_palette[5], color_palette[8]), c("rf", "lm")),
                      breaks = c("rf", "lm")) +
    geom_rug(data = data, aes(x = !!sym(feat), y = NULL), linetype = "solid", color = "black", alpha = 0.1) +
    basic_theme
  return(p)
}

features <- setdiff(colnames(X), c("SA_cm3"))
av_imp <- average_importances(combined_importance) %>% dplyr::arrange(desc(.))
features <- setdiff(av_imp$feature, c("SA_cm3"))

# plot_feature_effect_models(features[[1]], data = fit_obj$testData, res = ale_results, bgc = "gray")

l1 <- lapply(features, plot_feature_effect_models, data = fit_obj$testData, res = ale_results, bgc = "#F5F5DC")
plot1 <- ggpubr::ggarrange(plotlist = l1, nrow = 4, ncol = 5, common.legend = TRUE, legend = "bottom")
plot1 <- annotate_figure(plot1, top = ggpubr::text_grob(paste("ALE,", fit_obj$datasetName), size = 20))
plot1


# plot1 <- ggarrange(plotlist = l1, nrow = 4, ncol = 4, common.legend = TRUE, legend = "bottom")
# 
# # Annotate the figure with a main title
# final_plot <- annotate_figure(plot1, top = ggpubr::text_grob("Main title", face = "bold", size = 14))
# 
# # Print the final plot
# print(final_plot)



predictor <- Predictor$new(fit_obj$rf, data = fit_obj$testData %>% dplyr::select(-SA_cm3), y = fit_obj$testData$SA_cm3)

imp <- FeatureImp$new(predictor, loss = "rmse")

X <- fit_obj$testData %>% dplyr::select(-SA_cm3)
y <- fit_obj$testData$SA_cm3

mod <- Predictor$new(fit_obj$rf, data = X, y = y)
eff <- FeatureEffects$new(mod)


#############################
##### Interaction plots #####
#############################
# read https://christophm.github.io/interpretable-ml-book/interaction.html

fit_obj <- fits[[7]]

data <- fit_obj$testData

mod_rf <- Predictor$new(fit_obj$rf, data = data %>% dplyr::select(-SA_cm3), y = data$SA_cm3)
mod_lm <- Predictor$new(fit_obj$lm, data = data %>% dplyr::select(-SA_cm3), y = data$SA_cm3)

eff_rf <- FeatureEffects$new(mod_rf)
eff_lm <- FeatureEffects$new(mod_lm)

# Takes time and memory
interact <- Interaction$new(mod_rf)#, grid.size = 15)
plot(interact)

interact <- Interaction$new(mod_rf, feature = "global_radiation")
p <- plot(interact)
p
