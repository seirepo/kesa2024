library("iml", lib = "/scratch/dongelr1/laantito/")
library("prediction", lib = "/scratch/dongelr1/laantito/")
library("Metrics", lib = "/scratch/dongelr1/laantito/")
library(tidyr)
library(dplyr)
library(future.apply)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)

setwd("/scratch/dongelr1/susannar/kesa2024")

load("/scratch/dongelr1/susannar/kesa2024/model_script_fitted_models.RData")
load("/scratch/dongelr1/susannar/kesa2024/model_script_score_df.RData")

# dat <- read.csv("/scratch/dongelr1/susannar/kesa2024/data/all_data_merged.csv", stringsAsFactors = FALSE) %>% drop_na() %>% within(rm(Time))
# dat <- dat %>% mutate(temp_K = temperature + 273.15) %>% within(rm(wind_direction, temperature)) #select(-wind_direction, -temperature)
# dat <- dat %>% filter(global_radiation > 10 & SO2 > 0.1)
# smpl <- sample_n(dat, dim(dat)[1] * 0.001)

get_importances <- function(model, data) {
  predictor <- Predictor$new(model, data = data %>% dplyr::select(-SA_cm3), y = data$SA_cm3)
  imp <- FeatureImp$new(predictor, loss = "rmse")
  return(imp)
}

# predictor_rf <- Predictor$new(fit7$rf, data = fit7$testData %>% within(rm(SA_cm3)), y = fit7$testData$SA_cm3)
# imp_rf <- FeatureImp$new(predictor_rf, loss = "rmse")
# p1 <- plot(imp_rf) + ggtitle("Random forest")
# predictor_lm <- Predictor$new(fit7$lm, data = fit7$testData %>% within(rm(SA_cm3)), y = fit7$testData$SA_cm3)
# imp_lm <- FeatureImp$new(predictor_lm, loss = "rmse")
# p2 <- plot(imp_lm) + ggtitle("Linear model")
# plot1 <- ggarrange(plotlist = list(p1, p2), common.legend = TRUE, nrow = 1, ncol = 2, legend = "bottom")
# annotate_figure(plot1, top = text_grob("Feature importances of model with all features and proxies"))

# plot_importances <- function(model, data) {
#   predictor <- Predictor$new(model, data = data, y = data$SA_cm3)
#   imp <- FeatureImp$new(predictor, loss = "rmse")
#   plot(imp)
# }


# Calculate and add features to test data to calculate importances for model using all features
mutate_test_data <- function(dat) {
  # Calculate reaction constant
  M <- 0.101 * (1.381 * 1e-23 * dat$temp_K)^-1
  k1 <- 4e-31
  k2 <- 3.3
  k3 <- 2e-12
  k5 <- -0.8
  A <- k1 * M * (300 / dat$temp_K)^k2
  k <- A * k3 / (A + k3) * exp(k5 * (1 + log10(A / k3)^2)^-1)
  
  # Calculate proxies x1-x5 and mutate data
  dat <- dat %>% mutate(k = k) %>%
    mutate(x1 = k * global_radiation * SO2 / CS_rate) %>%
    mutate(x2 = k * global_radiation * SO2) %>%
    mutate(x3 = k * global_radiation * SO2^0.5) %>%
    mutate(x4 = k * global_radiation * SO2 / relative_humidity) %>%
    mutate(x5 = k * global_radiation * SO2 / (CS_rate * relative_humidity))
  
  return(dat)
}

fit_obj <- fits[[8]]

# TODO: calculate FI for training data and see whether the model overfits (= FI for training data actually differs from ratio 1 but FI for training data is closer to 1)

# m_test <- mutate_test_data(fit7$testData)
imp_rf_test <- get_importances(fit_obj$rf, fit_obj$testData)
imp_lm_test <- get_importances(fit_obj$lm, fit_obj$testData)

imp_rf_train <- get_importances(fit_obj$rf, fit_obj$trainData)
imp_lm_train <- get_importances(fit_obj$lm, fit_obj$trainData)


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

create_comparison_plot <- function(imp_train, imp_test, title) {
  combined <- combine_test_train_results(list(imp_train, imp_test))
  p <- ggplot(combined, aes(x = importance, y = feature, shape = dataset)) +
    geom_point(aes(fill = dataset), size = 2.5) +
    scale_shape_manual(values = c(21, 24)) +
    theme(legend.position = "bottom") +
    ggtitle(title)
  return(p)
}

create_comparison_plot(imp_rf_train, imp_rf_test, "Feature importances of random forest with all features")
create_comparison_plot(imp_lm_train, imp_lm_test, "Feature importances of linear model with all features")


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

create_importance_plot <- function(combined_df) {

  # TODO: Calculate mean importance for each of the features (importance in rf model and importance in linear model) and
  # combine them into one dataframe (one row per feature with its mean importance over models)

  # mean_importance <- combined_df %>%
  #   group_by("feature") %>%
  #   summarise(mean_importance = mean(importance, na.rm = TRUE))
  # print(mean_importance)

  #combined_df$feature <- factor(combined_df$feature, levels = mean_importance$feature)


  color_palette <- brewer.pal(n = 4, name = "Dark2")

  p <- ggplot(combined_df, aes(x = importance, y = feature, shape = model)) +
    geom_point(data = combined_df, aes(x = importance, y = feature, fill = model), size = 2.5, stroke = 1) +
    scale_fill_manual(values = setNames(color_palette, c("rf", "lm"))) +
    scale_shape_manual(values = c(21, 24)) +
    # coord_cartesian(xlim = c(1, 3.3)) +
    theme(panel.border = element_rect(linetype = 1, fill = NA)) +
    ylab("Feature") +
    xlab("Importance") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom")

  return(p)
}


r <- combine_importance_results(list(imp_rf_train, imp_lm_train))

create_importance_plot(r)







