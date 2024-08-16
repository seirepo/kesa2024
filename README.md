A repository for a summer intern's project during summer 2024. Code contains meteorological measurement data and code for processing it, a modeling code to train a linear model and a random forest model on two sets of preprocessed data and a bunch of functions to explain the resulting model predictions.

# Data
Contains data from three different sites, Hyytiälä, Beijing and Siberia, from which only Hyytiälä and Beijing are used. In `hyytiala/raw`, the folder `time_over_land` contains the data extracted from the .mat files in `tol_mat` using `preprocess/convert_tol.mlx`. The features are SA, air_pressure, temperature, global_radiation, NOx, O3, relative_humidity, SO2, wind_direction, wind_speed, UVB, tol, sector for Hyytiälä and SA, temperature, NOx, O3, relative_humidity, SO2, wind_direction, wind_speed, UVB, tol, sector for Beijing.

# Preprocessing
`preprocess/preprocess_hyytiala.R` produces two sets of preprocessed data, one with outliers in CS, NOx and SO2 filtered and one without, each with 4 different ways to further filter all observations by excluding nighttime observations and observations with too little SO2. They are saved using a fixed file path. Out of these, the data filtered with UVB and SO2 with outliers CS, NOx and SO2 filtered out is the most relevant, saved to `/data/hyytiala/preprocessed/uvb_so2_filtered.csv"`. Unfiltered data was used only to see that filtering out the outliers improves the modeling results. The resulting data is averaged or imputed (time over land) to 30 minute resolution. Missing values are not imputed (except time over land).

`preprocess/preprocess_beijing.R` also produces two sets of preprocessed data but each with 3 different ways to filter out observations, out of which only the unfiltered observations with similar outlier filtering of CS, NOx and SO2 is the most relevant, saved to `data/beijing/preprocessed/unfiltered.csv`. Missing values are not imputed.

# Modeling
The `models/common_model.R` is used to fit a random forest model and linear model to both preprocessed datasets separately. There is a fixed test-train split of 0.75. During the model training, learning curves can also be calculated optionally. In the end of the file, `train_models_with_outlier_filtering`, calling `fit_models`, is the default method to fit the models. The features used in the training are defined as an argument to `fit_models` and are either the ones to use or ones to exclude, depending on the flag `exclude_features`. The results, fit object, model scores and learning curves are then saved to `/results/site/dataset/result_type`, where dataset describes the data used to train the model.

# Explaining results
The functions used to explain the model predictions are mostly in `results/explore_results_functions.R` and are called for the different results from `results/calculate_scores_fi_ale.R`. `results/visual_verification.R` creates some plots of the predicted and actual test data. All results of the code are saved under `results/site/explain_results/dataset/`. It's best to run all code manually function by function.

`results/create_comparison_plots.R` would need to be updated to be used!
