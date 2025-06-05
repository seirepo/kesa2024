setwd("/scratch/dongelr1/susannar/kesa2024")

#####
fnames <- list.files("/scratch/dongelr1/susannar/kesa2024/results/hyytiala/scores/same_features_as_beijing", full.names=TRUE)
rdata <- lapply(fnames, readRDS)

path_hyy  <- "/scratch/dongelr1/susannar/kesa2024/results2025/hyytiala"

t_hyy <- file.path(path_hyy, "scores")

lapply(rdata, function(rdata) {
  p <- file.path(path_hyy, "scores", paste(rdata$dataset_name[[1]], ".csv", sep=""))
  write.csv(rdata, file=p, row.names=FALSE)
})
#####


#####
fnames <- list.files("/scratch/dongelr1/susannar/kesa2024/results/beijing/scores/same_features_as_hyy", full.names=TRUE)
rdata <- lapply(fnames, readRDS)

path_bei  <- "/scratch/dongelr1/susannar/kesa2024/results2025/beijing"

t_hyy <- file.path(path_bei, "scores")

lapply(rdata, function(rdata) {
  p <- file.path(path_bei, "scores", paste(rdata$dataset_name[[1]], ".csv", sep=""))
  write.csv(rdata, file=p, row.names=FALSE)
})
#####
