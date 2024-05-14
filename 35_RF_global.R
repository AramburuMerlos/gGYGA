# author: Fernando Aramburu Merlos
# date: 2022-08-08
# update: 2022-09-29 for NNDM LOO CV and AOA and new predictors


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(terra)
library(ranger)
library(caret)
library(CAST)
library(stringr)


# DATA  ###########################################################

d <- fread("extrap_method/training_data/Y_ws_predictors.csv")

crops <- unique(d$crop) |> sort()

## Predictor names --------
pl <- fread("extrap_method/predictors.csv")
preds_rfed <- pl[type %like% "bioclim|soil|lat_abs", var] |>
  c(pl[type == "crop", str_subset(var, "maiz")]) |>
  str_replace("maiz", "crop")
preds_irri <- preds_rfed[!preds_rfed %in% pl[type == "soil", var]]

## NNDM LOOCV indices -------------
nndm_global <- readRDS("extrap_method/NNDM/nndm_global_indices.RDS")


# TUNE RF ###########################################################

tp_rfed <- expand.grid(
  mtry = seq(4, ceiling(length(preds_rfed) * .8), 2),
  min.node.size = c(1, 2, 4, 8),
  splitrule = "variance"
) |> setDT()

tp_irri <- expand.grid(
  mtry = seq(2, ceiling(length(preds_irri) * .8), 2),
  min.node.size = seq(2, 14, 4),
  splitrule = "variance"
) |> setDT()


mod_list <- vector(mode = "list", length = length(crops))

# loop across crops
for(i in seq_along(crops)) {

  if(crops[i] %like% "rice") {
    preds_rfed <- c(preds_rfed, "crop_ints")
    preds_irri <- c(preds_irri, "crop_ints")
  }

  tc_i <- trainControl(
    method = "cv",
    savePredictions = TRUE,
    index = nndm_global[[crops[i]]]$indx_train,
    indexOut = nndm_global[[crops[i]]]$indx_test
  )

  if(grepl("Rainfed", crops[i])) {
    mod_list[[i]] <- train(
      x = d[crop == crops[i], ..preds_rfed],
      y = d[crop == crops[i], yw],
      method = "ranger",
      trControl = tc_i,
      tuneGrid = tp_rfed,
      importance = "impurity",
      seed = 0
    )
  } else {
    mod_list[[i]] <- train(
      x = d[crop == crops[i], ..preds_irri],
      y = d[crop == crops[i], yp],
      method = "ranger",
      trControl = tc_i,
      tuneGrid = tp_irri,
      importance = "impurity",
      seed = 0
    )
  }
  if(crops[i] %like% "rice") {
    preds_rfed <- preds_rfed[preds_rfed != "crop_ints"]
    preds_irri <- preds_irri[preds_irri != "crop_ints"]
  }
}

names(mod_list) <- crops
saveRDS(mod_list, "extrap_method/NNDM/NNDM_LOOCV_RF_model_list.RDS")


# RF tuning parameters with lowest NNDM LOO CV RMSE and its R2
dt <- rbindlist(lapply(mod_list, `[[`, "pred"), idcol = "crop")
dt <- dt[, .(RMSE = RMSE(pred, obs), R2 = cor(pred, obs)^2), by = .(crop, mtry, min.node.size)]
dt[, .SD[which.min(RMSE)], by = crop]

fwrite(dt, "extrap_method/NNDM/NNDM_LOOCV_RF_global_results.csv")
