# author: Fernando Aramburu Merlos

# I use ranger to re train the model with the tuning parameters selected through NNDM


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
library(stringr)

outdir <- "extrap_method/global_estimates/30sec/unmasked/"
dir.create(outdir, F, T)


# DATA  ###########################################################

d <- fread("extrap_method/training_data/Y_ws_predictors.csv")
crops <- d[, unique(crop)]

## Predictors --------------
pred_list <- fread("extrap_method/predictors.csv")

### Climate -------
clim_rast <- rast(pred_list[type %like% "bioclim", path])
clim_nams <- pred_list[type %like% "bioclim", var]
names(clim_rast) <- clim_nams


### Soil ----------------
# soil and crop variables are crop dependent
soil_rast <- rast(pred_list[type == "soil", path])
soil_nams <- pred_list[type %like% "soil", var]
names(soil_rast) <- soil_nams

### Lat -------
lat_abs_rast <- rast(pred_list[type == "lat_abs", path])

### Crop -----------------
crop_rast <- rast(pred_list[type %like% "crop", path])


## Predictors names --------
preds_rfed <- pred_list[type %like% "bioclim|soil|lat_abs", var] |>
  c(pred_list[type == "crop", str_subset(var, "maiz")]) |>
  str_replace("maiz", "crop")
preds_irri <- preds_rfed[!preds_rfed %in% pred_list[type %in% c("bioclim_prec", "soil"), var]]


# Prediciton function ------------
pf <- function(model, ...) {
  library(ranger)
  predict(model, ..., num.threads = 3)$predictions
}

# TUNING results ---------------
dt <- fread("extrap_method/NNDM/NNDM_LOOCV_RF_global_results.csv")
dt <- dt[, .SD[which.min(RMSE)], by = crop]

crp = "Rainfed wheat"

# TRAIN and PREDICT Yw ------------------------------
for(crp in str_subset(crops, "Rainfed")) {

  crp_code <- str_extract(crp, "(?<= ).{4}")

  if(crp %like% "rice") {
    preds_rfed <- c(preds_rfed, "crop_ints")
    preds_irri <- c(preds_irri, "crop_ints")
  }

  dd <- d[crop == crp, .SD, .SDcols = c("yw", preds_rfed)]
  mtry <- dt[crop == crp, mtry]
  mns <- dt[crop == crp, min.node.size]

  ## train ------------------
  mod <- ranger(
    yw ~ .,
    data = dd,
    num.trees = 1000,
    mtry = mtry,
    min.node.size = mns,
    seed = 0,
    keep.inbag = TRUE
  )

  ## predict ---------------
  pred_rast <- str_subset(names(crop_rast), crp_code) |>
    subset(x = crop_rast) |>
    c(clim_rast, soil_rast, lat_abs_rast)

  names(pred_rast) <- gsub(crp_code, "crop", names(pred_rast))
  pred_rast <- subset(pred_rast, names(dd)[-1])

  predict(
    pred_rast, mod, fun = pf, cores = 3, na.rm = TRUE,
    filename = paste0(outdir, crp_code, "_Yw.tif"),
    overwrite = TRUE,
    wopt = list(names = crp, filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )

  if(crp %like% "rice") {
    preds_rfed <- preds_rfed[preds_rfed != "crop_ints"]
    preds_irri <- preds_irri[preds_irri != "crop_ints"]
  }
}



# TRAIN and PREDICT Yp ------------------------------


for(crp in str_subset(crops, "Irrigated")) {

  crp_code <- str_extract(crp, "(?<= ).{4}")

  if(crp %like% "rice") {
    preds_rfed <- c(preds_rfed, "crop_ints")
    preds_irri <- c(preds_irri, "crop_ints")
  }

  dd <- d[crop == crp, .SD, .SDcols = c("yp", preds_irri)]
  mtry <- dt[crop == crp, mtry]
  mns <- dt[crop == crp, min.node.size]

  ## train ------------------
  mod <- ranger(
    yp ~ .,
    data = dd,
    num.trees = 1000,
    mtry = mtry,
    min.node.size = mns,
    seed = 0,
    keep.inbag = TRUE
  )

  ## predict ---------------
  pred_rast <- str_subset(names(crop_rast), crp_code) |>
    subset(x = crop_rast) |>
    c(clim_rast, lat_abs_rast)

  names(pred_rast) <- gsub(crp_code, "crop", names(pred_rast))
  pred_rast <- subset(pred_rast, names(dd)[-1])

  predict(
    pred_rast, mod, fun = pf, cores = 3, na.rm = TRUE,
    filename = paste0(outdir, crp_code, "_Yp.tif"),
    overwrite = TRUE,
    wopt = list(names = crp, filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )

  if(crp %like% "rice") {
    preds_rfed <- preds_rfed[preds_rfed != "crop_ints"]
    preds_irri <- preds_irri[preds_irri != "crop_ints"]
  }

}
