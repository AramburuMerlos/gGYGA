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
library(caret)
library(CAST)
library(stringr)


# DATA  ###########################################################

d <- fread("extrap_method/training_data/Y_ws_predictors.csv")


## Predictor names --------
pl <- fread("extrap_method/predictors_5min.csv")
preds_rfed <- pl[type %like% "bioclim|soil|lat_abs", var] |>
  c(pl[type == "crop", str_subset(var, "maiz")]) |>
  str_replace("maiz", "crop")
preds_irri <- preds_rfed[!preds_rfed %in% pl[type %in% c("bioclim_prec", "soil"), var]]


## SPAM --------
# rainfed and irrigated wheat, maize, and rice
spam <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA|MAIZ|RICE") |>
  str_subset("_(R|I).tif$") |>
  rast()

names(spam) |>
  str_extract("(WHEA|MAIZ|RICE)_.$") |>
  str_replace("WHEA", "wheat") |>
  str_replace("MAIZ", "maize") |>
  str_replace("RICE", "rice") |>
  c() -> .; paste(ifelse(grepl("_R$", .), "Rainfed", "Irrigated"), .) |>
  str_remove("_.$") -> names(spam)

crops <- names(spam)



# Autocorr ranges ----------

# there is no clear spatial autocorrelation in the residuals for no crop
library(raster)
library(gstat)

# mod_LOO <- train(
#   x = d[crop == "Rainfed wheat", ..preds_rfed],
#   y = d[crop == "Rainfed wheat", yw],
#   method = "ranger",
#   trControl = trainControl(method = "LOOCV", savePredictions = T),
#   seed = 0
# )
# d[crop == "Rainfed wheat",
#   res:= yw - predict(mod_LOO, newdata = .SD),
#   .SDcols = preds_rfed]
#
# res_pts <- vect(d[crop == "Rainfed wheat"], crs = "+proj=longlat +datum=WGS84") |>
#   as("Spatial")
# variog <- variogram(res~1, data = res_pts, width = 100, cutoff = 3000)
# plot(variog)

# let's use the autocorrelation in the predictors, just in case

pts_gdd <- vect(unique(d[,.(lon, lat, GDD)]), crs = "+proj=longlat +datum=WGS84") |>
  as("Spatial")
v_gdd <- variogram(GDD~1, data = pts_gdd, width = 100, cutoff = 6000)
plot(v_gdd)
set.seed(0)
fv_gdd <- fit.variogram(v_gdd, vgm(model = "Gau", nugget = 1e4, range = 3500, psill = 8e6))
fv_gdd
# 3500 for GDD
plot(v_gdd, fv_gdd)

pts_ts <- vect(unique(d[,.(lon, lat, bio_4)]), crs = "+proj=longlat +datum=WGS84") |>
  as("Spatial")
v_ts <- variogram(bio_4~1, data = pts_ts, width = 100, cutoff = 5500)
plot(v_ts)
set.seed(0)
fv_ts <- fit.variogram(v_ts, vgm(model = "Gau"))
fv_ts
# 3000 for ts
plot(v_ts, fv_ts)

pts_ai <- vect(unique(d[,.(lon, lat, AIann)]), crs = "+proj=longlat +datum=WGS84") |>
  as("Spatial")
v_ai <- variogram(AIann~1, data = pts_ai, width = 100, cutoff = 3500)
plot(v_ai)
set.seed(0)
fv_ai <- fit.variogram(v_ai, vgm(model = "Sph", nugget = TRUE))
fv_ai
# 3400 for ai
plot(v_ai, fv_ai)


pts_pawhc <- unique(d[water == "Rainfed",.(lon, lat, PAWHC_0_1m)]) |>
  vect(crs = "+proj=longlat +datum=WGS84") |>
  as("Spatial")
v_pawhc <- variogram(PAWHC_0_1m~1, data = pts_pawhc, width = 100, cutoff = 3000)
set.seed(0)
fv_pawhc <- fit.variogram(v_pawhc, vgm(model = "Sph"))
fv_pawhc
# 1230 for pawhc
plot(v_pawhc, fv_pawhc)


range_rfed <- mean(c(1230, 3500, 3000, 3400)) * 1e3
range_irri <- mean(c(3500, 3000)) * 1e3



# GLOBAL EXTRAPOLATION ---------

nndm_global <- vector(mode = "list", length = length(crops))

i = 1
for(i in seq_along(crops)) {
    tpt <- d[crop == crops[i]] |>
      vect(crs = "+proj=longlat +datum=WGS84") |>
      sf::st_as_sf()
  tmp_r <- subset(spam, crops[i])
  tmp_r[tmp_r < 100] <- NA
  tmp_c <- cells(tmp_r)
  ppt <- xyFromCell(spam, tmp_c) |>
    vect(crs = "+proj=longlat +datum=WGS84") |>
    sf::st_as_sf()
  nndm_global[[i]] <- nndm(
    tpt,
    ppoints = ppt,
    phi = ifelse(grepl("Rainfed", crops[i]), range_rfed, range_irri)
  )
  rm(tmp_r, tmp_c, tpt, ppt)
}

gc()

names(nndm_global) <- crops

saveRDS(nndm_global, "extrap_method/NNDM/nndm_global_indices.RDS")



# COUNTRY INTERPOLATION #####################################

## define area of interest --------------
# objective: interpolation
# mask spam data to keep countries included in the atlas
r_countries <- rast("data/gadm/countries_buff.tif")
rcl_df <- levels(r_countries)[[1]]

spam_gyga <- vector(mode = "list", length = length(crops))

for(i in seq_along(crops)) {
  iso3_inc <- d[crop == crops[i], unique(ISO3)]
  # keep countries included in the Atlas
  rcl_df$mask <- ifelse(rcl_df$countries %in% iso3_inc, TRUE, NA)
  rcl_m <- cbind(rcl_df$value, rcl_df$mask)
  rcl_df$mask <- NULL
  ctry_msk <- classify(r_countries, rcl_m)
  spam_gyga[[i]] <- mask(spam[[crops[i]]], ctry_msk)
}
spam_gyga <- do.call(c, spam_gyga)



## indices -------

nndm_interp <- vector(mode = "list", length = length(crops))

i = 1
for(i in seq_along(crops)) {
    tpt <- d[crop == crops[i]] |>
      vect(crs = "+proj=longlat +datum=WGS84") |>
      sf::st_as_sf()
  tmp_r <- subset(spam_gyga, crops[i])
  tmp_r[tmp_r < 100] <- NA
  tmp_c <- cells(tmp_r)
  ppt <- xyFromCell(spam, tmp_c) |>
    vect(crs = "+proj=longlat +datum=WGS84") |>
    sf::st_as_sf()
  nndm_interp[[i]] <- nndm(
    tpt,
    ppoints = ppt,
    phi = ifelse(grepl("Rainfed", crops[i]), range_rfed, range_irri)
  )
  rm(tmp_r, tmp_c, tpt, ppt)
}

gc()

names(nndm_interp) <- crops

dir.create("extrap_method/NNDM", F, T)
saveRDS(nndm_interp, "extrap_method/NNDM/nndm_interp_indices.RDS")
