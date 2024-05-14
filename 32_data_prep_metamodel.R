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



# FUNCTION #########################################################

extract_buf <- function(lon, lat, pred, weight = NULL) {
  pts <- vect(cbind(lon, lat), crs = "+proj=longlat +datum=WGS84")
  bz <- buffer(pts, 1e5)

  if(is.null(weight)) {
    extract(pred, bz, fun = "mean", na.rm = TRUE)[,-1]
  } else {
    if(!isTRUE(compareGeom(pred, weight))) stop("rasters geom don't match")
    r <- c(pred, weight)
    dd <- extract(r, bz) |> setDT()
    names(dd) <- c("id", paste0("v", seq_len(nlyr(pred))), "w")
    dd[, w:= w/sum(w, na.rm = TRUE), by = id]
    dd[is.na(w), w:= 0]
    dd
    dd[,
       lapply(.SD, weighted.mean, w, na.rm = TRUE),
       .SDcols = paste0("v", seq_len(nlyr(pred))),
       by = id
    ][, -1]
  }
}


# DATA PREP ###########################################################

## SPAM --------
# rainfed and irrigated wheat, maize, and rice
spam <- "data/SPAM/harvested_area_global_merged_30sec/*.tif" |>
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

# total wheat, maize, and rice
spam_tot <- "data/SPAM/harvested_area_global_merged_30sec/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA|MAIZ|RICE") |>
  str_subset("_A.tif$") |>
  rast()

names(spam_tot) |>
  str_extract("(WHEA|MAIZ|RICE)") |>
  str_replace("WHEA", "wheat") |>
  str_replace("MAIZ", "maize") |>
  str_replace("RICE", "rice") -> names(spam_tot)


## yield data -----------
d <- fread("data/API/Y_ws_area.csv")

d <- d[crop %in% crops]

# add lon lat to d
ws_pos <- fread("data/API/ws.csv")

d <- ws_pos[, .(station_id, longitude, latitude)][d, on = "station_id"]
setnames(d, c("longitude", "latitude"), c("lon", "lat"))


## Predictors --------------
pred_list <- fread("extrap_method/predictors.csv")


### Climate -------
clim_rast <- rast(pred_list[type %like% "bioclim", path])
clim_nams <- pred_list[type %like% "bioclim", var]
names(clim_rast) <- clim_nams

# extract climate values for d locations
d[, (clim_nams):= extract(clim_rast, cbind(lon, lat))]

# when is NA (in the ocean), use buffer to extract average value
d[is.na(GDD)]
d[is.na(GDD), (clim_nams):= extract_buf(lon, lat, clim_rast)]
stopifnot(!d[,any(is.na(GDD))])


### Soil ----------------
# soil and crop variables are crop dependent
soil_rast <- rast(pred_list[type == "soil", path])
soil_nams <- pred_list[type %like% "soil", var]
names(soil_rast) <- soil_nams

for(i in 1:length(crops)) {
  if(grepl("Irrig", crops[i])) next
  d[crop == crops[i],
    (soil_nams):= extract_buf(lon, lat, soil_rast, spam_tot[[gsub("Rainfed ", "", crops[i])]])]
}

stopifnot(!d[, any(water == "Rainfed" & is.na(PAWHC_0_1m))])
stopifnot(!d[, any(water == "Rainfed" & is.na(PAWHC_1_2m))])


### Crop -----------------
crop_rast <- rast(pred_list[type %like% "crop", path])

for(i in crops) {
  sp <- str_extract(i, "(?<= ).{4}")
  rs <- subset(crop_rast, names(crop_rast)[names(crop_rast) %like% sp])
  for(j in names(rs)) {
    cn <- paste0("crop_", substr(j, 6, nchar(j)))
    d[crop == i, (cn):= extract_buf(lon, lat, rs[[j]])]
  }
}
stopifnot(!any(d[,crop_sp == "rice" & is.na(crop_ints)]))
stopifnot(!any(d[,is.na(crop_tavg_1)]))



### Latitude -------------
d[, lat_abs:= abs(lat)]



## Irrigated wheat -------------------
dd <- d[crop == "Rainfed wheat",]
dd[, crop:= "Irrigated wheat"]
dd[, yw:= NA]
d <- rbind(d, dd)




## write to disk --------
dir.create("extrap_method/training_data", F, T)
fwrite(d, "extrap_method/training_data/Y_ws_predictors.csv")




