# author: Fernando Aramburu Merlos
# date: 2022-10-05

# setup ---------

host <- system("hostname", TRUE)


library(data.table)
library(terra)
library(raster)
library(ranger)
library(caret)
library(CAST)
library(stringr)


if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

# DATA PREP ###########################################################

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
crops <- sort(crops)

# total wheat, maize, and rice
spam_tot <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
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
pred_list <- fread("extrap_method/predictors_5min.csv")

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


## models --------------------------------

mod_list_global <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list.RDS")
mod_list_interp <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list_interp.RDS")

all_mods_list <- list(
  global = mod_list_global,
  interp = mod_list_interp
)


# COMPUTE AOA -----------------------------------

aoa_meta_list <- vector("list", length(all_mods_list))
names(aoa_meta_list) <- names(all_mods_list)

for(j in seq_along(aoa_meta_list)) {
  aoa_meta_list[[j]] <- vector("list", length(crops))
  names(aoa_meta_list[[j]]) <- crops
}

i = 1
j = 1

for(i in 1:length(crops)) {
  crop_sp_i <- str_extract(crops[i], "(?<= ).+")
  r_crop_i <- subset(spam, crops[i])
  r_crop_i[r_crop_i < 1] <- NA

  s <- substr(crop_sp_i, 1, 4) |>
    grep(names(crop_rast), value = TRUE) |>
    subset(x = crop_rast) |>
    c(clim_rast, soil_rast, lat_abs_rast) |>
    mask(r_crop_i) |>
    stack()


  names(s) <- gsub(substr(crop_sp_i, 1, 4), "crop", names(s))

  for(j in 1:length(all_mods_list)) {
       aoa_meta_list[[j]][[crops[i]]] <- CAST::aoa(newdata = s, model = all_mods_list[[j]][[crops[i]]])
  }
}

saveRDS(aoa_meta_list, "extrap_method/NNDM/aoa_all_models.RDS")

rm(s)
gc()


# MASK BY COUNTRY #################################
# for country interpolation objective, mask by countries included in the Atlas
aoa_meta_list <- readRDS("extrap_method/NNDM/aoa_all_models.RDS")

crops <- names(aoa_meta_list$interp)

d <- fread("data/API/Y_country.csv")
d <- d[, .(crop, country_iso3)]
world <- geodata::world(resolution = 1, path = "data/gadm")


for(i in seq_along(crops)) {
 if(crops[i] %like% "Irrigated") {
   iso3 <- d[crop %like% str_remove(crops[i], "^[:alpha:]+ "), country_iso3] |> unique()
 } else {
   iso3 <- d[crop == crops[i], country_iso3]
 }
 ctries <- world[world$GID_0 %in% iso3,] |> as("Spatial")
 aoa_meta_list$interp[[i]]$AOA <- mask(aoa_meta_list$interp[[i]]$AOA, ctries, updatevalue = 0)
}

saveRDS(aoa_meta_list, "extrap_method/NNDM/aoa_all_models.RDS")

