# author: Fernando Aramburu Merlos
# date: 2022-08-01
# updated: 2022-07-29. Add new predictors based on WUR experience


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(stringr)
library(terra)


# SOILGRIDS 30 sec -------------------------------
# to be used as model for other rasters
tavg <- geodata::worldclim_global("tavg", 0.5, "data/climate")

## Aggregate -----------------------
fn <- Sys.glob("data/soil/SOILGRIDS/orig/wv*mean.tif")
r <- rast(fn)

res(tavg)

fn_ag <- str_replace(fn, "mean", "1km")
fn_ag <- str_replace(fn_ag, "orig", "1km")
ln_ag <- str_replace(names(r), "mean", "1km")

for(i in 1:nlyr(r)) {
  aggregate(
    r[[i]],
    fact = 4,
    fun = "mean",
    na.rm = TRUE,
    cores = 4,
    filename = fn_ag[i],
    overwrite = TRUE,
    wopt = list(names = ln_ag[i], filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
}

gc()



## project ----------------------

fn <- Sys.glob("data/soil/SOILGRIDS/1km/wv*1km.tif")
r <- rast(fn)

names(r) <- str_remove(names(r), "_1km")

project(
  r,
  tavg,
  filename = "data/soil/SOILGRIDS/30sec/wv_all_30sec.tif",
  overwrite = TRUE,
  wopt = list(names = names(r), filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)


gc()


## compute PAWHC --------

r <- rast("data/soil/SOILGRIDS/30sec/wv_all_30sec.tif")
compareGeom(r, tavg)

names(r) <- names(r) |>
  str_replace("0033", "fc") |>
  str_replace("1500", "wp") |>
  str_replace("-", "_") |>
  str_remove("cm") |>
  str_remove("wv")

layers <- str_remove(names(r), "(fc|wp)_") |> unique()

for(i in seq_along(layers)) {
  rr <- subset(r, paste0(c("fc", "wp"), "_", layers[i]))
  top <- as.numeric(str_extract(layers[i], "^\\d+(?=_)"))
  bot <- as.numeric(str_extract(layers[i], "(?<=_)\\d+$"))
  dep <- (bot - top)/100
  lapp(rr, function(x, y) pmax(x - y, 0) * dep,
       filename = paste0("data/soil/SOILGRIDS/30sec/PAWHC_", layers[i], "_cm.tif"),
       overwrite = TRUE,
       wopt = list(names = paste0("pawhc_", layers[i], "_cm.tif"), filetype = "GTiff",
                   gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6")))
}

### 0 - 1 m -------------
r_pawhc <- rast(paste0("data/soil/SOILGRIDS/30sec/PAWHC_", layers, "_cm.tif"))


subset(r_pawhc, "pawhc_100_200_cm.tif", negate = TRUE) |>
  app(sum) |>
  classify(
    cbind(0, NA),
    filename = "data/soil/SOILGRIDS/30sec/PAWHC_0-1m.tif",
    overwrite = TRUE,
    wopt = list(names = "PAWHC_0_1m", filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )

### 1-2m ----------------

# replace 0 by NA
subset(r_pawhc, "pawhc_100_200_cm.tif") |>
  classify(
    cbind(0, NA),
    filename = "data/soil/SOILGRIDS/30sec/PAWHC_1-2m.tif",
    overwrite = TRUE,
    wopt = list(names = "PAWHC_1_2m", filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )

## Visualize -----------
r_pawhc_1m <- rast("data/soil/SOILGRIDS/30sec/PAWHC_0-1m.tif")
r_pawhc_2m <- rast("data/soil/SOILGRIDS/30sec/PAWHC_1-2m.tif")

plot(r_pawhc_1m)
plot(r_pawhc_2m)



# SOILGRIDS 5 min -------------------------------
# to be used as model for other rasters
tavg <- geodata::worldclim_global("tavg", 5, "data/climate")

## Aggregate -----------------------
fn <- Sys.glob("data/soil/SOILGRIDS/orig/wv*mean.tif")
r <- rast(fn)

res(tavg)

fn_ag <- str_replace(fn, "mean", "7km")
fn_ag <- str_replace(fn_ag, "orig", "7km")
ln_ag <- str_replace(names(r), "mean", "7km")

for(i in 1:nlyr(r)) {
  aggregate(
    r[[i]],
    fact = 7000/250,
    fun = "mean",
    na.rm = TRUE,
    cores = 4,
    filename = fn_ag[i],
    overwrite = TRUE,
    wopt = list(names = ln_ag[i], filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
}

gc()



## project ----------------------

fn <- Sys.glob("data/soil/SOILGRIDS/7km/wv*7km.tif")
r <- rast(fn)

names(r) <- str_remove(names(r), "_7km")

project(
  r,
  tavg,
  filename = "data/soil/SOILGRIDS/5min/wv_all_5min.tif",
  overwrite = TRUE,
  wopt = list(names = names(r), filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)


gc()


## compute PAWHC --------

r <- rast("data/soil/SOILGRIDS/5min/wv_all_5min.tif")
compareGeom(r, tavg)

names(r) <- names(r) |>
  str_replace("0033", "fc") |>
  str_replace("1500", "wp") |>
  str_replace("-", "_") |>
  str_remove("cm") |>
  str_remove("wv")

layers <- str_remove(names(r), "(fc|wp)_") |> unique()

for(i in seq_along(layers)) {
  rr <- subset(r, paste0(c("fc", "wp"), "_", layers[i]))
  top <- as.numeric(str_extract(layers[i], "^\\d+(?=_)"))
  bot <- as.numeric(str_extract(layers[i], "(?<=_)\\d+$"))
  dep <- (bot - top)/100
  lapp(rr, function(x, y) pmax(x - y, 0) * dep,
       filename = paste0("data/soil/SOILGRIDS/5min/PAWHC_", layers[i], "_cm.tif"),
       overwrite = TRUE,
       wopt = list(names = paste0("pawhc_", layers[i], "_cm.tif"), filetype = "GTiff",
                   gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6")))
}

### 0 - 1 m -------------
r_pawhc <- rast(paste0("data/soil/SOILGRIDS/5min/PAWHC_", layers, "_cm.tif"))


subset(r_pawhc, "pawhc_100_200_cm.tif", negate = TRUE) |>
  app(sum) |>
  classify(
    cbind(0, NA),
    filename = "data/soil/SOILGRIDS/5min/PAWHC_0-1m.tif",
    overwrite = TRUE,
    wopt = list(names = "PAWHC_0_1m", filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )

### 1-2m ----------------

# replace 0 by NA
subset(r_pawhc, "pawhc_100_200_cm.tif") |>
  classify(
    cbind(0, NA),
    filename = "data/soil/SOILGRIDS/5min/PAWHC_1-2m.tif",
    overwrite = TRUE,
    wopt = list(names = "PAWHC_1_2m", filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )

## Visualize -----------
r_pawhc_1m <- rast("data/soil/SOILGRIDS/5min/PAWHC_0-1m.tif")
r_pawhc_2m <- rast("data/soil/SOILGRIDS/5min/PAWHC_1-2m.tif")

plot(r_pawhc_1m)
plot(r_pawhc_2m)


