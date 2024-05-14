# author: Fernando Aramburu Merlos

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(raster)

tavg <- geodata::worldclim_global("tavg", 0.5, "data/climate")
aoa_list <- readRDS("extrap_method/NNDM/aoa_all_models.RDS")$global
names(aoa_list)

## AOA -----------------------------------
dir.create("extrap_method/AOA", F, T)

for(i in seq_along(aoa_list)) {

  fn <- paste0("extrap_method/AOA/", names(aoa_list)[i], ".tif")
  aoa <- rast(aoa_list[[i]]$AOA)
  r <- disagg(aoa, round(res(aoa)/res(tavg))[1])

  if(compareGeom(r, tavg, stopOnError = FALSE)) {
    writeRaster(
      r,
      filename = fn,
      overwrite = TRUE,
      wopt = list(names = "AOA", filetype = "GTiff",
                  gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
    )
  } else {
    resample(
      r, tavg, method = "near",
      filename = fn,
      overwrite = TRUE,
      wopt = list(names = "AOA", filetype = "GTiff",
                  gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
    )
  }
}

## DI -----------------------------------
dir.create("extrap_method/DI", F, T)

for(i in seq_along(aoa_list)) {

  fn <- paste0("extrap_method/DI/", names(aoa_list)[i], ".tif")
  di <- rast(aoa_list[[i]]$DI)
  r <- disagg(di, round(res(di)/res(tavg))[1])

  if(compareGeom(r, tavg, stopOnError = FALSE)) {
    writeRaster(
      r,
      filename = fn,
      overwrite = TRUE,
      wopt = list(names = "DI", filetype = "GTiff",
                  gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
    )
  } else {
    resample(
      r, tavg, method = "near",
      filename = fn,
      overwrite = TRUE,
      wopt = list(names = "DI", filetype = "GTiff",
                  gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
    )
  }
}
