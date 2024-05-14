# author: Fernando Aramburu Merlos


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(data.table)

tavg <- geodata::worldclim_global("tavg", 0.5, "data/climate")
fn <- list.files("data/crop_calendar/5min", "plant|harvest|nday|ints", full.names = T)
fn_out <- gsub("5min", "30sec", fn)

# disagg and resampl ------------------
for(i in seq_along(fn)){
  r <- rast(fn[i])
  # if(file.exists(fn_out[i])) next # comment or uncomment line to overwrite files
  r <- disagg(r, round(res(r)/res(tavg))[1])
  if(compareGeom(r, tavg, stopOnError = FALSE)) {
    writeRaster(
      r,
      filename = fn_out[i],
      overwrite = TRUE,
      wopt = list(names = names(r), filetype = "GTiff",
                  gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
    )
  } else {
    resample(
      r, tavg,
      filename = fn_out[i],
      overwrite = TRUE,
      wopt = list(names = names(r), filetype = "GTiff",
                  gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
    )
  }
}

