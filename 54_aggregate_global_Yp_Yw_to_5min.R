# author: Fernando Aramburu Merlos

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(stringr)

ypot <- Sys.glob("extrap_method/global_estimates/30sec/masked_by_AOA/*.tif") |>
  rast()
crops <- names(ypot)

dir <- "extrap_method/global_estimates/5min/masked_by_aoa/"
dir.create(dir, F, T)

spam <- rast("data/SPAM/harvested_area_global_merged_v2/spam_global_H_MAIZ_A.tif")
fact <- (res(spam)/res(ypot))[1]

for(i in seq_along(crops)) {

  r <- aggregate(ypot[[i]], fact = fact, fun = 'mean', na.rm = TRUE)

   if(compareGeom(r, spam, stopOnError = FALSE)) {
     writeRaster(
       r,
       filename = paste0(dir, crops[i], ".tif"),
       overwrite = TRUE,
       wopt = list(names = crops[i], filetype = "GTiff",
                   gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
     )
   } else {
     resample(
       r, spam,
       filename = paste0(dir, crops[i], ".tif"),
       overwrite = TRUE,
       wopt = list(names = crops[i], filetype = "GTiff",
                   gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
     )
   }
}
