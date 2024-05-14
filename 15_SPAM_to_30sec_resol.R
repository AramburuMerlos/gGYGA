# author: Fernando Aramburu Merlos

# Get cropland data and dissagregate SPAM

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(stringr)

dir.create("data/cropland", F, T)
cl <- geodata::cropland(source = "WorldCover", "data/cropland")
tavg <- geodata::worldclim_global("tavg", 0.5, "data/climate")
cl <- extend(cl, tavg)

compareGeom(cl, tavg)

spam <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA|MAIZ|SOYB|RICE|OILP") |>
  rast()

csz <- cellSize(spam[[1]], unit = "ha")
spam <- spam/csz

dir.create("data/SPAM/harvested_area_global_merged_30sec", F, T)
disagg(spam, 10,
       filename = "data/SPAM/harvested_area_global_merged_30sec.tif",
       overwrite = TRUE,
       wopt = list(names = names(spam), filetype = "GTiff",
                   gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

spam <- rast("data/SPAM/harvested_area_global_merged_30sec.tif")
compareGeom(spam, cl)
ext(spam)
ext(cl)
# using method nearest because the resampling is very very small
spam <- resample(spam, cl, method = "near", threads = TRUE)


for(i in seq_len(nlyr(spam))) {
  app(c(spam[[i]], cl), min,
       filename = paste0("data/SPAM/harvested_area_global_merged_30sec/", names(spam)[i], ".tif"),
       overwrite = TRUE,
       wopt = list(names = names(spam)[i], filetype = "GTiff",
                   gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
}



spam_fn <- Sys.glob("data/SPAM/harvested_area_global_merged_30sec/*.tif")
spam <- rast(spam_fn)
dir.create("data/SPAM/harvested_area_global_merged_30sec_masked/", F, T)

for(i in seq_len(nlyr(spam))) {
  classify(spam[[i]], cbind(0, 0.001, NA),
      filename = paste0("data/SPAM/harvested_area_global_merged_30sec_masked/", names(spam)[i], ".tif"),
      overwrite = TRUE,
      wopt = list(names = names(spam)[i], filetype = "GTiff",
                  gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
}

