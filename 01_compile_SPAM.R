# author: Fernando Aramburu Merlos
# date: 2022-11-08

# Compile SPAM global 2010 (V2.0) and SPAM Africa 2017 (v2.1)


host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)

spam_dir <- "data/SPAM"
dir.create(spam_dir, FALSE, TRUE)

# Africa data -------

africa_rfed <- Sys.glob("data/SPAM/harvested_area_africa_2017_v2.1/*_H_*_R.tif") |>
  rast()
names(africa_rfed) <- gsub("spam2017V2r1_SSA_H_", "", names(africa_rfed))

africa_irri <- Sys.glob("data/SPAM/harvested_area_africa_2017_v2.1/*_H_*_I.tif") |>
  rast()
names(africa_irri) <- gsub("spam2017V2r1_SSA_H_", "", names(africa_irri))

africa_totl <- Sys.glob("data/SPAM/harvested_area_africa_2017_v2.1/*_H_*_A.tif") |>
  rast()
names(africa_totl) <- gsub("spam2017V2r1_SSA_H_", "", names(africa_totl))


# Global data -------------

global_rfed <- Sys.glob("data/SPAM/harvested_area_global_2010_v2.0/*_H_*_R.tif") |>
  rast()
names(global_rfed) <- gsub("spam2010V2r0_global_H_", "", names(global_rfed))

global_irri <- Sys.glob("data/SPAM/harvested_area_global_2010_v2.0/*_H_*_I.tif") |>
  rast()
names(global_irri) <- gsub("spam2010V2r0_global_H_", "", names(global_irri))

global_totl <- Sys.glob("data/SPAM/harvested_area_global_2010_v2.0/*_H_*_A.tif") |>
  rast()
names(global_totl) <- gsub("spam2010V2r0_global_H_", "", names(global_totl))

all.equal(names(global_rfed), names(africa_rfed))
all.equal(names(global_irri), names(africa_irri))
all.equal(names(global_totl), names(africa_totl))


# Compile data ---------------
dir.create("data/SPAM/harvested_area_global_merged_v2", F, T)

for(i in seq_len(nlyr(global_rfed))) {
  # rainfed
  crop_r <- names(global_rfed)[i]
  fn_rfed <- paste0("data/SPAM/harvested_area_global_merged_v2/spam_global_H_", crop_r, ".tif")
  merge(
    africa_rfed[[i]], global_rfed[[i]],
    filename = fn_rfed,
    overwrite = T,
    wopt = list(names = crop_r, filetype = "GTiff",
                gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
  )
  # irrigated
  crop_i <- names(global_irri)[i]
  fn_irri <- paste0("data/SPAM/harvested_area_global_merged_v2/spam_global_H_", crop_i, ".tif")
  merge(
    africa_irri[[i]], global_irri[[i]],
    filename = fn_irri,
    overwrite = T,
    wopt = list(names = crop_i, filetype = "GTiff",
                gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
  )
  # total
  crop_a <- names(global_totl)[i]
  fn_totl <- paste0("data/SPAM/harvested_area_global_merged_v2/spam_global_H_", crop_a, ".tif")
  merge(
    africa_totl[[i]], global_totl[[i]],
    filename = fn_totl,
    overwrite = T,
    wopt = list(names = crop_a, filetype = "GTiff",
                gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
  )
  rm(crop_r, crop_i, crop_a, fn_irri, fn_rfed, fn_totl)
}

