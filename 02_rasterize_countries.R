# author: Fernando Aramburu Merlos
# date: 2022-09-13

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)

# get countries
dir.create("data/gadm", FALSE, TRUE)
world <- geodata::world(path = "data/gadm", resolution = 1)

# raster model
r <- "data/SPAM/harvested_area_global_merged_v2/*MAIZ_A.tif" |>
  Sys.glob() |>
  rast()

# rasterize them
rworld <- rasterize(world, r, "GID_0", touches = TRUE)

# add a buffer to avoid NA countries in SPAM data
buf <- buffer(world, 3e4)

rbuf <- rasterize(buf, r, "GID_0")


cover(
  rworld, rbuf, values = NA,
  filename = "data/gadm/countries_buff.tif",
  overwrite = T,
  wopt = list(names = "countries", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)

