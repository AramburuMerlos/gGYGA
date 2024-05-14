# author: Fernando Aramburu Merlos

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(stringr)


ypot <- Sys.glob("extrap_method/global_estimates/30sec/unmasked/*.tif") |>
  rast()
crops <- names(ypot)
aoa <- paste0("extrap_method/AOA/", crops, ".tif") |>
  rast()

dir <- "extrap_method/global_estimates/30sec/masked_by_aoa/"
dir.create(dir, F, T)

for(i in seq_along(crops)) {
  mask(ypot[[i]], aoa[[i]], maskvalues = c(0, NA),
       filename = paste0(dir, crops[i], ".tif"),
       overwrite = TRUE,
       wopt = list(names = crops[i], filetype = "GTiff",
                   gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
}
