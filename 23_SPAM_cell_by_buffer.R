# author: Fernando Aramburu Merlos
# date: 2022-07-21

# get SPAM cells inside each buffer (and their weight)
# needed to quickly analyze crop area coverage by each buffer

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(data.table)

# cells --------

# to which buffer each SPAM cell belong?

# spam
mz <- rast("data/SPAM/harvested_area_global_merged_v2/spam_global_H_Maiz_A.tif")

# buffers
bf <- vect("data/world_buffers/buffer_zones.shp")

d <- cells(mz, bf, exact = TRUE, touches = TRUE)

d <- as.data.table(d)

dbf <- as.data.table(as.data.frame(bf))
dbf[, ID:= 1:.N]
dd <- merge(d, dbf, by = "ID", all = TRUE)

dd[, ID:=NULL]

dir <- "data/SPAM/cell_coverage"
fwrite(dd, file.path(dir, "SPAM_cells_per_bf.csv"))
