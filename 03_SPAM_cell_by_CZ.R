# author: Fernando Aramburu Merlos
# date: 2022-07-19

# get SPAM cells inside each CZ (and their weight)
# needed to quickly analyze crop area coverage by each CZ

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

# install.packages("terra")
library(terra)
library(data.table)

# cells --------

# to which CZ each SPAM cell belong?

# spam
mz <- rast("data/SPAM/harvested_area_global_2010_v2.0/spam2010V2r0_global_H_Maiz_A.tif")

# climate zones
cz <- vect("data/CZ/GYGAClimateZones.shp")

d <- cells(mz, cz, exact = TRUE, touches = TRUE)

d <- as.data.table(d)
dcz <- as.data.table(as.data.frame(cz))
dd <- merge(d, dcz, by = "ID", all = TRUE)

# remove cells without crop
dd[, area_mz:= extract(mz, cell)]
ddc <- dd[!is.na(area_mz)]

# compare sums
ddc[, sum(area_mz * weights)]
global(mz, sum, na.rm = TRUE)
# some area might be outside any CZ

ddc[, area_mz:= NULL]

dir <- "data/SPAM/cell_coverage"
dir.create(dir, FALSE, TRUE)
fwrite(ddc, file.path(dir, "SPAM_cells_per_CZ.csv"))
