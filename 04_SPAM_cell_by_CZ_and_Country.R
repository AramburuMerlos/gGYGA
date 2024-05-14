# author: Fernando Aramburu Merlos
# date: 2022-07-19

# SPAM cell by CZ and country combination
# done in for loop only for countries with GYGA data
# because of problems with global CZ map

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(terra)


# climate zones
cz <- vect("data/CZ/GYGAClimateZones.shp")

# countries
# install.packages("geodata")
dir <- "data/gadm"
dir.create(dir, F, T)
cty <- geodata::world(resolution = 1, path = dir)|>
  project(crs(cz))

# list of climate zones country combinations
d <- fread("data/API/Y_ws.csv")
d <- unique(d[, .(country_iso3, climatezone)])

# check if all CZ and all countries are in the shapefiles
all(d$country_iso3 %in% cty$GID_0)
all(d$climatezone %in% cz$GYGA_CZ)

# spam
mz <- rast("data/SPAM/harvested_area_global_merged_v2/spam_global_H_MAIZ_A.tif")

ld <- vector("list", nrow(d))

for(i in 1:nrow(d)) {
  icz <- d$climatezone[i]
  tcz <- cz[cz$GYGA_CZ == icz]
  icy <- d$country_iso3[i]
  tcy <- cty[cty$GID_0 == icy]
  tv <- intersect(tcz, tcy)
  tm <- cells(mz, tv, exact = TRUE)
  ld[[i]] <- data.table(GYGA_CZ = icz, ISO3 = icy,
                        cell = tm[, "cell"], weights = tm[, "weights"])
}

dd <- rbindlist(ld)

fwrite(dd, "data/SPAM/cell_coverage/SPAM_cells_per_CZ_and_country.csv")

