# author: Fernando Aramburu Merlos
# date: 2022-07-20

# create buffer zones for the whole world

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(data.table)


# weather station lists (includes some ws without data, also all with data)
ws <- fread("data/API/ws.csv")

# country, climate zones, and check if all WS have their matching cty and CZ
cz <- vect("data/CZ/GYGAClimateZones.shp")
cty_dir <- "data/gadm"
cty <- geodata::world(resolution = 1, path = file.path(getwd(), cty_dir))
cty <- project(cty, crs(cz))

all(ws$country_iso3 %in% cty$GID_0)
all(ws$climatezone %in% cz$GYGA_CZ)


# buffers that don't fall in their alleged climate zone
pws <- vect(ws, geom = c("longitude", "latitude"))
crs(pws) <- crs(cz)
q <- intersect(pws, cz)
q <- as.data.table(as.data.frame(q))
qi <- q[climatezone != GYGA_CZ]
setnames(qi, c("climatezone", "GYGA_CZ"), c("reported_CZ", "real_CZ"))
torm <- c("country_id", "country_iso3", "station_id", "ID")
qi[, (torm):=NULL]
setorder(qi, "country")
fwrite(qi, "data/API/ws_outside_their_cz.csv")

# create bfs in loop for each country, cz, combination
# note that not all crops include all buffers, but this is taking care of later
ccz <- ws[, .(country_iso3, climatezone)] |> unique()

for(i in 1:nrow(ccz)) {
  tws <- ws[ccz[i,], on = .NATURAL] |>
    vect(geom = c("longitude", "latitude"))
  tws <- tws[, "station_id"]
  tcz <- cz[cz$GYGA_CZ == ccz$climatezone[i], "GYGA_CZ"]
  tcy <- cty[cty$GID_0 == ccz$country_iso3[i], "GID_0"]
  crs(tws) <- crs(cty)
  tbf <- buffer(tws, width = 1e5)
  if(nrow(tbf) > 1) {
    vor <- voronoi(tws[, "station_id"], bnd = tcy)
    names(vor) <- "voronoi"
    un <- union(tbf, vor)
    tbf <- un[un$station_id == un$voronoi,]
    tbf$voronoi <- NULL
  }
  tbf <- intersect(tbf, tcy)
  tbf <- intersect(tbf, tcz)
  if(any(duplicated(tbf$station_id))) {
    tbf <- aggregate(tbf, by = "station_id")
    tbf$agg_n <- NULL
    names(tbf)[2] <- "GYGA_CZ"
  }
  tbf$ID <- NULL
  if(i == 1) {
    bf <- tbf
  }  else {
    bf <- rbind(bf, tbf)
  }
}

# buffers that do not exist because CZ > 100km from WS
nbf <- ws[!station_id %in% bf$station_id]
fwrite(nbf, "data/API/ws_more_than_100km_from_cz.csv")


dir.create("data/world_buffers", F, T)
writeVector(bf, "data/world_buffers/buffer_zones.shp", overwrite = TRUE)

summary(expanse(bf))

