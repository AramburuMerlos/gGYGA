# author: Fernando Aramburu Merlos
# date: 2022-11-11


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(raster)
library(terra)
library(data.table)
library(stringr)



# UPLOAD DATA -------------------------------

## crops --------
# custom order
crops <- c("Rainfed maize")

## SPAM --------
# rainfed and irrigated wheat, maize, soybean and rice
spam <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  str_subset("MAIZ") |>
  str_subset("_R.tif$") |>
  rast()

## WS -------
ws <- fread("data/API/ws.csv")
d <- fread("data/API/Y_ws.csv")
ws <- ws[d[crop == "Rainfed maize"], on = .(station_id)]
setnames(ws, c("longitude", "latitude"), c("lon", "lat"))
pts <- vect(ws, crs = "+proj=longlat +datum=WGS84")

## world boundaries -----
w <- geodata::world(resolution = 1, path = "data/gadm")
wext <- ext(c(-130,130,-90,90))
w <- crop(w, wext)

## global yw prediction ------
yw <- rast("extrap_method/global_estimates/5min/masked_by_aoa/Rainfed maize.tif")

# crop and project ---------
pcrs <- "+proj=moll +ellps=WGS84 +datum=WGS84 +no_defs"

w <- project(w, pcrs)
wbord <- graticule(wext[1:2], wext[3:4], crs = pcrs)

spam <- crop(spam, wext) |> project(pcrs)
yw <- crop(yw, wext) |> project(pcrs)
pts <- crop(pts, wext) |> project(pcrs)

# plot ----------
stk <- geom(pts)
stk[, "y"] <- stk[, "y"] + 100000
stk[, "x"] <- stk[, "x"] - 30000
stk <- vect(stk)

bal <- geom(stk)
bal[, "y"] <- bal[, "y"] + 100000
bal[, "x"] <- bal[, "x"] - 28000
bal <- vect(bal)

spam[spam < 50] <- NA

col <- ggthemes::tableau_color_pal(palette = "Classic Area Green", type = "ordered-sequential")
col <- colorRampPalette(col(11))(256)
#col <- colorRampPalette(c("yellowgreen", "gray50"))(64)

dir.create("extrap_method/maps/Fig1", F, T)

ws[, ywi:= cut(yw, breaks = seq(2,20,2))]


col <- RColorBrewer::brewer.pal(11, "RdYlGn")[-1]
tiff(filename = "extrap_method/maps/Fig1/Yw_pts.tif", width = ncol(spam)/200,  height = nrow(spam)/200,
     units = "in", type = "cairo", res = 400, compression = "zip")
plot(w, axes = FALSE,  mar = c(1,5,1,0))
plot(pts, "yw",  breaks = seq(2,20,2), add = T, legend = FALSE,
     pch = 21, col = "black", bg = col[ws$ywi], cex = 4)
lines(wbord, xpd = NA)
dev.off()

col <- viridis::viridis(10, end = 0.9)
tiff(filename = "extrap_method/maps/Fig1/Yw_grid.tif", width = ncol(yw)/200,  height = nrow(yw)/200,
     units = "in", type = "cairo", res = 400, compression = "zip")
plot(yw, breaks = c(seq(2,14,2),20), legend = FALSE, col = col,  axes = FALSE,  mar = c(1,2,1,2))
plot(w, add = T)
lines(wbord, xpd = NA)
dev.off()

