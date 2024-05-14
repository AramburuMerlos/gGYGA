# author: Fernando Aramburu Merlos


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(raster)
library(ranger)
library(caret)
library(CAST)
library(stringr)
library(scam)
library(terra)


# LOAD DATA ----------------------

crops <- c("Rainfed maize", "Irrigated maize",
           "Rainfed wheat", "Irrigated wheat",
           "Rainfed rice", "Irrigated rice")

## AOA ---------------
aoa_list <- readRDS("extrap_method/NNDM/aoa_all_models.RDS")$global

## SPAM --------
spam <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA|MAIZ|RICE") |>
  str_subset("_(R|I).tif$") |>
  rast()

names(spam) |>
  str_extract("(WHEA|MAIZ|RICE)_.$") |>
  str_replace("WHEA", "wheat") |>
  str_replace("MAIZ", "maize") |>
  str_replace("RICE", "rice") |>
  c() -> .; paste(ifelse(grepl("_R$", .), "Rainfed", "Irrigated"), .) |>
  str_remove("_.$") -> names(spam)

all(crops %in% names(spam))

## world -----
world <- geodata::world(path = "data/gadm") |>
  project("+proj=natearth +datum=WGS84")

### box ---------
box <- ext(c(-11500000, 16000000, -5400000, 7100000))
world <- crop(world, box)

cell_size <- cellSize(spam, mask = FALSE, unit = "ha")

# Spatial manipulation --------------
r_aoa <- sapply(aoa_list, function(x)rast(x$AOA))
r_aoa <- rast(r_aoa)
r_aoa <- subset(r_aoa, crops)
spam <- subset(spam, crops)

area_included <- mask(spam, r_aoa, maskvalues = c(0, NA))/cell_size
area_excluded <- mask(spam, r_aoa, maskvalues = c(1, NA))/cell_size
area_included <- project(area_included, "+proj=natearth +datum=WGS84") |> crop(box)
area_excluded <- project(area_excluded, "+proj=natearth +datum=WGS84") |> crop(box)

# MAP AOA #######################################
grys <- c("white", gray.colors(5, start = 0.2, end = 0.8, rev = T))
reds <- c("white", RColorBrewer::brewer.pal(7, "Reds")[-c(1,2)])

leg_txt <- c("< 0.5", "0.5 - 10", "10 - 20", "20 - 40", "40 - 80", "> 80")
brks = c(0,0.005,.1,.2,.4,.8, 2.5)

dir <- "extrap_method/maps"
dir.create(dir, FALSE, TRUE)

tiff(filename = file.path(dir, "AOA.tif"),
     width = 8.8+1.3, height = 6+.3,
     units = "in", type = "cairo", res = 600, compression = "zip")

par(mfrow = c(3,2), omi = c(0,.3,.3,1))

i=1
for(i in seq_along(crops)) {

  plot(area_included[[crops[i]]], breaks = brks, col = grys,
       pax = list(ticks = FALSE, labels = FALSE),
       maxcell = ncell(area_included), mar = c(.1,.1,.1,.1), legend = FALSE)

  plot(area_excluded[[crops[i]]], breaks = brks, col = reds, legend = FALSE,
       axes = FALSE, add = TRUE, maxcell = ncell(area_excluded))

  plot(world, add = TRUE, lwd = 0.4)

  if(i == 1) {
    mtext("maize", 2, adj = 5/6+.05, outer = TRUE, font = 2, cex = 1.5)
    mtext("wheat", 2, adj = 3/6, outer = TRUE, font = 2, cex = 1.5)
    mtext("rice", 2, adj = 1/6, outer = TRUE, font = 2, cex = 1.5)
    mtext("rainfed", 3, adj = .22, outer = T, font = 2, cex = 1.5)
    mtext("irrigated", 3, adj = .78, outer = T, font = 2, cex = 1.5)

  }

  if(i == 4) {

    text(x = box[2] + (box[2]-box[1])*.13, y = (box[4]-box[3])*.87+box[3],
         "Crop area (%)", cex = 1.3, font = 2)

    legend(legend = leg_txt,
           fill = grys, box.col = "white",
           cex = 1.2, title = "In", title.adj = .15,
           x = "right", inset = -0.2, xpd = NA)

    legend(legend = leg_txt, x.intersp = 0.6,
           fill = reds, box.col = "white",
           cex = 1.2, title = "Out", title.adj = .2,
           x = "right", inset = -.24, xpd = NA)

  }
}

dev.off()

