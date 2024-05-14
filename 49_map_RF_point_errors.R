# author: Fernando Aramburu Merlos
# date: 2023-02-16

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(terra)
library(stringr)

dir.create("extrap_method/plots/metamodel", F, T)

## data -------
# global models
mlg <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list.RDS")
d <- fread("data/API/Y_ws.csv")
ws <- fread("data/API/ws.csv")
d <- ws[d, on = .NATURAL]
setnames(d, c("latitude", "longitude"), c("lat", "lon"))

## crops --------
crops <- c("maize", "wheat", "rice")
wcrops <- c(paste("Rainfed", crops), paste("Irrigated", crops))

## world -------
w <- geodata::world(path = "data/gadm")

## colors -----
col <- RColorBrewer::brewer.pal(11, "RdBu") |> rev()

# Map errors ------------------------------------
i=1
dir <- "extrap_method/maps/pred_errors/"
dir.create(dir, FALSE, TRUE)


for(i in seq_along(wcrops)) {

  if(wcrops[i] %like% "Rainfed") {
    y <- "yw"
    dd <- d[crop == wcrops[i]]
  } else if(wcrops[i] %in% c("Irrigated maize", "Irrigated rice")){
    y <- "yp"
    dd <- d[crop == wcrops[i]]
  } else {
    y <- "yp"
    dd <- d[crop_sp == str_remove(wcrops[i], "Irrigated ")]
  }

  dt <- as.data.table(mlg[[wcrops[i]]]$pred)
  ddt <- dt[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy <- dt[ddt[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]
  # this shouldn't be necessary. To be checked. Only happens with Irrigated wheat.
  if(!isTRUE(all.equal(dd[, get(y)], xy$obs))) {
    setorderv(dd, y)
    setorder(xy, obs)
  }
  if(!isTRUE(all.equal(dd[, get(y)], xy$obs))) stop("obs doesn't match")
  dd[, y_hat:= xy$pred]
  dd[, y_err:= y_hat - get(y)]
  maxerr <- ceiling(max(abs(dd$y_err)))
  brk <- seq(-maxerr, maxerr, length.out = 12)
  pts <- vect(dd, crs = "+proj=longlat +datum=WGS84")
  pts$icol <- cut(pts$y_err, brk)

  fn <- paste0(dir, wcrops[i], ".tif")
  tiff(filename = fn, width = 7, height = 4, type = "cairo", compression = "zip", units = "in", res = 300)
  plot(pts, col = "white", pax = list(labels = NA))
  plot(w, add = T, border = "gray60")
  plot(pts, bg = col[pts$icol], pch = 21, cex = .7, add = T)
  leg <- paste0("(",round(brk[-12],1),",", round(brk[-1],1), "]")
  legend("top", leg[1:6], pch = 21, pt.bg = col[1:6], cex = .7, horiz = TRUE, inset = -.1, xpd = NA)
  legend("bottom", leg[7:11], pch = 21, pt.bg = col[7:11], cex = .7, horiz = TRUE, inset = -.1, xpd = NA)
  dev.off()
}

