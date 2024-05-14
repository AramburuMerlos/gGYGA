# author: Fernando Aramburu Merlos


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(terra)
#library(raster)
library(ranger)
library(caret)
library(CAST)
library(stringr)
library(scam)


# LOAD DATA ----------------------

## world -----
world <- geodata::world(path = "data/gadm") |>
  project("+proj=natearth +datum=WGS84")

### box ---------
box <- ext(c(-11500000, 16000000, -5400000, 7100000))
world <- crop(world, box)

## Crops --------
crops <- c("Rainfed maize", "Irrigated maize",
           "Rainfed wheat", "Irrigated wheat",
           "Rainfed rice", "Irrigated rice")

## SPAM --------
# rainfed and irrigated wheat, maize, and rice
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

spam <- subset(spam, crops)
cs <- cellSize(spam, unit = "ha")
spam <- spam/cs * 100
masks <- spam
masks[masks < 0.5] <- NA

## yield ---------
yield <- "extrap_method/global_estimates/5min/masked_by_aoa/*.tif" |>
  Sys.glob() |>
  rast() |>
  subset(crops) |>
  mask(masks)


## models --------------------------------
mod_list <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list.RDS")

## AOA ---------------
aoa_list <- readRDS("extrap_method/NNDM/aoa_all_models.RDS")$global

aoa_calib <- lapply(crops,
                    function(x) calibrate_aoa(
                      AOA = aoa_list[[x]],
                      model = mod_list[[x]],
                      showPlot = FALSE,
                    ))

rmse <- lapply(aoa_calib, function(x) x$AOA$expected_RMSE) |> rast()
names(rmse) <- crops
rmse <- mask(rmse, masks)

# RMSE vs DI plots #################################
library(latticeExtra)
names(aoa_calib) <- crops

png("extrap_method/plots/RMSE_DI_calib.png", 5, 7, units = "in", res = 300)

par(mfrow = c(3,2), mar = c(2,2,1,1), xpd = NA, las = 1,
    mgp = c(2,0.7,0), oma = c(2,3,2,2), pty = "s")

for(i in seq_along(crops)) {

  x <- trellis.panelArgs(aoa_calib[[crops[i]]]$plot, packet.number = 1)$x
  mxx <- ceiling(max(x)*10)/10
  y <- trellis.panelArgs(aoa_calib[[crops[i]]]$plot, packet.number = 1)$y
  mxy <- ceiling(max(y))
  mod <- scam(y ~ s(x, k=6, m=2, bs = "mpi"), stats::gaussian(link="identity"))
  axd <- ifelse(mxy >3, 1, 0.5)
  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxx), ylim = c(0,mxy))

  if(i == 1) {
    mtext(bquote(Spatially~cross-validated~RMSE~(t~ha^-1)), side = 2, outer = T, las=0, line = 0.5)
    mtext("Dissimilarity Index", side = 1, outer = T, line = .3)
    mtext("Rainfed", side = 3, line = .2)
  }
  if(i == 2) mtext("Irrigated", side = 3, line = .2)
  if(i%%2 == 0) {
    text(x = mxx*1.15, y = mxy/2, str_remove(crops[i], "Irrigated "), srt = 270, cex = 1.5)
  }

  axis(side = 1, at = seq(0, mxx, 0.1), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxy, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxx, 0.1), pos = mxy, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxy, axd), pos = mxx, cex.axis = .9, tcl = 0, labels = FALSE)

  points(x, y, pch = 21)
  clip(0,mxx,0,mxy)
  lines(x, predict(mod), lty = 3, col = "red", lwd = 2)
  ct <- cor.test(x,y)
  if(ct$p.value < 0.001) {
    text(0.01, mxy*.99, bquote(italic(r)~"="~.(round(cor(x,y), 2))), adj = c(0,1))
    text(0.01, mxy*.90, bquote(italic(P)~"<"~0.001), adj = c(0,1))
  }
 }


dev.off()



# MAP RMSE #######################################
dir <- "extrap_method/maps"
dir.create(dir, FALSE, TRUE)

rmsep <- 100 * rmse/yield
rmsep <- project(rmsep, crs(world))
rmsep <- crop(rmsep, ext(world))

i = 1

#brks_rmse <- list(
#  `Rainfed maize` =  seq(0,4,.5),
#  `Irrigated maize` =  seq(0,4,.5),
#  `Rainfed wheat` =  seq(0,2,.25),
#  `Irrigated wheat` =  seq(0,2,.25),
#  `Rainfed rice` =  seq(0,2,.25),
#  `Irrigated rice` =  seq(0,2,.25)
#)
brk <- c(seq(0,35,5), Inf)

col_rmse <- viridis::viridis(8)

tiff(filename = file.path(dir, "RMSEp.tif"),
     width = 8.8+1.3, height = 6+.3,
     units = "in", type = "cairo", res = 600, compression = "zip")

par(mfrow = c(3,2), omi = c(0,.3,.3,1))

i=1
for(i in seq_along(crops)) {

  plot(rmsep[[crops[i]]], breaks = brk, col = col_rmse,
       pax = list(labels = FALSE, ticks = FALSE),
       maxcell = ncell(rmse), mar = c(.1,.1,.1,.1), legend = FALSE)
  plot(world, add = TRUE, lwd = 0.4, border = "gray20")

  if(i == 1) {
    mtext("maize", 2, adj = 5/6+.05, outer = TRUE, font = 2, cex = 1.5)
    mtext("wheat", 2, adj = 3/6, outer = TRUE, font = 2, cex = 1.5)
    mtext("rice", 2, adj = 1/6, outer = TRUE, font = 2, cex = 1.5)
    mtext("rainfed", 3, adj = .22, outer = T, font = 2, cex = 1.5)
    mtext("irrigated", 3, adj = .78, outer = T, font = 2, cex = 1.5)

  }

  if(i == 4) {
    leg_txt <- c(paste0(brk[-(length(brk) - 1:0)], " - ", brk[-c(1, length(brk))]), "> 35")
    leg_ttle <- expression(bold(RMSE)~("%"))

    legend(
      legend = leg_txt,
      fill = col_rmse, box.col = "white",
      cex = 1.2, x = "right",
      title = leg_ttle,
      title.adj = .5,
      inset = -.22,
      xpd = NA
    )
  }
}

dev.off()



# MAP Yp|YW #######################################
yield <- yield
yield <- project(yield, crs(world))
yield <- crop(yield, ext(world))

brks_yield <- list(
  `Rainfed maize` =  seq(2,22,2),
  `Irrigated maize` =  seq(2,22,2),
  `Rainfed wheat` =  seq(0,15,1.5),
  `Irrigated wheat` =  seq(0,15,1.5),
  `Rainfed rice` =  seq(0,15,1.5),
  `Irrigated rice` =  seq(0,15,1.5)
)

col_yield <- viridis::turbo(10, direction = -1)

tiff(filename = file.path(dir, "yield.tif"),
     width = 8.8+1.3, height = 6+.3,
     units = "in", type = "cairo", res = 600, compression = "zip")

par(mfrow = c(3,2), omi = c(0,.3,.3,1))

i=1
for(i in seq_along(crops)) {

  brk <- brks_yield[[crops[i]]]

  plot(yield[[crops[i]]], breaks = brk, col = col_yield,
       pax = list(ticks = 0, labels = FALSE),
       maxcell = ncell(yield), mar = c(.1,.1,.1,.1), legend = FALSE, reset = FALSE)
  plot(world, add = TRUE, lwd = 0.4, border = "gray20")

  if(i == 1) {
    mtext("maize", 2, adj = 5/6+.05, outer = TRUE, font = 2, cex = 1.5)
    mtext("wheat", 2, adj = 3/6, outer = TRUE, font = 2, cex = 1.5)
    mtext("rice", 2, adj = 1/6, outer = TRUE, font = 2, cex = 1.5)
    mtext("rainfed", 3, adj = .22, outer = T, font = 2, cex = 1.5)
    mtext("irrigated", 3, adj = .78, outer = T, font = 2, cex = 1.5)

  }

  if(i %% 2 == 0) {
    leg_txt <- paste0(brk[-length(brk)], " - ", brk[-1])
#    leg_ttle <- expression(atop(atop(textstyle(bold(Yield)), textstyle(bold(potential))),(t~ha^-1)))
    leg_ttle <- expression(bold(Ypot)~(t~ha^-1))

    legend(
      legend = leg_txt,
      fill = col_yield, bty = "n",
      cex = 1,
      y.intersp = 0.5,
      title = if(i == 2) leg_ttle else NA,
      title.adj = 1, title.cex = 1.1,
      x = box[2]*1.01, xjust = 0,
      y = box[3] + (box[4] - box[3])/2, yjust = 0.5,
      xpd = NA
    )
  }
}

dev.off()


# MAP Yp|YW points #######################################
d <- fread("extrap_method/training_data/Y_ws_predictors.csv")
d[, lapply(.SD, range, na.rm = TRUE), .SDcols = c("yp", "yw"), by = .(crop_sp)]
d[crop_sp == "maize" & yw < 2, yw:= 2.01]

brks_yield <- list(
  `Rainfed maize` =  seq(2,22,2),
  `Irrigated maize` =  seq(2,22,2),
  `Rainfed wheat` =  seq(0,15,1.5),
  `Irrigated wheat` =  seq(0,15,1.5),
  `Rainfed rice` =  c(seq(0,13.5,1.5), 16),
  `Irrigated rice` =  c(seq(0,13.5,1.5), 16)
)

col_yield <- viridis::turbo(10, direction = -1)

tiff(filename = file.path(dir, "GYGA_YwYp_pts.tif"),
     width = 8.8+1.3, height = 6+.3,
     units = "in", type = "cairo", res = 600, compression = "zip")

par(mfrow = c(3,2), omi = c(0,.3,.3,1))

i=1
for(i in seq_along(crops)) {

  pts <- vect(d[crop == crops[i]], crs = "+proj=longlat +datum=WGS84")
  pts <- project(pts, crs(world))

  brk <- brks_yield[[crops[i]]]

  plot(world, lwd = 0.4, border = "gray20",
       pax = list(ticks = FALSE, labels = FALSE),
       mar = c(.1,.1,.1,.1), legend = FALSE, main = "")
  plot(pts, if(grepl("Rainfed", crops[i])) "yw" else "yp", add = TRUE,
       breaks = brk, col = col_yield, cex = 0.6, main = "", legend = FALSE)

  if(i == 1) {
    mtext("maize", 2, adj = 5/6+.05, outer = TRUE, font = 2, cex = 1.5)
    mtext("wheat", 2, adj = 3/6, outer = TRUE, font = 2, cex = 1.5)
    mtext("rice", 2, adj = 1/6, outer = TRUE, font = 2, cex = 1.5)
    mtext("rainfed", 3, adj = .22, outer = T, font = 2, cex = 1.5)
    mtext("irrigated", 3, adj = .78, outer = T, font = 2, cex = 1.5)

  }

  if(i %% 2 == 0) {
    leg_txt <- paste0(brk[-length(brk)], " - ", brk[-1])
    leg_ttle <- expression(bold(Ypot)~(t~ha^-1))

    legend(
      legend = leg_txt,
      fill = col_yield, bty = "n",
      cex = 1.2,
      title = if(i == 2) leg_ttle else NA,
      title.adj = 1, title.cex = 1.1,
      x = box[2]*1.01, xjust = 0,
      y = box[3] + (box[4] - box[3])/2, yjust = 0.5,
      xpd = NA
    )
  }
}

dev.off()


