# author: Fernando Aramburu Merlos
# date: 2022-10-07

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(stringr)
library(metrica)

dir.create("extrap_method/plots/", F, T)

## data -------
# gyga data with NNDM LOOCV
d <- fread("extrap_method/NNDM/GYGA_ws_NNDM_LOOCV.csv")
# global models
mlg <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list.RDS")
mli <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list_interp.RDS")

## crops --------
crops <- c("maize", "wheat", "rice")
rfed <- paste("Rainfed", crops)
irri <- paste("Irrigated", crops)

ldt_global <- ldt_interp <-  vector(mode = "list", length = length(crops))

## colors ------
blue <- "#3F00FF60"
green <- "#228B2260"


# XY plpots -------------

## Global --------

png("extrap_method/plots/obs_pred_global_GYGA_CV_subset.png",
    height = length(crops)*2, width = 4.5, units = "in", res = 300)

par(mfrow = c(length(crops),2), mar = c(2,2,1,1), xpd = NA, las = 1,
    mgp = c(2,0.7,0), oma = c(2,2,2,2))
i = 1

for(i in seq_along(crops)) {

  xy_gyga_rfed <- d[!is.na(yw_hat_global) & crop == rfed[i], .(obs = yw, pred = yw_hat_global)]
  xy_gyga_irri <- d[!is.na(yp_hat_global) & crop == irri[i], .(obs = yp, pred = yp_hat_global)]

  xy_gyga_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_gyga_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  dt_rfed <- as.data.table(mlg[[rfed[i]]]$pred)
  ddt_rfed <- dt_rfed[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_rfed <- dt_rfed[ddt_rfed[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]
  if(!isTRUE(d[crop == rfed[i], all.equal(yw, xy_rfed$obs)])) stop("mod$obs and d[,yw] don't match")
  xy_rfed <- xy_rfed[d[crop == rfed[i], which(!is.na(yw_hat_global))],]

  dt_irri <- as.data.table(mlg[[irri[i]]]$pred)
  ddt_irri <- dt_irri[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_irri <- dt_irri[ddt_irri[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]
  if(!isTRUE(d[crop == irri[i], all.equal(yp, xy_irri$obs)])) stop("mod$obs and d[,yp] don't match")
  xy_irri <- xy_irri[d[crop == irri[i], which(!is.na(yp_hat_global))],]


  xy_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  axd <- ifelse(max(xy_irri) < 10, 2, ifelse(max(xy_irri) < 15, 4, 5))
  mxi <- ceiling(max(xy_irri)/axd)*axd

  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) {
    mtext(bquote(GYGA~yield~(Mg~ha^-1)), side = 1, outer = T, line = .5)
    mtext(bquote(Extrapolated~yield~(Mg~ha^-1)), side = 2, outer = T, las=0)
    mtext("CZ extrapolation", side = 3, line = .5)
  }

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_gyga_irri, pch = 19, col = blue, cex = .8)
  points(xy_gyga_rfed, pch = 19, col = green, cex = .8)

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)

  xy_gyga_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
  xy_gyga_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]


  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) mtext("Metamodel", side = 3, line = .5)
  text(x = mxi*1.15, y = mxi/2, str_remove(rfed[i], "Rainfed "), srt = 270, cex = 1.5)

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_irri, pch = 19, col = blue, cex = .8)
  points(xy_rfed, pch = 19, col = green, cex = .8)

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)

  xy_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
  xy_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]

  ldt_global[[i]] <- rbindlist(list(
    data.table(crop = crops[i], water = "Rainfed", method = "gyga",
               obs = xy_gyga_rfed$obs, pred = xy_gyga_rfed$pred),
    data.table(crop = crops[i], water = "Rainfed", method = "meta",
               obs = xy_rfed$obs, pred = xy_rfed$pred),
    data.table(crop = crops[i], water = "Irrigat", method = "gyga",
               obs = xy_gyga_irri$obs, pred = xy_gyga_irri$pred),
    data.table(crop = crops[i], water = "Irrigat", method = "meta",
               obs = xy_irri$obs, pred = xy_irri$pred)
  ))

}

legend("bottomright", c("Rainfed", "Irrigated"),
       pch = 19, col = c(green, blue), bty = "n",
       inset = c(0.04, 0.03))


dev.off()



## Interp --------

png("extrap_method/plots/obs_pred_interp_GYGA_CV_subset.png",
    height = length(crops)*2, width = 4.5, units = "in", res = 300)

par(mfrow = c(length(crops),2), mar = c(2,2,1,1), xpd = NA, las = 1,
    mgp = c(2,0.7,0), oma = c(2,2,2,2))
i = 1

for(i in seq_along(crops)) {

  xy_gyga_rfed <- d[!is.na(yw_hat_interp) & crop == rfed[i], .(obs = yw, pred = yw_hat_interp)]
  xy_gyga_irri <- d[!is.na(yp_hat_interp) & crop == irri[i], .(obs = yp, pred = yp_hat_interp)]

  xy_gyga_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_gyga_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  dt_rfed <- as.data.table(mli[[rfed[i]]]$pred)
  ddt_rfed <- dt_rfed[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_rfed <- dt_rfed[ddt_rfed[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]
  if(!isTRUE(d[crop == rfed[i], all.equal(yw, xy_rfed$obs)])) stop("mod$obs and d[,yw] don't match")
  xy_rfed <- xy_rfed[d[crop == rfed[i], which(!is.na(yw_hat_interp))],]

  dt_irri <- as.data.table(mli[[irri[i]]]$pred)
  ddt_irri <- dt_irri[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_irri <- dt_irri[ddt_irri[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]
  if(!isTRUE(d[crop == irri[i], all.equal(yp, xy_irri$obs)])) stop("mod$obs and d[,yp] don't match")
  xy_irri <- xy_irri[d[crop == irri[i], which(!is.na(yp_hat_interp))],]


  xy_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  axd <- ifelse(max(xy_irri) < 10, 2, ifelse(max(xy_irri) < 15, 4, 5))
  mxi <- ceiling(max(xy_irri)/axd)*axd

  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) {
    mtext(bquote(GYGA~yield~(Mg~ha^-1)), side = 1, outer = T, line = .5)
    mtext(bquote(Interpolated~yield~(Mg~ha^-1)), side = 2, outer = T, las=0)
    mtext("CZ interpolation", side = 3, line = .5)
  }

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_gyga_irri, pch = 19, col = blue, cex = .8)
  points(xy_gyga_rfed, pch = 19, col = green, cex = .8)

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)
  xy_gyga_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
  xy_gyga_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]


  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) mtext("Metamodel", side = 3, line = .5)
  text(x = mxi*1.15, y = mxi/2, str_remove(rfed[i], "Rainfed "), srt = 270, cex = 1.5)

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_irri, pch = 19, col = blue, cex = .8)
  points(xy_rfed, pch = 19, col = green, cex = .8)

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)
  xy_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
  xy_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]

  ldt_interp[[i]] <- rbindlist(list(
    data.table(crop = crops[i], water = "Rainfed", method = "gyga",
               obs = xy_gyga_rfed$obs, pred = xy_gyga_rfed$pred),
    data.table(crop = crops[i], water = "Rainfed", method = "meta",
               obs = xy_rfed$obs, pred = xy_rfed$pred),
    data.table(crop = crops[i], water = "Irrigat", method = "gyga",
               obs = xy_gyga_irri$obs, pred = xy_gyga_irri$pred),
    data.table(crop = crops[i], water = "Irrigat", method = "meta",
               obs = xy_irri$obs, pred = xy_irri$pred)
  ))


}

legend("bottomright", c("Rainfed", "Irrigated"),
       pch = 19, col = c(green, blue), bty = "n",
       inset = c(0.04, 0.03))


dev.off()



# Table -------------------
dir.create("extrap_method/tables", F, T)
mtrcs <- c("RMSE", "CCC")

## global ---------------
dt <- rbindlist(ldt_global)
dm <- dt[
  , metrics_summary(obs = obs, pred = pred, metrics_list = mtrcs, type = "regression")
  , by = .(crop, water, method)
]
dm[, Score:= round(Score, 2)]
dm[, cid:= ifelse(crop == "maize", 1, ifelse(crop == "wheat", 2, 3))]
dm[, rid:= ifelse(water == "Rainfed" , 1, 2)]

dcast(dm, cid + rid + crop + water ~ Metric + method, value.var = "Score") |>
  fwrite("extrap_method/tables/global_accuracy_GYGA_CV_subset.csv")

## interp ---------------
dt <- rbindlist(ldt_interp)
dm <- dt[
  , metrics_summary(obs = obs, pred = pred, metrics_list = mtrcs, type = "regression")
  , by = .(crop, water, method)
]
dm[, Score:= round(Score, 2)]
dm[, cid:= ifelse(crop == "maize", 1, ifelse(crop == "wheat", 2, 3))]
dm[, rid:= ifelse(water == "Rainfed" , 1, 2)]

dcast(dm, cid + rid + crop + water ~ Metric + method, value.var = "Score") |>
  fwrite("extrap_method/tables/interp_accuracy_GYGA_CV_subset.csv")
