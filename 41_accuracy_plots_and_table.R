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




# Global XY plots -------------

rrmse <- function(x, obs = "obs", pred = "pred", na.rm = TRUE, digits = 0) {
    o <- x[[obs]]
    p <- x[[pred]]
  rrmse <- sqrt(mean((o - p)^2, na.rm = na.rm))/mean(o, na.rm = na.rm)*100
  rrmse <- round(rrmse, digits = digits)
  return(bquote(RMSE~"="~.(rrmse)*"%"))
}

png("extrap_method/plots/obs_pred_global.png",
    height = length(crops)*1.5+1, width = 3.75, units = "in", res = 300)

par(mfrow = c(length(crops),2), mar = c(2,2,1,1), xpd = NA, las = 1,
    mgp = c(2,0.7,0), oma = c(2,2,2,2), pty = "s")


i = 1

for(i in seq_along(crops)) {

  dt_rfed <- as.data.table(mlg[[rfed[i]]]$pred)
  ddt_rfed <- dt_rfed[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_rfed <- dt_rfed[ddt_rfed[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]

  dt_irri <- as.data.table(mlg[[irri[i]]]$pred)
  ddt_irri <- dt_irri[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_irri <- dt_irri[ddt_irri[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]

  xy_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  axd <- ifelse(max(xy_irri) < 10, 2, ifelse(max(xy_irri) < 15, 4, 5))
  mxi <- ceiling(max(xy_irri)/axd)*axd

  xy_gyga_rfed <- d[!is.na(yw_hat_global) & crop == rfed[i], .(obs = yw, pred = yw_hat_global)]
  xy_gyga_irri <- d[!is.na(yp_hat_global) & crop == irri[i], .(obs = yp, pred = yp_hat_global)]

  xy_gyga_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_gyga_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) {
    mtext(bquote(Cross-validation~gridded~Ypot~(t~ha^-1)), side = 2, outer = T, las=0)
    mtext(bquote(GYGA~Ypot~(t~ha^-1)), side = 1, outer = T, line = .5)
    mtext("Climate Zones", side = 3, line = .2)
  }

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_gyga_irri, pch = 19, col = blue, cex = .8)
  points(xy_gyga_rfed, pch = 19, col = green, cex = .8)

  text(.3, mxi - .3, adj = c(0,1), label = rrmse(rbind(xy_gyga_irri, xy_gyga_rfed)))

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)

  #  xy_gyga_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
  #  xy_gyga_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]


  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) mtext("Metamodel", side = 3, line = .2)
  text(x = mxi*1.15, y = mxi/2, str_remove(rfed[i], "Rainfed "), srt = 270, cex = 1.5)

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_irri, pch = 19, col = blue, cex = .8)
  points(xy_rfed, pch = 19, col = green, cex = .8)

  text(.3, mxi - .3, adj = c(0,1), label = rrmse(rbind(xy_irri, xy_rfed)))

  if(i == length(crops)) {
    legend("bottomright", c("Rainfed", "Irrigated"), xpd = NA, cex = 0.8,
           pch = 19, col = c(green, blue), bty = "n",# horiz = TRUE,
           inset = c(0.02, 0.02))
           #inset = c(-0.4, -0.45))
  }

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)

  #  xy_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
  #  xy_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]

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


dev.off()


# National XY plots -------------

png("extrap_method/plots/obs_pred_national.png",
    height = length(crops)*1.5+1, width = 3.75, units = "in", res = 300)

par(mfrow = c(length(crops),2), mar = c(2,2,1,1), xpd = NA, las = 1,
    mgp = c(2,0.7,0), oma = c(2,2,2,2), pty = "s")


i = 1

for(i in seq_along(crops)) {

  dt_rfed <- as.data.table(mli[[rfed[i]]]$pred)
  ddt_rfed <- dt_rfed[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_rfed <- dt_rfed[ddt_rfed[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]

  dt_irri <- as.data.table(mli[[irri[i]]]$pred)
  ddt_irri <- dt_irri[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_irri <- dt_irri[ddt_irri[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]

  xy_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  axd <- ifelse(max(xy_irri) < 10, 2, ifelse(max(xy_irri) < 15, 4, 5))
  mxi <- ceiling(max(xy_irri)/axd)*axd

  xy_gyga_rfed <- d[!is.na(yw_hat_interp) & crop == rfed[i], .(obs = yw, pred = yw_hat_interp)]
  xy_gyga_irri <- d[!is.na(yp_hat_interp) & crop == irri[i], .(obs = yp, pred = yp_hat_interp)]

  xy_gyga_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_gyga_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) {
    mtext(bquote(Cross-validation~gridded~Ypot~(t~ha^-1)), side = 2, outer = T, las=0)
    mtext(bquote(GYGA~Ypot~(t~ha^-1)), side = 1, outer = T, line = .5)
    mtext("GYGA upscaling", side = 3, line = .2)
  }

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_gyga_irri, pch = 19, col = blue, cex = .8)
  points(xy_gyga_rfed, pch = 19, col = green, cex = .8)

  text(.3, mxi - .3, adj = c(0,1), label = rrmse(rbind(xy_gyga_irri, xy_gyga_rfed)))

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)

  #  xy_gyga_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
  #  xy_gyga_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]


  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) mtext("Metamodel", side = 3, line = .2)
  text(x = mxi*1.15, y = mxi/2, str_remove(rfed[i], "Rainfed "), srt = 270, cex = 1.5)

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_irri, pch = 19, col = blue, cex = .8)
  points(xy_rfed, pch = 19, col = green, cex = .8)

  text(.3, mxi - .3, adj = c(0,1), label = rrmse(rbind(xy_irri, xy_rfed)))

  if(i == length(crops)) {
    legend("bottomright", c("Rainfed", "Irrigated"), xpd = NA, cex = 0.8,
           pch = 19, col = c(green, blue), bty = "n",# horiz = TRUE,
           inset = c(0.02, 0.02))
    #inset = c(-0.4, -0.45))
  }

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)

  #  xy_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
  #  xy_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]

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


dev.off()




# All XY plots -------------

png("extrap_method/plots/obs_pred_all.png",
    height = length(crops)*1.5+1, width = 6.75, units = "in", res = 300)

par(mfrow = c(length(crops),4), mar = c(2,2,1,1), xpd = NA, las = 1,
    mgp = c(2,0.7,0), oma = c(2,2,4,2), pty = "s")



i = 1

for(i in seq_along(crops)) {

  dt_rfed <- as.data.table(mli[[rfed[i]]]$pred)
  ddt_rfed <- dt_rfed[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_rfed <- dt_rfed[ddt_rfed[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]

  dt_irri <- as.data.table(mli[[irri[i]]]$pred)
  ddt_irri <- dt_irri[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_irri <- dt_irri[ddt_irri[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]

  xy_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  axd <- ifelse(max(xy_irri) < 10, 2, ifelse(max(xy_irri) < 15, 4, 5))
  mxi <- ceiling(max(xy_irri)/axd)*axd

  xy_gyga_rfed <- d[!is.na(yw_hat_interp) & crop == rfed[i], .(obs = yw, pred = yw_hat_interp)]
  xy_gyga_irri <- d[!is.na(yp_hat_interp) & crop == irri[i], .(obs = yp, pred = yp_hat_interp)]

  xy_gyga_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_gyga_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) {
    mtext(bquote(GYGA~yield~potential~(t~ha^-1)), side = 1, outer = T, line = .5)
    mtext("GYGA-CZ", side = 3, line = .2)
  }

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_gyga_irri, pch = 19, col = blue, cex = .8)
  points(xy_gyga_rfed, pch = 19, col = green, cex = .8)

  rmse <- rbind(xy_gyga_irri, xy_gyga_rfed) |> RMSE(obs = obs, pred = pred) |> unlist() |> round(1)
  text(.3, mxi - .3, adj = c(0,1), label = bquote(RMSE~"="~.(rmse)))

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)
#  xy_gyga_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
#  xy_gyga_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]

  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) mtext("Metamodel", side = 3, line = .2)
  #text(x = mxi*1.15, y = mxi/2, str_remove(rfed[i], "Rainfed "), srt = 270, cex = 1.5)

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_irri, pch = 19, col = blue, cex = .8)
  points(xy_rfed, pch = 19, col = green, cex = .8)

  rmse <- rbind(xy_irri, xy_rfed) |> RMSE(obs = obs, pred = pred) |> unlist() |> round(1)
  text(.3, mxi - .3, adj = c(0,1), label = bquote(RMSE~"="~.(rmse)))

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)
#  xy_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
#  xy_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]

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

  dt_rfed <- as.data.table(mlg[[rfed[i]]]$pred)
  ddt_rfed <- dt_rfed[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_rfed <- dt_rfed[ddt_rfed[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]

  dt_irri <- as.data.table(mlg[[irri[i]]]$pred)
  ddt_irri <- dt_irri[, .(RMSE = caret::RMSE(pred, obs)), by = .(mtry, min.node.size)]
  xy_irri <- dt_irri[ddt_irri[which.min(RMSE), .(mtry, min.node.size)], on = .NATURAL][, .(obs, pred)]

  xy_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  axd <- ifelse(max(xy_irri) < 10, 2, ifelse(max(xy_irri) < 15, 4, 5))
  mxi <- ceiling(max(xy_irri)/axd)*axd

  xy_gyga_rfed <- d[!is.na(yw_hat_global) & crop == rfed[i], .(obs = yw, pred = yw_hat_global)]
  xy_gyga_irri <- d[!is.na(yp_hat_global) & crop == irri[i], .(obs = yp, pred = yp_hat_global)]

  xy_gyga_rfed[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]
  xy_gyga_irri[
    , sma_hat:= unlist(B0_sma(obs=obs, pred=pred)) + unlist(B1_sma(obs=obs, pred=pred)) * obs
  ]

  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) {
    mtext(bquote(Cross-validation~predicted~yield~potential~(t~ha^-1)), side = 2, outer = T, las=0)
    mtext("GYGA-CZ", side = 3, line = .2)
    mtext("INTERPOLATION", adj = 0.18, outer = TRUE, line = 1, font = 2)
    mtext("EXTRAPOLATION", adj = 0.82, outer = TRUE, line = 1, font = 2)
  }

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_gyga_irri, pch = 19, col = blue, cex = .8)
  points(xy_gyga_rfed, pch = 19, col = green, cex = .8)

  rmse <- rbind(xy_gyga_irri, xy_gyga_rfed) |> RMSE(obs = obs, pred = pred) |> unlist() |> round(1)
  text(.3, mxi - .3, adj = c(0,1), label = bquote(RMSE~"="~.(rmse)))

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)

#  xy_gyga_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
#  xy_gyga_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]


  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

  if(i == 1) mtext("Metamodel", side = 3, line = .2)
  text(x = mxi*1.15, y = mxi/2, str_remove(rfed[i], "Rainfed "), srt = 270, cex = 1.5)

  axis(side = 1, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 2, at = seq(0, mxi, axd), pos = 0, cex.axis = .9, tcl = -0.4)
  axis(side = 3, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)
  axis(side = 4, at = seq(0, mxi, axd), pos = mxi, cex.axis = .9, tcl = 0, labels = FALSE)

  points(xy_irri, pch = 19, col = blue, cex = .8)
  points(xy_rfed, pch = 19, col = green, cex = .8)

  rmse <- rbind(xy_irri, xy_rfed) |> RMSE(obs = obs, pred = pred) |> unlist() |> round(1)
  text(.3, mxi - .3, adj = c(0,1), label = bquote(RMSE~"="~.(rmse)))

  if(i == length(crops)) {
    legend("bottom", c("Rainfed", "Irrigated"), xpd = NA, pt.cex = 1.2,
           pch = 19, col = c(green, blue), bty = "o", horiz = TRUE,
           inset = c(-0.4, -0.45))
  }

  clip(0,mxi,0,mxi)
  abline(a =0 , b = 1, col = "grey40", lty = 2)

#  xy_rfed[, lines(obs, sma_hat, col = "dark green", lwd = 2)]
#  xy_irri[, lines(obs, sma_hat, col = "dark blue", lwd = 2)]

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



par(new = T, mfrow = c(1, 1), xpd = TRUE, mar = c(2,2,2,2), oma = c(2,0,0,0))
plot(1, type = "n", axes = F, ann = F, xlim = c(0,1), ylim = c(0,1))
abline(v = 0.5, lty = 3)


dev.off()



# Table -------------------
dir.create("extrap_method/tables", F, T)
mtrcs <- c("RMSE", "RRMSE", "CCC", "R2", "MBE", "PLA", "PLP", "B0", "B1")

## by water regime and crop ---------------
dt <- rbind(global = rbindlist(ldt_global), interp = rbindlist(ldt_interp), idcol = "target")

dm <- dt[
  , metrics_summary(obs = obs, pred = pred, metrics_list = mtrcs, type = "regression")
  , by = .(target, crop, water, method)
]

dm[, Score:= round(Score, 2)]

dc <- dcast(dm, crop + water +  target + method ~ Metric, value.var = "Score")
setcolorder(dc, c("crop", "water", "target", "method", "RMSE","RRMSE", "CCC", "R2", "MBE", "PLA", "PLP"))
setorderv(dc, cols = colnames(dc)[1:4], order = c(1,-1,-1,1))

dc[, water:= tolower(water)]
dc[water == "irrigat", water:= "irrigated"]
dc[target == "interp", target:= "interpolation"]
dc[target == "global", target:= "extrapolation"]
dc[method == "gyga", method:= "GYGA-CZ"]
dc[method == "meta", method:= "metamodel"]
dc[, RRMSE:= RRMSE * 100]

fwrite(dc, "extrap_method/tables/accuracy.csv")

dc[target == "interpolation" & method == "GYGA-CZ"]



## by crop sp ---------------
dma <- dt[
  , metrics_summary(obs = obs, pred = pred, metrics_list = mtrcs, type = "regression")
  , by = .(target, crop, method)
]

dma[, Score:= round(Score, 2)]

dca <- dcast(dma, crop + target + method ~ Metric, value.var = "Score")
setcolorder(dca, c("crop", "target", "method", "RMSE","RRMSE", "CCC", "R2", "MBE", "PLA", "PLP"))
setorderv(dca, cols = colnames(dca)[1:3], order = c(1,-1,1))

dca[target == "interp", target:= "interpolation"]
dca[target == "global", target:= "extrapolation"]
dca[method == "gyga", method:= "GYGA-CZ"]
dca[method == "meta", method:= "Metamodel"]
dca[, RRMSE:= RRMSE * 100]

fwrite(dca, "extrap_method/tables/accuracy_water_aggregated.csv")

dca[target == "interpolation"]
dc[method == "metamodel", .(crop, water, PLA)]
