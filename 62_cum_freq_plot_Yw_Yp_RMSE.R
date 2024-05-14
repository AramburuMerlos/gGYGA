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
library(raster)
library(caret)
library(CAST)
library(stringr)
library(scam)


# LOAD DATA ----------------------

crops <- c("Rainfed maize", "Irrigated maize",
           "Rainfed wheat", "Irrigated wheat",
           "Rainfed rice", "Irrigated rice")


## yield ---------
yield <- "extrap_method/global_estimates/5min/masked_by_aoa/*.tif" |>
  Sys.glob() |>
  rast()
yield <- subset(yield, crops)


## models --------------------------------
mod_list <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list.RDS")

## AOA ---------------
aoa_list <- readRDS("extrap_method/NNDM/aoa_all_models.RDS")$global

## SPAM --------
# rainfed and irrigated wheat, maize, soybean and rice
spam <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA|MAIZ|RICE") |>
  str_subset("_(R|I).tif$") |>
  rast()

names(spam) |>
  str_extract("(WHEA|MAIZ|SOYB|RICE)_.$") |>
  str_replace("WHEA", "wheat") |>
  str_replace("MAIZ", "maize") |>
  str_replace("RICE", "rice") |>
  c() -> .; paste(ifelse(grepl("_R$", .), "Rainfed", "Irrigated"), .) |>
  str_remove("_.$") -> names(spam)

crops <- names(spam)

# Cum freqs #######################################

## Yp|Yw ------------
ld <- vector("list", length(crops))
i = 1

for(i in seq_along(crops)) {
  yi <- subset(yield, crops[i])
  si <- subset(spam, crops[i])
  dt <- data.table(cell = which(values(aoa_list[[crops[i]]]$AOA) == 1))
  dt[, `:=`(
    crop = crops[i],
    area = extract(si, cell)[,1],
    yield = extract(yi, cell)[,1]
  )]
  ld[[i]] <- dt
  rm(dt)
}
d <- rbindlist(ld, use.names = TRUE)
d <- d[complete.cases(d)]
rm(ld)

d[, yield:= round(yield, 2)]
setorder(d, crop, yield, na.last = TRUE)
d <- d[, .(area = sum(area)), by = .(crop, yield)]
d[, cum_area:= cumsum(area), by = .(crop)]
d[, cum_prop:= cum_area/sum(area), by = .(crop)]

d[, crop_sp:= str_extract(crop, "(?<= ).*")]
da <- copy(d)
setorder(da, crop_sp, yield, na.last = TRUE)
da[, cum_area:= cumsum(area), by = .(crop_sp)]
da[, cum_prop:= cum_area/sum(area), by = .(crop_sp)]


## RMSE -----------
ld <- vector("list", length(crops))
i = 1
for(i in seq_along(crops)) {
  aoa_calib_i <- calibrate_aoa(
    AOA = aoa_list[[crops[i]]],
    model = mod_list[[crops[i]]],
    showPlot = FALSE,
  )
  rmse_i <- aoa_calib_i$AOA$expected_RMSE
  yi <- subset(yield, crops[i])
  si <- subset(spam, crops[i])

  dt <- data.table(cell = which(values(aoa_list[[crops[i]]]$AOA) == 1))
  dt[, `:=`(
    crop = crops[i],
    area = extract(si, cell)[,1],
    yield = extract(yi, cell)[,1],
    rmse = extract(rmse_i, cell)[,1]
  )]
  ld[[i]] <- dt
  rm(dt)
}

dd <- rbindlist(ld, use.names = TRUE)
dd <- dd[complete.cases(dd)]
rm(ld)
ddp <- copy(dd)

dd[, rmse:= round(rmse,2)]
setorder(dd, crop, rmse, na.last = TRUE)
dd <- dd[, .(area = sum(area)), by = .(crop, rmse)]
dd[, cum_area:= cumsum(area), by = .(crop)]
dd[, cum_prop:= cum_area/sum(area), by = .(crop)]


ddp[, rmsep:= 100 * rmse/yield]
ddp[, rmsep:= round(rmsep,2)]
setorder(ddp, crop, rmsep, na.last = TRUE)
ddp <- ddp[, .(area = sum(area)), by = .(crop, rmsep)]
ddp[, cum_area:= cumsum(area), by = .(crop)]
ddp[, cum_prop:= cum_area/sum(area), by = .(crop)]

ddp[, crop_sp:= str_extract(crop, "(?<= ).*")]
ddpa <- copy(ddp)
setorder(ddpa, crop_sp, rmsep, na.last = TRUE)
ddpa[, cum_area:= cumsum(area), by = .(crop_sp)]
ddpa[, cum_prop:= cum_area/sum(area), by = .(crop_sp)]


# PLOT ###################################
d[, range(yield)]

lty = rep(2:1,3)
#cols = RColorBrewer::brewer.pal(3, "Dark2")
cols = rep(ggsci::pal_npg()(4)[c(1,4,3)], each = 2)
ylab = 'Cumulative frequency (%)'

fig.file = "extrap_method/plots/cumfreqs.png"
png(file = fig.file, width = 7.5, height = 2.5, unit = "in", res = 300)

par(mfrow = c(1,3), mgp = c(0,0.8,0), las = 1,
    oma = c(2.5,2.5,0,0), mar = c(.5,.5,.5,.5), pty = 's')

xr = c(0,20)
yr = c(0,100)
plot(1, axes = FALSE, type = "n", xlim = xr, ylim = yr, ylab = "", xlab = "")
clip(xr[1], xr[2], yr[1], yr[2])
xlab = expression(bold(Yield~potential~(t~ha^-1)))

for(i in seq_along(crops)) {
  d[crop == crops[i], lines(yield, cum_prop*100, col = cols[i], lwd = 3, lty = lty[i])]
}
mtext(text = xlab, side = 1, line = 2, font = 2)
mtext(text = ylab, side = 2, line = 2, las = 0, font = 2)

axis(side = 1, at = pretty(xr), pos = yr[1], lwd = 1)
axis(side = 2, at = pretty(yr), pos = xr[1], lwd =1, las = 1)
axis(side = 3, at = pretty(xr), pos = yr[2], lwd = 1, lwd.ticks = 0, labels = F)
axis(side = 4, at = pretty(yr), pos = xr[2], lwd = 1, lwd.ticks = 0, labels = F)

dd[, range(rmse)]
xr = c(0,4)

plot(1, axes = FALSE, type = "n", xlim = xr, ylim = yr, ylab = "", xlab = "")
clip(xr[1], xr[2], yr[1], yr[2])
xlab = expression(bold(RMSE~(t~ha^-1)))

for(i in seq_along(crops)) {
  dd[crop == crops[i], lines(rmse, cum_prop*100, col = cols[i], lwd = 3, lty = lty[i])]
}
mtext(text = xlab, side = 1, line = 2, font = 2)

axis(side = 1, at = pretty(xr), pos = yr[1], lwd = 1)
axis(side = 2, at = pretty(yr), pos = xr[1], lwd =1, las = 1)
axis(side = 3, at = pretty(xr), pos = yr[2], lwd = 1, lwd.ticks = 0, labels = F)
axis(side = 4, at = pretty(yr), pos = xr[2], lwd = 1, lwd.ticks = 0, labels = F)

range(ddp[, rmsep])
xr = c(0,50)

plot(1, axes = FALSE, type = "n", xlim = xr, ylim = yr, ylab = "", xlab = "")
clip(xr[1], xr[2], yr[1], yr[2])
xlab = "RMSE (%)"

for(i in seq_along(crops)) {
  ddp[crop == crops[i], lines(rmsep, cum_prop*100, col = cols[i], lwd = 3, lty = lty[i])]
}
mtext(text = xlab, side = 1, line = 1.9, font = 2)

axis(side = 1, at = pretty(xr), pos = yr[1], lwd = 1)
axis(side = 2, at = pretty(yr), pos = xr[1], lwd =1, las = 1)
axis(side = 3, at = pretty(xr), pos = yr[2], lwd = 1, lwd.ticks = 0, labels = F)
axis(side = 4, at = pretty(yr), pos = xr[2], lwd = 1, lwd.ticks = 0, labels = F)

crops_sp <- str_extract(crops, "(?<= ).*") |>  unique()
water_rg <- c("rainfed", "irrigated")
legend(x = xr[2], y = 0, xjust = 1, yjust = 0,
       legend = c(crops_sp, water_rg),
       col = c(unique(cols), rep("black", 2)), lty = c(rep(1,3), 1,2), lwd = rep(2,5), seg.len = 2.5)


dev.off()

dd[, as.list(range(rmse)), by = .(crop)]

