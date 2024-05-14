# author: Fernando Aramburu Merlos


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(stringr)
library(data.table)
library(quantreg)


# LOAD DATA ----------------------

## gridded yield ---------
yield <- "extrap_method/global_estimates/5min/masked_by_aoa/*.tif" |>
  Sys.glob() |>
  rast()

## GAEZ Yield -------
gaez_mz <- rast("../GAEZ/agroclimatic_Yp/maiz200b_yld.tif")/.845/1e3
gaez_mz[gaez_mz == 0] <- NA

## CZ yields -----------
dcz <- fread("data/API/Y_cz.csv")
dcz[, ISO3_cz:= paste0(country_iso3, climatezone)]
dcz[, ypot:= ifelse(water == "Rainfed", yw, yp)]
ycz <- dcz[crop_sp %in% c("maize", "wheat", "rice"), .(ISO3_cz, crop, ypot)] |>
  dcast(ISO3_cz ~ crop, value.var = "ypot")

ssa_cz <- vect("data/CZ/regions/SSA/SSA.shp")
ssa_cz$ISO3_cz <- paste0(ssa_cz$GID_0, ssa_cz$GYGA_CZ)
ssa_cz <- merge(ssa_cz, ycz, by = "ISO3_cz")


## SPAM ---------
spam <- rast("data/SPAM/harvested_area_africa_2017_v2.1/spam2017V2r1_SSA_H_MAIZ_R.tif")

## Precipitation --------
#pp_wc <- list.files("data/crop_specific/5min", "maize_prec_..tif", full.names = TRUE) |>
#  rast() |>
#  app(sum)
pp_wc <- geodata::worldclim_global("bio", 0.5, "data/climate")[[12]]
pp_gaez <- rast("../GAEZ/agroclimatic_resources/prc_CRUTS32_Hist_8110.tif")
ai <- rast("data/climate/wc2.1_5m/AIann.tif")

# DEFINE REGION -----
world <- geodata::world(path = "data/gadm")
cc <- geodata::country_codes()
world <- merge(world, cc, by.x = "GID_0", by.y = "ISO3")

east_africa <- world[world$UNREGION1 == "Eastern Africa",]
east_africa <- east_africa[!east_africa$GID_0 %in% c("SYC", "MUS", "REU", "COM", "MYT"),]

ea_mz_ypot <- crop(yield$`Rainfed maize`, east_africa) |> mask(east_africa)

ea_mz_ypot_cz <- ssa_cz[ssa_cz$GID_0 %in% east_africa$GID_0, ]
ea_mz_ypot_cz <- ea_mz_ypot_cz[!is.na(ea_mz_ypot_cz$`Rainfed maize`), ]
ea_mz_ypot_cz <- rasterize(ea_mz_ypot_cz, ea_mz_ypot, "Rainfed maize")


# Data table -------
d <- data.table(cell = cells(ea_mz_ypot))
d[, c("lon", "lat"):= as.data.frame(xyFromCell(ea_mz_ypot, cell))]
d[, Yw_mm:= extract(ea_mz_ypot, cbind(lon, lat))]
d[, Yw_gaez:= extract(gaez_mz, cbind(lon, lat))]
d[, Yw_cz:= extract(ea_mz_ypot_cz, cbind(lon, lat))]
d[, tcl:= extract(spam, cbind(lon, lat))]
d[, prec:= extract(pp_wc, cbind(lon, lat))]
d[, prec_gaez:= extract(pp_gaez, cbind(lon, lat))]
d[, ai:= extract(ai, cbind(lon, lat))]

d <- d[!is.na(Yw_gaez) & !is.na(prec)]
d <- d[tcl > 0,]
d[, w:= tcl/max(tcl)]

# Plot -----------

## Points ------------
setorder(d, prec)

d[, black:= paste0("#000000", formatC(round(w * 50), width = 2, flag = "0"))]


yl <- c(0,20)
xl <- c(0, 2000)

lo_mm <- loess(Yw_mm ~ prec, d[prec < xl[2]], w, span = 2)
lo_gaez <- loess(Yw_gaez ~ prec, d[prec < xl[2]], w, span = 2)

png("extrap_method/plots/Yw_fx_prec_rfed_maize_East_Africa.png", 6,3, units = "in", res = 300)
par(mfrow = c(1,2), pty = "s", mar = c(1,1,1,1), cex.axis = 0.8, mgp = c(2,0.8,0), oma = c(2,2,0,0))

d[
  prec < xl[2],
  plot(Yw_mm ~ prec, col = black, axes = FALSE, xlim=xl, ylim = yl, xlab="", ylab="",
       pch = 16, cex = 0.5)
]
#lines(d[prec < xl[2], prec], predict(lo_mm), col = "red", lwd = 1.5)
d[prec > 450 & prec < 1900, .(prec, predict(lo_mm, prec))] |>
  lines(col = "red", lwd = 1.5)

axis(1, at = seq(xl[1], xl[2], 500), pos = yl[1])
axis(2, at = seq(yl[1], yl[2], 4), las = 1, pos = xl[1])
axis(3, at = xl, pos = yl[2], lwd.ticks = 0, labels = FALSE)
axis(4, at = yl, pos = xl[2], lwd.ticks = 0, labels = FALSE)
mtext("Metamodel", font = 2)
#mtext("Annual precipitation (mm)", 1, line = 1.8)
mtext(expression(Yw~(t~ha^-1)), 2, line = 1.8)

d[
  prec < xl[2],
  plot(Yw_gaez ~ prec, col = black, axes = FALSE, xlim=xl, ylim = yl, xlab="", ylab="",
       pch = 16, cex = 0.5)
]
d[prec > 450 & prec < 1900, .(prec, predict(lo_gaez, prec))] |>
  lines(col = "red", lwd = 1.5)
#lines(d[prec < xl[2], prec], predict(lo_gaez), col = "red", lwd = 1.5)
axis(1, at = seq(xl[1], xl[2], 500), pos = yl[1])
axis(2, at = seq(yl[1], yl[2], 4), las = 1, pos = xl[1])
axis(3, at = xl, pos = yl[2], lwd.ticks = 0, labels = FALSE)
axis(4, at = yl, pos = xl[2], lwd.ticks = 0, labels = FALSE)
mtext("GAEZ", font = 2)
#mtext("Annual precipitation (mm)", 1, line = 1.8)
mtext("Annual precipitation (mm)", 1, outer = TRUE, xpd = NA, line = 0.8)


# setorder(d, ai)
# xl <- c(0, 1.5)
#
# lo_mm <- loess(Yw_mm ~ ai, d[ai < 1.5,], w, span = 1)
# lo_gaez <- loess(Yw_gaez ~ ai, d[ai < 1.5,], w, span = 1)
#
# d[
#   ai < xl[2],
#   plot(Yw_mm ~ ai, col = black, axes = FALSE, xlim=xl, ylim = yl, xlab="", ylab="",
#        pch = 16, cex = 0.5)
# ]
# lines(d[ai < 1.5, ai], predict(lo_mm), col = "red", lwd = 1.5)
#
# axis(1, at = seq(xl[1], xl[2], 0.5), pos = yl[1])
# axis(2, at = seq(yl[1], yl[2], 4), las = 1, pos = xl[1])
# axis(3, at = xl, pos = yl[2], lwd.ticks = 0, labels = FALSE)
# axis(4, at = yl, pos = xl[2], lwd.ticks = 0, labels = FALSE)
# mtext(expression(Yw~(t~ha^-1)), 2, line = 1.8)
# mtext(expression(frac(Annual~precipitaiton~(mm),Potential~evapotranspiration~(mm))), 1, line = 1.8)
#
# d[
#   ai < xl[2],
#   plot(Yw_gaez ~ ai, col = black, axes = FALSE, xlim=xl, ylim = yl, xlab="", ylab="",
#        pch = 16, cex = 0.5)
# ]
# lines(d[ai < xl[2], ai], predict(lo_gaez), col = "red", lwd = 1.5)
# axis(1, at = seq(xl[1], xl[2], 0.5), pos = yl[1])
# axis(2, at = seq(yl[1], yl[2], 4), las = 1, pos = xl[1])
# axis(3, at = xl, pos = yl[2], lwd.ticks = 0, labels = FALSE)
# axis(4, at = yl, pos = xl[2], lwd.ticks = 0, labels = FALSE)
# mtext("GAEZ", font = 2)
# mtext(expression(frac(Annual~precipitaiton~(mm),Potential~evapotranspiration~(mm))), 1, line = 1.8)

dev.off()


## Density -----------
## create breaks ---------------------
nbreaks <- 11
columns <- grep("Yw|prec", names(d), value = TRUE)
fbrks <- function(x) seq(min(x, na.rm = T), max(x, na.rm = T), l = nbreaks)
dbrks <- d[, lapply(.SD, fbrks) , .SDcols = columns]
## create bins ---------------------
dbins <- paste0(columns, "_bin")
fbins <- function(x, brks) findInterval(x, brks, rightmost.closed = T)
d[, (dbins):= mapply(fbins, .SD, dbrks, SIMPLIFY = F), .SDcols = columns]
d[, sum(tcl), by = .(Yw_mm_bin, prec_bin)][, max(V1)]
d[, sum(tcl), by = .(Yw_gaez_bin, prec_bin)][, max(V1)]
names(d)

dd <- d[, .(Yw = mean(Yw_mm), area = sum(tcl)/sum(d$tcl)), , by = Yw_mm_bin]
setorder(dd, Yw)
barplot(dd$area, names.arg = round(dd$Yw))


dd <- d[, .(Yw = mean(Yw_gaez), area = sum(tcl)/sum(d$tcl)), , by = Yw_gaez_bin]
setorder(dd, Yw)
barplot(dd$area, names.arg = round(dd$Yw))
d[Yw_gaez > 10 & Yw_gaez < 12, sum(tcl)]/sum(d$tcl)
d[Yw_mm > 10 & Yw_mm < 12, sum(tcl)]/sum(d$tcl)

source("G:/My Drive/globcropdiv/Functions/my_plot.R")
max.area = 72000
ibrks <- seq(0, max.area, l = 513)
fig.file = "extrap_method/plots/Yw_fx_prec_rfed_maize_East_Africa.png"
png(filename = fig.file, units = 'in', width = 4.2,
   height = 2, type = "cairo", res = 300, pointsize = 11)
par(mgp = c(0,0.9,0), oma = c(4,4,2,0), mar = c(.8,0.2,.2,0.2))
layout(t(1:3), width = c(5,5,1), height = c(1,1))
xr <- c(0, 2500)
yr <- c(0, 20)
plot_frame(xr, yr, ylabel = bquote(Yw~(t~ha^-1)))
ad_plot(d, "prec", "Yw_mm", dbrks$prec, dbrks$Yw_mm, add = T,
       ibrks = ibrks, showmax = T, low_white = TRUE)
mtext("Metamodel", font = 2, cex = 1.1)
plot_frame(xr, yr, yl = F)
ad_plot(d, "prec", "Yw_gaez", dbrks$prec, dbrks$Yw_gaez, add = T,
       ibrks = ibrks, showmax = T, low_white = TRUE)
mtext("Annual precipitation (mm)", side = 1, outer = T, line = 1.5, adj = 10/11 * .5)
mtext("GAEZ", font = 2, cex = 1.1)
legend_image <- as.raster(matrix(c("white", viridis::mako(513)), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
text(x = 0.7, y = 0.95, 'k ha', cex = 1.1, adj = 0.5)
rasterImage(legend_image, 0, 0.1, 1, .85)
text(x = 1.2, y = seq(0.12,0.83, l = 5), adj = 0,
    labels = seq(0, max.area, l = 5)/1e3, cex = 0.9)
dev.off()
# WP boundary function ------------
qr <- function(x, y, q = 0.01) {
 mod <- rq(x ~ y, tau = q)
 b <- 1/coef(mod)[2]
 a <- - coef(mod)[1]/coef(mod)[2]
 return(c(a,b))
}
qr_mm_wc <- d[, qr(prec, Yw_mm)]
qr_mm_gaez <- d[, qr(prec_gaez, Yw_mm)]
qr_gaez_wc <- d[, qr(prec, Yw_gaez)]
qr_gaez_gaez <- d[, qr(prec_gaez, Yw_gaez)]
qr_mm_wc
qr_mm_gaez
qr_gaez_wc
qr_gaez_gaez
png("extrap_method/plots/Yw_fx_prec_rfed_maize_East_Africa.png", 6,6, units = "in", res = 300)
par(mfrow = c(2,2), pty = "s", mar = c(2,2,1,1), cex.axis = 0.8, mgp = c(2,0.8,0), oma = c(2,2,0,0))
yl <- c(0,20)
xl <- c(0, 2500)
d[prec < xl[2], plot(Yw_mm ~ prec, col = "#00000005", axes = FALSE, xlim=xl, ylim = yl, xlab="", ylab="")]
axis(1, at = seq(xl[1], xl[2], 500), pos = yl[1])
axis(2, at = seq(yl[1], yl[2], 4), las = 1, pos = xl[1])
axis(3, at = xl, pos = yl[2], lwd.ticks = 0, labels = FALSE)
axis(4, at = yl, pos = xl[2], lwd.ticks = 0, labels = FALSE)
text(500, (qr_mm_wc[2]*500+qr_mm_wc[1]),
    bquote(.(round(qr_mm_wc[2]*1e3))~kg~ha^-1~mm^-1),
    srt = (atan(qr_mm_wc[2]*(diff(xl)/diff(yl)))/(2*pi))*360, pos = 3
)
clip(xl[1], xl[2], yl[1], yl[2])
abline(a = qr_mm_wc[1], b = qr_mm_wc[2], col = "red", lwd = 2)
mtext(expression(Metamodel~Yw~(t~ha^-1)), 2, line = 1.8)
d[prec_gaez < xl[2], plot(Yw_mm ~ prec_gaez, col = "#00000005", axes = FALSE, xlim=xl, ylim = yl, xlab="", # ylab="")]
axis(1, at = seq(xl[1], xl[2], 500), pos = yl[1])
axis(2, at = seq(yl[1], yl[2], 4), las = 1, pos = xl[1])
axis(3, at = xl, pos = yl[2], lwd.ticks = 0, labels = FALSE)
axis(4, at = yl, pos = xl[2], lwd.ticks = 0, labels = FALSE)
text(500, (qr_mm_gaez[2]*500+qr_mm_gaez[1]),
    bquote(.(round(qr_mm_gaez[2]*1e3))~kg~ha^-1~mm^-1),
    srt = (atan(qr_mm_gaez[2]*(diff(xl)/diff(yl)))/(2*pi))*360, pos = 3
)
clip(xl[1], xl[2], yl[1], yl[2])
abline(a = qr_mm_gaez[1], b = qr_mm_gaez[2], col = "red", lwd = 2)
d[prec < xl[2], plot(Yw_gaez ~ prec, col = "#00000005", axes = FALSE, xlim=xl, ylim = yl, xlab="", ylab=""# )]
axis(1, at = seq(xl[1], xl[2], 500), pos = yl[1])
axis(2, at = seq(yl[1], yl[2], 4), las = 1, pos = xl[1])
axis(3, at = xl, pos = yl[2], lwd.ticks = 0, labels = FALSE)
axis(4, at = yl, pos = xl[2], lwd.ticks = 0, labels = FALSE)
text(500, (qr_gaez_wc[2]*500+qr_gaez_wc[1]),
    bquote(.(round(qr_gaez_wc[2]*1e3))~kg~ha^-1~mm^-1),
    srt = (atan(qr_gaez_wc[2]*(diff(xl)/diff(yl)))/(2*pi))*360, pos = 3
)
clip(xl[1], xl[2], yl[1], yl[2])
abline(a = qr_gaez_wc[1], b = qr_gaez_wc[2], col = "red", lwd = 2)
mtext(expression(GAEZ~Yw~(t~ha^-1)), 2, line = 1.8)
mtext(expression(Worldclim~annual~precipitaiton(mm)), 1, line = 1.8)
d[prec_gaez < xl[2], plot(Yw_gaez ~ prec_gaez, col = "#00000005", axes = FALSE, xlim=xl, ylim = yl, xlab# ="", ylab="")]
axis(1, at = seq(xl[1], xl[2], 500), pos = yl[1])
axis(2, at = seq(yl[1], yl[2], 4), las = 1, pos = xl[1])
axis(3, at = xl, pos = yl[2], lwd.ticks = 0, labels = FALSE)
axis(4, at = yl, pos = xl[2], lwd.ticks = 0, labels = FALSE)
text(500, (qr_gaez_gaez[2]*500+qr_gaez_gaez[1]),
    bquote(.(round(qr_gaez_gaez[2]*1e3))~kg~ha^-1~mm^-1),
    srt = (atan(qr_gaez_gaez[2]*(diff(xl)/diff(yl)))/(2*pi))*360, pos = 3
)
clip(xl[1], xl[2], yl[1], yl[2])
abline(a = qr_gaez_gaez[1], b = qr_gaez_gaez[2], col = "red", lwd = 2)
mtext(expression(GAEZ~annual~precipitaiton(mm)), 1, line = 1.8)
dev.off()
