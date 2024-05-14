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
library(stringr)
library(gridBase)
library(grid)

# data  ---------------------------------
d <- fread("extrap_method/training_data/Y_ws_predictors.csv")
d <- d[crop == "Rainfed wheat"]
# training pts
tpt <- vect(d, crs = "+proj=longlat +datum=WGS84")

spam <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA") |>
  str_subset("_R.tif$") |>
  rast()
# prediciton area pts
spam[spam < 100] <- NA
ppt <- xyFromCell(spam, cells(spam)) |>
  vect(crs = "+proj=longlat +datum=WGS84")

nndm <- readRDS("extrap_method/NNDM/nndm_global_indices.RDS")
#nndm_interp <- readRDS("extrap_method/NNDM/nndm_interp_indices.RDS")




# countries --------
w <- geodata::world(path = "data/gadm")
wc <- geodata::country_codes()
w <- merge(w, wc, by.x = "GID_0", by.y = "ISO3", all.x = TRUE, all.y = FALSE)


# functions ----------
## fx to extract sites ------
ext_pts <- function(d, nndm, crp, rws) {
  dd <- d[crop == crp]
  i <- dd[,.I[station == rws]]
  dd[nndm[[crp]]$indx_test[[i]], class:= "test"]
  dd[nndm[[crp]]$indx_train[[i]], class:= "train"]
  dd[nndm[[crp]]$indx_exclude[[i]], class:= "exclude"]
  pts <- vect(dd[, .(lon,lat,class,country, station)], crs = "+proj=longlat +datum=WGS84")
  rm(dd)
  return(pts)
}

## distribution fx -----------
# function adapted from
# https://github.com/carlesmila/NNDM/blob/main/R/nndm.R
nndm_cumfreq <- function(x, ...){

  # Prepare data for plotting: Gij function
  Gij_df <- data.frame(r=x$Gij[order(x$Gij)])
  Gij_df$val <- 1:nrow(Gij_df)/nrow(Gij_df)
  Gij_df <- Gij_df[Gij_df$r <= x$phi,]
  Gij_df <- rbind(Gij_df, data.frame(r=0, val=0))
  Gij_df <- rbind(Gij_df, data.frame(r=x$phi,
                                     val=sum(x$Gij<=x$phi)/length(x$Gij)))
  Gij_df$Function <- "prediction"

  # Prepare data for plotting: Gjstar function
  Gjstar_df <- data.frame(r=x$Gjstar[order(x$Gjstar)])
  Gjstar_df$val <- 1:nrow(Gjstar_df)/nrow(Gjstar_df)
  Gjstar_df <- Gjstar_df[Gjstar_df$r <= x$phi,]
  Gjstar_df <- rbind(Gjstar_df, data.frame(r=0, val=0))
  Gjstar_df <- rbind(Gjstar_df, data.frame(r=x$phi,
                                           val=sum(x$Gjstar<=x$phi)/length(x$Gjstar)))
  Gjstar_df$Function <- "training"

  # Prepare data for plotting: G function
  Gj_df <- data.frame(r=x$Gj[order(x$Gj)])
  Gj_df$val <- 1:nrow(Gj_df)/nrow(Gj_df)
  Gj_df <- Gj_df[Gj_df$r <= x$phi,]
  Gj_df <- rbind(Gj_df, data.frame(r=0, val=0))
  Gj_df <- rbind(Gj_df, data.frame(r=x$phi,
                                   val=sum(x$Gj<=x$phi)/length(x$Gj)))
  # Merge data for plotting
  Gplot <- rbind(Gij_df, Gjstar_df)
  setnames(Gplot, c("r", "val", "Function"), c("dist", "cf", "step"))
  return(Gplot)
}

# NNDM dist cumfreq ------------
dd <- nndm_cumfreq(nndm$`Rainfed wheat`)
setDT(dd)
setorder(dd, step, dist)
dd[, dist:= dist/1e3] # m to km

# selection examples ------------
ex1 <- ext_pts(d, nndm, "Rainfed wheat", rws = "Pinsk")
ex2 <- ext_pts(d, nndm, "Rainfed wheat", rws = "Duval")



# mapping --------
cols <- RColorBrewer::brewer.pal(3, "Set1")

mar <- rep(.5,4)

png("extrap_method/maps/NNDM_LOOCV_example.png", 6, 3.8, res = 300, units = "in")
layout(matrix(c(rep(1,3), 2:4), nrow = 2, byrow = TRUE), heights = c(3,2), widths = rep(1,3))
layout.show(4)

map_ext <- ext(-155, 180, -50,65)
plot(w, ext = map_ext, col = "white", background = "lightblue", mar = mar, pax = list(tick = FALSE, labels = FALSE))
plot(ppt, col = "lightgoldenrod", add = TRUE, cex = 0.01)
plot(tpt, col = "darkred", add = TRUE, cex = 0.5)
legend("bottomleft", col = c("lightgoldenrod","darkred"), pch = 19,
       legend = c("Prediciton grid", "GYGA Yw"), xpd = NA, cex = 0.9, inset = c(0,0.02))

map_ext <- ext(-23,58,35,72)
plot(w, ext = map_ext, col = "white", background = "lightblue", mar = mar, pax = list(tick = FALSE, labels = FALSE))
plot(ex1[ex1$class == "test",], col = cols[1], add = T, pch = 8, cex = 1.2)
plot(ex1[ex1$class == "train",], col = cols[3], add = T)
plot(ex1[ex1$class == "exclude",], col = cols[2], add = T, pch = 15)
legend("topleft", col = rev(cols), pch = rev(c(8, 15, 19)),
       legend = rev(c("Test", "Exclude", "Train")), xpd = NA, cex = 0.9, inset = c(0.001,0.01))

map_ext <- ext(-130,-69.5,28,58)
plot(w, ext = map_ext, col = "white", background = "lightblue", mar = mar ,pax = list(tick = FALSE, labels = FALSE))
plot(ex2[ex2$class == "test"], col = cols[1], add = T, pch = 8, cex = 1.2)
plot(ex2[ex2$class == "train"], col = cols[3], add = T)
plot(ex2[ex2$class == "exclude"], col = cols[2], add = T, pch = 15)
legend("topright", col = rev(cols), pch = rev(c(8, 15, 19)),
       legend = rev(c("Test", "Exclude", "Train")), xpd = NA, cex = 0.9, inset = c(0.009,0.005))


par(mar = c(3,3,.4,.4),  mgp = c(2,0.7,0))
plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,3000), ylim = c(0,1))

axis(side = 1, at = seq(0, 3000, 1000), pos = 0, cex.axis = .7, tcl = -0.4)
axis(side = 2, at = seq(0, 1, .2), pos = 0, cex.axis = .7, tcl = -0.4, las = 1)
axis(side = 3, at = seq(0, 3000, 1000), pos = 1, tcl = 0, labels = FALSE)
axis(side = 4, at = 0:1, pos = 3000, cex.axis = .9, tcl = 0, labels = FALSE)

dd[step == "prediction", lines(cf~dist, col = "red", lwd = 3)]
dd[step == "training", lines(cf~dist, lwd = 2)]
mtext("Distance (km)", 1, line = 1.8, cex = 0.6)
mtext("Cumulative frequency", 2, line = 1.8, cex= 0.6)
legend(3000, 0, c("NNDM LOOCV", "Prediction"), col = c("black", "red"),
       lwd = c(2,3), xjust = 1, yjust = 0)

dev.off()


