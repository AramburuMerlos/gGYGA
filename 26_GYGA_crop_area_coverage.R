# author: Fernando Aramburu Merlos
# date: 2022-07-20

# area coverage of GYGA project at different aggregation levels
# THIS NEEDS TO BE DONE BY CROP, SINCE NOT ALL CROPS INCLUDE ALL SAME CZ AND BFS

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

## load SPAM data ------
crops <- fread("data/crop_list.csv")
r <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  rast()

crops_spam <- unique(crops$spam)

sel_layers <- c(
  paste0(toupper(crops_spam), "_R"),
  paste0(toupper(crops_spam), "_I")
)

r <- r[[sel_layers]]

# add millets
r$MILL_R <- r$PMIL_R + r$SMIL_R
r$MILL_I <- r$PMIL_I + r$SMIL_I

r$PMIL_R <- NULL
r$SMIL_R <- NULL
r$PMIL_I <- NULL
r$SMIL_I <- NULL

crops[crop_sp == "millet", spam:= "mill"]
crops <- unique(crops)


# World Totals -------------
totals <- global(r, sum, na.rm = TRUE)
totals$spam <- rownames(totals) |>
  str_remove("_.$") |>
  tolower()

totals$water <- ifelse(grepl("_R$", rownames(totals)), "Rainfed", "Irrigated")
setDT(totals)

setnames(totals, "sum", "world")
totals <- crops[totals, on = "spam"]
setorderv(totals, "world", order = -1L)

totals <- totals[, .(world = sum(world)), by = .(crop_sp, water)]
totals[, crop:= paste(water, crop_sp)]


# Country level -------------
ctry <- fread("data/API/Y_country_area.csv")

ctry[crop == "Rainfed maize" & country_iso3 == "USA", .(harv_area, crop_area_ha)]

ctry_tot <- ctry[
  , .(ctry_spam = sum(crop_area_ha, na.rm = T), ctry_gyga = sum(harv_area, na.rm = T))
  , by = .(crop_sp, water)
]

totals <- ctry_tot[totals, on = .NATURAL]

# harvested area percentage covered by CZ and organize totals data
totals[, ctry_p_spam:= ctry_spam/world * 100]
totals[, ctry_p_gyga:= ctry_gyga/world * 100]

# remove non available crops
unique(ctry$crop)[! unique(ctry$crop) %in% totals$crop]
totals <- totals[crop %in% unique(ctry$crop)]


# CZ level -------------------
## (without considering country borders)
## i.e. one Yx estimation in a CZ is enough for the whole CZ across the globe
cz_areas <- fread("data/API/CZ_crop_area.csv")

# GYGA data
dcz <- fread("data/API/Y_cz.csv")
setnames(dcz, "climatezone", "CZ")


# subset CZ for which there is area from all CZ crop areas
ddcz <- unique(dcz[, .(crop_sp, water, CZ)])
ddcz <- cz_areas[ddcz, on = .NATURAL]
cz_tot <- ddcz[, .(cz_spam = sum(crop_area_ha)), by = .(crop_sp, water)]

# add cz totals to totals
totals <- cz_tot[totals, on = .NATURAL]

totals[, cz_p_spam:= cz_spam/world * 100]
setorderv(totals, "world", -1L)



# CZ-country level -------------------
## (considering country borders)
## Yx estimations in a CZ-country combination is restricted to country borders
cz_ctry <- fread("data/API/Y_cz_area.csv")
cz_ctry_tot <- cz_ctry[
  , .(cz_ctry_spam = sum(crop_area_ha, na.rm = T), cz_ctry_gyga = sum(harv_area, na.rm = T))
  , by = .(crop_sp, water)
]

totals <- cz_ctry_tot[totals, on = .NATURAL]

totals[, cz_ctry_p_spam:= cz_ctry_spam/world * 100]
totals[, cz_ctry_p_gyga:= cz_ctry_gyga/world * 100]



# Buffer zone level -------------------
ws <- fread("data/API/Y_ws_area.csv")
ws_tot <- ws[
  , .(ws_spam = sum(crop_area_ha, na.rm = T), ws_gyga = sum(harv_area, na.rm = T))
  , by = .(crop_sp, water)
]

totals <- ws_tot[totals, on = .NATURAL]

totals[, ws_p_spam:= ws_spam/world * 100]
totals[, ws_p_gyga:= ws_gyga/world * 100]


# organize and save ----------------------
totals
setcolorder(totals, c("crop_sp","water","world", "ctry_p_spam", "ctry_p_gyga", "cz_p_spam",
                      "cz_ctry_p_spam", "cz_ctry_p_gyga", "ws_p_spam", "ws_p_gyga"))

#print(totals, digits = 1)

fwrite(totals, "data/API/GYGA_total_area_coverage_R_I.csv")

d <- totals[, lapply(.SD, sum), by = .(crop_sp), .SDcols = c("world", "ctry_spam", "cz_ctry_spam", "ws_spam", "cz_spam")]

dp <- d[, .(
  crop = crop_sp,
  country = round(ctry_spam/world * 100),
  CZ = round(cz_ctry_spam/world * 100),
  buffer = round(ws_spam/world * 100),
  CZ_bb = round(cz_spam/world * 100),
  area_mha = round(world/1e6, 1)
  )
]

fwrite(dp, "data/API/GYGA_total_area_coverage_simple.csv")



# plot GYGA vs SPAM ------------------------
sp <- c("maize", "wheat", "soybean", "rice")

cols <- RColorBrewer::brewer.pal(4, "Set1")
pchs <- c(22,21)

png("extrap_method/plots/crop_area_comp_gyga_vs_spam.png", 8, 2.5, units = "in", res = 300)

par(mfrow = c(1,3), mar = c(2,2,.5,.5), oma = c(2,2,1,4))

## buffer ---------
xy <- ws[
  crop_sp %in% sp
  , .(x = crop_area_ha/1e6, y = harv_area/1e6, col = as.factor(crop_sp), pch = as.factor(water))
]

max(xy[, .(x, y)], na.rm = TRUE)

mxi <- 1.6

plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

axis(side = 1, at = seq(0, mxi, .4), pos = 0, cex.axis = .9, tcl = -0.4)
axis(side = 2, at = seq(0, mxi, .4), pos = 0, cex.axis = .9, tcl = -0.4)
axis(side = 1, at = c(0, mxi), labels = FALSE, pos = mxi, lwd.ticks = 0)
axis(side = 2, at = c(0, mxi), labels = FALSE, pos = mxi, lwd.ticks = 0)

xy[, points(x = x, y = y, bg = cols[col], pch = pchs[pch])]

mtext("Buffer zone", cex = .8)

clip(0,mxi,0,mxi)
abline(a =0 , b = 1, lty = 3)

## cz_ctry ---------
xy <- cz_ctry[
  crop_sp %in% sp
  , .(x = crop_area_ha/1e6, y = harv_area/1e6, col = as.factor(crop_sp), pch = as.factor(water))
]

max(xy[, .(x, y)], na.rm = TRUE)

mxi <- 8

plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

axis(side = 1, at = seq(0, mxi, 2), pos = 0, cex.axis = .9, tcl = -0.4)
axis(side = 2, at = seq(0, mxi, 2), pos = 0, cex.axis = .9, tcl = -0.4)
axis(side = 1, at = c(0, mxi), labels = FALSE, pos = mxi, lwd.ticks = 0)
axis(side = 2, at = c(0, mxi), labels = FALSE, pos = mxi, lwd.ticks = 0)

xy[, points(x = x, y = y, bg = cols[col], pch = pchs[pch])]

mtext("Climate zone", cex = .8)

clip(0,mxi,0,mxi)
abline(a =0 , b = 1, lty = 3)


## ctry ---------
xy <- ctry[
  crop_sp %in% sp
  , .(x = crop_area_ha/1e6, y = harv_area/1e6, col = as.factor(crop_sp), pch = as.factor(water))
]

max(xy[, .(x, y)], na.rm = TRUE)

mxi <- 32

plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0,mxi), ylim = c(0,mxi))

axis(side = 1, at = seq(0, mxi, 8), pos = 0, cex.axis = .9, tcl = -0.4)
axis(side = 2, at = seq(0, mxi, 8), pos = 0, cex.axis = .9, tcl = -0.4)
axis(side = 1, at = c(0, mxi), labels = FALSE, pos = mxi, lwd.ticks = 0)
axis(side = 2, at = c(0, mxi), labels = FALSE, pos = mxi, lwd.ticks = 0)

xy[, points(x = x, y = y, bg = cols[col], pch = pchs[pch])]

mtext("Country", cex = .8)

clip(0,mxi,0,mxi)
abline(a =0 , b = 1, col = "red", lty = 3)

mtext("SPAM area (Mha)", 1, outer = T)
mtext("GYGA area (Mha)", 2, outer = T)



legend(
       x = mxi + 1, y = mxi/2, xjust = 0, yjust = 0.5,
       legend = c(levels(xy$col), "", levels(xy$pch)),
       pch = c(rep(21, length(cols)), NA, pchs),
       pt.bg = c(cols, "white", rep("gray60",2)),
       xpd = NA
)

dev.off()
