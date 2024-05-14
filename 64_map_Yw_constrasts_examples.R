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

# LOAD DATA ----------------------



## gridded yield ---------
yield <- "extrap_method/global_estimates/30sec/masked_by_aoa/*.tif" |>
  Sys.glob() |>
  rast()

## CZ yield ---------
dcz <- fread("data/API/Y_cz.csv")
dcz[, ISO3_cz:= paste0(country_iso3, climatezone)]
dcz[, ypot:= ifelse(water == "Rainfed", yw, yp)]
ycz <- dcz[crop_sp %in% c("maize", "wheat", "rice"), .(ISO3_cz, crop, ypot)] |>
  dcast(ISO3_cz ~ crop, value.var = "ypot")

cz <- vect("data/CZ/GYGA_CZ_gadm.shp")
cz$ISO3_cz <- paste0(cz$GID_0, cz$GYGA_CZ)
cz <- merge(cz, ycz, by = "ISO3_cz")

ssa_cz <- vect("data/CZ/regions/SSA/SSA.shp")
ssa_cz$ISO3_cz <- paste0(ssa_cz$GID_0, ssa_cz$GYGA_CZ)
ssa_cz <- merge(ssa_cz, ycz, by = "ISO3_cz")



## GAEZ Yield -------
gaez_mz <- rast("../GAEZ/agroclimatic_Yp/maiz200b_yld.tif")/.845/1e3
gaez_mz[gaez_mz == 0] <- NA
gaez_wh <- app(
  c(
    rast("../GAEZ/agroclimatic_Yp/swhe200b_yld.tif"),
    rast("../GAEZ/agroclimatic_Yp/wwhe200b_yld.tif")
  ), max, na.rm = TRUE)/.865/1e3
gaez_wh[gaez_wh == 0] <- NA

gaez_rw <- rast("../GAEZ/agroclimatic_Yp/ricw200a_yld.tif")/.86/1e3
gaez_rw[gaez_rw == 0] <- NA

# DEFINE REGIONS -----
world <- geodata::world(path = "data/gadm")
cc <- geodata::country_codes()
world <- merge(world, cc, by.x = "GID_0", by.y = "ISO3")
niche_africa <- world[world$GID_0 %in% c("KEN", "UGA", "TZA", "RWA", "BDI"),]
west_africa <- world[world$UNREGION1 == "Western Africa",]
west_africa <- west_africa[!west_africa$GID_0 %in% c("SHN", "CPV"),]

east_africa <- world[world$UNREGION1 == "Eastern Africa",]
east_africa <- east_africa[!east_africa$GID_0 %in% c("SYC", "MUS", "REU", "COM", "MYT"),]

center_eurasia <- world[world$UNREGION1 %in% c("Eastern Europe", "Western Asia", "Central Asia"),]
center_eurasia <- crop(center_eurasia, ext(c(20, 80, 35, 75)))

east_europe <- world[
  world$UNREGION1 %in% c("Eastern Europe", "Northern Europe", "Southern Europe", "Western Asia", "Central Asia") |
    world$GID_0 == "IRN",]
east_europe <- crop(east_europe, ext(c(15, 55, 36, 71)))

southern_america <- world[world$UNREGION1 == "South America",] |>
  crop(ext(c(-85,-30, -45,-10)))

north_america <- world[world$UNREGION1 %in% c("Northern America", "Central America", "Caribbean"),] |>
  crop(ext(c(-125,-65,7,49)))

#southeast_asia <- world[
#  world$UNREGION1 == "South-Eastern Asia" | world$GID_0 %in% c("CHN", "BGD", "IND"),
#]
#sea_ext <- ext(southeast_asia)
#southeast_asia <- crop(southeast_asia, c(88, 127, sea_ext[3], 30))

southeast_asia <- world[world$UNREGION1 == "South-Eastern Asia",]
sea_ext <- ext(southeast_asia)
southeast_asia <- crop(southeast_asia, c(sea_ext[1], 127, sea_ext[3:4]))


# MAP Ypot #######################################
dir.create("extrap_method/maps/Ypot_examples/", F, T)
col_yield <- paletteer::paletteer_c("ggthemes::Red-Green-Gold Diverging", 256)

## South East Asia ---------

ea_rw_ypot <- crop(yield$`Irrigated rice`, southeast_asia) |> mask(southeast_asia)

ea_rw_ypot_gaez <- crop(gaez_rw, southeast_asia) |>
  mask(southeast_asia) |>
  disagg(10) |>
  resample(ea_rw_ypot) |>
  mask(ea_rw_ypot)
ea_rw_ypot_gaez[ea_rw_ypot_gaez < 6] <- NA

ea_rw_ypot_cz <- cz[cz$GID_0 %in% southeast_asia$GID_0, ]
ea_rw_ypot_cz <- ea_rw_ypot_cz[!is.na(ea_rw_ypot_cz$`Irrigated rice`), ]
range(ea_rw_ypot_cz$`Irrigated rice`)
global(ea_rw_ypot_gaez, range, na.rm = TRUE)
global(ea_rw_ypot, range, na.rm = TRUE)

ea_rw_ypot_gaez[ea_rw_ypot_gaez >= 12] <- 11.99
ea_rw_ypot[ea_rw_ypot >= 12] <- 11.99

tiff(filename = "extrap_method/maps/ypot_examples/SE_Asia_rice_Yp.tif",
     width = 9, height = 3,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(1,3), omi = c(0,.3,.3,.5))

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_rw_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_rw_ypot_gaez, maxcell = ncell(ea_rw_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous", legend = FALSE, range = c(6,12))
plot(southeast_asia, add = TRUE, lwd = 1)
mtext("GAEZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_rw_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_rw_ypot_cz, "Irrigated rice", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(6,12))
plot(southeast_asia, add = TRUE, lwd = 1)
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_rw_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_rw_ypot, col = col_yield, add = TRUE, maxcell = ncell(ea_rw_ypot),
     type = "continuous", range = c(6,12), plg = list(title = "Rice\nYp (t/ha)"))
plot(southeast_asia, add = TRUE, lwd = 1)
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)

dev.off()


## East Africa ---------

ea_mz_ypot <- crop(yield$`Rainfed maize`, east_africa) |> mask(east_africa)

ea_mz_ypot_gaez <- crop(gaez_mz, east_africa) |>
  mask(east_africa) |>
  disagg(10) |>
  resample(ea_mz_ypot) |>
  mask(ea_mz_ypot)

ea_mz_ypot_cz <- ssa_cz[ssa_cz$GID_0 %in% east_africa$GID_0, ]
ea_mz_ypot_cz <- ea_mz_ypot_cz[!is.na(ea_mz_ypot_cz$`Rainfed maize`), ]
range(ea_mz_ypot_cz$`Rainfed maize`)

tiff(filename = "extrap_method/maps/ypot_examples/EA_mz_ypot.tif",
     width = 7, height = 3,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(1,3), omi = c(0,.3,.3,.5))

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_mz_ypot_gaez, maxcell = ncell(ea_mz_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous", legend = FALSE, range = c(2,20))
plot(east_africa, add = TRUE, lwd = 1)
mtext("GAEZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_mz_ypot_cz, "Rainfed maize", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(2,20))
plot(east_africa, add = TRUE, lwd = 1)
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_mz_ypot, col = col_yield, add = TRUE, maxcell = ncell(ea_mz_ypot),
     type = "continuous", range = c(2,20), plg = list(title = "Maize\nYw (t/ha)"))
plot(east_africa, add = TRUE, lwd = 1)
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)

dev.off()

## West Africa ---------
wa_mz_ypot <- crop(yield$`Rainfed maize`, west_africa) |> mask(west_africa)
wa_mz_ypot[wa_mz_ypot >= 16] <- 15.99

wa_mz_ypot_gaez <- crop(gaez_mz, west_africa) |>
  mask(west_africa) |>
  disagg(10) |>
  resample(wa_mz_ypot) |>
  mask(wa_mz_ypot)

wa_mz_ypot_cz <- ssa_cz[ssa_cz$GID_0 %in% west_africa$GID_0, ]
range(wa_mz_ypot_cz$`Rainfed maize`)
wa_mz_ypot_cz <- ssa_cz[ssa_cz$GID_0 %in% west_africa$GID_0, ]
range(wa_mz_ypot_cz$`Rainfed maize`)
wa_mz_ypot_cz$`Rainfed maize`[wa_mz_ypot_cz$`Rainfed maize` >= 16] <- 15.99


tiff(filename = "extrap_method/maps/ypot_examples/WA_mz_ypot.tif",
     width = 8, height = 2.5,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(1,3), omi = c(0,.3,.3,.5))

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(wa_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(wa_mz_ypot_gaez, maxcell = ncell(wa_mz_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous", legend = FALSE, range = c(2,16))
plot(west_africa, add = TRUE, lwd = 1)

mtext("GAEZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(wa_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(wa_mz_ypot_cz, "Rainfed maize", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(2,16))
plot(west_africa, add = TRUE, lwd = 1)
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(wa_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(wa_mz_ypot, col = col_yield, add = TRUE, maxcell = ncell(wa_mz_ypot),
     type = "continuous", range = c(2,16))
plot(west_africa, add = TRUE, lwd = 1)
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)

dev.off()


## East Europe ---------
ee_box <- extend(ext(east_europe), 0.5)
ee_wh_ypot <- crop(yield$`Rainfed wheat`, ee_box)

ee_wh_ypot_gaez <- crop(gaez_wh, ee_box) |>
  disagg(10) |>
  resample(ee_wh_ypot) |>
  mask(ee_wh_ypot)

ee_wh_ypot_cz <- cz[cz$GID_0 %in% east_europe$GID_0, ]

tiff(filename = "extrap_method/maps/ypot_examples/EE_wh_ypot.tif",
     width = 5.8, height = 3,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(1,3), omi = c(0,.3,.3,.5))

plot(world,  border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(east_europe),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ee_wh_ypot_gaez, maxcell = ncell(ee_wh_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous",legend = FALSE,  range = c(0,12))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",],add = TRUE, border = "gray20", col = "lightblue")
mtext("GAEZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(east_europe),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ee_wh_ypot_cz, "Rainfed wheat", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,12))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(east_europe),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ee_wh_ypot, col = col_yield, add = TRUE, maxcell = ncell(ee_wh_ypot),
     type = "continuous",  range = c(0,12), plg = list(title = "Wheat\nYw (t/ha)"))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)

dev.off()

## Southern America ---------
sa_box <- extend(ext(southern_america), 0.5)
sa_wh_ypot <- crop(yield$`Rainfed wheat`, sa_box)
sa_wh_ypot[sa_wh_ypot >= 10] <- 9.99

sa_wh_ypot_gaez <- crop(gaez_wh, sa_box) |>
  disagg(10) |>
  resample(sa_wh_ypot) |>
  mask(sa_wh_ypot)

sa_wh_ypot_cz <- cz[cz$GID_0 %in% southern_america$GID_0, ]
sa_wh_ypot_cz <- sa_wh_ypot_cz[!is.na(sa_wh_ypot_cz$`Rainfed wheat`),]


tiff(filename = "extrap_method/maps/ypot_examples/SAm_wh_ypot.tif",
     width = 8, height = 3,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(1,3), omi = c(0,.3,.3,.5))

plot(world,  border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(southern_america),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(sa_wh_ypot_gaez, maxcell = ncell(sa_wh_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,10))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")
mtext("GAEZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(southern_america),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(sa_wh_ypot_cz, "Rainfed wheat", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,10))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(southern_america),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(sa_wh_ypot, col = col_yield, add = TRUE, maxcell = ncell(sa_wh_ypot),
     type = "continuous", range = c(0,10))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)

dev.off()

## North America ---------
nam_box <- extend(ext(north_america), 0.5)
nam_mz_ypot <- crop(yield$`Rainfed maize`, nam_box)
nam_mz_ypot[nam_mz_ypot >= 10] <- 9.99

nam_mz_ypot_gaez <- crop(gaez_mz, nam_box) |>
  disagg(10) |>
  resample(nam_mz_ypot) |>
  mask(nam_mz_ypot)

nam_mz_ypot_cz <- cz[cz$GID_0 %in% north_america$GID_0, ]
nam_mz_ypot_cz <- nam_mz_ypot_cz[!is.na(nam_mz_ypot_cz$`Rainfed maize`),]
hist(nam_mz_ypot_cz$`Rainfed maize`)

tiff(filename = "extrap_method/maps/ypot_examples/NAm_mz_ypot.tif",
     width = 9, height = 3,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(1,3), omi = c(0,.3,.3,.5))

plot(world,  border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(north_america),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(nam_mz_ypot_gaez, maxcell = ncell(nam_mz_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,14))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")
mtext("GAEZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(north_america),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(nam_mz_ypot_cz, "Rainfed maize", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,14))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(north_america),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(nam_mz_ypot, col = col_yield, add = TRUE, maxcell = ncell(nam_mz_ypot),
     type = "continuous", range = c(0,14))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)

dev.off()

## Extended Data Figure ----------

tiff(filename = "extrap_method/maps/ypot_examples/Figure.tif",
     width = 6.5, height = 8.,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(3,3), omi = c(0,.3,.3,.5))

### East Africa---------------
plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_mz_ypot_gaez, maxcell = ncell(ea_mz_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous", legend = FALSE, range = c(2,20))
plot(east_africa, add = TRUE, lwd = 1)
mtext("GAEZ", 3, line = 0.2, font = 2, cex = 1)
mtext("Rainfed maize", side = 2, line = 0.2)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_mz_ypot_cz, "Rainfed maize", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(2,20))
plot(east_africa, add = TRUE, lwd = 1)
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_mz_ypot, col = col_yield, add = TRUE, maxcell = ncell(ea_mz_ypot),
     type = "continuous", range = c(2,20), plg = list(title = "Ypot\n(t/ha)", title.font = 2, cex= 1.2))
plot(east_africa, add = TRUE, lwd = 1)
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)

### East Europe -------------

plot(world,  border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(east_europe),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ee_wh_ypot_gaez, maxcell = ncell(ee_wh_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous",legend = FALSE,  range = c(0,12))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",],add = TRUE, border = "gray20", col = "lightblue")
mtext("Rainfed wheat", side = 2, line = 0.2)

plot(world, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(east_europe),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ee_wh_ypot_cz, "Rainfed wheat", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,12))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")

plot(world, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(east_europe),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ee_wh_ypot, col = col_yield, add = TRUE, maxcell = ncell(ee_wh_ypot),
     type = "continuous",  range = c(0,12), plg = list(cex = 1.2))
plot(world, add = TRUE, border = "gray20")
plot(world[world$GID_0 == "XCA",], add = TRUE, border = "gray20", col = "lightblue")

### SE Asia -------------

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_rw_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_rw_ypot_gaez, maxcell = ncell(ea_rw_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous", legend = FALSE, range = c(6,12))
plot(southeast_asia, add = TRUE, lwd = 1)
mtext("Irrigated rice", side = 2, line = 0.2)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_rw_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_rw_ypot_cz, "Irrigated rice", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(6,12))
plot(southeast_asia, add = TRUE, lwd = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_rw_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_rw_ypot, col = col_yield, add = TRUE, maxcell = ncell(ea_rw_ypot),
     type = "continuous", range = c(6,12), plg = list(cex = 1.2))
plot(southeast_asia, add = TRUE, lwd = 1)

dev.off()




# DON'T RUN -----------

## Niche Africa ---------
na_mz_ypot <- crop(yield$`Rainfed maize`, niche_africa) |> mask(niche_africa)

na_mz_ypot_gaez <- crop(gaez_mz, niche_africa) |>
  mask(niche_africa) |>
  disagg(10) |>
  resample(na_mz_ypot) |>
  mask(na_mz_ypot)

na_mz_ypot_cz <- ssa_cz[ssa_cz$GID_0 %in% niche_africa$GID_0, ]

tiff(filename = "extrap_method/maps/ypot_examples/Niche_mz_ypot.tif",
     width = 7, height = 3,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(1,3), omi = c(0,.3,.3,.5))

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(na_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(na_mz_ypot_gaez, maxcell = ncell(na_mz_ypot_gaez), col = col_yield, add = TRUE,
     type = "continuous",legend = FALSE,  range = c(2,18))
plot(niche_africa, add = TRUE, lwd = 1)

mtext("GAEZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(na_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(na_mz_ypot_cz, "Rainfed maize", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(2,18))
plot(niche_africa, add = TRUE, lwd = 1)
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(na_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(na_mz_ypot, col = col_yield, add = TRUE, maxcell = ncell(na_mz_ypot),
     type = "continuous", range = c(2,18))
plot(niche_africa, add = TRUE, lwd = 1)
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)


dev.off()

## East Africa GYGA only -------------
tiff(filename = "extrap_method/maps/ypot_examples/EA_mz_ypot_GYGA.tif",
     width = 5, height = 3,
     units = "in", type = "cairo", res = 600, compression = "zip"
)

par(mfrow = c(1,2), omi = c(0,.3,.3,.5))

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_mz_ypot_cz, "Rainfed maize", col = col_yield, lwd = 0.1, add = TRUE,
     type = "continuous", legend = FALSE, range = c(2,20))
plot(east_africa, add = TRUE, lwd = 1)
mtext("GYGA CZ", 3, line = 0.2, font = 2, cex = 1)

plot(world, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ext(ea_mz_ypot),
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(ea_mz_ypot, col = col_yield, add = TRUE, maxcell = ncell(ea_mz_ypot),
     type = "continuous", range = c(2,20))
plot(east_africa, add = TRUE, lwd = 1)
mtext("Metamodel", 3, line = 0.2, font = 2, cex = 1)

dev.off()
