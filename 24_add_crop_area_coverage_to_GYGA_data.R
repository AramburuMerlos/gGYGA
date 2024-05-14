# author: Fernando Aramburu Merlos
# date: 2022-07-20

# add area covered by each polygon at different aggregation levels
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


# CZ level -------------------
## (without considering country borders)
## i.e. one Yx estimation in a CZ is enough for the whole CZ across the globe

cz <- fread("data/SPAM/cell_coverage/SPAM_cells_per_CZ.csv")

# rainfed area per crop
for(i in crops_spam) {
  ln <- paste0(toupper(i), "_R")
  col <- paste0(i, "_r")
  cz[, (col):= extract(r[ln], cell)]
}

# irrigated area per crop
for(i in crops_spam) {
  ln <- paste0(toupper(i), "_I")
  col <- paste0(i, "_i")
  cz[, (col):= extract(r[ln], cell)]
}

# summarize crop area per CZ
cols <- c(paste0(crops_spam, "_r"), paste0(crops_spam, "_i"))
cz_areas <- cz[,
               lapply(.SD, function(x) sum(x * weights)),
               .SDcols = cols,
               by = GYGA_CZ
]

# reshape to long
dcz <- melt(cz_areas, id.vars = "GYGA_CZ", variable.name = "spam_w", value.name = "crop_area_ha")

# add crop name and water regimen
dcz[, spam:= substr(spam_w, 1, 4)]
dcz[, water:= ifelse(grepl("_i$", spam_w), "Irrigated", "Rainfed")]


# deal with millet and other pulses
mill <- dcz[spam %in% c("pmil", "smil"),
            .(crop_area_ha = sum(crop_area_ha)),
            by = .(GYGA_CZ, water)]
mill[, spam := "mill"]

dcz <- rbind(dcz, mill, fill = TRUE)
dcz <- dcz[!spam %in% c("pmil", "smil"),]

dcz <- rbind(
  dcz,
  dcz[spam == "opul"][, spam:= "peas"],
  dcz[spam == "opul"][, spam:= "fbea"]
)

dcz <- dcz[spam != "opul"]
dcz[, spam_w:= NULL]

mcrops <- rbind(
  crops[!spam %in% c("smil", "pmil", "opul")],
  data.table(crop_sp = c("millet", "faba bean", "pea"), spam = c("mill", "fbea", "peas"))
)

dcz <- mcrops[dcz, on = .NATURAL]

dcz[, crop:= paste(water, crop_sp)]
dcz[, spam:= NULL]
setcolorder(dcz, c("crop_sp", "water", "GYGA_CZ", "crop_area_ha"))

setnames(dcz, "GYGA_CZ", "CZ")


# this isn't added to the GYGA data because the GYGA data for CZ across borders
# cannot be retrieved from the API
fwrite(dcz, "data/API/CZ_crop_area.csv")



# CZ-country level -------------------
## (considering country borders)
## Yx estimations in a CZ-country combination is restricted to country borders

cz_cty <- fread("data/SPAM/cell_coverage/SPAM_cells_per_CZ_and_country.csv")

# rainfed area per crop
for(i in crops_spam) {
  ln <- paste0(toupper(i), "_R")
  col <- paste0(i, "_r")
  cz_cty[, (col):= extract(r[ln], cell)]
}

# irrigated area per crop
for(i in crops_spam) {
  ln <- paste0(toupper(i), "_I")
  col <- paste0(i, "_i")
  cz_cty[, (col):= extract(r[ln], cell)]
}

# summarize crop area per CZ and country
cols <- c(paste0(crops_spam, "_r"), paste0(crops_spam, "_i"))
cz_cty_areas <- cz_cty[,
               lapply(.SD, function(x) sum(x * weights, na.rm = TRUE)),
               .SDcols = cols,
               by = .(GYGA_CZ, ISO3)
]
# reshape to long
dcz_cty <- melt(cz_cty_areas, id.vars = c("GYGA_CZ", "ISO3"), variable.name = "spam_w", value.name = "crop_area_ha")

# add spam name and water regimen
dcz_cty[, spam:= substr(spam_w, 1, 4)]
dcz_cty[, water:= ifelse(grepl("_i$", spam_w), "Irrigated", "Rainfed")]


# deal with millet and other pulses
mill <- dcz_cty[spam %in% c("pmil", "smil"),
                .(crop_area_ha = sum(crop_area_ha)),
                by = .(GYGA_CZ, water, ISO3)]
mill[, spam := "mill"]

dcz_cty <- rbind(dcz_cty, mill, fill = TRUE)
dcz_cty <- dcz_cty[!spam %in% c("pmil", "smil"),]

dcz_cty <- rbind(
  dcz_cty,
  dcz_cty[spam == "opul"][, spam:= "peas"],
  dcz_cty[spam == "opul"][, spam:= "fbea"]
)

dcz_cty <- dcz_cty[spam != "opul"]
dcz_cty[, spam_w:= NULL]

dcz_cty <- mcrops[dcz_cty, on = .NATURAL]

dcz_cty[, crop:= paste(water, crop_sp)]
dcz_cty[, spam:= NULL]
setcolorder(dcz_cty, c("crop_sp", "water", "GYGA_CZ", "ISO3", "crop_area_ha"))

# GYGA yield data
dcz_cty_y <- fread("data/API/Y_cz.csv")

dcz_cty_y[, crop:= gsub("rapse", "rape", crop)] # this should've been done b4

setkey(dcz_cty, crop_sp, water, ISO3, GYGA_CZ, crop)
setkey(dcz_cty_y, crop_sp, water, country_iso3, climatezone, crop)

dd <- dcz_cty[dcz_cty_y]

# crop area data for oil palm smallholders and large plant isn't included
dd[is.na(crop_area_ha)]

setnames(dd, "GYGA_CZ", "CZ")

fwrite(dd, "data/API/Y_cz_area.csv")



# Buffer zone level -------------------
bf <- fread("data/SPAM/cell_coverage/SPAM_cells_per_bf.csv")

# rainfed area per crop
for(i in crops_spam) {
  ln <- paste0(toupper(i), "_R")
  col <- paste0(i, "_r")
  bf[, (col):= extract(r[ln], cell)]
}

# irrigated area per crop
for(i in crops_spam) {
  ln <- paste0(toupper(i), "_I")
  col <- paste0(i, "_i")
  bf[, (col):= extract(r[ln], cell)]
}

# summarize crop area per buffer
cols <- c(paste0(crops_spam, "_r"), paste0(crops_spam, "_i"))
bf_areas <- bf[,
               lapply(.SD, function(x) sum(x * weights, na.rm = TRUE)),
               .SDcols = cols,
               by = station_id
]

# reshape to long
dbf <- melt(bf_areas, id.vars = "station_id", variable.name = "spam_w", value.name = "crop_area_ha")

# add spam name and water regimen
dbf[, spam:= substr(spam_w, 1, 4)]
dbf[, water:= ifelse(grepl("_i$", spam_w), "Irrigated", "Rainfed")]

# deal with millet and other pulses
mill <- dbf[spam %in% c("pmil", "smil"),
            .(crop_area_ha = sum(crop_area_ha)),
            by = .(station_id, water)]
mill[, spam := "mill"]

dbf <- rbind(dbf, mill, fill = TRUE)
dbf <- dbf[!spam %in% c("pmil", "smil"),]

dbf <- rbind(
  dbf,
  dbf[spam == "opul"][, spam:= "peas"],
  dbf[spam == "opul"][, spam:= "fbea"]
)

dbf <- dbf[spam != "opul"]
dbf[, spam_w:= NULL]

dbf <- mcrops[dbf, on = .NATURAL]

dbf[, crop:= paste(water, crop_sp)]
dbf[, spam:= NULL]

# GYGA yield data
dws <- fread("data/API/Y_ws.csv")

dws[, crop:= gsub("rapse", "rape", crop)] # this should've been done b4

d <- dbf[dws, on = .NATURAL]

# weather stations without crop area data bc of having no area at all
# plus oil palm smallholders and large plantations
d[is.na(crop_area_ha)]
# weather stations with 0 crop area for the crop they provide data.
d[crop_area_ha == 0]

setnames(d, c("climatezone", "country_iso3"), c("CZ", "ISO3"))

fwrite(d, "data/API/Y_ws_area.csv")

