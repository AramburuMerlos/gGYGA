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

# extract country totals from SPAM ----------
w <- geodata::world(resolution = 1, path = "data/gadm")

w_area <- extract(r, w, fun = sum, na.rm = TRUE)

wd <- cbind(as.data.frame(w), w_area) |> as.data.table()

wd <- melt(wd, id.vars = c("GID_0", "NAME_0", "ID"), variable.name = "ln", value.name = "crop_area_ha")

setnames(wd, "GID_0", "country_iso3")

# add area to country data -------------
cty <- fread("data/API/Y_country.csv")

cty <- merge(cty, crops, by = "crop_sp", all = FALSE)

cty[, ln:=  paste0("",
                   toupper(spam),
                   ifelse(water == "Rainfed", "_R", "_I")),]

cty_area <- wd[, .(country_iso3, ln, crop_area_ha)][cty, on = .NATURAL]

cty_area[, ln:= NULL]


fwrite(cty_area, "data/API/Y_country_area.csv")
