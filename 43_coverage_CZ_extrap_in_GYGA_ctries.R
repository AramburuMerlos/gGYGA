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
r <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  grep(pattern = "WHEA|RICE|MAIZ", value = TRUE) |>
  grep(pattern = "_(I|R).tif$", value = TRUE) |>
  rast()
names(r)

# mask by GYGA Ctries ------
d <- fread("data/API/Y_country.csv")
d <- d[crop_sp %in% c("wheat", "maize", "rice"), .(crop_sp, water, country_iso3)]
d[, id_spam:= toupper(paste0(substr(crop_sp,1,4), "_", substr(water,1,1)))]

w <- geodata::world(path = "data/gadm")
lcells <- vector("list", nlyr(r))
names(lcells) <- names(r)

for(i in seq_len(nlyr(r))) {
  ww <- w[w$GID_0 %in% d[id_spam == names(r)[i], country_iso3]]
  r[[i]] <- mask(r[[i]], ww, touches = TRUE)
  dt <- cells(r[[i]], ww, touches = TRUE) |> as.data.table()
  dt[, ID:= NULL]
  lcells[[i]] <- dt
  rm(ww,dt)
}



# add SPAM area -------------------
## (without considering country borders)
## i.e. one Yx estimation in a CZ is enough for the whole CZ across the globe

cz <- fread("data/SPAM/cell_coverage/SPAM_cells_per_CZ.csv")
lcz <- vector("list", nlyr(r))
names(lcz) <- names(r)

for(i in seq_len(nlyr(r))) {
  dt <- copy(cz)
  dt <- dt[lcells[[i]], on = .(cell)]
  dt[, names(r[[i]]):= extract(r[[i]], cell)]
  lcz[[i]] <- dt
  rm(dt)
}

# CZ included in GYGA -----------
dd <- fread("data/API/Y_cz.csv")
dd[, id_spam:= toupper(paste0(substr(crop_sp,1,4), "_", substr(water,1,1)))]
dd <- dd[, .(id_spam, climatezone)]
setnames(dd, "climatezone",  "GYGA_CZ")
df <- data.table(crop = names(lcz))

for(i in names(lcz)){
  dt <- lcz[[i]]
  incl <- dt[GYGA_CZ %in% dd[id_spam == i, GYGA_CZ], sum(get(i) * weights, na.rm = T)]
  totl <- dt[, sum(get(i) * weights, na.rm = T)]
  df[crop == i, included:= incl]
  df[crop == i, total:= totl]
}

df[, crop_sp:= substr(crop, 1, 4)]
df[, round(100*sum(included)/sum(total)), by = .(crop_sp)]
