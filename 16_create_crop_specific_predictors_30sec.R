# author: Fernando Aramburu Merlos
# date: 2022-08-01
# updated: 2022-09-27. Add new predictors based on WUR experience


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(data.table)
library(stringr)

dir.create("data/crop_specific/30sec", F, T)

spam <- "data/SPAM/harvested_area_global_merged_30sec/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA|MAIZ|RICE|SOYB") |>
  str_subset("_A") |>
  rast()

tavg <- geodata::worldclim_global("tavg", 0.5, "data/climate")
prec <- geodata::worldclim_global("prec", 0.5, "data/climate")
srad <- geodata::worldclim_global("srad", 0.5, "data/climate")
gdd <- rast("data/climate/wc2.1_30s/mgdd.tif")

dm <- data.table(
  month = 1:12,
  days_in_month = c(31,28,31,30,31,30,30,31,30,31,30,31)
)

dir.create('data/crop_specific', F, T)

# Wheat, Maize and Soybean -----------------
crops <- c("wheat", "maize", "soybean")

i=1

for(i in seq_along(crops)) {

  ## cells with crop area  > 0.01% ----------
  spam_i <- subset(spam, paste0(toupper(substr(crops[i], 1, 4)), "_A"))
  stopifnot(compareGeom(spam_i, tavg))

  d <- data.table(cell = cells(spam_i))
  d[, prop:= extract(spam_i, cell)]
  d <- d[prop > 0.0001]
  d[, prop:= NULL]

  ## planting and harvest dates -------
  # (days, month, etc.)
  r_plan <- rast(paste0("data/crop_calendar/30sec/", crops[i], "_plant_day.tif"))
  r_harv <- rast(paste0("data/crop_calendar/30sec/", crops[i], "_harvest_day.tif"))

  stopifnot(compareGeom(spam_i, r_plan))
  stopifnot(compareGeom(spam_i, r_harv))

  d[, plant_day:= extract(r_plan, cell)]
  d[, harvest_day:= extract(r_harv, cell)]
  d <- d[!is.na(plant_day) & !is.na(harvest_day)]

  d[, plant_date:= as.Date("2022-12-31") + plant_day]
  d[, harvest_date:= as.Date("2022-12-31") + harvest_day]

  d[, plant_month:= month(plant_date)]
  d[, harvest_month:= month(harvest_date)]

  d[, plant_mday:= mday(plant_date)]
  d[, harvest_mday:= mday(harvest_date)]

  d[, plant_date:= NULL]
  d[, harvest_date:= NULL]

  ## compute total GDD  ----------
  d[, (as.character(1:12)):= extract(gdd, cell)]
  d <- d[!is.na(`1`)]
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "gdd")
  d[, (as.character(1:12)):= NULL]

  dd[, month:= as.numeric(month)]
  # subset months within cropping season
  dd <- dd[
    (plant_month < harvest_month & (month >= plant_month & month <= harvest_month)) |
      (plant_month > harvest_month & (month >= plant_month | month <= harvest_month)) |
      (plant_month == harvest_month)
  ]
  # month of the season
  dd[plant_month < harvest_month, season_month:= month - plant_month]
  dd[plant_month > harvest_month & month >= plant_month, season_month:= month - plant_month]
  dd[plant_month > harvest_month & month < plant_month, season_month:= month + (12 - plant_month)]
  dd[plant_month == harvest_month & month >= plant_month, season_month:= month - plant_month]
  dd[plant_month == harvest_month & month < plant_month, season_month:= month + (12 - plant_month)]
  setorder(dd, cell, season_month)
  #dd[,.N, by = .(season_month)]

  # the GDD the months adds to the total depends on how many days is the crop in that month
  dd <- dd[dm, on = .(month)]
  dd[month != plant_month & month != harvest_month, mtot_gdd:= gdd * days_in_month]
  dd[month == plant_month & month != harvest_month, mtot_gdd:= gdd * (days_in_month - plant_mday)]
  dd[month != plant_month & month == harvest_month, mtot_gdd:= gdd * harvest_mday]
  dd[month == plant_month & month == harvest_month, mtot_gdd:= gdd * ((days_in_month - plant_mday) + harvest_mday)]
  dd[, tot_gdd:= sum(mtot_gdd), by = .(cell)]


  ## Split GDD season in 3 ----------
  setorder(dd, cell, season_month)
  dd[, cum_gdd:= cumsum(mtot_gdd), by = .(cell)]

  dm1 <- dd[(cum_gdd - tot_gdd/3) > 0]
  dm1 <- dm1[dm1[, .I[which.min(season_month)], by = .(cell)]$V1, .(cell, end_t1_month = month)]
  d <- d[dm1, on = .(cell)]
  dd <- dd[dm1, on = .(cell)]
  dd1 <- dd[month == end_t1_month, .(cell, end_t1_mday = ceiling(((tot_gdd/3) - (cum_gdd - mtot_gdd))/gdd))]
  dd1[end_t1_mday == 0, end_t1_mday:= 1]
  d <- d[dd1, on = .(cell)]
  rm(dm1, dd1)

  dm2 <- dd[(cum_gdd - tot_gdd * 2/3) > 0]
  dm2 <- dm2[dm2[, .I[which.min(season_month)], by = .(cell)]$V1, .(cell, end_t2_month = month)]
  d <- d[dm2, on = .(cell)]
  dd <- dd[dm2, on = .(cell)]
  dd2 <- dd[month == end_t2_month, .(cell, end_t2_mday = ceiling(((tot_gdd * 2/3) - (cum_gdd - mtot_gdd))/gdd))]
  dd2[end_t2_mday == 0, end_t2_mday:= 1]
  d <- d[dd2, on = .(cell)]
  rm(dm2, dd2)
  rm(dd)
  invisible(gc())

  ## Precipitation per third ----------
  d[, (as.character(1:12)):= extract(prec, cell)]
  stopifnot(d[, !any(is.na(`1`))])

  ### First ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "prec")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (plant_month < end_t1_month & (month >= plant_month & month <= end_t1_month)) |
      (plant_month > end_t1_month & (month >= plant_month | month <= end_t1_month)) |
      (plant_month == end_t1_month)
  ]
  dd <- dd[dm, on = .(month)]

  dd[month != plant_month & month != end_t1_month, mtot_prec:= prec]
  dd[month == plant_month & month != end_t1_month, mtot_prec:= round((prec/days_in_month) * (days_in_month - plant_mday))]
  dd[month != plant_month & month == end_t1_month, mtot_prec:= round((prec/days_in_month) * end_t1_mday)]
  dd[month == plant_month & month == end_t1_month, mtot_prec:= round((prec/days_in_month) * ((days_in_month - plant_mday) + end_t1_mday))]

  dt <- dd[, .(prec_t1 = sum(mtot_prec)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$prec_t1
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_prec_1.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_prec_1"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Second ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "prec")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t1_month < end_t2_month & (month >= end_t1_month & month <= end_t2_month)) |
      (end_t1_month > end_t2_month & (month >= end_t1_month | month <= end_t2_month)) |
      (end_t1_month == end_t2_month)
  ]
  dd <- dd[dm, on = .(month)]

  dd[month != end_t1_month & month != end_t2_month, mtot_prec:= prec]
  dd[month == end_t1_month & month != end_t2_month, mtot_prec:= round((prec/days_in_month) * (days_in_month - end_t1_mday))]
  dd[month != end_t1_month & month == end_t2_month, mtot_prec:= round((prec/days_in_month) * end_t2_mday)]
  dd[month == end_t1_month & month == end_t2_month, mtot_prec:= round((prec/days_in_month) * ((days_in_month - end_t1_mday) + end_t2_mday))]

  dt <- dd[, .(prec_t2 = sum(mtot_prec)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$prec_t2
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_prec_2.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_prec_2"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Third ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "prec")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t2_month < harvest_month & (month >= end_t2_month & month <= harvest_month)) |
      (end_t2_month > harvest_month & (month >= end_t2_month | month <= harvest_month)) |
      (end_t2_month == harvest_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t2_month & month != harvest_month, mtot_prec:= prec]
  dd[month == end_t2_month & month != harvest_month, mtot_prec:= round((prec/days_in_month) * (days_in_month - end_t2_mday))]
  dd[month != end_t2_month & month == harvest_month, mtot_prec:= round((prec/days_in_month) * harvest_mday)]
  dd[month == end_t2_month & month == harvest_month, mtot_prec:= round((prec/days_in_month) * ((days_in_month - end_t2_mday) + harvest_mday))]

  dt <- dd[, .(prec_t3 = sum(mtot_prec)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$prec_t3
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_prec_3.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_prec_3"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ## Solar Radiation per third ----------
  d[, (as.character(1:12)):= extract(srad, cell)]
  stopifnot(d[, !any(is.na(`1`))])

  ### First ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "srad")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (plant_month < end_t1_month & (month >= plant_month & month <= end_t1_month)) |
      (plant_month > end_t1_month & (month >= plant_month | month <= end_t1_month)) |
      (plant_month == end_t1_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != plant_month & month != end_t1_month, mtot_srad:= srad]
  dd[month == plant_month & month != end_t1_month, mtot_srad:= round((srad/days_in_month) * (days_in_month - plant_mday))]
  dd[month != plant_month & month == end_t1_month, mtot_srad:= round((srad/days_in_month) * end_t1_mday)]
  dd[month == plant_month & month == end_t1_month, mtot_srad:= round((srad/days_in_month) * ((days_in_month - plant_mday) + end_t1_mday))]

  dt <- dd[, .(srad_t1 = sum(mtot_srad)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$srad_t1
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_srad_1.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_srad_1"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Second ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "srad")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t1_month < end_t2_month & (month >= end_t1_month & month <= end_t2_month)) |
      (end_t1_month > end_t2_month & (month >= end_t1_month | month <= end_t2_month)) |
      (end_t1_month == end_t2_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t1_month & month != end_t2_month, mtot_srad:= srad]
  dd[month == end_t1_month & month != end_t2_month, mtot_srad:= round((srad/days_in_month) * (days_in_month - end_t1_mday))]
  dd[month != end_t1_month & month == end_t2_month, mtot_srad:= round((srad/days_in_month) * end_t2_mday)]
  dd[month == end_t1_month & month == end_t2_month, mtot_srad:= round((srad/days_in_month) * ((days_in_month - end_t1_mday) + end_t2_mday))]

  dt <- dd[, .(srad_t2 = sum(mtot_srad)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$srad_t2
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_srad_2.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_srad_2"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Third ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "srad")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t2_month < harvest_month & (month >= end_t2_month & month <= harvest_month)) |
      (end_t2_month > harvest_month & (month >= end_t2_month | month <= harvest_month)) |
      (end_t2_month == harvest_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t2_month & month != harvest_month, mtot_srad:= srad]
  dd[month == end_t2_month & month != harvest_month, mtot_srad:= round((srad/days_in_month) * (days_in_month - end_t2_mday))]
  dd[month != end_t2_month & month == harvest_month, mtot_srad:= round((srad/days_in_month) * harvest_mday)]
  dd[month == end_t2_month & month == harvest_month, mtot_srad:= round((srad/days_in_month) * ((days_in_month - end_t2_mday) + harvest_mday))]

  dt <- dd[, .(srad_t3 = sum(mtot_srad)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$srad_t3
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_srad_3.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_srad_3"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ## Tavg per third ----------
  d[, (as.character(1:12)):= extract(tavg, cell)]
  stopifnot(d[, !any(is.na(`1`))])

  ### First ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "tavg")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (plant_month < end_t1_month & (month >= plant_month & month <= end_t1_month)) |
      (plant_month > end_t1_month & (month >= plant_month | month <= end_t1_month)) |
      (plant_month == end_t1_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != plant_month & month != end_t1_month, mtot_days:= days_in_month]
  dd[month == plant_month & month != end_t1_month, mtot_days:= days_in_month - plant_mday]
  dd[month != plant_month & month == end_t1_month, mtot_days:= end_t1_mday]
  dd[month == plant_month & month == end_t1_month, mtot_days:= (days_in_month - plant_mday) + end_t1_mday]

  dd[, tot_days:= sum(mtot_days), by = .(cell)]
  dd[, w_tavg:= tavg * mtot_days/tot_days]

  dt <- dd[, .(tavg_t1 = sum(w_tavg)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$tavg_t1
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_tavg_1.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_tavg_1"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Second ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "tavg")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t1_month < end_t2_month & (month >= end_t1_month & month <= end_t2_month)) |
      (end_t1_month > end_t2_month & (month >= end_t1_month | month <= end_t2_month)) |
      (end_t1_month == end_t2_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t1_month & month != end_t2_month, mtot_days:= days_in_month]
  dd[month == end_t1_month & month != end_t2_month, mtot_days:= days_in_month - end_t1_mday]
  dd[month != end_t1_month & month == end_t2_month, mtot_days:= end_t2_mday]
  dd[month == end_t1_month & month == end_t2_month, mtot_days:= (days_in_month - end_t1_mday) + end_t2_mday]

  dd[, tot_days:= sum(mtot_days), by = .(cell)]
  dd[, w_tavg:= tavg * mtot_days/tot_days]

  dt <- dd[, .(tavg_t2 = sum(w_tavg)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$tavg_t2
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_tavg_2.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_tavg_2"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Third ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "tavg")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t2_month < harvest_month & (month >= end_t2_month & month <= harvest_month)) |
      (end_t2_month > harvest_month & (month >= end_t2_month | month <= harvest_month)) |
      (end_t2_month == harvest_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t2_month & month != harvest_month, mtot_days:= days_in_month]
  dd[month == end_t2_month & month != harvest_month, mtot_days:= days_in_month - end_t2_mday]
  dd[month != end_t2_month & month == harvest_month, mtot_days:= harvest_mday]
  dd[month == end_t2_month & month == harvest_month, mtot_days:= (days_in_month - end_t2_mday) + harvest_mday]

  dd[, tot_days:= sum(mtot_days), by = .(cell)]
  dd[, w_tavg:= tavg * mtot_days/tot_days]

  dt <- dd[, .(tavg_t3 = sum(w_tavg)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$tavg_t3
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/", crops[i], "_tavg_3.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0(substr(crops[i],1,4), "_tavg_3"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())
}



# Rice -----------

r_plan <- rast("data/crop_calendar/30sec/rice_plant_day.tif")
r_harv <- rast("data/crop_calendar/30sec/rice_harvest_day.tif")

## cells with crop area  > 0.01% ----------
spam_i <- subset(spam, "RICE_A")
stopifnot(compareGeom(spam_i, tavg))


for(i in seq_len(nlyr(r_plan))) {

  d <- data.table(cell = cells(spam_i))
  d[, prop:= extract(spam_i, cell)]
  d <- d[prop > 0.0001]
  d[, prop:= NULL]

  ## planting and harvest dates -------
  # (days, month, etc.)
  d[, plant_day:= extract(r_plan[[i]], cell)]
  d[, harvest_day:= extract(r_harv[[i]], cell)]
  d <- d[!is.na(plant_day) & !is.na(harvest_day)]

  d[, plant_date:= as.Date("2022-12-31") + plant_day]
  d[, harvest_date:= as.Date("2022-12-31") + harvest_day]

  d[, plant_month:= month(plant_date)]
  d[, harvest_month:= month(harvest_date)]

  d[, plant_mday:= mday(plant_date)]
  d[, harvest_mday:= mday(harvest_date)]

  d[, plant_date:= NULL]
  d[, harvest_date:= NULL]

  ## compute total GDD  ----------
  d[, (as.character(1:12)):= extract(gdd, cell)]
  d <- d[!is.na(`1`)]
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "gdd")
  d[, (as.character(1:12)):= NULL]

  dd[, month:= as.numeric(month)]
  # subset months within cropping season
  dd <- dd[
    (plant_month < harvest_month & (month >= plant_month & month <= harvest_month)) |
      (plant_month > harvest_month & (month >= plant_month | month <= harvest_month)) |
      (plant_month == harvest_month)
  ]
  # month of the season
  dd[plant_month < harvest_month, season_month:= month - plant_month]
  dd[plant_month > harvest_month & month >= plant_month, season_month:= month - plant_month]
  dd[plant_month > harvest_month & month < plant_month, season_month:= month + (12 - plant_month)]
  dd[plant_month == harvest_month & month >= plant_month, season_month:= month - plant_month]
  dd[plant_month == harvest_month & month < plant_month, season_month:= month + (12 - plant_month)]
  setorder(dd, cell, season_month)
  #dd[,.N, by = .(season_month)]

  # the GDD the months adds to the total depends on how many days is the crop in that month
  dd <- dd[dm, on = .(month)]
  dd[month != plant_month & month != harvest_month, mtot_gdd:= gdd * days_in_month]
  dd[month == plant_month & month != harvest_month, mtot_gdd:= gdd * (days_in_month - plant_mday)]
  dd[month != plant_month & month == harvest_month, mtot_gdd:= gdd * harvest_mday]
  dd[month == plant_month & month == harvest_month, mtot_gdd:= gdd * ((days_in_month - plant_mday) + harvest_mday)]
  dd[, tot_gdd:= sum(mtot_gdd), by = .(cell)]


  ## Split GDD season in 3 ----------
  setorder(dd, cell, season_month)
  dd[, cum_gdd:= cumsum(mtot_gdd), by = .(cell)]

  dm1 <- dd[(cum_gdd - tot_gdd/3) > 0]
  dm1 <- dm1[dm1[, .I[which.min(season_month)], by = .(cell)]$V1, .(cell, end_t1_month = month)]
  d <- d[dm1, on = .(cell)]
  dd <- dd[dm1, on = .(cell)]
  dd1 <- dd[month == end_t1_month, .(cell, end_t1_mday = ceiling(((tot_gdd/3) - (cum_gdd - mtot_gdd))/gdd))]
  dd1[end_t1_mday == 0, end_t1_mday:= 1]
  d <- d[dd1, on = .(cell)]
  rm(dm1, dd1)

  dm2 <- dd[(cum_gdd - tot_gdd * 2/3) > 0]
  dm2 <- dm2[dm2[, .I[which.min(season_month)], by = .(cell)]$V1, .(cell, end_t2_month = month)]
  d <- d[dm2, on = .(cell)]
  dd <- dd[dm2, on = .(cell)]
  dd2 <- dd[month == end_t2_month, .(cell, end_t2_mday = ceiling(((tot_gdd * 2/3) - (cum_gdd - mtot_gdd))/gdd))]
  dd2[end_t2_mday == 0, end_t2_mday:= 1]
  d <- d[dd2, on = .(cell)]
  rm(dm2, dd2)
  rm(dd)
  invisible(gc())

  ## Precipitation per third ----------
  d[, (as.character(1:12)):= extract(prec, cell)]
  stopifnot(d[, !any(is.na(`1`)) | !any(is.na(cell))])

  ### First ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "prec")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (plant_month < end_t1_month & (month >= plant_month & month <= end_t1_month)) |
      (plant_month > end_t1_month & (month >= plant_month | month <= end_t1_month)) |
      (plant_month == end_t1_month)
  ]
  dd <- dd[dm, on = .(month)]

  dd[month != plant_month & month != end_t1_month, mtot_prec:= prec]
  dd[month == plant_month & month != end_t1_month, mtot_prec:= round((prec/days_in_month) * (days_in_month - plant_mday))]
  dd[month != plant_month & month == end_t1_month, mtot_prec:= round((prec/days_in_month) * end_t1_mday)]
  dd[month == plant_month & month == end_t1_month, mtot_prec:= round((prec/days_in_month) * ((days_in_month - plant_mday) + end_t1_mday))]

  dt <- dd[!is.na(cell), .(prec_t1 = sum(mtot_prec)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$prec_t1
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_prec_1.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_prec_1"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Second ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "prec")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t1_month < end_t2_month & (month >= end_t1_month & month <= end_t2_month)) |
      (end_t1_month > end_t2_month & (month >= end_t1_month | month <= end_t2_month)) |
      (end_t1_month == end_t2_month)
  ]
  dd <- dd[dm, on = .(month)]

  dd[month != end_t1_month & month != end_t2_month, mtot_prec:= prec]
  dd[month == end_t1_month & month != end_t2_month, mtot_prec:= round((prec/days_in_month) * (days_in_month - end_t1_mday))]
  dd[month != end_t1_month & month == end_t2_month, mtot_prec:= round((prec/days_in_month) * end_t2_mday)]
  dd[month == end_t1_month & month == end_t2_month, mtot_prec:= round((prec/days_in_month) * ((days_in_month - end_t1_mday) + end_t2_mday))]

  dt <- dd[!is.na(cell), .(prec_t2 = sum(mtot_prec)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$prec_t2
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_prec_2.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_prec_2"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Third ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "prec")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t2_month < harvest_month & (month >= end_t2_month & month <= harvest_month)) |
      (end_t2_month > harvest_month & (month >= end_t2_month | month <= harvest_month)) |
      (end_t2_month == harvest_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t2_month & month != harvest_month, mtot_prec:= prec]
  dd[month == end_t2_month & month != harvest_month, mtot_prec:= round((prec/days_in_month) * (days_in_month - end_t2_mday))]
  dd[month != end_t2_month & month == harvest_month, mtot_prec:= round((prec/days_in_month) * harvest_mday)]
  dd[month == end_t2_month & month == harvest_month, mtot_prec:= round((prec/days_in_month) * ((days_in_month - end_t2_mday) + harvest_mday))]

  dt <- dd[!is.na(cell), .(prec_t3 = sum(mtot_prec)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$prec_t3
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_prec_3.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_prec_3"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ## Solar Radiation per third ----------
  d[, (as.character(1:12)):= extract(srad, cell)]
  stopifnot(d[, !any(is.na(`1`))])

  ### First ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "srad")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (plant_month < end_t1_month & (month >= plant_month & month <= end_t1_month)) |
      (plant_month > end_t1_month & (month >= plant_month | month <= end_t1_month)) |
      (plant_month == end_t1_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != plant_month & month != end_t1_month, mtot_srad:= srad]
  dd[month == plant_month & month != end_t1_month, mtot_srad:= round((srad/days_in_month) * (days_in_month - plant_mday))]
  dd[month != plant_month & month == end_t1_month, mtot_srad:= round((srad/days_in_month) * end_t1_mday)]
  dd[month == plant_month & month == end_t1_month, mtot_srad:= round((srad/days_in_month) * ((days_in_month - plant_mday) + end_t1_mday))]

  dt <- dd[!is.na(cell), .(srad_t1 = sum(mtot_srad)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$srad_t1
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_srad_1.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_srad_1"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Second ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "srad")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t1_month < end_t2_month & (month >= end_t1_month & month <= end_t2_month)) |
      (end_t1_month > end_t2_month & (month >= end_t1_month | month <= end_t2_month)) |
      (end_t1_month == end_t2_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t1_month & month != end_t2_month, mtot_srad:= srad]
  dd[month == end_t1_month & month != end_t2_month, mtot_srad:= round((srad/days_in_month) * (days_in_month - end_t1_mday))]
  dd[month != end_t1_month & month == end_t2_month, mtot_srad:= round((srad/days_in_month) * end_t2_mday)]
  dd[month == end_t1_month & month == end_t2_month, mtot_srad:= round((srad/days_in_month) * ((days_in_month - end_t1_mday) + end_t2_mday))]

  dt <- dd[!is.na(cell), .(srad_t2 = sum(mtot_srad)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$srad_t2
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_srad_2.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_srad_2"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Third ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "srad")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t2_month < harvest_month & (month >= end_t2_month & month <= harvest_month)) |
      (end_t2_month > harvest_month & (month >= end_t2_month | month <= harvest_month)) |
      (end_t2_month == harvest_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t2_month & month != harvest_month, mtot_srad:= srad]
  dd[month == end_t2_month & month != harvest_month, mtot_srad:= round((srad/days_in_month) * (days_in_month - end_t2_mday))]
  dd[month != end_t2_month & month == harvest_month, mtot_srad:= round((srad/days_in_month) * harvest_mday)]
  dd[month == end_t2_month & month == harvest_month, mtot_srad:= round((srad/days_in_month) * ((days_in_month - end_t2_mday) + harvest_mday))]

  dt <- dd[!is.na(cell), .(srad_t3 = sum(mtot_srad)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$srad_t3
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_srad_3.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_srad_3"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ## Tavg per third ----------
  d[, (as.character(1:12)):= extract(tavg, cell)]
  stopifnot(d[, !any(is.na(`1`))])

  ### First ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "tavg")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (plant_month < end_t1_month & (month >= plant_month & month <= end_t1_month)) |
      (plant_month > end_t1_month & (month >= plant_month | month <= end_t1_month)) |
      (plant_month == end_t1_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != plant_month & month != end_t1_month, mtot_days:= days_in_month]
  dd[month == plant_month & month != end_t1_month, mtot_days:= days_in_month - plant_mday]
  dd[month != plant_month & month == end_t1_month, mtot_days:= end_t1_mday]
  dd[month == plant_month & month == end_t1_month, mtot_days:= (days_in_month - plant_mday) + end_t1_mday]

  dd[, tot_days:= sum(mtot_days), by = .(cell)]
  dd[, w_tavg:= tavg * mtot_days/tot_days]

  dt <- dd[!is.na(cell), .(tavg_t1 = sum(w_tavg)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$tavg_t1
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_tavg_1.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_tavg_1"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Second ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "tavg")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t1_month < end_t2_month & (month >= end_t1_month & month <= end_t2_month)) |
      (end_t1_month > end_t2_month & (month >= end_t1_month | month <= end_t2_month)) |
      (end_t1_month == end_t2_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t1_month & month != end_t2_month, mtot_days:= days_in_month]
  dd[month == end_t1_month & month != end_t2_month, mtot_days:= days_in_month - end_t1_mday]
  dd[month != end_t1_month & month == end_t2_month, mtot_days:= end_t2_mday]
  dd[month == end_t1_month & month == end_t2_month, mtot_days:= (days_in_month - end_t1_mday) + end_t2_mday]

  dd[, tot_days:= sum(mtot_days), by = .(cell)]
  dd[, w_tavg:= tavg * mtot_days/tot_days]

  dt <- dd[!is.na(cell), .(tavg_t2 = sum(w_tavg)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$tavg_t2
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_tavg_2.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_tavg_2"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())

  ### Third ----------------
  dd <-  melt(d, measure.vars = as.character(1:12), variable.name = "month", value.name = "tavg")
  dd[, month:= as.numeric(month)]

  # subset months within cropping season
  dd <- dd[
    (end_t2_month < harvest_month & (month >= end_t2_month & month <= harvest_month)) |
      (end_t2_month > harvest_month & (month >= end_t2_month | month <= harvest_month)) |
      (end_t2_month == harvest_month)
  ]

  dd <- dd[dm, on = .(month)]

  dd[month != end_t2_month & month != harvest_month, mtot_days:= days_in_month]
  dd[month == end_t2_month & month != harvest_month, mtot_days:= days_in_month - end_t2_mday]
  dd[month != end_t2_month & month == harvest_month, mtot_days:= harvest_mday]
  dd[month == end_t2_month & month == harvest_month, mtot_days:= (days_in_month - end_t2_mday) + harvest_mday]

  dd[, tot_days:= sum(mtot_days), by = .(cell)]
  dd[, w_tavg:= tavg * mtot_days/tot_days]

  dt <- dd[!is.na(cell), .(tavg_t3 = sum(w_tavg)), by = .(cell)]

  rm(dd)
  gc()

  #### create raster -------
  r <- rast(spam_i)
  v <- rep(NA_integer_, ncell(r))
  v[dt$cell] <- dt$tavg_t3
  values(r) <- v
  writeRaster(
    r,
    filename = paste0("data/crop_specific/30sec/rice_s", i, "_tavg_3.tif"),
    overwrite = TRUE,
    wopt = list(names = paste0("rice_s", i, "_tavg_3"), filetype = "GTiff",
                gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
  )
  rm(v, r)
  invisible(gc())
}


## Average across seasons-----------------
bars <- c("prec", "tavg", "srad")

for(i in seq_along(bars)) {
  for(j in 1:3){
  fn <- list.files(
    path = "data/crop_specific/30sec/",
    pattern = paste0("rice_s\\d_", bars[i], "_", j),
    full.names = TRUE
  )
  r <- rast(fn)
  out_name <- paste0("rice_", bars[i], "_", j)
  app(r, mean, na.rm = TRUE,
      filename = paste0("data/crop_specific/30sec/", out_name, ".tif"),
      overwrite = TRUE,
      wopt = list(names = out_name, filetype = "GTiff",
                  gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6")))
  }
}
