# author: Fernando Aramburu Merlos
# date: 2022-08-01
# updated: 2022-09-27. Add new predictors based on WUR experience
# updated: 2023-09-05. Add new crop calendar info from Crop Monitor poject


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
library(ranger)


# Climate --------
## Worldclim
prec <- geodata::worldclim_global("prec", 5, "data/climate")
bioc <- geodata::worldclim_global("prec", 5, "data/climate")
names(prec) <- str_remove(names(prec), "wc2.1_5m_")
names(bioc) <- str_remove(names(bioc), "wc2.1_5m_")



# SPAM ------------
# to be used as model for other rasters
spam <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA|MAIZ|SOYB|RICE") |>
  str_subset("_A.tif$") |>
  rast()

names(spam) <- str_extract(names(spam), "WHEA|MAIZ|SOYB|RICE")
compareGeom(spam, prec)

countries <- geodata::world(path = "data/gadm")

# CROP CALENDAR ----------------------
dir.create("data/crop_calendar/5min", F, T)

# All crops sources:
# https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/
# https://cropmonitor.org/index.php/eodatatools/baseline-data/

v <- vect("data/crop_calendar/crop_monitor/EW_Calendars_distr_v1.0.shp")

# function to select main crop dates and fill with second crop when no first crop is data is available
f1 <- function(x, y){
  x[is.na(x)] <- y[is.na(x)]
  return(x)
}


## Wheat ------

### Crop Monitor -------
swh_plan_cm <- rasterize(v[v$crop == "Spring Wheat"], prec, field = "planting")
wwh_plan_cm <- rasterize(v[v$crop == "Winter Wheat"], prec, field = "planting")
swh_harv_cm <- rasterize(v[v$crop == "Spring Wheat"], prec, field = "harvest")
wwh_harv_cm <- rasterize(v[v$crop == "Winter Wheat"], prec, field = "harvest")

wh_plan_cm <- lapp(c(wwh_plan_cm, swh_plan_cm), f1)
wh_harv_cm <- lapp(c(wwh_harv_cm, swh_harv_cm), f1)

wh_leng_cm <- wh_harv_cm - wh_plan_cm
wh_leng_cm[wh_leng_cm < 0] <- wh_leng_cm[wh_leng_cm < 0] + 365


### Sacks ----
swh_sk <- rast("data/crop_calendar/sacks/Wheat.crop.calendar.nc")
wwh_sk <- rast("data/crop_calendar/sacks/Wheat.Winter.crop.calendar.nc")

wh_leng_sk <- lapp(c(wwh_sk$tot.days, swh_sk$tot.days), f1)
wh_plan_sk <- lapp(c(wwh_sk$plant, swh_sk$plant), f1)
wh_harv_sk <- lapp(c(wwh_sk$harvest, swh_sk$harvest), f1)


### Merge and fill ----------
wh_leng <- lapp(c(wh_leng_cm, wh_leng_sk), f1)
wh_plan <- lapp(c(wh_plan_cm, wh_plan_sk), f1)
wh_harv <- lapp(c(wh_harv_cm, wh_harv_sk), f1)
wh_plan <- ceiling(wh_plan)
wh_harv <- ceiling(wh_harv)

wh_gaps <- spam$WHEA > 0 & is.na(wh_leng)
wh_gaps[wh_gaps == 0] <- NA

dtrain <- data.table(cell = which(!is.na(values(wh_leng))))
set.seed(0)
dtrain <- dtrain[sample(.N, 4e4)]
dtrain[, (names(bioc)):= extract(bioc, cell)]
dtrain[, lat:= yFromCell(wh_leng, cell)]
dtrain[, lon:= xFromCell(wh_leng, cell)]
preds <- c(names(bioc), "lat", "lon")

dtrain <- dtrain[complete.cases(dtrain)]
dtrain[, leng:= extract(wh_leng, cell)]
dtrain[, plan:= extract(wh_plan, cell)]

m_leng <- ranger(x = dtrain[, ..preds], y = dtrain$leng)
m_leng$r.squared

m_plan <- ranger(x = dtrain[, ..preds], y = dtrain$plan)
m_plan$r.squared

d2fill <- data.table(cell = which(values(wh_gaps) == 1))
d2fill[, (names(bioc)):= extract(bioc, cell)]
d2fill[, lat:= yFromCell(wh_leng, cell)]
d2fill[, lon:= xFromCell(wh_leng, cell)]
d2fill[!complete.cases(d2fill)]
d2fill[, leng:= predict(m_leng, .SD)$predictions, .SDcols = preds]
d2fill[, plan:= predict(m_plan, .SD)$predictions, .SDcols = preds]
d2fill[, harv:= plan + leng]
for(j in c("leng", "plan", "harv")) d2fill[, (j):= round(get(j))]
d2fill[harv > 365, harv:= harv - 365]

wh_leng[d2fill$cell] <- round(d2fill$leng)
wh_plan[d2fill$cell] <- round(d2fill$plan)
wh_harv[d2fill$cell] <- round(d2fill$harv)


writeRaster(
  wh_leng,
  filename = "data/crop_calendar/5min/wheat_nday.tif",
  overwrite = TRUE,
  wopt = list(names = "whea_nday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

writeRaster(
  wh_plan,
  filename = "data/crop_calendar/5min/wheat_plant_day.tif",
  overwrite = TRUE,
  wopt = list(names = "whea_pday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

writeRaster(
  wh_harv,
  filename = "data/crop_calendar/5min/wheat_harvest_day.tif",
  overwrite = TRUE,
  wopt = list(names = "whea_hday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)



## Maize --------

### Crop Monitor ---------
mz1_plan_cm <- rasterize(v[v$crop == "Maize 1"], prec, field = "planting")
mz2_plan_cm <- rasterize(v[v$crop == "Maize 2"], prec, field = "planting")
mz1_harv_cm <- rasterize(v[v$crop == "Maize 1"], prec, field = "harvest")
mz2_harv_cm <- rasterize(v[v$crop == "Maize 2"], prec, field = "harvest")

mz_plan_cm <- lapp(c(mz1_plan_cm, mz2_plan_cm), f1)
mz_harv_cm <- lapp(c(mz1_harv_cm, mz2_harv_cm), f1)

mz_leng_cm <- mz_harv_cm - mz_plan_cm
mz_leng_cm[mz_leng_cm < 0] <- mz_leng_cm[mz_leng_cm < 0] + 365

### Sacks --------
mz1_sk <- rast("data/crop_calendar/sacks/Maize.crop.calendar.nc")
mz2_sk <- rast("data/crop_calendar/sacks/Maize.2.crop.calendar.nc")


mz_leng_sk <- lapp(c(mz1_sk$tot.days, mz2_sk$tot.days), f1)
mz_plan_sk <- lapp(c(mz1_sk$plant, mz2_sk$plant), f1)
mz_harv_sk <- lapp(c(mz1_sk$harvest, mz2_sk$harvest), f1)

### Merge and fill ------------
mz_leng <- lapp(c(mz_leng_cm, mz_leng_sk), f1)
mz_plan <- lapp(c(mz_plan_cm, mz_plan_sk), f1)
mz_harv <- lapp(c(mz_harv_cm, mz_harv_sk), f1)
mz_plan <- ceiling(mz_plan)
mz_harv <- ceiling(mz_harv)

mz_gaps <- spam$MAIZ > 0 & is.na(mz_leng)
mz_gaps[mz_gaps == 0] <- NA


dtrain <- data.table(cell = which(!is.na(values(mz_leng))))
set.seed(0)
dtrain <- dtrain[sample(.N, 4e4)]
dtrain[, (names(bioc)):= extract(bioc, cell)]
dtrain[, lat:= yFromCell(mz_leng, cell)]
dtrain[, lon:= xFromCell(mz_leng, cell)]
preds <- c(names(bioc), "lat", "lon")

dtrain <- dtrain[complete.cases(dtrain)]
dtrain[, leng:= extract(mz_leng, cell)]
dtrain[, plan:= extract(mz_plan, cell)]

m_leng <- ranger(x = dtrain[, ..preds], y = dtrain$leng)
m_leng$r.squared

m_plan <- ranger(x = dtrain[, ..preds], y = dtrain$plan)
m_plan$r.squared

d2fill <- data.table(cell = which(values(mz_gaps) == 1))
d2fill[, (names(bioc)):= extract(bioc, cell)]
d2fill[, lat:= yFromCell(mz_leng, cell)]
d2fill[, lon:= xFromCell(mz_leng, cell)]
na_pts <- d2fill[
  !complete.cases(d2fill),
  vect(cbind(lon, lat), crs = "+proj=longlat +datum=WGS84")
]
na_bz <- buffer(na_pts, 5e4)
na_bioc <- extract(bioc, na_bz, fun = "mean", na.rm = TRUE)[,-1] |> round()
d2fill[!complete.cases(d2fill), (names(bioc)):= na_bioc]


d2fill[, leng:= predict(m_leng, .SD)$predictions, .SDcols = preds]
d2fill[, plan:= predict(m_plan, .SD)$predictions, .SDcols = preds]
d2fill[, harv:= plan + leng]
for(j in c("leng", "plan", "harv")) d2fill[, (j):= round(get(j))]
d2fill[harv > 365, harv:= harv - 365]

mz_leng[d2fill$cell] <- round(d2fill$leng)
mz_plan[d2fill$cell] <- round(d2fill$plan)
mz_harv[d2fill$cell] <- round(d2fill$harv)
mz_harv[mz_harv == 0] <- 1

writeRaster(
  mz_leng,
  filename = "data/crop_calendar/5min/maize_nday.tif",
  overwrite = TRUE,
  wopt = list(names = "maiz_nday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

writeRaster(
  mz_plan,
  filename = "data/crop_calendar/5min/maize_plant_day.tif",
  overwrite = TRUE,
  wopt = list(names = "maiz_pday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

writeRaster(
  mz_harv,
  filename = "data/crop_calendar/5min/maize_harvest_day.tif",
  overwrite = TRUE,
  wopt = list(names = "maiz_hday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)


## Soybean ------------
sy <- rast("data/crop_calendar/sacks/Soybeans.crop.calendar.nc")

sy_leng <- sy$tot.days
sy_plan <- sy$plant
sy_harv <- sy$harvest

sy_gaps <- spam$SOYB > 0 & is.na(sy_leng)
sy_gaps[sy_gaps == 0] <- NA

dtrain <- data.table(cell = which(!is.na(values(sy_leng))))
set.seed(0)
dtrain <- dtrain[sample(.N, 4e4)]
dtrain[, (names(bioc)):= extract(bioc, cell)]
dtrain[, lat:= yFromCell(sy_leng, cell)]
dtrain[, lon:= xFromCell(sy_leng, cell)]
preds <- c(names(bioc), "lat", "lon")

dtrain <- dtrain[complete.cases(dtrain)]
dtrain[, leng:= extract(sy_leng, cell)]
dtrain[, plan:= extract(sy_plan, cell)]

m_leng <- ranger(x = dtrain[, ..preds], y = dtrain$leng)
m_leng$r.squared

m_plan <- ranger(x = dtrain[, ..preds], y = dtrain$plan)
m_plan$r.squared

d2fill <- data.table(cell = which(values(sy_gaps) == 1))
d2fill[, (names(bioc)):= extract(bioc, cell)]
d2fill[, lat:= yFromCell(sy_leng, cell)]
d2fill[, lon:= xFromCell(sy_leng, cell)]
na_pts <- d2fill[
  !complete.cases(d2fill),
  vect(cbind(lon, lat), crs = "+proj=longlat +datum=WGS84")
]
na_bz <- buffer(na_pts, 5e4)
na_bioc <- extract(bioc, na_bz, fun = "mean", na.rm = TRUE)[,-1] |> round()
d2fill[!complete.cases(d2fill), (names(bioc)):= na_bioc]

d2fill[, leng:= predict(m_leng, .SD)$predictions, .SDcols = preds]
d2fill[, plan:= predict(m_plan, .SD)$predictions, .SDcols = preds]
d2fill[, harv:= plan + leng]
for(j in c("leng", "plan", "harv")) d2fill[, (j):= round(get(j))]
d2fill[harv > 365, harv:= harv - 365]

sy_leng[d2fill$cell] <- round(d2fill$leng)
sy_plan[d2fill$cell] <- round(d2fill$plan)
sy_harv[d2fill$cell] <- round(d2fill$harv)


writeRaster(
  sy_leng,
  filename = "data/crop_calendar/5min/soybean_nday.tif",
  overwrite = TRUE,
  wopt = list(names = "soyb_nday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

writeRaster(
  sy_plan,
  filename = "data/crop_calendar/5min/soybean_plant_day.tif",
  overwrite = TRUE,
  wopt = list(names = "soyb_pday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

writeRaster(
  sy_harv,
  filename = "data/crop_calendar/5min/soybean_harvest_day.tif",
  overwrite = TRUE,
  wopt = list(names = "soyb_hday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)




## Rice ---------

# Rice crop calendar data was downloaded from
# https://dx.doi.org/10.7910/DVN/JE6R2R

### prepare data ------

# for rice I will take the average length and prec of all cropping seasons
# (rather than prioritizing the first/main one)

rice_calendar <- vect("data/crop_calendar/rice/RiceCalendar_v1/RiceCalendar_v1.shp")
head(rice_calendar)

dt <- data.frame(rice_calendar)
setDT(dt)

# for some reason the data.frame has 0 instead of NA when no second or third crop is grown
for(j in str_subset(names(dt), "PLANT|HARV|MATUR")) {
  set(dt, which(dt[[j]] == 0), j, NA)
}
values(rice_calendar) <- dt

rc_leng1 <- rasterize(rice_calendar, prec, field = "MATURITY1")
rc_leng2 <- rasterize(rice_calendar, prec, field = "MATURITY2")
rc_leng3 <- rasterize(rice_calendar, prec, field = "MATURITY3")

rc_plan1 <- rasterize(rice_calendar, prec, field = "PLANT_PK1")
rc_plan2 <- rasterize(rice_calendar, prec, field = "PLANT_PK2")
rc_plan3 <- rasterize(rice_calendar, prec, field = "PLANT_PK3")

rc_harv1 <- rasterize(rice_calendar, prec, field = "HARV_PK1")
rc_harv2 <- rasterize(rice_calendar, prec, field = "HARV_PK2")
rc_harv3 <- rasterize(rice_calendar, prec, field = "HARV_PK3")


# fill cells with missing crop calendar data but rice area > 0 with nearest neighbor

# most of these are because of inconsistencies in border limits between spam and rice_calendar
rc_gaps <- spam$RICE > 0 & is.na(rc_plan1) & is.na(rc_plan2) & is.na(rc_plan3)
rc_gaps[rc_gaps == 0] <- NA

cells_to_fill <- which(values(rc_gaps) == 1)


# function to fill specific NA cells using focal function
# a little slow, but the best I have
fill <- function(r, cells, pol, coln) {
  pts <- xyFromCell(r, cells) |> vect(crs = crs(r))
  # the following should have argument centroids = FALSE
  # but still doesn't work well in terra 1.6.17
  # see https://stackoverflow.com/questions/72561812/return-polygon-nearest-to-a-point-using-terra-in-r
  n <- nearest(pts, pol)
  v <- pol[[coln]][,1][n$to_id]
  r[cells] <- v
  return(r)
}

# the following step takes a while (~ 20 minutes in my PC)
rc_leng1 <- fill(r = rc_leng1, cells = cells_to_fill, pol = rice_calendar, coln = "MATURITY1")
rc_leng2 <- fill(r = rc_leng2, cells = cells_to_fill, pol = rice_calendar, coln = "MATURITY2")
rc_leng3 <- fill(r = rc_leng3, cells = cells_to_fill, pol = rice_calendar, coln = "MATURITY3")
rc_plan1 <- fill(r = rc_plan1, cells = cells_to_fill, pol = rice_calendar, coln = "PLANT_PK1")
rc_plan2 <- fill(r = rc_plan2, cells = cells_to_fill, pol = rice_calendar, coln = "PLANT_PK2")
rc_plan3 <- fill(r = rc_plan3, cells = cells_to_fill, pol = rice_calendar, coln = "PLANT_PK3")
rc_harv1 <- fill(r = rc_harv1, cells = cells_to_fill, pol = rice_calendar, coln = "HARV_PK1")
rc_harv2 <- fill(r = rc_harv2, cells = cells_to_fill, pol = rice_calendar, coln = "HARV_PK2")
rc_harv3 <- fill(r = rc_harv3, cells = cells_to_fill, pol = rice_calendar, coln = "HARV_PK3")

writeRaster(
  c(rc_plan1, rc_plan2, rc_plan3),
  filename = "data/crop_calendar/5min/rice_plant_day.tif",
  overwrite = TRUE,
  wopt = list(names = paste0("rice_pday_",1:3), filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

writeRaster(
  c(rc_harv1, rc_harv2, rc_harv3),
  filename = "data/crop_calendar/5min/rice_harvest_day.tif",
  overwrite = TRUE,
  wopt = list(names = paste0("rice_hday_",1:3), filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

writeRaster(
  c(rc_leng1, rc_leng2, rc_leng3),
  filename = "data/crop_calendar/5min/rice_length.tif",
  overwrite = TRUE,
  wopt = list(names = paste0("rice_nday_",1:3), filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)


### crop length ----------------

app(
  c(rc_leng1, rc_leng2, rc_leng3),
  fun = 'max', na.rm = TRUE,
  filename = "data/crop_calendar/5min/rice_nday.tif",
  overwrite = TRUE,
  wopt = list(names = "rice_nday", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)

# crop intensity -------------

app(
  c(rc_leng1, rc_leng2, rc_leng3),
  fun = function(x) rowSums(!is.na(x)),
  filename = "data/crop_calendar/5min/rice_ints.tif",
  overwrite = TRUE,
  wopt = list(names = "rice_ints", filetype = "GTiff",
              gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6"))
)
