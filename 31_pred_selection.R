# author: Fernando Aramburu Merlos
# date: 2022-09-29 new pred selection after adding WUR variables


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(stringr)
library(terra)

# DON'T RUN ------------
#
# # functions ------------
# extract_buf <- function(lon, lat, r, fun = "mean") {
#   pts <- vect(cbind(lon, lat), crs = "+proj=longlat +datum=WGS84")
#   bz <- buffer(pts, 1e5)
#   extract(r, bz, fun, na.rm = TRUE)[,-1]
# }
#
#
# # yield data -----------
# ws <- fread("data/API/Y_ws_area.csv")
# ws <- ws[crop %like% "wheat|maize|rice",]
# ws_pos <- fread("data/API/ws.csv")
# ws <- ws_pos[, .(station_id, longitude, latitude)][ws, on = "station_id"]
# ws[station_id == "196000004", `:=`(longitude = 32.943572, latitude = 34.636102)]
#
#
# # climate data ----------
# clim <- "data/climate/wc2.1_30s/*.tif" |>
#   Sys.glob() |>
#   str_subset("(?<!bio)_\\d+.tif$", negate = TRUE) |>
#   str_subset("PET.tif$", negate = TRUE) |>
#   rast()
#
# names(clim) <- names(clim) |>
#   str_remove("wc2.1_30s_")
#
# # crop calendar data ----------
# ccal <- "data/crop_calendar/*.tif" |>
#   Sys.glob() |>
#   str_subset("prec|nday|srad|tavg|ints") |>
#   rast()
#
#
# ## Discard correlated --------------------
#
# d <- ws[, .(lon = longitude, lat = latitude)]
# d[, (names(clim)):= extract(clim, cbind(lon, lat))]
# d[, (names(ccal)):= extract_buf(lon, lat, ccal)]
# d[, lat_abs:= abs(lat)]
# summary(d)
#
# crops <- c("maiz", "whea", "rice")
#
# # for(i in seq_along(crops)) {
# #   preds <- c(names(clim), str_subset(names(ccal), crops[i]), "lat_abs")
# #   corr <- cor(d[, ..preds], use = "pairwise.complete.obs")
# #   corrplot::corrplot(corr, method = "number", order = "hclust", number.cex = 0.5)
# # }
#

# selected vars:
sel_vars <- rbind(
  c("GDD",         "bioclim_temp", "data/climate/wc2.1_30s/GDD.tif",             "growing degree days"),
  c("bio_4",       "bioclim_temp", "data/climate/wc2.1_30s/wc2.1_30s_bio_4.tif",  "temp seasonality"),
  c("bio_2",       "bioclim_temp", "data/climate/wc2.1_30s/wc2.1_30s_bio_2.tif",  "mean diurnal range"),
  c("bio_5",       "bioclim_temp", "data/climate/wc2.1_30s/wc2.1_30s_bio_5.tif",  "max temp warmeest month"),
  c("bio_8",       "bioclim_temp", "data/climate/wc2.1_30s/wc2.1_30s_bio_8.tif",  "mean temp wettest month"),
  c("AIann",       "bioclim_prec", "data/climate/wc2.1_30s/AIann.tif",            "aridity index (annual)"),
  c("bio_15",      "bioclim_prec", "data/climate/wc2.1_30s/wc2.1_30s_bio_15.tif", "prec seasonality"),
  c("bio_13",      "bioclim_prec", "data/climate/wc2.1_30s/wc2.1_30s_bio_13.tif", "prec wettest month"),
  c("bio_18",      "bioclim_prec", "data/climate/wc2.1_30s/wc2.1_30s_bio_18.tif", "prec warmest quarter"),
  c("bio_19",      "bioclim_prec", "data/climate/wc2.1_30s/wc2.1_30s_bio_19.tif", "prec coldest quarter"),
  c("bio_17",      "bioclim_prec", "data/climate/wc2.1_30s/wc2.1_30s_bio_17.tif", "prec driest quarter"),
  c("whea_nday",   "crop",         "data/crop_calendar/30sec/wheat_nday.tif",     "crop cycle length (wheat)"),
  c("maiz_nday",   "crop",         "data/crop_calendar/30sec/maize_nday.tif",     "crop cycle length (maize)"),
  c("rice_nday",   "crop",         "data/crop_calendar/30sec/rice_nday.tif",      "crop cycle length (srice)"),
  c("whea_srad_1", "crop",         "data/crop_specific/30sec/wheat_srad_1.tif",   "1st tercile crop SRAD (wheat)"),
  c("maiz_srad_1", "crop",         "data/crop_specific/30sec/maize_srad_1.tif",   "1st tercile crop SRAD (maize)"),
  c("rice_srad_1", "crop",         "data/crop_specific/30sec/rice_srad_1.tif",    "1st tercile crop SRAD (rice)"),
  c("whea_prec_1", "crop",         "data/crop_specific/30sec/wheat_prec_1.tif",   "1st tercile crop prec (wheat)"),
  c("maiz_prec_1", "crop",         "data/crop_specific/30sec/maize_prec_1.tif",   "1st tercile crop prec (maize)"),
  c("rice_prec_1", "crop",         "data/crop_specific/30sec/rice_prec_1.tif",    "1st tercile crop prec (rice)"),
  c("whea_tavg_1", "crop",         "data/crop_specific/30sec/wheat_tavg_1.tif",   "1st tercile crop tavg (wheat)"),
  c("maiz_tavg_1", "crop",         "data/crop_specific/30sec/maize_tavg_1.tif",   "1st tercile crop tavg (maize)"),
  c("rice_tavg_1", "crop",         "data/crop_specific/30sec/rice_tavg_1.tif",    "1st tercile crop tavg (rice)"),
  c("whea_srad_2", "crop",         "data/crop_specific/30sec/wheat_srad_2.tif",   "2nd tercile crop SRAD (wheat)"),
  c("maiz_srad_2", "crop",         "data/crop_specific/30sec/maize_srad_2.tif",   "2nd tercile crop SRAD (maize)"),
  c("rice_srad_2", "crop",         "data/crop_specific/30sec/rice_srad_2.tif",    "2nd tercile crop SRAD (rice)"),
  c("whea_prec_2", "crop",         "data/crop_specific/30sec/wheat_prec_2.tif",   "2nd tercile crop prec (wheat)"),
  c("maiz_prec_2", "crop",         "data/crop_specific/30sec/maize_prec_2.tif",   "2nd tercile crop prec (maize)"),
  c("rice_prec_2", "crop",         "data/crop_specific/30sec/rice_prec_2.tif",    "2nd tercile crop prec (rice)"),
  c("whea_tavg_2", "crop",         "data/crop_specific/30sec/wheat_tavg_2.tif",   "2nd tercile crop tavg (wheat)"),
  c("maiz_tavg_2", "crop",         "data/crop_specific/30sec/maize_tavg_2.tif",   "2nd tercile crop tavg (maize)"),
  c("rice_tavg_2", "crop",         "data/crop_specific/30sec/rice_tavg_2.tif",    "2nd tercile crop tavg (rice)"),
  c("whea_srad_3", "crop",         "data/crop_specific/30sec/wheat_srad_3.tif",   "3rd tercile crop SRAD (wheat)"),
  c("maiz_srad_3", "crop",         "data/crop_specific/30sec/maize_srad_3.tif",   "3rd tercile crop SRAD (maize)"),
  c("rice_srad_3", "crop",         "data/crop_specific/30sec/rice_srad_3.tif",    "3rd tercile crop SRAD (rice)"),
  c("whea_prec_3", "crop",         "data/crop_specific/30sec/wheat_prec_3.tif",   "3rd tercile crop prec (wheat)"),
  c("maiz_prec_3", "crop",         "data/crop_specific/30sec/maize_prec_3.tif",   "3rd tercile crop prec (maize)"),
  c("rice_prec_3", "crop",         "data/crop_specific/30sec/rice_prec_3.tif",    "3rd tercile crop prec (rice)"),
  c("whea_tavg_3", "crop",         "data/crop_specific/30sec/wheat_tavg_3.tif",   "3rd tercile crop tavg (wheat)"),
  c("maiz_tavg_3", "crop",         "data/crop_specific/30sec/maize_tavg_3.tif",   "3rd tercile crop tavg (maize)"),
  c("rice_tavg_3", "crop",         "data/crop_specific/30sec/rice_tavg_3.tif",    "3rd tercile crop tavg (rice)"),
  c("rice_ints",   "crop",         "data/crop_calendar/30sec/rice_ints.tif",      "number or rice crops per season (rice)"),
  c("lat_abs",     "lat_abs",      "data/absolute_latitude_30sec.tif",            "absolute latitude"),
  c("PAWHC_0_1m",   "soil",        "data/soil/SOILGRIDS/30sec/PAWHC_0-1m.tif",    "plant avail soil water hold cap (0-1m)"),
  c("PAWHC_1_2m",   "soil",        "data/soil/SOILGRIDS/30sec/PAWHC_1-2m.tif",    "plant avail soil water hold cap (1-2m)")
) |> as.data.frame()

names(sel_vars) <- c("var", "type", "path", "desc")
fwrite(sel_vars, "extrap_method/predictors.csv")

setDT(sel_vars)
sel_vars <- sel_vars[, lapply(.SD, str_replace_all, "30sec", "5min")]
sel_vars <- sel_vars[, lapply(.SD, str_replace_all, "30s", "5m")]

fwrite(sel_vars, "extrap_method/predictors_5min.csv")

# DON'T RUN

# r <- geodata::worldclim_global("tavg", 0.5, "data/climate") |>
#   rast(nlyr = 1)
# values(r) <- abs(yFromCell(r, cells(r)))
#
# writeRaster(r, "data/absolute_latitude.tif", overwrite = TRUE,
#             wopt = list(names = "lat_abs", filetype = "GTiff",
#                         gdal=c("COMPRESS=Deflate","PREDICTOR=1", "ZLEVEL=6")))
#
# # soil data ------------
#
# # comSRADe wheat results with different soils and select most appropriate
#
# ## preSRADe data -------
# sel_vars <- fread("extrap_method/predictors.csv")
# setDT(sel_vars)
# sel_vars <- sel_vars[!(type %like% "crop" & !var %like% "whea")]
#
# d <- ws[crop == "Rainfed wheat", .(longitude, latitude, yw)]
# setnames(d, c("longitude", "latitude"), c("lon", "lat"))
# pts <- vect(d, crs = "+proj=longlat +datum=WGS84")
#
# r_biocl_preds <- rast(sel_vars[type %like% "bioclim", path])
# d[, (sel_vars[type %like% "bioclim", var]):= extract(r_biocl_preds, pts)[,-1]]
#
# r_cropc_preds <- rast(sel_vars[type %like% "crop", path])
# d[, (sel_vars[type %like% "crop", var]):= extract_buf(d$lon, d$lat, r_cropc_preds)]
#
# d[, lat_abs:= abs(lat)]
#
# wh <- "data/SPAM/harvested_area_global_merged_v2/*WHEA*_A.tif" |> Sys.glob() |> rast()
# wh_mask <- wh > 0
# wh_mask[wh_mask == 0] <- NA
#
# r_soilgr_1m <- rast("data/soil/SOILGRIDS/PAWHC_0-1m.tif")
# r_soilgrids <- mask(r_soilgr_1m, wh_mask)
# r_soilgr_2m <- rast("data/soil/SOILGRIDS/PAWHC_0-2m.tif")
# r_soilgrids <- mask(r_soilgr_2m, wh_mask)
# r_wise30sec <- rast("data/soil/WISE30sec/PASWHC_0-150cm.tif")
# r_wise30sec <- mask(r_wise30sec, wh_mask)
# r_wise30min <- rast("data/soil/wise_30min_v3/pawhc.tif")
# r_wise30min <- mask(r_wise30min, wh_mask)
# r_wise_dept <- rast("data/soil/wise_30min_v3/soil_depth.tif")
# r_wise_dept <- mask(r_wise_dept, wh_mask)
# r_soilgwise <- rast("data/soil/SOILGRIDS/PAWHC_wise30min_depth.tif")
# r_soilgwise <- mask(r_soilgwise, wh_mask)
#
#
# d[, soilgr_1m:= extract_buf(lon, lat, r_soilgr_1m)]
# d[, soilgr_2m:= extract_buf(lon, lat, r_soilgr_2m)]
# d[, wise30sec:= extract_buf(lon, lat, r_wise30sec)]
# d[, wise30min:= extract_buf(lon, lat, r_wise30min)]
# d[, wise_dept:= extract_buf(lon, lat, r_wise_dept)]
# d[, soilgwise:= extract_buf(lon, lat, r_soilgwise)]
#
# d[!complete.cases(d)]
#
# library(ranger)
#
# soil_preds <- c("soilgr_1m", "soilgr_2m", "wise30sec", "wise30min", "soilgwise", "soildepth")
#
# tp <- expand.grid(
#   mtry = seq(2,14,2),
#   node_size = c(1,3,5,7,9),
#   samp_size = seq(.6,1,.1)
# )
#
# setDT(tp)
#
# i = j = 1
#
# # loop across tuning SRADameters
# for(i in 1:length(soil_preds)) {
#   for(j in 1:nrow(tp)) {
#     if(soil_preds[i] == "soildepth") {
#       preds <- c(sel_vars$var, "soilgr_1m", "wise_dept")
#     } else {
#       preds <- c(sel_vars$var, soil_preds[i])
#     }
#     m <- ranger(
#       x = d[, ..preds],
#       y = d$yw,
#       num.trees = 500,
#       mtry = tp$mtry[j],
#       min.node.size = tp$node_size[j],
#       sample.fraction = tp$samp_size[j],
#       seed = 0
#     )
#     tp[j, paste0("oob_", soil_preds[i]):= sqrt(m$prediction.error)]
#   }
# }
#
# tp[, lapply(.SD, min), .SDcols = names(tp)[names(tp) %like% "oob"]]
#
#
# # soilgrids 1m win, adding soil depth improves the accuracy a little bit
#
# cor(d$soilgr_1m, d$wise_dept) # they are not correlated, we can keep both
#
# dd <- fread("extrap_method/predictors.csv")
#
# if(!"pawhc" %in% dd$var) {
#   rbind(
#     dd,
#     cbind("pawhc", "soil", "data/soil/SOILGRIDS/PAWHC_0-1m.tif", "plant avail soil water hold cap (1m)"),
#     cbind("soil_dpth", "soil", "data/soil/wise_30min_v3/soil_depth.tif", "soil depth (WISE 30min)"),
#     use.names = FALSE
#   ) |>
#     fwrite("extrap_method/predictors.csv")
# }
#
#
