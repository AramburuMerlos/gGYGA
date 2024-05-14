# author: Fernando Aramburu Merlos
# date: 2022-07-12

# this script downloads and prepare all necessary GYGA data


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

data_dir <- "data/API"
dir.create(data_dir, FALSE, TRUE)

# install.packages("httr")
# install.packages("jsonlite")
# install.packages("data.table")

library(httr)
library(jsonlite)
library(data.table)


# access token ---------

tok <- "b2183f47-346c-39cd-ff25-30fa97886d2f"


# functions ----------

json_url <- function(subdir, ...) {
  file.path("https://www.yieldgap.org/apigyga/json", subdir, ...)
}

metadata_url <- function(subdir, ...) {
  file.path("https://www.yieldgap.org/apigyga/metadata", subdir, ...)
}


get_data <- function(base_url, req_token = FALSE, token = NULL, pars = NULL) {

  if(req_token & !is.null(pars)){
    full_url <- paste0(base_url, "?")
  } else {
    full_url <- base_url
  }

  if(req_token) {
    if(is.null(token)) {
      warning("no token provided, but required. Using anonymous")
      token <- "anonymous"
    }
    full_url <- paste0(full_url, "accesstoken=", token)
  }

  if(!is.null(pars)){
    tpars <- paste0("&par", seq_along(pars), "=", pars)
    tpars <- tpars[!is.na(pars)] # remove NA pars
    tpars <- paste0(tpars, collapse = "")
    full_url <- paste0(full_url, tpars)
  }

  x <-  GET(full_url) |>
    content(as = "text") |>
    fromJSON()
  return(x$items)
}


# metadata -----------------
crop_country <- "list_crop_x_country" |>
  json_url() |>
  get_data()

m <- crop_country[,c("crop_id", "country_id")] |> as.matrix()
countries <- unique(crop_country[, c("country", "country_id")])

ws <- "stations" |>
  metadata_url() |>
  get_data()

## add ISO3_code to countries -------------
# remotes::install_github("rspatial/geodata")
cy_codes <- geodata::country_codes()
setDT(cy_codes)
setDT(countries)

# perfect matches
cy <- merge(countries, cy_codes[,.(NAME, ISO3)], by.x = "country",
                   by.y = "NAME", all.x = TRUE, all.y = FALSE)

# no match, add manually
cy[is.na(ISO3), unique(country)]
cy[country %like% "ivoire", ISO3:= cy_codes[NAME %like% "Ivoire", ISO3]]
cy[country %like% "Luxemb", ISO3:= cy_codes[NAME %like% "Luxemb", ISO3]]
cy[country %like% "^Viet" , ISO3:= cy_codes[NAME %like% "^Viet" , ISO3]]
cy[country %like% "Macedo", ISO3:= cy_codes[NAME %like% "Macedo", ISO3]]
cy[country %like% "Moldov", ISO3:= cy_codes[NAME == "Moldova", ISO3]]
cy[country %like% "Serbia", ISO3:= cy_codes[NAME == "Serbia", ISO3]]
cy[country %like% "United States", ISO3:= "USA"]

setnames(cy, "ISO3", "country_iso3")
cy[, country:= NULL]


# data --------------

## country -------------

ld_cy <- vector(mode = "list", length = nrow(crop_country))

for(i in 1:nrow(crop_country)) {
  ld_cy[[i]] <- "cropcountry" |>
    json_url() |>
    get_data(req_token = TRUE, token = tok, pars = m[i,])
}

d_cy <- rbindlist(ld_cy)

# remove columns
rmcols <- names(d_cy)[names(d_cy) %like% "^min_"]
d_cy[, (rmcols):= NULL]
d_cy[, wpa:= NULL]
d_cy[, wpp:= NULL]

# crop water condition (rainfed or irrigated)
d_cy[, water:= gsub(" .*$", "", crop)]

# crop name without water condition
d_cy[, crop_sp:= sub("^\\w+ ", "", crop, perl = TRUE)]
d_cy[crop_sp == 'rapseseed', crop_sp:= 'rapeseed']

d_cy <- cy[d_cy, on = "country_id"]

setcolorder(d_cy, c("crop_sp", "water", "country", "yp", "yw", "ya", "yg_rainfed", "yg_irrigated", "yp_cv_temporal", "yw_cv_temporal", "ya_cv_temporal"))

fwrite(d_cy, file = file.path(data_dir, "Y_country.csv"))



## Climate Zone -------------

ld_cz <- vector(mode = "list", length = nrow(crop_country))

for(i in 1:nrow(crop_country)) {
  ld_cz[[i]] <- "cropcountryclimatezone" |>
    json_url() |>
    get_data(req_token = TRUE, token = tok, pars = m[i,])
}

d_cz <- rbindlist(ld_cz)

# remove columns
rmcols <- names(d_cz)[names(d_cz) %like% "^min_"]
d_cz[, (rmcols):= NULL]
d_cz[, wpa:= NULL]
d_cz[, wpp:= NULL]

# crop water condition (rainfed or irrigated)
d_cz[, water:= gsub(" .*$", "", crop)]

# crop name without water condition
d_cz[, crop_sp:= sub("^\\w+ ", "", crop, perl = TRUE)]
d_cz[crop_sp == 'rapseseed', crop_sp:= 'rapeseed']

d_cz <- cy[d_cz, on = "country_id"]


setcolorder(d_cz, c("crop_sp", "water", "country", "climatezone", "yp", "yw", "ya", "yg_rainfed", "yg_irrigated", "yp_cv_temporal", "yw_cv_temporal", "ya_cv_temporal"))

fwrite(d_cz, file = file.path(data_dir, "Y_cz.csv"))




## Weather station -------------

ld_ws <- vector(mode = "list", length = nrow(crop_country))

for(i in 1:nrow(crop_country)) {
  ld_ws[[i]] <- "cropcountrystation" |>
    json_url() |>
    get_data(req_token = TRUE, token = tok, pars = m[i,])
}

d_ws <- rbindlist(ld_ws)

# remove columns
rmcols <- names(d_ws)[names(d_ws) %like% "^min_"]
d_ws[, (rmcols):= NULL]
d_ws[, wpa:= NULL]
d_ws[, wpp:= NULL]

# crop water condition (rainfed or irrigated)
d_ws[, water:= gsub(" .*$", "", crop)]

# crop name without water condition
d_ws[, crop_sp:= sub("^\\w+ ", "", crop, perl = TRUE)]
d_ws[crop_sp == 'rapseseed', crop_sp:= 'rapeseed']

d_ws <- cy[d_ws, on = "country_id"]

setcolorder(d_ws, c("crop_sp", "water", "country", "station", "yp", "yw", "ya", "yg_rainfed", "yg_irrigated", "yp_cv_temporal", "yw_cv_temporal", "ya_cv_temporal"))

fwrite(d_ws, file = file.path(data_dir, "Y_ws.csv"))


## FIXES ------------

# Ndiaye and Fanaye weather stations in Senegal have their coordinates switched
ws[ws$station == "Fanaye", "latitude"] <- 16.53
ws[ws$station == "Fanaye", "longitude"] <- -14.8
ws[ws$station == "Ndiaye", "latitude"] <- 16.22
ws[ws$station == "Ndiaye", "longitude"] <- -16.29

# # (deprecated, there too many inconsistencies)
# # Amarillo weather station, in TX, USA, is reported in the wrong CZ
# ws[ws$station == "Amarillo", "climatezone"] <- 5102
# #
#


# weather station list to create buffer zones. Includes some WS without data
ws <- cy[ws, on = "country_id"]
fwrite(ws, file = file.path(data_dir, "ws.csv"))


# CROP MODELS RUNS -------------------------------

# ld_cm <- vector(mode = "list", length = nrow(crop_country))
#
# for(i in 1:nrow(crop_country)) {
#   ld_cm[[i]] <- "cropcountrymodelruns" |>
#     json_url() |>
#     get_data(req_token = TRUE, token = tok, pars = m[i,])
# }
#
# d_cm <- rbindlist(ld_cm)
# fwrite(d_cm, file = file.path(data_dir, "crop_models_info.csv"))

