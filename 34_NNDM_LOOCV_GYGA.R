# author: Fernando Aramburu Merlos
# date: 2023-01-10


# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(stringr)


# data  ---------------------------------

d <- fread("extrap_method/training_data/Y_ws_predictors.csv")
nndm_global <- readRDS("extrap_method/NNDM/nndm_global_indices.RDS")
nndm_interp <- readRDS("extrap_method/NNDM/nndm_interp_indices.RDS")

all.equal(names(nndm_global), names(nndm_interp))
crops <- names(nndm_global)


# NNDM LOOCV predictions --------------------

## global ---------------

ld <- vector("list", length(crops))

for(i in seq_along(crops)) {
  indx_train <- nndm_global[[i]]$indx_train

  di <- d[crop == crops[i]]
  di[, I:= .I]
  for(j in seq_along(indx_train)) {
    czj <- d[crop == crops[i]][j, CZ]
    if(di[I %in% indx_train[[j]] & CZ == czj,.N] == 0) next
    if(grepl("Rainfed", crops[i])) {
      ywj <- di[
        I %in% indx_train[[j]] & CZ == czj
        , sum(yw * harv_area/sum(harv_area))
      ]
      di[j, yw_hat_global:= ywj]
    } else {
      ypj <- di[
        I %in% indx_train[[j]] & CZ == czj
        , sum(yp * harv_area/sum(harv_area), na.rm = TRUE)
      ]
      di[j, yp_hat_global:= ypj]
    }
  }
  ld[[i]] <- di
}

d <- rbindlist(ld, use.names = TRUE, fill = TRUE)



## interpolated ------------------

ld <- vector("list", length(crops))

for(i in seq_along(crops)) {
  indx_train <- nndm_interp[[i]]$indx_train
  di <- d[crop == crops[i]]
  di[, I:= .I]
  for(j in seq_along(indx_train)) {
    czj <- di[j, CZ]
    ctj <- di[j, country]
    if(di[I %in% indx_train[[j]] & CZ == czj & country == ctj, .N] == 0) next
    if(grepl("Rainfed", crops[i])) {
      ywj <- di[
        I %in% indx_train[[j]] & CZ == czj & country == ctj
        , sum(yw * harv_area/sum(harv_area))
      ]
      di[j, yw_hat_interp:= ywj]
    } else {
      ypj <- di[
        I %in% indx_train[[j]] & CZ == czj & country == ctj
        , sum(yp * harv_area/sum(harv_area), na.rm = TRUE)
      ]
      di[j, yp_hat_interp:= ypj]
    }
  }
  ld[[i]] <- di
}

d <- rbindlist(ld, use.names = TRUE, fill = TRUE)


d <- d[, .(crop, crop_sp, water, lon, lat, country, CZ, station, harv_area,
           yp, yp_hat_global, yp_hat_interp, yw, yw_hat_global, yw_hat_interp)]

fwrite(d, "extrap_method/NNDM/GYGA_ws_NNDM_LOOCV.csv")


