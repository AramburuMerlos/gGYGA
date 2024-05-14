# author: Fernando Aramburu Merlos
# date: 2022-08-01
# updated: 2022-07-27. Add new predictors based on WUR experience


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

# 30 second resolution ----------------------------

# get worldclim2 climatic and bioclimatic variables
bio <- geodata::worldclim_global("bio", 0.5, "data/climate")

tmin <- geodata::worldclim_global("tmin", 0.5, "data/climate")
tmax <- geodata::worldclim_global("tmax", 0.5, "data/climate")
tavg <- geodata::worldclim_global("tavg", 0.5, "data/climate")
prec <- geodata::worldclim_global("prec", 0.5, "data/climate")
srad <- geodata::worldclim_global("srad", 0.5, "data/climate")

## Annual SRAD ----------
app(srad, sum,
    filename =  "data/climate/wc2.1_30s/annual_srad.tif",
    overwrite = T,
    wopt = list(names = "annual_srad", filetype = "GTiff",
                gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)

## Growing Degree Days ---------
gdd_m <- classify(tavg, cbind(-Inf, 0, 0),
                  filename =  "data/climate/wc2.1_30s/mgdd.tif",
                  overwrite = T,
                  wopt = list(names = "mgdd", filetype = "GTiff",
                              gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)
gdd <- app(gdd_m, mean) * 365
writeRaster(
  gdd,
  filename =  "data/climate/wc2.1_30s/GDD.tif",
  overwrite = T,
  wopt = list(names = "GDD", filetype = "GTiff",
              gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)

## Potential Evapotranspiration (monthly average) ------------

### Radiation on top of the atmosphere (monthly average) -------------

## latitude
lat <- yFromRow(tavg, 1:nrow(tavg))

# install.packages("sirad")
# function is not vectorized over latituted
m <- matrix(NA_real_, nrow = length(lat), ncol = 12)

pb = txtProgressBar(min = 0, max = length(lat), initial = 0)

for(i in 1:length(lat)) {
  v <- sirad::extrat(1:365, sirad::radians(lat[i]))$ExtraTerrestrialSolarRadiationDaily
  m[i,] <- c(
    mean(v[  1: 31]),
    mean(v[ 32: 59]),
    mean(v[ 60: 90]),
    mean(v[ 91:120]),
    mean(v[121:151]),
    mean(v[152:181]),
    mean(v[182:212]),
    mean(v[213:243]),
    mean(v[244:273]),
    mean(v[274:304]),
    mean(v[305:334]),
    mean(v[335:365])
  )
  setTxtProgressBar(pb,i)
}
close(pb)

for(i in 1:nlyr(erad)) {
  erad <- rast(tavg, nlyrs = 1)
  values(erad) <- rep(m[,i], each = ncol(erad))
  erad_mm <- erad/2.45 # mm/day (see http://www.fao.org/3/X0490E/x0490e07.htm)

  writeRaster(
    erad_mm,
    filename = paste0("data/climate/wc2.1_30s/Ext_Rad_mm_", formatC(i, width = 2, flag = 0), ".tif"),
    overwrite = T,
    wopt = list(names = paste0("erad_", formatC(i, width = 2, flag = 0)), filetype = "GTiff",
                gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
  )
  rm(erad, erad_mm)
  cat(i)
}

erad_mm <- Sys.glob("data/climate/wc2.1_30s/Ext_Rad_mm_*.tif") |> rast()

# Hargreaves ETo equation
# ETo = 0.0023(Tmean + 17.8)(Tmax - Tmin)^(0.5) Ra

fpet <- function(erad_mm, tavg, tmax, tmin) {
  0.0023 * (tavg + 17.8) * sqrt(tmax - tmin) * erad_mm  # [mm/day]
}

for(i in 1:12){
  ii <- formatC(i, width = 2, flag = 0)
  pet <- rast(tavg, nlyr = 1)
  lapp(c(erad_mm[[i]], tavg[[i]], tmax[[i]], tmin[[i]]), fun = fpet,
       filename = paste0("data/climate/wc2.1_30s/PET_", ii, ".tif"),
       overwrite = T,
       wopt = list(names = paste0("PET_m", ii), filetype = "GTiff",
                   gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6")))
}

pet <- Sys.glob("data/climate/wc2.1_30s/PET_*.tif") |> rast()

## Pet seasonality ------------
app(
  pet, sd,
  filename = "data/climate/wc2.1_30s/PET_seasonality.tif",
  overwrite = T,
  wopt = list(names = "PET_seasonality", filetype = "GTiff",
              gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)



## Aridity Index --------------------------------------------------------
# pet is daily average
# function to compute GDD is also useful here to get annual sum of PET (mm)

### Annual -------------
pet_annual <- app(pet, mean) * 365
prec_annual <- app(prec, sum)

# Aridity index setting limits of 0 and 10
fai <- function(precip, potet) {
  ai <- precip/potet
  ai[ai < 0] <- 0
  ai[ai > 10] <- 10
  return(ai)
}

lapp(
  c(prec_annual, pet_annual), fun = fai,
  filename = "data/climate/wc2.1_30s/AIann.tif",
  overwrite = T,
  wopt = list(names = "AIann", filetype = "GTiff",
              gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)


# 5 minutes resolution ----------------------------

# get worldclim2 climatic and bioclimatic variables
bio <- geodata::worldclim_global("bio", 5, "data/climate")

tmin <- geodata::worldclim_global("tmin", 5, "data/climate")
tmax <- geodata::worldclim_global("tmax", 5, "data/climate")
tavg <- geodata::worldclim_global("tavg", 5, "data/climate")
prec <- geodata::worldclim_global("prec", 5, "data/climate")
srad <- geodata::worldclim_global("srad", 5, "data/climate")

## Annual SRAD ----------
app(srad, sum,
    filename =  "data/climate/wc2.1_5m/annual_srad.tif",
    overwrite = T,
    wopt = list(names = "annual_srad", filetype = "GTiff",
                gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)

## Growing Degree Days ---------
gdd_m <- classify(tavg, cbind(-Inf, 0, 0),
                  filename =  "data/climate/wc2.1_5m/mgdd.tif",
                  overwrite = T,
                  wopt = list(names = "mgdd", filetype = "GTiff",
                              gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)
gdd <- app(gdd_m, mean) * 365
writeRaster(
  gdd,
  filename =  "data/climate/wc2.1_5m/GDD.tif",
  overwrite = T,
  wopt = list(names = "GDD", filetype = "GTiff",
              gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)

## Potential Evapotranspiration (monthly average) ------------

### Radiation on top of the atmosphere (monthly average) -------------

## latitude
lat <- yFromRow(tavg, 1:nrow(tavg))

# install.packages("sirad")
# function is not vectorized over latituted
m <- matrix(NA_real_, nrow = length(lat), ncol = 12)

pb = txtProgressBar(min = 0, max = length(lat), initial = 0)

for(i in 1:length(lat)) {
  v <- sirad::extrat(1:365, sirad::radians(lat[i]))$ExtraTerrestrialSolarRadiationDaily
  m[i,] <- c(
    mean(v[  1: 31]),
    mean(v[ 32: 59]),
    mean(v[ 60: 90]),
    mean(v[ 91:120]),
    mean(v[121:151]),
    mean(v[152:181]),
    mean(v[182:212]),
    mean(v[213:243]),
    mean(v[244:273]),
    mean(v[274:304]),
    mean(v[305:334]),
    mean(v[335:365])
  )
  setTxtProgressBar(pb,i)
}
close(pb)

for(i in 1:nlyr(erad)) {
  erad <- rast(tavg, nlyrs = 1)
  values(erad) <- rep(m[,i], each = ncol(erad))
  erad_mm <- erad/2.45 # mm/day (see http://www.fao.org/3/X0490E/x0490e07.htm)

  writeRaster(
    erad_mm,
    filename = paste0("data/climate/wc2.1_5m/Ext_Rad_mm_", formatC(i, width = 2, flag = 0), ".tif"),
    overwrite = T,
    wopt = list(names = paste0("erad_", formatC(i, width = 2, flag = 0)), filetype = "GTiff",
                gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
  )
  rm(erad, erad_mm)
  cat(i)
}

erad_mm <- Sys.glob("data/climate/wc2.1_5m/Ext_Rad_mm_*.tif") |> rast()

# Hargreaves ETo equation
# ETo = 0.0023(Tmean + 17.8)(Tmax - Tmin)^(0.5) Ra

fpet <- function(erad_mm, tavg, tmax, tmin) {
  0.0023 * (tavg + 17.8) * sqrt(tmax - tmin) * erad_mm  # [mm/day]
}

for(i in 1:12){
  ii <- formatC(i, width = 2, flag = 0)
  pet <- rast(tavg, nlyr = 1)
  lapp(c(erad_mm[[i]], tavg[[i]], tmax[[i]], tmin[[i]]), fun = fpet,
       filename = paste0("data/climate/wc2.1_5m/PET_", ii, ".tif"),
       overwrite = T,
       wopt = list(names = paste0("PET_m", ii), filetype = "GTiff",
                   gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6")))
}

pet <- Sys.glob("data/climate/wc2.1_5m/PET_*.tif") |> rast()

## Pet seasonality ------------
app(
  pet, sd,
  filename = "data/climate/wc2.1_5m/PET_seasonality.tif",
  overwrite = T,
  wopt = list(names = "PET_seasonality", filetype = "GTiff",
              gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)



## Aridity Index --------------------------------------------------------
# pet is daily average
# function to compute GDD is also useful here to get annual sum of PET (mm)

### Annual -------------
pet_annual <- app(pet, mean) * 365
prec_annual <- app(prec, sum)

# Aridity index setting limits of 0 and 10
fai <- function(precip, potet) {
  ai <- precip/potet
  ai[ai < 0] <- 0
  ai[ai > 10] <- 10
  return(ai)
}

lapp(
  c(prec_annual, pet_annual), fun = fai,
  filename = "data/climate/wc2.1_5m/AIann.tif",
  overwrite = T,
  wopt = list(names = "AIann", filetype = "GTiff",
              gdal = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)


