# author: Fernando Aramburu Merlos

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(terra)
library(data.table)
library(stringr)

# COUNTY DATA -------

d <- fread("countries/USA/data/usda_nass/survey_corn_county_area_and_yield_2005-2022.csv")
d <- d[grain_yield_t_ha > 0,]

# create geoid that matches the one in the shapefile
fgeoid <- function(state_code, county_code) {
  state_code <- formatC(state_code, width = 2, format = "d", flag = "0")
  county_code <- formatC(county_code, width = 3, format = "d", flag = "0")
  paste0(state_code, county_code)
}
d[, geoid:= fgeoid(state_ansi, county_ansi)]

if(anyDuplicated(d[, .(geoid, year)]) > 0) stop("Duplicated GEOID by year records")

d <- d[year > 2004 & year < 2016,]
dn <- d[,.N, by = .(geoid)]
d <- d[geoid %in% dn[N > 2, geoid]]

d[, prop_irrig:= grain_irrig_ha_harv/grain_ha_harv]


## add proportion of irrigated area from census data ---------
dc <- fread("countries/USA/data/usda_nass/census_corn_county_area_and_yield_2005-2022.csv")
dc[, .N, by = .(year)]
dc <- dc[year %in% c(2007,2012)]

dc[, geoid:= fgeoid(state_ansi, county_ansi)]
dc <- dc[, .(prop_irrig_cs = mean(grain_irrig_ha_harv/grain_ha_harv, na.rm = TRUE)), by = .(geoid)]
dc <- dc[complete.cases(dc)]
d <- dc[d, on = .(geoid)]
d[is.na(prop_irrig), prop_irrig:= prop_irrig_cs]
d[, prop_irrig_cs:= NULL]

d[, prop_irrig:= ifelse(is.na(prop_irrig), mean(prop_irrig, na.rm = TRUE), prop_irrig), by = .(geoid)]

d[is.na(prop_irrig) | prop_irrig < 0.05, grain_non_irrig_yield_t_ha:= grain_yield_t_ha]

dd <- d[, .(
  prop_irrig = mean(prop_irrig, na.rm = TRUE),
  Ya_mz_rfed = mean(grain_non_irrig_yield_t_ha, na.rm = TRUE)),
  by = .(geoid)
]

# load counties
counties <- vect("countries/USA/data/counties_shapefile/cb_2022_us_county_500k.shp")
states <- aggregate(counties, "STATE_NAME")

# total county area
d[, all(geoid %in% counties$GEOID)]

counties <- merge(counties, dd, by.x = "GEOID", by.y = "geoid")
counties <- counties[counties$STUSPS %in% c("ND",'SD','NE','KS','MN','IA','MO','WI','IL','MI','IN','OH')]

# GRIDDED DATA ---------------
yield <- "extrap_method/global_estimates/5min/masked_by_aoa/Rainfed maize.tif" |>
  rast()
names(yield) <- "Yw_mm"

gaez_mz <- rast("../GAEZ/agroclimatic_Yp/maiz200b_yld.tif")
names(gaez_mz) <- "Yw_GAEZ"

# COMPARE ------------------
counties <- project(counties, yield)
counties <- extract(yield, counties, fun = 'mean', na.rm = TRUE, bind = TRUE)

counties <- project(counties, gaez_mz)
counties <- extract(gaez_mz, counties, fun = 'mean', na.rm = TRUE, bind = TRUE)
counties$Yw_GAEZ <- counties$Yw_GAEZ/1e3
counties$Yw_GAEZ <- counties$Yw_GAEZ/0.845


df <- as.data.frame(counties)
df$Yg_mm <- df$Yw_mm - df$Ya_mz_rfed
df$Yg_GAEZ <- df$Yw_GAEZ - df$Ya_mz_rfed


png("extrap_method/plots/Yg_mz_midewst.png", width = 6, height = 3, units = "in", res = 600)

ymx <- 180
xlims <- range(df$Yg_mm, na.rm = TRUE)
xlims <- c(floor(xlims)[1], ceiling(xlims)[2])

par(mfrow = c(1,2), oma = c(0,2,0,0), mar = c(4,2,3,.5), las = 1)
hist(
  df$Yg_mm, breaks = seq(xlims[1], xlims[2],.5),
  xlim = xlims, xlab = "",
  ylim = c(0, ymx),
  axes = FALSE,
  main = "Metamodel",
  col = rgb(0,0,1,0.5),
  cex.axis = 0.8
)
axis(1, at = seq(xlims[1], xlims[2], 2), pos = 0)
axis(2, at = seq(0, ymx, 30), pos = xlims[1])
mtext("number of counties", 2, outer = T, las = 0, line = 0.2)
mtext(expression(Yg~(t~ha^-1)), 1, line = 2)
abline(v = 0, lty = 3, lwd = 1.5)
ngp <- round(sum(df$Yg_mm < 0, na.rm = TRUE)/sum(!is.na(df$Yg_mm)) * 100)
text(-1, 150, paste0("Negative\nYg = ", ngp, "%"), cex = 0.6)

xlims <- range(df$Yg_GAEZ, na.rm = TRUE)
xlims <- c(floor(xlims)[1], ceiling(xlims)[2])
hist(
  df$Yg_GAEZ, breaks = seq(xlims[1], xlims[2]),
  xlim = xlims, xlab = "",
  ylim = c(0,ymx),
  axes = FALSE,
  main = "GAEZ",
  col = rgb(1,0,0,0.5),
  cex.axis = 0.8
)
axis(1, at = seq(xlims[1], xlims[2], 3), pos = 0)
axis(2, at = seq(0, ymx, 30), pos = xlims[1])
mtext(expression(Yg~(t~ha^-1)), 1, line = 2)
abline(v = 0,lwd = 1.5, lty = 3)
ngp <- round(sum(df$Yg_GAEZ < 0, na.rm = TRUE)/sum(!is.na(df$Yg_GAEZ)) * 100)
text(-3, 150, paste0("Negative\nYg = ", ngp, "%"), cex = 0.6)


dev.off()

sum(df$Yg_GAEZ < 0, na.rm = TRUE)/sum(!is.na(df$Yg_GAEZ))

col <- RColorBrewer::brewer.pal(8, "RdBu")[-c(4,5)]

counties$Yg_GAEZ <- counties$Yw_GAEZ - counties$Ya_mz_rfed
png("extrap_method/maps/US_midW_Yg_GAEZ.png", 6,4, unit = "in", res = 300)
plot(states, lwd = 2, col = "white", background = "lightgray", ext = ext(counties), pax = list(labels = FALSE, tick = FALSE))
plot(counties[!is.na(counties$Yg_GAEZ),], "Yg_GAEZ", add = TRUE,
     breaks = c(-6,-4,-2,0,3,6,9), col = col, lwd = 0.5)
plot(states, lwd = 2, add = TRUE)
dev.off()

counties$Yg_mm <- counties$Yw_mm - counties$Ya_mz_rfed
png("extrap_method/maps/US_midW_Yg_mm.png", 6,4, unit = "in", res = 300)
plot(states, lwd = 2, col = "white", background = "lightgray", ext = ext(counties), pax = list(labels = FALSE, tick = FALSE))
plot(counties[!is.na(counties$Yg_mm),], "Yg_mm", add = TRUE,
     breaks = c(-6,-4,-2,0,3,6,9), col = col, lwd = 0.5)
plot(states, lwd = 2, add = TRUE)
dev.off()
