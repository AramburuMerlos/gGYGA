# author: Fernando Aramburu Merlos
# date: 2022-10-05

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(terra)
library(raster)
library(ranger)
library(caret)
library(CAST)
library(stringr)


## SPAM --------
# rainfed and irrigated wheat, maize, and rice
spam <- "data/SPAM/harvested_area_global_merged_v2/*.tif" |>
  Sys.glob() |>
  str_subset("WHEA|MAIZ|RICE") |>
  str_subset("_(R|I).tif$") |>
  rast()

names(spam) |>
  str_extract("(WHEA|MAIZ|RICE)_.$") |>
  str_replace("WHEA", "wheat") |>
  str_replace("MAIZ", "maize") |>
  str_replace("RICE", "rice") |>
  c() -> .; paste(ifelse(grepl("_R$", .), "Rainfed", "Irrigated"), .) |>
  str_remove("_.$") -> names(spam)

crops <- names(spam)
crops <- sort(crops)

## models --------------------------------

mod_list_global <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list.RDS")
mod_list_interp <- readRDS("extrap_method/NNDM/NNDM_LOOCV_RF_model_list_interp.RDS")

all_mods_list <- list(
  global = mod_list_global,
  interp = mod_list_interp
)


# MAP AOA #######################################

aoa_meta_list <- readRDS("extrap_method/NNDM/aoa_all_models.RDS")

world <- geodata::world(path = "data/gadm")
cell_size <- cellSize(spam, mask = FALSE, unit = "ha")

# colors
#grys <- c("white", RColorBrewer::brewer.pal(9, "Blues")[c(2,4,6,8,9)])
grys <- c("white", gray.colors(5, end = 0.6, rev = T))
reds <- c("white", RColorBrewer::brewer.pal(9, "Reds")[5:9])

leg_txt <- c("< 0.5", "0.5 - 10", "10 - 20", "20 - 40", "40 - 80", "> 80")

dir <- "extrap_method/maps/area_coverage/"
dir.create(dir, FALSE, TRUE)

dcover <- expand.grid(crop = crops, cv_method = names(all_mods_list)) |>
  setDT()


for(i in 1:length(crops)) {

  crop_sp_i <- str_extract(crops[i], "(?<= ).+")
  r_crop_i <- subset(spam, crops[i])
  #r_crop_i[r_crop_i < 100] <- NA

  for(j in 1:length(all_mods_list)) {
    r_aoa_ji <- rast(aoa_meta_list[[j]][[i]]$AOA)

    area_included <- mask(r_crop_i, r_aoa_ji, maskvalues = c(0, NA))
    area_excluded <- mask(r_crop_i, r_aoa_ji, maskvalues = c(1, NA))

    tot_incl <- global(area_included, sum, na.rm = TRUE)[1,1]
    tot_excl <- global(area_excluded, sum, na.rm = TRUE)[1,1]
    cover <- round(tot_incl / (tot_excl + tot_incl) * 100, 1)
    dcover[crop == crops[i] & cv_method == names(all_mods_list)[j], coverage:= cover]

    area_included <- area_included/cell_size
    area_excluded <- area_excluded/cell_size

    brks = c(0,0.005,.1,.2,.4,.8, ceiling(global(area_included, max, na.rm = T)[1,1]*10)/10)

    fn <- paste0(dir, "metamodel_", names(all_mods_list[j]), "_", tolower(gsub(" ", "_", crops[i])), ".tif")

    tiff(filename = fn, width = ncol(area_included)/300, height = nrow(area_included)/300,
         units = "in", type = "cairo", res = 300, compression = "zip")

    plot(area_included, breaks = brks, col = grys,
         #
         maxcell = ncell(area_included), mar = c(.1,.1,.1,.1), legend = FALSE)

    plot(area_excluded, breaks = brks, col = reds,
         axes = FALSE, add = TRUE, maxcell = ncell(area_excluded))

    plot(world, add = TRUE)

    legend(legend = leg_txt,
           fill = grys, box.col = "white",
           cex = 1.3, x = "bottomleft",
           title = "In - ", title.adj = .2, inset = c(0.01, 0))

    legend(legend = leg_txt,
           fill = reds, box.col = "white",
           cex = 1.3, x = "bottomleft", inset = c(0.04, 0),
           title = "Out (%)", title.adj = .4)

    txt <- paste0(crops[i], " (", cover, "%)")
    mtext(txt, side = 1, line = -2, adj = .9, cex = 1.5)

    rm(area_excluded, area_included, tot_excl, tot_incl, r_aoa_ji, cover)
    dev.off()
  }
  rm(crop_sp_i, r_crop_i)
}

fwrite(dcover, "extrap_method/tables/models_coverage.csv")

