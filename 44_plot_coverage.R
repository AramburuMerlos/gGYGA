# author: Fernando Aramburu Merlos
# date: 2022-10-07

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/GYGA")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/GYGA")
}

library(data.table)
library(stringr)

# prepare data

mm_cvg <- fread("extrap_method/tables/models_coverage.csv")
mm_cvg[, crop_sp:= str_extract(crop, "(?<= ).+")]
mm_cvg <- dcast(mm_cvg, crop + crop_sp ~ cv_method, value.var = "coverage")


gg_cvg <- fread("data/API/GYGA_total_area_coverage_R_I.csv")

cvg <- gg_cvg[mm_cvg, on = .NATURAL]
cvg[, global:= global/100 * world]
cvg[, interp:= interp/100 * world]

cvg <- cvg[, lapply(.SD, sum), by = .(crop_sp), .SDcols = c("world", "ctry_spam", "global", "interp", "cz_spam", "cz_ctry_spam")]
cvg <- cvg[, .(crop = crop_sp, meta_global = global/world,
               gyga_global = cz_spam/world, cz_global = cz_ctry_spam/world,
               meta_interp = interp/ctry_spam, gyga_interp = cz_ctry_spam/ctry_spam)]

cvg_global <- cvg[, t(cbind(cz_global, gyga_global, meta_global))]
rownames(cvg_global) <- c("CZ inter", "CZ extra", "Metamodel")
colnames(cvg_global) <- cvg$crop

cvg_interp <- cvg[, t(cbind(gyga_interp, meta_interp))]
rownames(cvg_interp) <- c("CZ inter", "Metamodel")
colnames(cvg_interp) <- cvg$crop


# Individual Plot ------------------------------
#cols <- RColorBrewer::brewer.pal(3, "Set1")
cols <- viridis::plasma(3, end = 0.8)

### Global ---------------
cvg_global <- cvg_global[, c("maize", "wheat", "rice")]
cvg_global <- cvg_global[-grep("inter", row.names(cvg_global)),]

png("extrap_method/plots/global_coverage.png", 4, 3, "in", res = 300)

par(mar = c(3,4,1, 1), cex.axis = .8, mgp = c(2,.7,0))

barplot(
  cvg_global * 100,
  beside = TRUE,
  col = cols[c(1,3)],
  axes = FALSE,
  ylim = c(0,100),
  las = 1,
  cex.names = 1.1
)

axis(1, at = c(0.6, 6 * 1.2), pos = 0, labels = c("", ""), lwd.ticks = 0, xpd = NA)
axis(2, at = seq(0,100,20), pos = 0.6, las = 1)

mtext("Global crop area coverage (%)", 2, line = 2.5)

legend("bottomright", legend = c("Climate Zones", "Metamodel"),
       inset = c(0, -0.3), xpd = NA, cex = 0.9, horiz = TRUE, x.intersp = 0.5, text.width = 3,
       fill = cols[c(1,3)], bty = "n" )

dev.off()


### interp --------------------

cvg_interp <- cvg_interp[, c("maize", "wheat", "rice")]

png("extrap_method/plots/interp_coverage.png", 4, 3, "in", res = 300)

par(mar = c(3,4,1, 5), cex.axis = .8, mgp = c(2,.7,0))

barplot(
  cvg_interp * 100,
  beside = TRUE,
  col = cols[c(1,3)],
  axes = FALSE,
  ylim = c(0,100),
  las = 1,
  cex.names = 1.1
)

axis(1, at = c(0.5, 8 * 1.2), pos = 0, labels = c("", ""), lwd.ticks = 0, xpd = NA)
axis(2, at = seq(0,100,20), pos = 0.5, las = 1)

mtext("Country Coverage (%)", 2, line = 2.5)
legend(x = par("usr")[2]*.98, y = 50, legend = c("CZ interp.", "Metamodel"), fill = cols[c(1,3)],
       xpd = NA, cex = 0.8, bty = "n", xjust = 0, yjust = 0.5)

dev.off()




# Two panel plot ------------------------------

cvg_global <- cvg_global[rownames(cvg_global) != "CZ inter",]


#cols <- RColorBrewer::brewer.pal(3, "Set1")
cols <- viridis::plasma(2, end = 0.8)

png("extrap_method/plots/coverage.png", 7, 3, "in", res = 300)

par(mfrow = c(1,2), oma = c(0,0,0,5), mar = c(4,4,1,1), cex.axis = .8, mgp = c(2,.7,0), xpd = NA)

barplot(
  cvg_interp * 100,
  beside = TRUE,
  col = cols,
  axes = FALSE,
  ylim = c(0,100),
  las = 1,
  cex.names = 1.1
)

axis(1, at = c(0.5, 8 * 1.2), pos = 0, labels = c("", ""), lwd.ticks = 0, xpd = NA)
axis(2, at = seq(0,100,20), pos = 0.5, las = 1)
mtext("Average national coverage (%)", 2, line = 2)
mtext("INTERPOLATION", 1, line = 2.5, font = 2)

barplot(
  cvg_global * 100,
  beside = TRUE,
  col = cols,
  axes = FALSE,
  ylim = c(0,100),
  las = 1,
  cex.names = 1.1
)

axis(1, at = c(0.5, 8 * 1.2), pos = 0, labels = c("", ""), lwd.ticks = 0, xpd = NA)
axis(2, at = seq(0,100,20), pos = 0.6, las = 1)
mtext("Global coverage (%)", 2, line = 2)


legend("right", legend = c("GYGA-CZ", "metamodel"), inset = c(-0.55),
       fill = cols, xpd = NA, bty = "n", x.intersp = 0.5, cex = .9)

mtext("EXTRAPOLATION", 1, line = 2.5, font = 2)


dev.off()


# TABLE ----------------
round(cvg_global*100)
round(cvg_interp*100)
# CZ-Country global
gg_cvg[
  crop_sp %in% c("wheat", "maize", "rice")
  , round(100 * sum(cz_ctry_spam)/sum(world))
  , by = .(crop_sp)]
# CZ extrap in GYGA countries

