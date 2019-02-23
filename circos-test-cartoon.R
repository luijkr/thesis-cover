require(RColorBrewer)
library(circlize)
library(dplyr)

load("circos_chap5.Rdata")

setEPS()
postscript("circos-test.eps", height = 6, width = 6)

circos.par(points.overflow.warning = FALSE)
factors <- 1:22
circos.initialize(factors = factors, xlim = xlim)
circos.track(
  factors = factors,
  ylim = c(0, 1), bg.col = "grey",
  bg.border = NA, track.height = 0.05
)


tab <- sort(table(pairs$grs), decreasing = TRUE)
pal <- brewer.pal(n = 8, name = "Blues")
# pal <- c("#c57b3d", "#c2c991", "#b2dae7", "#c1cb4d", "#bc7244", "#d4c650", "#b6994e")
# pal <- c("#90AFC5", "#336B87", "#2A3132"); pal <- rep(pal, length = 7)
names(pal) <- head(names(tab), length(pal))
for (currentGRS in names(pal)) {
  
  probes <- pairs$probe[pairs$grs == currentGRS]
  
  grsStart <- grsSectors$grs_sector_start[grsSectors$grs == currentGRS]
  grsEnd <- grsSectors$grs_sector_end[grsSectors$grs == currentGRS]
  grsChr <- grsSectors$chr[grsSectors$grs == currentGRS] %>% as.character() %>% as.numeric()
  
  # circos.par(points.overflow.warning = FALSE)
  # factors <- 1:22
  # circos.initialize(factors = factors, xlim = xlim)
  # circos.track(
  #   factors = factors,
  #   ylim = c(0, 1), bg.col = "grey",
  #   bg.border = NA, track.height = 0.05
  # )
  
  for (probe in probes) {
    probeStart <- probeSectors$probe_sector_start[probeSectors$probe == probe]
    probeEnd <- probeSectors$probe_sector_end[probeSectors$probe == probe]
    probeChr <- probeSectors$chr[probeSectors$probe == probe] %>% as.character() %>% as.numeric()
    
    circos.link(grsChr, c(grsStart, grsEnd), probeChr, c(probeStart, probeEnd), col = pal[currentGRS])
  }
}

dev.off()

# c("#c57b3d", "#c2c991", "#b2dae7", "#c1cb4d", "#bc7244", "#d4c650", "#b6994e", "#b9e4ed")

