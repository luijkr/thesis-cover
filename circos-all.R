library(circlize)
library(dplyr)

width <- 6
height <- 11
titleX <- -0.8
titleY <- 1
titles <- c('3', '4', '5')
cols <- c("#637694", "#d4526a", "#ecc771")

setEPS()
postscript("circos-all.eps", height = height, width = width)
par(mfrow = c(height, width))


############
# CHAPTER 3
############

load('circos_chap3.Rdata')

for (currentGRS in sample( unique(pairs$grs) )) {
  
  probes <- pairs$probe[pairs$grs == currentGRS]
  
  grsStart <- grsSectors$grs_sector_start[grsSectors$grs == currentGRS]
  grsEnd <- grsSectors$grs_sector_end[grsSectors$grs == currentGRS]
  grsChr <- grsSectors$chr[grsSectors$grs == currentGRS] %>% as.character() %>% as.numeric()
  
  circos.par(points.overflow.warning = FALSE)
  factors <- 1:22
  circos.initialize(factors = factors, xlim = xlim)
  circos.track(
    factors = factors,
    ylim = c(0, 1), bg.col = "grey",
    bg.border = NA, track.height = 0.05
  )
  
  col <- sample(cols, 1)
  doneSectors <- NULL
  for (probe in probes) {
    if (probe %in% probeSectors$probe) {
      if (!probeSectors$label[probeSectors$probe == probe] %in% doneSectors) {
        probeStart <- probeSectors$probe_sector_start[probeSectors$probe == probe]
        probeEnd <- probeSectors$probe_sector_end[probeSectors$probe == probe]
        probeChr <- probeSectors$chr[probeSectors$probe == probe] %>% as.character() %>% as.numeric()
        
        circos.link(grsChr, c(grsStart, grsEnd), probeChr, c(probeStart, probeEnd), col = col) #cols[1])
        doneSectors <- c(doneSectors, probeSectors$label[probeSectors$probe == probe])
      }
    }
  }
  text(titles[1], x = titleX, y = titleY, col = "darkgrey", cex = 0.8)
}


############
# CHAPTER 4
############

load('circos_chap4.Rdata')

for (currentGRS in sample( unique(pairs$grs) )) {
  
  probes <- pairs$probe[pairs$grs == currentGRS]
  
  grsStart <- grsSectors$grs_sector_start[grsSectors$grs == currentGRS] %>% unique()
  grsEnd <- grsSectors$grs_sector_end[grsSectors$grs == currentGRS] %>% unique()
  grsChr <- grsSectors$chr[grsSectors$grs == currentGRS] %>% as.character() %>% as.numeric() %>% unique()
  
  circos.par(points.overflow.warning = FALSE)
  factors <- c(grsChr, 'X')
  circos.initialize(factors = factors, xlim = xlim %>% filter(Chromosome %in% c(grsChr, 'X')) %>% select(start, end) %>% as.matrix())
  circos.track(
    factors = factors,
    ylim = c(0, 1), bg.col = "grey",
    bg.border = NA, track.height = 0.05
  )
  
  col <- sample(cols, 1)
  for (probe in probes) {
    probeStart <- probeSectors$probe_sector_start[probeSectors$probe == probe]
    probeEnd <- probeSectors$probe_sector_end[probeSectors$probe == probe]
    probeChr <- probeSectors$chr[probeSectors$probe == probe] %>% as.character()
    
    circos.link(grsChr, c(grsStart, grsEnd), probeChr, c(probeStart, probeEnd), col = col) # cols[2]
  }
  text(titles[2], x = titleX, y = titleY, col = "darkgrey", cex = 0.8)
}


############
# CHAPTER 5
############

load('circos_chap5.Rdata')

for (currentGRS in sample( unique(pairs$grs) )) {
  
  probes <- pairs$probe[pairs$grs == currentGRS]
  
  grsStart <- grsSectors$grs_sector_start[grsSectors$grs == currentGRS]
  grsEnd <- grsSectors$grs_sector_end[grsSectors$grs == currentGRS]
  grsChr <- grsSectors$chr[grsSectors$grs == currentGRS] %>% as.character() %>% as.numeric()
  
  circos.par(points.overflow.warning = FALSE)
  factors <- 1:22
  circos.initialize(factors = factors, xlim = xlim)
  circos.track(
    factors = factors,
    ylim = c(0, 1), bg.col = "grey",
    bg.border = NA, track.height = 0.05
  )
  
  col <- sample(cols, 1)
  for (probe in probes) {
    probeStart <- probeSectors$probe_sector_start[probeSectors$probe == probe]
    probeEnd <- probeSectors$probe_sector_end[probeSectors$probe == probe]
    probeChr <- probeSectors$chr[probeSectors$probe == probe] %>% as.character() %>% as.numeric()
    
    circos.link(grsChr, c(grsStart, grsEnd), probeChr, c(probeStart, probeEnd), col = col) # cols[3]
  }
  text(titles[3], x = titleX, y = titleY, col = "darkgrey", cex = 0.8)
}

dev.off()

