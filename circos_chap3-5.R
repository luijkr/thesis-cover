library(circlize)
library(dplyr)

load('circos_chap3.Rdata')

setEPS()
postscript("circos-bios.eps", height = 6, width = 5)
par(mfrow = c(6, 5))
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
  
  doneSectors <- NULL
  for (probe in probes) {
    if (probe %in% probeSectors$probe) {
      if (!probeSectors$label[probeSectors$probe == probe] %in% doneSectors) {
        probeStart <- probeSectors$probe_sector_start[probeSectors$probe == probe]
        probeEnd <- probeSectors$probe_sector_end[probeSectors$probe == probe]
        probeChr <- probeSectors$chr[probeSectors$probe == probe] %>% as.character() %>% as.numeric()
        
        circos.link(grsChr, c(grsStart, grsEnd), probeChr, c(probeStart, probeEnd), col = "#637694")
        doneSectors <- c(doneSectors, probeSectors$label[probeSectors$probe == probe])
      }
    }
  }
}
dev.off()


load('circos_chap4.Rdata')


setEPS()
postscript("circos-x.eps", height = 3, width = 1)
par(mfrow = c(3, 1))
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
  
  for (probe in probes) {
    probeStart <- probeSectors$probe_sector_start[probeSectors$probe == probe]
    probeEnd <- probeSectors$probe_sector_end[probeSectors$probe == probe]
    probeChr <- probeSectors$chr[probeSectors$probe == probe] %>% as.character()
    
    circos.link(grsChr, c(grsStart, grsEnd), probeChr, c(probeStart, probeEnd), col = "#d4526a")
  }
}
dev.off()


load('circos_chap5.Rdata')


setEPS()
postscript("circos-networks.eps", height = 6, width = 5)
par(mfrow = c(6, 5))
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
  
  for (probe in probes) {
    probeStart <- probeSectors$probe_sector_start[probeSectors$probe == probe]
    probeEnd <- probeSectors$probe_sector_end[probeSectors$probe == probe]
    probeChr <- probeSectors$chr[probeSectors$probe == probe] %>% as.character() %>% as.numeric()
    
    circos.link(grsChr, c(grsStart, grsEnd), probeChr, c(probeStart, probeEnd), col = "#ecc771")
  }
}
dev.off()

