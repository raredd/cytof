## r stuff for xshift
## readr::read_csv for speed
source('fns.R')


## read in fcs data after import
dat <- as.data.frame(readr::read_csv('data/data.csv'))

## read in cluster data after clustering is complete
clust <- as.data.frame(readr::read_csv('data/cluster.csv'))
clust <- within(clust, {
  Filename <- `File Name`
  Group <- gsub('^(.)|.', '\\U\\1', Filename, perl = TRUE)
})

## read in force-directed layout coordinates after FDL is complete
coords <- as.data.frame(readr::read_delim('data/coords.csv', ';'))

coords <- within(coords, {
  ## data reflected about y-axis on mac but not windows?
  Y <- Y * -1
})


datm <- merge(coords, clust, by = c('Filename', 'EventID'))


pdf('~/desktop/pl1.pdf', width = 8, height = 4)
par(mfrow = c(1,2))
fdl(datm)
fdl(datm, 'Group')
dev.off()

pdf('~/desktop/pl2.pdf', width = 8, height = 8)
par(mfrow = c(2,2))
fdl(datm, 'CD3')
fdl(datm, 'CD8a')
fdl(datm, 'CD4')
fdl(datm, 'PD')
abline(h = grconvertY(.5, 'ndc'), v = grconvertX(.5, 'ndc'), xpd = NA)
dev.off()


wh <- c('PD', 'Tim', 'LAG', 'CTLA', 'ICOS',
        'BB', 'OX', 'Ki', 'HLA', 'CD38')
pdf('~/desktop/pl3.pdf', width = 8, height = 4)
par(mfrow = c(1,2))
for (ii in wh) {
  fdl_split(datm, ii)
  abline(v = grconvertX(0.5, 'ndc'), xpd = NA)
}
dev.off()
