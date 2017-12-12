## r stuff for xshift
## readr::read_csv for speed
source('00-fns.R')


## read in fcs data after import (not necessary)
# dat <- as.data.frame(readr::read_csv('data/data.csv'))

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


pdf('pl1.pdf', width = 10, height = 12)
par(mfrow = c(3,2))
fdl(datm, 'ClusterID', FALSE)
fdl(datm, 'Group')

fdl(datm, 'CD3')
fdl(datm, 'CD8a')
fdl(datm, 'CD4')

fdl(datm, NULL, legend = list(title = 'CD3/CD8+, CD4-'))
## choose groups to highlight manually
# km(datm)
## groups 6, maybe 16, look like CD8+ t-cells
km(datm, groups = c(6, 16), spider = FALSE, col = 'red')
legend('topright', legend = 'CD3/CD8+\nCD4-', col = 'red', lty = 2L)

# fdl(datm, 'PD')

abline(h = grconvertY(1:2 / 3, 'ndc'), v = grconvertX(.5, 'ndc'), xpd = NA)
dev.off()


wh <- c('PD', 'Tim', 'LAG', 'CTLA4', 'ICOS',
        '4-1BB', 'OX40', 'Ki-67', 'HLA', 'CD38')
pdf('pl2.pdf', width = 10, height = 4)
par(mfrow = c(1,2))
for (ii in wh) {
  fdl_split(datm, ii)
  abline(v = grconvertX(0.5, 'ndc'), xpd = NA)
}
dev.off()
