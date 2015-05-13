#' @import grid
#' @import IRanges
PlotLRR <- function(snps, segments.gr) {
  grid.lines(c(0, 1), c(0.5, 0.5), gp = gpar(col = 'grey'))

  grid.points(snps$Position, snps$LRR, pch = '.', gp = gpar(fill = 'black'))
  # grid.xaxis()
  grid.text('LRR', -0.05, 0.5, rot = 90)

  for (i in 1:length(segments.gr)) {
    begin <- start(segments.gr[i])
    end <- end(segments.gr[i])
    copy <- segments.gr[i]$copy
    lrr <- segments.gr[i]$LRR
    grid.lines(
      c(begin, end), 
      c(lrr, lrr), 
      default.units = 'native',
      gp = gpar(col = 'red', lwd = 2)
    )
  }
}
