#' @import grid
PlotBAF <- function(snps) {
  grid.text('BAF', -0.05, 0.5, rot = 90)
  grid.lines(c(0, 1), c(0.5, 0.5), gp = gpar(col = 'grey'))
  grid.points(snps$Position, snps$BAF, pch = '.', gp = gpar(fill = 'black'))
}
