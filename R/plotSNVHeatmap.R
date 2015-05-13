# Author: Jonathan Ellis <jonathan.ellis@qimrberghofer.edu.au>
# Created: 30 Jun 2014
# Last modified: 30 Jun 2014

plotSNVHeatmap <- function(x, margins = c(5, 7), y.cex.axis = 1.0) {
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  sum <- apply(x, 1, sum)
  oc <- apply(x, 1, paste, collapse = ".")
  x <- x[order(paste(sum, oc), decreasing = FALSE), ]
  nrow <- nrow(x)
  ncol <- ncol(x)
  x <- t(x)
  par(mar = c(margins[1], 2, 2, margins[2]))
  image(
    x = 1:ncol,
    y = 1:nrow,
    z = x, 
    xlab = "", 
    ylab = "", 
    col = c("darkblue", "gold"), 
    axes = FALSE
  )

  axis(1, 1:ncol, rownames(x),las = 2, tick = FALSE, cex.axis = 1.2)
  axis(4, 1:nrow, colnames(x), las = 2, tick = FALSE, cex.axis = y.cex.axis)

  for(i in 0:ncol) 
    abline(v = i + 0.5, col = "black")

  for (i in 0:nrow) 
    abline(h = i + 0.5, col = "black")
}
