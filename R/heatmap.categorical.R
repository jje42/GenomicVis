# Author: Jonathan Ellis <jonathan.ellis@qimrberghofer.edu.au>
# Created: 27 Jun 2014
# Last modified: 27 Jun 2014

heatmap.categorical <- function(x, col, colormap, margins = c(5, 7), yaxis = TRUE) {

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  if (missing(col)) 
    col <- c("green", "darkblue", "gold", "black", "red", "yellow")
  if (missing(colormap))
    colormap <- data.frame(Cat = unique(unlist(x)), Col = col[1:length(unique(unlist(x)))])
  nrow <- dim(x)[1]
  ncol <- dim(x)[2]
  n <- length(unique(x))
  col <- col[1:n]
  if (! yaxis) 
    margins[2] <- 2
  par(mar = c(margins[1], 2, 2, margins[2]))
  plot(0, xlim = c(1, ncol + 1), ylim = c(1, nrow + 1), axes = FALSE, col = "white", xlab = "")
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      v <- as.character(x[i, j])
      c <- as.character(colormap[colormap[, 1] == v, 2])
      rect(j, i, j + 1, i + 1, col = c)
    }
  }
  axis(1, 1:ncol + 0.5, colnames(x), las = 2, tick = FALSE, cex.axis = 1.2)
  if (yaxis) 
    axis(4, 1:nrow + 0.5, rownames(x), las = 2, tick = FALSE)
}
