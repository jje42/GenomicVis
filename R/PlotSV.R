#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @import grid
PlotSv <- function(chr, svs, snps) {
  chr.pal <- chromosome.palette(chr, svs)
  if (nrow(svs) > 0) {
    message(sprintf("Found %d SVs to plot", nrow(svs)))
    for (i in 1:nrow(svs)) {
      tmp <- svs[i, ]
      begin <- tmp$Pos1
      end <- tmp$Pos2
      chr1 <- as.character(tmp$Chr1)
      chr2 <- as.character(tmp$Chr2)
      pos1 <- tmp$Pos1
      pos2 <- tmp$Pos2
      type <- if (is.null(tmp$Type)) 'Unknown' else tmp$Type
      score <- tmp$Support

      # There is consistently (i.e., happens in virtually all BreakDancer
      # output files) situations where the type is not CTX but the breakpoints
      # are on different chromosomes.  This can happen with any SV type.

      if (type != 'CTX' && chr1 != chr2) {
	warning(sprintf("weird variant: type is %s but chroms are %s and %s", 
	    type, chr1, chr2))
	next
      }

      if (type != 'CTX') {
	width <- end - begin
	if (width < 1) {
	  warning(sprintf("SV with width of %s %s", width, type))
	  print(tmp)
	}

	colour <- if (type == 'DEL')
	  'blue'
	else if (type == 'ITX')
	  'grey'
	else if (type == 'INS')
	  'red'
	else if (type == 'INV')
	  'yellow'
	else if (type == 'DUP')
	  'orange'
	else
	  'black'

	lwd <- log(score, base = 2) / 2
	gp <- gpar(col = colour, lwd = lwd)

	xs <- c(begin, begin, end, end)
	ys <- c(0, 0.5, 0.5, 0)
	grid.bezier(xs, ys, default.units = 'native', gp = gp)
      } else {
	# Is a CTX variant.
	ct <- if (chr1 == chr) chr2 else chr1
	colour <- chr.pal[[ct]]
	lwd <- log(score, base = 2) / 2
	gp = gpar(col = colour, lwd = lwd)

	if (chr1 == chr) {
	  if (chr1 < chr2) {
	    xs <- c(begin, begin, max(snps$Position) * 1.5, max(snps$Position) * 1.5)
	    ys <- c(0, 0.75, 0.75, 0.5)
	    grid.bezier(xs, ys, default.units = 'native', gp = gp)
	  } else {
	    xs <- c(-1e6, -1e6, begin, begin)
	    ys <- c(0.5, 0.75, 0.75, 0)
	    grid.bezier(xs, ys, default.units = 'native', gp = gp)
	  }
	} else {
	  if (chr1 < chr2) {
	    # xs <- c(min(x$Position) * 0.5, min(x$Position) * 0.5, end, end)
	    xs <- c(-1e6, -1e6, end, end)
	    ys <- c(0.5, 0.75, 0.75, 0)
	    grid.bezier(xs, ys, default.units = 'native', gp = gp)
	  } else {
	    xs <- c(end, end, max(snps$Position) * 1.5, max(snps$Position) * 1.5)
	    ys <- c(0, 0.75, 0.75, 0.5)
	    grid.bezier(xs, ys, default.units = 'native', gp = gp)
	  }
	}

      }
    }
  } else {
    message("no structural variants")
  }
}
