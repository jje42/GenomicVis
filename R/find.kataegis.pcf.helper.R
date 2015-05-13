find.kataegis.pcf.helper <- function(chr, x, min.snvs, max.mean.distance, 
  maxseg, maxk) 
{
  seq.levels <- GenomicRanges::seqlevels(x)
  x <- x[seqnames(x) == chr]
  n <- length(x)
  if (n >= min.snvs) {
    r <- data.frame(i = rep(NA, n), j = rep(NA, n))
    idx <- 1

    segments <- get.segments(x, maxseg = maxseg, maxk = maxk)

    for (i in 2:length(segments)) {
      j <- i - 1
      index.i <- segments[i]
      index.j <- segments[j]
      if ((index.i - index.j) >= min.snvs) {
	mean.dist <- mean(x[index.j:index.i, ]$imd)
	if (mean.dist < max.mean.distance) {
	  r[idx, ] <- c(start(x[index.j]), end(x[index.i]))
	}
      }
    }

    r <- r[!is.na(r$i), ]
    g <- GenomicRanges::GRanges(
      seqnames = IRanges::Rle(chr, nrow(r)), 
      ranges = IRanges::IRanges(start = r$i, end = r$j)
    )
    GenomicRanges::seqlevels(g) <- seq.levels
    reduceWithin(g)
  } else {
    # If there are less than min.snvs variants there can't be any kataegis
    # by definition.
    NULL
  }
}

