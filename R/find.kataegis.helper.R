find.kataegis.helper <- function(chr, x) {
  seq.levels <- GenomicRanges::seqlevels(x)
  x <- x[GenomicRanges::seqnames(x) == chr]
  imd <- x$imd
  n <- length(imd)
  r <- data.frame(i = rep(NA, n), j = rep(NA, n))
  idx <- 1
  for (i in 1:length(imd)) {
    for (j in length(imd):i) {
      xs <- imd[i:j]
      if (length(xs) >= 6 & mean(xs) <= 1000) {
	r[idx, ] <- c(start(x[i]), end(x[j]))
	idx <- idx + 1
	break
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
}

