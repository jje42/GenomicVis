
find.kataegis.helper <- function(chr, x, min.snvs=6, max.mean.distance=1000) {
  seq.levels <- GenomeInfoDb::seqlevels(x)
  x <- x[GenomeInfoDb::seqnames(x) == chr]
  imd <- x$imd
  n <- length(imd)
  r <- data.frame(i = rep(NA, n), j = rep(NA, n))
  idx <- 1
  for (i in 1:length(imd)) {
    for (j in length(imd):i) {
      xs <- imd[i:j]
      if (length(xs) >= min.snvs & mean(xs) <= max.mean.distance) {
	r[idx, ] <- c(start(x[i]), end(x[j]))
	idx <- idx + 1
	break
      }
    }
  }
  r <- r[!is.na(r$i), ]
  g <- GenomicRanges::GRanges(
    seqnames = S4Vectors::Rle(chr, nrow(r)),
    ranges = IRanges::IRanges(start = r$i, end = r$j)
  )
  GenomeInfoDb::seqlevels(g) <- seq.levels
  reduceWithin(g)
}

