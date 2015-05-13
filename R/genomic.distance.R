#' @import GenomicRanges
genomic.distance <- function(snv) {
  # calculate the genetic distance of SNVs
  pos <- c(0, start(snv))
  length(pos) <- length(pos) - 1
  imd <- start(snv) - pos
  idx <- start(seqnames(snv))
  imd[idx] = -1
  imd    
}
