#' Prepare data for plotting.
#'
#' This is an internal helper function for \code{cnv.plot}.
#'
#' The three main \code{data.frame}s are subset to only include the reqested
#' chromosome and a \code{GRange} object is created from the segmentation
#' data.
#'
#' @param chr chr
#' @param snps snps
#' @param segments segments
#' @param svs svs
#'
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @import GenomicRanges
#' @import IRanges
prepare.data <- function(chr, snps, segments, svs) {
  snps <- snps[snps$Chr == chr, ]
  snps <- snps[!is.na(snps$BAF), ]
  snps <- snps[!is.na(snps$LRR), ]

  snps$LRR[snps$LRR < -1.5] <- -1.5
  snps$LRR[snps$LRR > 1.5] <- 1.5

  svs <- svs[svs$Chr1 == chr | svs$Chr2 == chr, ]

  segments <- segments[segments$Chr == chr, ]
  segments[segments$Copy > 7, ]$Copy  <- 7
  copy <- segments$Copy

  foo <- Rle(copy)
  snps.gr <- GRanges(Rle(snps$Chr), IRanges(start = snps$Position, width = 1),
    BAF = snps$BAF, LRR = snps$LRR)

  segments.gr <- GRanges(Rle(chr, length(runLength(foo))),
    IRanges(start = segments[start(foo), ]$Begin, end = segments[end(foo), ]$End),
    copy = runValue(foo)
  )
  segments.gr$LRR <- NaN

  # FIXME: This is the costly part of this function.

  # for (i in 1:length(segments.gr)) {
  #   segments.gr[i]$LRR <- mean(subsetByOverlaps(snps.gr, segments.gr[i])$LRR)
  # }

  ol <- findOverlaps(snps.gr, segments.gr)
  func <- function(i) mean(snps.gr[queryHits(ol)[subjectHits(ol) == i]]$LRR)
  segments.gr$LRR <- sapply(sort(unique(subjectHits(ol))), func)

  list(
    snps = snps, 
    segments = segments, 
    svs = svs,
    snps.gr = snps.gr,
    segments.gr = segments.gr
  )
}
