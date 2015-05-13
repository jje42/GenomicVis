#' @import GenomicRanges
reduceWithin <- function(x) {
  # Remove any range that is completely contained within another range, but do
  # not remove (or merge) overlapping ranges.  This is an order of magnitude
  # slower than a simple reduce().
  x[is.na(findOverlaps(x, type = 'within', ignoreSelf = TRUE, select = 'arbitrary'))]
}
