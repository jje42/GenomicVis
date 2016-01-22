#' Find gene in a genomic interval
#'
#' This is an internal helper function for \code{cnv.heatmap}.
#'
#' @param chr chr
#' @param begin begin
#' @param end end
#' @param genes.gr genes.gr
#' @return A character containing a list of gene symbols (note it's not a
#' character vector, well it's a character vector of length 1).
#' @author Lutz Krause <lutz.krause@@qimrberghofer.edu.au>
#' @import GenomicRanges
#' @import IRanges
#' @import S4Vectors

genes.in.region <- function(chr, begin, end, genes.gr) {
  if (is.numeric(chr))
    chr <- as.character(chr)
  gr <- GRanges(
    seqnames = Rle(chr), 
    ranges = IRanges(start = begin, end = end)
  )
  overlap <- findOverlaps(gr, genes.gr)
  query.hits <- queryHits(overlap)
  map.hits <- subjectHits(overlap)  
  genes <- rep("", length(chr))

  for (i in unique(query.hits)) {    
    index <- which(query.hits == i)
    hits <- map.hits[index]
    symbol <- genes.gr[hits, ]$symbol
    genes[i] <- paste(symbol, collapse = "; ")
  }

  genes
}
