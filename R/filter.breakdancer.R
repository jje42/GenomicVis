#' Illustrative funtion to filter Breakdancer calls.
#'
#' This function is an example of filtering Breakdancer calls that have been
#' read with \code{read.breakdancer}.  It is illustrative only, and not
#' intended to necessarily represent the best way to filter Breakdancer
#' calls.
#'
#' @param sv A \code{data.frame} typically the returned value from
#' \code{read.breakdancer}
#' @param minTumour minimum number of reads supporting SV in the tumour
#' sample.
#' @param maxNormal maximum number of reads supporting SV in the normal
#' sample.
#' @param minRatio minimum ration between supporting reads in the tumour /
#' normal.
#' @param chr  Chromosome to process
#' @param conf Minimum confidence (score) required.
#' @return The filtered \code{data.frame}
#' @author Lutz Krause <lutz.krause@@qimrberghofer.edu.au>
#' @export
filter.breakdancer <- function(sv,
                               minTumour = 10,
                               maxNormal = 4,
                               minRatio = 5,
                               chr = NA,
                               conf = 95) {
   if (! "Tumour" %in% names(sv))
     stop("ERROR: Tumour not defined in dataframe")
   if (! "Normal" %in% names(sv))
     stop("ERROR: Normal not defined in dataframe")

   sv <- sv[sv$Tumour >= minTumour, ]
   sv <- sv[sv$Normal <= maxNormal, ]
   sv <- sv[sv$Score >= conf, ]

   if (! is.na(chr))
     sv <- sv[sv$Chr1 == chr | sv$Chr2 == chr, ]

   ratio <- sv$Tumour / sv$Normal
   sv <- sv[ratio >= minRatio, ]
   sv$Support <- sv$Tumour
   return(sv)
}
