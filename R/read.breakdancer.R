#' Illustrative function to read the output of Breakdancer.
#'
#' This function reads a Breakdancer output file and returns a
#' \code{data.frame} suitable as input to \code{cnv.plot}.  This only serves
#' as an example, Breakdancer output is not required to run \code{cnv.plot},
#' the structural variant data may come from any source.
#'
#' @param f The Breakdancer output file name
#' @param chrom The chromosome to process
#' @param normal.regex A regular expression used on the input file names to
#' identify the normal sample.
#' @param tumour.regex A regular expression used on the input file names to
#' identify the tumour sample.
#' @return A \code{data.frame} suitable as input to \code{cnv.plot}
#' @examples
#' \dontrun{
#' read.breakdancer()
#' }
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @import data.table
#' @export
read.breakdancer <- function(f, 
                             chrom = NA, 
                             normal.regex = 'NORMAL', 
                             tumour.regex = 'TUMOUR') {
  peek <- readLines(f, n = 100)
  skip <- sum(grepl("^#", peek)) - 1

  t0 <- Sys.time()
  sv <- read.table(
    f,
    skip = skip,
    comment.char = '$',
    header = TRUE,
    fill = TRUE,
    stringsAsFactors = FALSE
  )
  names(sv) <- sub("^X\\.", "", names(sv))

  sv$Chr1 <- as.character(sv$Chr1)
  sv$Chr2 <- as.character(sv$Chr2)

  if (! is.na(chrom))
    sv <- sv[sv$Chr1 == chrom | sv$Chr2 == chrom, ]

  func <- function(i, regex) {
    ii <- strsplit(i, ':')[[1]]
    j <- ii[grep(regex, ii)]
    if (length(j) == 0L)
      0
    else
      strsplit(j, "\\|")[[1]][2]
  }

  sv$Normal <- as.numeric(sapply(sv$num_Reads_lib, func, normal.regex))
  sv$Tumour <- as.numeric(sapply(sv$num_Reads_lib, func, tumour.regex))
  sv
}
