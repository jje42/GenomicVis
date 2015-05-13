#' Illustrative function to an Illumina GenomeStudio file.
#'
#' This function is an example of reading the output of GenomeStudio and
#' producing a \code{data.frame} required by \code{cnv.plot}.
#'
#' The input file is the output (i.e., final report) from GenomeStudio.  It
#' must contain at least six columns with the SNP name, sample ID, chromosome,
#' position B allele frequency and log R ratio in it.  The exact column name
#' must match the parameters \code{chr.name}, \code{pos.name}, \code{baf.name}
#' and \code{lrr.name} respectively.
#'
#' @param file The GenomeStudio file to process.
#' @param snp.name The name of the SNP ID column.
#' @param sample.name The name of the sample ID column.
#' @param chr.name The name of the chromosome column.
#' @param pos.name The name of the SNP position column.
#' @param baf.name The name of the B allele frequency column.
#' @param lrr.name The name of the log R ratio column.
#' @return A \code{data.table} instance with six columns: \code{SNP.Name},
#' \code{Sample.ID}, \code{Chr}, \code{Position}, \code{BAF} and \code{LRR}.
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @examples
#' \dontrun{
#' x <- read.illumina('LC1_TUMOUR_A_FinalReport.txt')
#' }
#' @export
#' @import data.table
read.illumina <- function(file,
			  snp.name    = 'SNP Name',
			  sample.name = 'Sample ID',
                          chr.name    = 'Chr',
                          pos.name    = 'Position',
                          baf.name    = 'B Allele Freq',
                          lrr.name    = 'Log R Ratio') {
  illum <- fread(file)
  cols <- c(snp.name, sample.name, chr.name, pos.name, baf.name, lrr.name)
  if (!all(cols %in% colnames(illum)))
    stop("read.illumina: missing column names")
  illum <- illum[, cols, with = FALSE]
  setnames(
    illum, 
    c(snp.name, sample.name, chr.name, pos.name, baf.name, lrr.name), 
    c('SNP.Name', 'Sample.ID', 'Chr', 'Position', 'BAF', 'LRR')
  )
  illum
}
