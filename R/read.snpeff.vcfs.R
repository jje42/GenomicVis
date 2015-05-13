#' Illustrative function to read multiple VCFs
#'
#' Illustrative function to read multiple VCFs and create an appropriate
#' input \code{data.frame} for \code{snv.heatmap}.
#'
#' @param vcf.files vcf.files
#' @param genome genome
#' @param sample.names sample.names
#' @return A \code{data.frame} suitable as input to \code{snv.heatmap}.
#' @export
read.snpeff.vcfs <- function(vcf.files, genome, sample.names = NULL) {
  if (is.null(sample.names))
    sample.names <- basename(vcf.files)

  func <- function(i) 
    read.snpeff.vcf(vcf.files[i], genome, sample.names[i])

  do.call(rbind, lapply(1:length(vcf.files), func))
}
