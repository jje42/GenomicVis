#' Create a venn diagram of SNV overlap in a group of VCF files.
#'
#' There is a maximum number of samples (nine) that can be included in a venn
#' diagram (this limit is imposed by the R package producing the figures).
#'
#' @param vcf.files A character vector of VCF file to process.
#' @param sample.names A character vector of samples names for each of the VCF
#' files.  Must be the same length as \code{vcf.files}.  By default the VCF
#' files names are used as sample names.
#' @param genome A character string identifying the genome
#' @param ... Additionally parameters to pass to \code{read.vcf}.
#' @return A list containing:
#' \item{venn}{An object of class \code{Venn}, see documentation of package
#' \code{Vennerable} for further details and methods.}
#' \item{data}{The data list that was used to create the \code{Venn} instance.}
#' @examples
#' \dontrun{
#' f1 <- system.file('extdata', 'LC1_TUMOUR_A.vcf', package = 'GenomicVis')
#' f2 <- system.file('extdata', 'LC1_TUMOUR_B.vcf', package = 'GenomicVis')
#' vcf.files <- c(f1, f2)
#' sample.names <- c('LC1_A', 'LC1_B')
#' v <- vcf.venn(vcf.files, 'GRCh37', sample.names)
#' plot(v$venn)
#' }
#' @import VariantAnnotation
#' @export
vcf.venn <- function(vcf.files, genome, sample.names = NULL, ...)
{
  if (length(vcf.files) > 9)
    stop("do.venn: too many VCF files (maximum is 9)")

  if (is.null(sample.names)) {
    sample.names <- basename(vcf.files)
  } else {
    if (length(vcf.files) != length(sample.names))
      stop("do.venn: length(sample.name) must equal length(vcf.files)")
  }

  x <- lapply(vcf.files, function(f) names((rowData(read.vcf(f, genome, ...)))))
  names(x) <- sample.names
  list(venn = Vennerable::Venn(x), data = x)
}
