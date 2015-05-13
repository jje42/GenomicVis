#' Read VCF
#' 
#' Read a VCF file and return the called SNVs as a \code{GRanges} instance.
#' Only SNVs with a single ALT allele are returned, and, by default, all
#' filtered SNVs are excluded from the returned value.
#'
#' details
#'
#' @param f A \code{TabixFile} instance or character() name of the VCF file to
#' be processed.
#' @param genome A genome identifier.
#' @param exclude.filtered filt
#' @param read.info Read INFO field, default is to ignore the INFO field.  Set
#' this to \code{TRUE} if INFO fields are required in the filter function (see
#' Details).
#' @param read.geno Read genotypes from VCF, the default is to ignore the
#' genotypes. Set this to \code{TRUE} if genotypes are needed in the filter
#' function (see Details).
#' @param filter.func a function used to filter variants from the returned
#' \code{GRanges}
#' @return A \code{VCF} instance from the \pkg{VariantAnnotation} package. 
#' @export
#' @import VariantAnnotation 
#' @import GenomicRanges 
#' @import IRanges
#' @examples
#' \dontrun{
#' # To simply read a VCF excluding filtered variants use
#' x <- read.vcf(f, 'hg19', read.info = TRUE, filter.func = func)
#'
#' # For more complicated filtering you can pass a filter function.  This
#' # example assuming the VCF has been annotated with SnpEff and we are only
#' # interested in variants that have not been filtered and have a SnpEff 
#' # impact of HIGH or MODERATE.
#'
#' func <- function(vcf) {
#'   x <- rowData(vcf)
#'   x[unlist(lapply(info(vcf)$EFF, 
#'     function(x) any(grepl('(HIGH)|(MODERATE)', x))))]
#' }
#'
#' f <- system.file('extdata', 'example.vcf', package = 'GenomicVis')
#' x <- read.vcf(f, 'hg19', read.info = TRUE, filter.func = func)
#' }
read.vcf <- function(f, 
                     genome, 
                     exclude.filtered = TRUE, 
                     read.info = FALSE, 
                     read.geno = FALSE, 
                     filter.func = NULL) {
  # FIXME: There must be a better way to do this!
  params <- if (read.info == FALSE & read.geno == FALSE)
    ScanVcfParam(info = NA, geno = NA)
  else if (read.info == FALSE & read.geno == TRUE)
    ScanVcfParam(info = NA)
  else if (read.info == TRUE & read.geno == FALSE)
    ScanVcfParam(geno = NA)
  else
    ScanVcfParam()

  vcf <- readVcf(f, genome, params)

  if (is.function(filter.func))
    vcf <- filter.func(vcf) 

  # if (class(x) != 'GRanges') 
  #   stop('read.vcf: filter.func returned a non-GRanges object')

  if (exclude.filtered)
    vcf <- vcf[fixed(vcf)$FILTER %in% c('PASS', '.')]
  
  # Remove non-SNVs (i.e., anything where the REF or ALT sequence is more than
  # 1 base) and variants with multiple alternative alleles.

  vcf <- vcf[width(ref(vcf)) == 1]
  vcf <- vcf[elementLengths(alt(vcf)) == 1]
  vcf <- vcf[width(unlist(alt(vcf))) == 1]
  vcf
}
