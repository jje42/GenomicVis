#' Read a SnpEff annotated VCF file.
#'
#' Illustrative function to read gene names from a VCF file that has been
#' annotated with SnpEff.
#'
#' @param f f
#' @param genome genome
#' @param tumour.id tumour id
#' @param exclude.filtered logical, exclude filtered variants (defined as
#' variants with some thing other that 'PASS' or '.' in their FILTER
#' column).
#' @return A \code{data.frame}.
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @export
read.snpeff.vcf <- function(f, genome, tumour.id, exclude.filtered = TRUE) {
  vcf <- readVcf(f, genome)

  if (exclude.filtered)
    vcf <- vcf[fixed(vcf)$FILTER %in% c('PASS', ''), ]

  x <- Filter(function(y) grepl("(HIGH)|(MODERATE)", y), unlist(info(vcf)$EFF))
  genes <- unique(sapply(strsplit(x, '\\|'), function(y) y[[6]]))
  n <- length(genes)
  dat <- data.frame(
    Gene = genes, 
    Tumour = rep(tumour.id, n),
    stringsAsFactors = FALSE
  )
  dat
}
