#' Cluster samples based on SNVs.
#'
#' Perform hierarchical clustering of samples based on their somatic SNVs. 
#'
#' Each vcf file should contain called somatic SNVs.  use exclude.filtered =
#' TRUE (the default) to only use SNVs with 'PASS' or '.' in the FILTER column.
#'
#' @param vcf.files A character vector containing the paths to the VCF files to
#' process.
#' @param genome A character string identifying the genome
#' @param sample.names A character vector of samples names corresponding to
#' each VCF file, by default the file names of the VCFs will be used as sample
#' names.
#' @param ... Additional parameters pass to \code{read.vcf}.  These can be used
#' to apply additional filtering of the SNVs, see the documentation of
#' \code{read.vcf} for further details.
#' @return Invisible returns an object of class \code{hclust} that describes
#' the tree produced by the clustering process.
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @examples
#' vcf.files <- c(
#'   system.file('extdata', 'LC1_TUMOUR_A.vcf', package = 'GenomicVis'),
#'   system.file('extdata', 'LC1_TUMOUR_B.vcf', package = 'GenomicVis'),
#'   system.file('extdata', 'LC2_TUMOUR_A.vcf', package = 'GenomicVis'),
#'   system.file('extdata', 'LC2_TUMOUR_B.vcf', package = 'GenomicVis'),
#'   system.file('extdata', 'LC3_TUMOUR_A.vcf', package = 'GenomicVis'),
#'   system.file('extdata', 'LC3_TUMOUR_B.vcf', package = 'GenomicVis'))
#' sample.names <- tools::file_path_sans_ext(basename(vcf.files))
#' snv.clustering(vcf.files, sample.names, genome = 'hg19')
#' @import VariantAnnotation
#' @export
snv.clustering <- function(vcf.files, genome, sample.names = NULL, ...) {

  if (is.null(sample.names)) 
    sample.names <- basename(vcf.files)

  read.vcffiles <- function(vcf.files, sample.names, genome, ...) {
    x <- lapply(vcf.files, function(f) names(rowData(read.vcf(f, genome, ...))))
    names(x) <- sample.names
    x
  }

  x <- read.vcffiles(
    vcf.files,
    sample.names = sample.names, 
    genome, 
    ...
  )

  a <- unlist(sapply(sample.names, function(i) rep(i, length(x[[i]]))))
  b <- unlist(x)
  dat <- table(b, a)
  cl <- hclust(dist(t(dat)))
  plot(cl, hang = -1, xlab = '', ylab = '', sub = '', main = '', axes = FALSE)
  invisible(cl)
}

