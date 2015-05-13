#' Calculate regions of kataegis from a VCF file.
#'
#' Identify regions of kataegis.  The input \code{GRanges} instance should
#' only contain SNV (i.e., no INDELs) that have a single ALT allele.  The
#' input \code{GRanges} instance must also contain a valid \code{Seqinfo}
#' class that gives the lengths of the chromosomes.  The easiest way to
#' obtain this from a VCF file is via the \code{read.vcf} function from this
#' package.  
#'
#' Details ...
#'
#' @param x A \code{GRanges} instance containing SNVs.
#' @param min.snvs the minimum number of SNVs in a kataegis region
#' @param max.mean.distance the minimum mean genetic distance in a kataegis
#' region.
#' @param pcf Use piece-wise constant curve fitting algorithm to identify
#' kataegis. See `Details` below.
#' @param maxseg maximum segments in PCF algorithm, see Details.
#' @param maxk maximum length of segments in PCF algorithm, see Details.
#' @param ncpus Run the function in parallel using this number of CPUs.
#' @return A \code{GRanges} object representing the regions of kataegis
#' identified
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @examples
#' \dontrun{
#' vcf.file <- system.file('extdata', 'example.vcf', 
#'   package = 'GenomicVis')
#' vcf <- read.vcf(vcf.file, 'GRCh37')
#' x <- rowData(vcf)
#' kat <- kataegis(x)
#' }
#' @export
#' @keywords methods
kataegis <- function(x,
                     min.snvs = 6, 
                     max.mean.distance = 1000, 
                     pcf = FALSE, 
		     maxseg = NA,
		     maxk = NA,
                     ncpus = getOption('cl.cores', 1)) {
  # currently ncpus will only work with pcf = FALSE.
  # min.snvs and max.mean.distance define the definition of kataegis
  # x <- read.vcf(vcf.file)
  x$imd <- genomic.distance(x)

  k <- if (pcf)
    find.kataegis.pcf(x, min.snvs, max.mean.distance, maxseg, maxk, 
      ncpus = ncpus)
  else
    find.kataegis(x, min.snvs, max.mean.distance, ncpus = ncpus)

  # r <- list(snvs = x, kataegis = k)
  # class(r) <- "kataegisList"
  # r
  k
}
