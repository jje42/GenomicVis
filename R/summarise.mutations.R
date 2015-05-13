#' Summarise the types of mutations in a region of kataegis.
#' 
#' Summarise the types of mutations in a region of kataegis.
#'
#' @param x a \code{kataegisList} object, usually created by \code{kataegis}.
#' @param pos the index of the kataegis region in \code{x} to summarise.
#' @return returns a contingency table summarising the types of mutation found
#' in the specified region of kataegis.
#' @examples
#' \dontrun{
#' x <- kataegis(system.file('extdata', 'example.vcf', package = 'GenomicVis')
#' }
#' data(KataegisExample)
#' summarise.mutations(KataegisExample, pos = 2)
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @export
#' @import GenomicRanges
summarise.mutations <- function(x, pos = 1) {
  y <- subsetByOverlaps(x$snvs, x$kataegis[pos])
  table(paste(y$REF, unlist(y$ALT), sep = '>'))
}
