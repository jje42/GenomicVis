#' Print function for kataegisList
#'
#' Print function for kataegisList
#'
#' What is a kataegis plot and how is kataegis defined?
#'
#' @param x a \code{kataegisList} produced by \code{kataegis}
#' @param ... passed to what?
#' @return none
#' @author Jonatha Ellis <jonathan.j.ellis@@gmail.com>
#' @examples
#' \dontrun{
#' vcf.file <- system.file('extdata', 'example.vcf', 
#'   package = 'GenomicVis')
#' x <- kataegis(vcf.file)
#' print(x)
#' }
#' @keywords methods
print.kataegisList <- function(x, ...) {
  print(sprintf("KataegisList with %d SNVs and %d kataegis loci", 
      length(x$snvs), length(x$kataegis)))
  print(x$snvs)
  print(x$kataegis)
}
