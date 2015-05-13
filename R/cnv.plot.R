#' Create a copy number and structural variant plot.
#'
#' Creates a figure containing structural variants (large INDELs,
#' translocations), B allele frequency, copy number changes and log R
#' ratios.  Each of these data types is supplied as a \code{data.frame} so
#' the function is agnostic about the source of the data.
#'
#' While the function is agnostic about where the data comes from, the
#' \pkg{GenomicVis} package provides functions and examples to obtain data
#' from Illumina SNP-Chips that have been processed with GenomeStudio and
#' GAP and structural variants from BreakDancer.  Ultimately, however, this
#' data may come from any source (or even multiple sources) so long as it is
#' presented to the function via the three standadised \code{data.frame}s.
#' 
#' \describe{
#' \item{foo}{a foo bar}
#' }
#' 
#' @param chr The chromosome to process
#' @param snps A \code{data.frame} containing the following columns Chr LRR BAF
#' Position
#' @param segments A \code{data.frame} containing the following columns Chr
#' Copy LRR
#' @param svs A \code{data.frame} containing the following columns Chr1 Chr2
#' Pos1 Pos2 Type Support
#' @return none
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @examples
#' data(SNPExample)
#' data(CNVExample)
#' data(SVExample)
#' cnv.plot('18', SNPExample, CNVExample, SVExample)
#' @export
cnv.plot <- function(chr, snps, segments, svs) {
  res <- prepare.data(chr, snps, segments, svs)
  snps <- res$snps
  segment <- res$segments
  svs <- res$svs
  segments.gr <- res$segments.gr

  grid.newpage()

  vp <- viewport(x = 0.5, y = 0.55, width = 0.8, height = 0.85, 
    xscale = c(1, max(snps$Position)))
  pushViewport(vp)
  grid.text(sprintf("Chromosome %s", chr), x = 0.5, y = 0.95, 
    gp = gpar(fontsize = 14))

  grid.xaxis(name = 'xaxis')
  at = seq(0, max(snps$Position), length.out = 6)
  grid.edit(gPath('xaxis'), at = at)
  grid.edit(gPath('xaxis::labels'), label = format(signif(at, 1) / 1000, 
      scientific = FALSE))

  grid.text('kb', y = unit(-3, 'lines'), gp = gpar(fontsize = 14))

  # Structural variants ---------------------------------------------

  vp1 <- viewport(x = 0.5, y = 0.875, width = 1.0, height = 0.25, 
    xscale = c(1, max(snps$Position)), yscale = c(0, 1), clip = 'on')
  pushViewport(vp1)
  PlotSv(chr, svs, snps)
  upViewport()

  # BAF -------------------------------------------------------------

  vp2 <- viewport(x = 0.5, y = 0.625, width = 1.0, height = 0.25, 
    xscale = c(1, max(snps$Position)), yscale = range(snps$BAF))
  pushViewport(vp2)
  PlotBAF(snps)
  upViewport()

  # Copy number -----------------------------------------------------

  vp3 <- viewport(x = 0.5, y = 0.375, width = 1.0, height = 0.25, 
    xscale = c(1, max(snps$Position)), yscale = c(0, 1))
  pushViewport(vp3)
  PlotCNA(segments.gr)
  upViewport()

  # LRR -------------------------------------------------------------

  vp4 <- viewport(x = 0.5, y = 0.125, width = 1.0, height = 0.25, 
    xscale = c(1, max(snps$Position)), 
    yscale = c(-max(abs(snps$LRR)), max(abs(snps$LRR))))
  pushViewport(vp4)
  PlotLRR(snps, segments.gr)

  invisible(res)
}
