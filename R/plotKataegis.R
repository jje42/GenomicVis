#' Plot a kataegis rainfall figure.
#'
#' Create a rainfall plot showing potential regions of kataegis.  The input
#' \code{GRanges} instance should only contain SNV (i.e., no INDELs) that have
#' a single ALT allele.  The input \code{GRanges} instance must also contain a
#' valid \code{Seqinfo} class that gives the lengths of the chromosomes.  The
#' easiest way to obtain this from a VCF file is via the \code{read.vcf}
#' function from this package.  
#'
#' @param x A \code{GRanges} instance containing SNVs.
#' @param chr Create a rainfall plot for a single chromosome specified by this
#' parameter.
#' @param all all
#' @param alpha.f number in (0, 1] specifiying the alpha level with which to
#' draw the points.
#' @param colour.map A list specifying the colours to use in the figure.  See
#' \code{kataegis.colours} for exact format.
#' @param main Title for plot
#' @param newpage logical. If TRUE create figure on a new device, otherwise
#' print to current device.
#' @return None
#' @examples
#' library(VariantAnnotation)
#' vcf.file <- system.file('extdata', 'example.vcf', package = 'GenomicVis')
#' vcf <- read.vcf(vcf.file, 'GRCh37')
#' x <- rowData(vcf)
#' plotKataegis(x)
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @import GenomicRanges
#' @import IRanges
#' @export
plotKataegis <- function(x, chr = NULL, all = FALSE, alpha.f = 1.0, colour.map = kataegis.colours, main = NULL, newpage = TRUE) {

  if (is.function(colour.map))
    colour.map <- colour.map()

  # Create a variable that holds the genomic position of each SNV, that is,
  # the cumulative genomic position across all chromosomes.

  if (is.null(chr)) {
    cs <- c(0, cumsum(as.numeric(seqlengths(x))))
    length(cs) <- length(cs) - 1
    y <- split(x, seqnames(x))
    names(cs) <- names(y)
    y <- endoapply(y, function(x) {
      chrom <- runValue(seqnames(x))
      x$pos <- start(x) + cs[chrom]
      x
    })
    x <- unlist(y)
  } else {
    x <- x[seqnames(x) == chr]
    x$pos <- start(x)
  }

  x$context <- sprintf("%s>%s", as.character(x$REF), as.character(unlist(x$ALT)))
  contexts  <- c('C>T', 'C>A', 'C>G', 'T>A', 'T>G', 'T>C')
  if (all)
    contexts <- c(contexts, c('G>T', 'G>A', 'G>C', 'A>T', 'A>G', 'A>C'))

  x$imd <- genomic.distance(x)
  x = x[! x$imd == -1]

  # vp <- viewport(width = 0.95, height = 0.9)
  # pushViewport(vp)
  # grid.text('Genomic Position', y = 0.01)
  # grid.text('log(Genomic Distance)', x = -0.01, rot = 90)

  if (newpage)
    grid.newpage()

  vp1 <- viewport(
    # y = 0.55, 
    # width = 0.9, 
    # height = 0.8, 
    x = unit(5, 'lines'),
    y = unit(4, 'lines'),
    width = unit(1, 'npc') - unit(7, 'lines'),
    height = unit(1, 'npc') - unit(7, 'lines'),
    just = c('left', 'bottom'),
    xscale = c(1, max(x$pos)),
    yscale = range(log10(x$imd))
  )
  pushViewport(vp1)

  grid.text('Genomic Position', y = unit(-3, 'lines'))
  grid.text('log(Genomic Distance)', x = unit(-4, 'lines'), rot = 90)
  if (!is.null(main))
    grid.text(main, y = unit(1, 'npc') + unit(1.5, 'lines'), 
      gp = gpar(fontsize = 16))
 
  # Create grey rect for alternate chromosomes. 

  if (is.null(chr)) {
    ns <- cumsum(as.numeric(seqlengths(x)))
    imd <- log10(x$imd)
    for (i in seq(1, length(ns), 2)) {
      n <- ns[i]
      xi <- if (i > 1) ns[i - 1] else 0
      width <- n - xi
      grid.rect(
	xi, 0, width, max(imd), 
	just = c('left', 'bottom'), 
	default.units = 'native', 
	gp = gpar(col = 'white', fill = 'gray90')
	)
    }
  }

  for (context in contexts) {
    y <- x[x$context == context]
    grid.points(
      y$pos, 
      log10(y$imd), 
      pch = 19, 
      size = unit(4, 'point'), 
      gp = gpar(col = adjustcolor(colour.map[context], alpha.f = alpha.f))
    )
  }

  grid.rect()
  grid.xaxis()
  grid.yaxis()

  # legd <- legendGrob(contexts, pch = 19, gp = gpar(col = colour.map[contexts]))
  # # gg <- packGrob(packGrob(frameGrob(), gt), legd, side = 'right')
  # gg <- packGrob(frameGrob(), gt)
  # grid.draw(gt)
  upViewport()
}
